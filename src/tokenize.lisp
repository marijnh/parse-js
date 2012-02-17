(in-package #:parse-js)

(defstruct token type value line char pos newline-before comments-before)
(defun tokenp (token type value)
  (and (eq (token-type token) type)
       (eql (token-value token) value)))
(defun token-type-p (token type)
  (eq (token-type token) type))
(defun token-id (token)
  (token-value token))

(defvar *line*)
(defvar *char*)
(defvar *position*)

(define-condition js-parse-error (simple-error)
  ((line :initform *line* :reader js-parse-error-line)
   (char :initform *char* :reader js-parse-error-char)))
(defmethod print-object ((err js-parse-error) stream)
  (call-next-method)
  (format stream " (line ~a, character ~a)" (js-parse-error-line err) (js-parse-error-char err)))
(defun js-parse-error (control &rest args)
  (error 'js-parse-error :format-control control :format-arguments args))

(defparameter *operator-chars* "+-*&%=<>!?|~^")
(defparameter *operators*
  (let ((ops (make-hash-table :test 'equal)))
    (dolist (op '(:in :instanceof :typeof :new :void :delete :++ :-- :+ :- :! :~ :& :|\|| :^ :* :/ :%
                  :>> :<< :>>> :< :> :<= :>= :== :=== :!= :!== :? := :+= :-= :/= :*= :%= :>>= :<<=
                  :>>>= :~= :%= :|\|=| :^= :&= :&& :|\|\||))
      (setf (gethash (string-downcase (string op)) ops) op))
    ops))

(defparameter *whitespace-chars*
  (concatenate '(vector character) (list #\space #\tab #.(code-char 11) #\page #\return #\newline
                                         (code-char #xa0) (code-char #x2028) (code-char #x2029))))
(defparameter *line-terminators*
  (concatenate '(vector character) (list #\newline #\return (code-char #x2028) (code-char #x2029))))

(defparameter *keywords*
  (let ((keywords (make-hash-table :test 'equal)))
    (dolist (word '(:break :case :catch :continue :debugger :default :delete :do :else :false
                    :finally :for :function :if :in :instanceof :new :null :return :switch
                    :throw :true :try :typeof :var :void :while :with))
      (setf (gethash (string-downcase (string word)) keywords) word))
    keywords))
(defparameter *keywords-before-expression* '(:return :new :delete :throw :else :case))
(defparameter *atom-keywords* '(:false :null :true :undefined))
(defparameter *reserved-words-ecma-3*
  (let ((words (make-hash-table :test 'equal)))
    (dolist (word '("abstract" "enum" "int" "short" "boolean" "export" "interface" "static"
                    "byte" "extends" "long" "super" "char" "final" "native" "synchronized"
                    "class" "float" "package" "throws" "const" "goto" "private" "transient"
                    "debugger" "implements" "protected" "volatile" "double" "import" "public"))
      (setf (gethash word words) t))
    words))
(defparameter *reserved-words-ecma-5*
  (let ((words (make-hash-table :test 'equal)))
    (dolist (word '("class" "enum" "extends" "super" "const" "export" "import"))
      (setf (gethash word words) t))
    words))
(defparameter *check-for-reserved-words* nil)
(defparameter *ecma-version* 3)

(defun read-js-number (stream &key junk-allowed)
  (flet ((peek-1 () (peek-char nil stream nil nil))
         (next-1 () (read-char stream nil nil)))
    (read-js-number-1 #'peek-1 #'next-1 :junk-allowed junk-allowed)))

(defun read-js-number-1 (peek next &key junk-allowed)
  (labels ((digits (radix)
             (with-output-to-string (out)
               (loop :for ch := (funcall peek) :while (and ch (digit-char-p ch radix)) :do
                  (write-char (funcall next) out)))))
    (let ((minus (case (funcall peek) (#\+ (funcall next) nil) (#\- (funcall next) t)))
          (body (digits 10))
          (*read-default-float-format* 'double-float))
      (flet ((ret (x)
               (return-from read-js-number-1
                 (and x (or junk-allowed (eq (funcall peek) nil)) (if minus (if (eq x :infinity) :-infinity (- x)) x)))))
        (cond ((and (equal body "0") (find (funcall peek) "xX") (funcall next))
               (ret (parse-integer (digits 16) :junk-allowed t :radix 16)))
              ((find (funcall peek) ".eE")
               (let ((base (if (string= body "") 0 (parse-integer body)))
                     (expt 0) (expt-neg nil))
                 (if (and (eql (funcall peek) #\.) (funcall next))
                     (let ((digs (digits 10)))
                       (if (string= digs "")
                           (when (string= body "") (ret nil))
                           (loop (handler-case
                                     (return (incf base (/ (parse-integer digs) (expt 10d0 (length digs)))))
                                   (floating-point-overflow () (setf digs (subseq digs 0 (1- (length digs)))))))))
                     (when (equal body "") (ret nil)))
                 (when (and (find (funcall peek) "eE") (funcall next))
                   (setf expt-neg (and (find (funcall peek) "+-") (eql (funcall next) #\-)))
                   (let ((digs (digits 10)))
                     (when (equal digs "") (ret nil))
                     (setf expt (parse-integer digs))))
                 (handler-case (ret (* base (expt 10d0 (if expt-neg (- expt) expt))))
                   (floating-point-overflow () (ret :infinity))
                   (floating-point-underflow () (ret 0d0)))))
              ((equal body "") (ret nil))
              ((and (char= (char body 0) #\0)
                    (loop :for i :from 1 :below (length body) :do
                       (unless (digit-char-p (char body i)) (return nil))
                       :finally (return t)))
               (ret (parse-integer body :radix 8)))
              ((equal body "") (ret nil))
              (t (ret (parse-integer body))))))))

(defun/defs lex-js (stream &key include-comments)
  (def expression-allowed t)
  (def newline-before nil)
  (def line 1)
  (def char 0)
  (def position 0)
  (def comments-before nil)

  (def start-token ()
    (setf *line* line
          *char* char
          *position* position))
  (def token (type value)
    (setf expression-allowed
          (or (and (eq type :operator)
                   (not (member value '("++" "--") :test #'string=)))
              (and (eq type :keyword)
                   (member value *keywords-before-expression*))
              (and (eq type :punc)
                   (find value "[{(,.;:"))))
    (prog1 (make-token :type type :value value :line *line* :char *char* :pos *position*
                       :newline-before newline-before
                       :comments-before (reverse comments-before))
      (setf newline-before nil)
      (setf comments-before nil)))

  (def peek ()
    (peek-char nil stream nil))
  (def next (&optional eof-error in-string)
    (let ((ch (read-char stream eof-error)))
      (when ch
        (incf position)
        (if (find ch *line-terminators*)
            (progn
              (setf line (1+ line) char 0)
              (unless in-string (setf newline-before t)))
            (incf char)))
      ch))

  (def skip-whitespace ()
    (loop :for ch := (peek)
          :while (and ch (find ch *whitespace-chars*))
          :do (next)))
  (def read-while (pred)
    (with-output-to-string (*standard-output*)
      (loop :for ch := (peek)
            :while (and ch (funcall pred ch))
            :do (princ (next)))))

  (def read-num (&optional start)
    (let ((num (or (read-js-number-1 (lambda () (if start start (peek)))
                                     (lambda () (if start (prog1 start (setf start nil)) (next)))
                                     :junk-allowed t)
                   (js-parse-error "Invalid syntax."))))
      (token :num num)))

  (def handle-dot ()
    (next)
    (if (digit-char-p (peek))
        (read-num #\.)
        (token :punc #\.)))

  (def hex-bytes (n char)
    (loop :with num := 0
          :for pos :from (1- n) :downto 0
          :do (let ((digit (digit-char-p (next t) 16)))
                (if digit
                    (incf num (* digit (expt 16 pos)))
                    (js-parse-error "Invalid \\~a escape pattern." char)))
          :finally (return num)))
  (def read-escaped-char (&optional in-string)
    (let ((ch (next t in-string)))
      (case ch
        (#\n #\newline) (#\r #\return) (#\t #\tab)
        (#\b #\backspace) (#\v #.(code-char 11)) (#\f #\page) (#\0 #\null)
        (#\x (code-char (hex-bytes 2 #\x)))
        (#\u (code-char (hex-bytes 4 #\u)))
        (#\newline nil)
        (t (let ((num (digit-char-p ch 8)))
             (if num
                 (loop :for nx := (digit-char-p (peek) 8) :do
                    (when (or (not nx) (>= num 32)) (return (code-char num)))
                    (next)
                    (setf num (+ nx (* num 8))))
                 ch))))))
  (def read-string ()
    (let ((quote (next)))
      (handler-case
          (token :string
                 (with-output-to-string (*standard-output*)
                   (loop (let ((ch (next t)))
                           (cond ((eql ch #\\) (let ((ch (read-escaped-char t))) (when ch (write-char ch))))
                                 ((find ch *line-terminators*) (js-parse-error "Line terminator inside of string."))
                                 ((eql ch quote) (return))
                                 (t (write-char ch)))))))
        (end-of-file () (js-parse-error "Unterminated string constant.")))))

  (def add-comment (type c)
    (when include-comments
      ;; doing this instead of calling (token) as we don't want
      ;; to put comments-before into a comment token
      (push (make-token :type type
                        :value c
                        :line *line*
                        :char *char*
                        :pos *position*
                        :newline-before newline-before)
            comments-before)))

  (def read-line-comment ()
    (next)
    (if include-comments
        (add-comment :comment1
                     (with-output-to-string (out)
                       (loop :for ch := (next)
                          :until (or (find ch *line-terminators*) (not ch))
                          :do (write-char ch out))))
        (loop :for ch := (next)
           :until (or (find ch *line-terminators*) (not ch)))))

  (def read-multiline-comment ()
    (next)
    (if include-comments
        (add-comment :comment2
                     (with-output-to-string (out)
                       (loop :with star := nil
                          :for ch := (or (next) (js-parse-error "Unterminated comment."))
                          :until (and star (eql ch #\/))
                          :do
                          (setf star (eql ch #\*))
                          (write-char ch out))))
        (loop :with star := nil
           :for ch := (or (next) (js-parse-error "Unterminated comment."))
           :until (and star (eql ch #\/))
           :do (setf star (eql ch #\*)))))

  (def read-regexp ()
    (handler-case
        (token :regexp
               (cons
                (with-output-to-string (*standard-output*)
                  (loop :with backslash := nil :with inset := nil
                        :for ch := (next t) :until (and (not backslash) (not inset) (eql ch #\/)) :do
                     (unless backslash
                       (when (eql ch #\[) (setf inset t))
                       (when (and inset (not backslash) (eql ch #\])) (setf inset nil)))
                     (setf backslash (and (eql ch #\\) (not backslash)))
                     ;; Handle \u sequences, since CL-PPCRE does not understand them.
                     (if (and backslash (eql (peek) #\u))
                         (let* ((code (progn
                                        (setf backslash nil)
                                        (next)
                                        (hex-bytes 4 #\u)))
                                (ch (code-char code)))
                           ;; on CCL, parsing /\uFFFF/ fails because (code-char #xFFFF) returns NIL.
                           ;; so when NIL, we better use the original sequence.
                           (if ch
                               (write-char ch)
                               (format t "\\u~4,'0X" code)))
                         (write-char ch))))
                (read-while #'identifier-char-p)))
      (end-of-file () (js-parse-error "Unterminated regular expression."))))

  (def read-operator (&optional start)
    (labels ((grow (str)
               (let ((bigger (concatenate 'string str (string (peek)))))
                 (if (gethash bigger *operators*)
                     (progn (next) (grow bigger))
                     (token :operator (gethash str *operators*))))))
      (grow (or start (string (next))))))

  (def handle-slash ()
    (next)
    (case (peek)
      (#\/ (read-line-comment)
           (next-token))
      (#\* (read-multiline-comment)
           (next-token))
      (t (if expression-allowed
             (read-regexp)
             (read-operator "/")))))

  (def identifier-char-p (ch) (or (and (alphanumericp ch) (not (find ch *whitespace-chars*))) (eql ch #\$) (eql ch #\_)))
  (def read-word ()
    (let* ((unicode-escape nil)
           (word (with-output-to-string (*standard-output*)
                   (loop :for ch := (peek) :do
                      (cond ((eql ch #\\)
                             (next)
                             (unless (eql (next) #\u) (js-parse-error "Unrecognized escape in identifier."))
                             (write-char (code-char (hex-bytes 4 #\u)))
                             (setf unicode-escape t))
                            ((and ch (identifier-char-p ch)) (write-char (next)))
                            (t (return))))))
           (keyword (and (not unicode-escape) (gethash word *keywords*))))
      (cond ((and *check-for-reserved-words* (not unicode-escape)
                  (gethash word (ecase *ecma-version* (3 *reserved-words-ecma-3*) (5 *reserved-words-ecma-5*))))
             (js-parse-error "'~a' is a reserved word." word))
            ((not keyword) (token :name word))
            ((gethash word *operators*) (token :operator keyword))
            ((member keyword *atom-keywords*) (token :atom keyword))
            (t (token :keyword keyword)))))

  (def next-token (&optional force-regexp)
    (if force-regexp
        (read-regexp)
        (progn
          (skip-whitespace)
          (start-token)
          (let ((next (peek)))
            (cond ((not next) (token :eof "EOF"))
                  ((digit-char-p next) (read-num))
                  ((find next "'\"") (read-string))
                  ((eql next #\.) (handle-dot))
                  ((find next "[]{}(),;:") (token :punc (next)))
                  ((eql next #\/) (handle-slash))
                  ((find next *operator-chars*) (read-operator))
                  ((or (identifier-char-p next) (eql next #\\)) (read-word))
                  (t (js-parse-error "Unexpected character '~a'." next)))))))

  #'next-token)
