(cl:in-package :js2cl)

(define-condition javascript-exception ()
  ((value :initarg :value :reader js-exception-value)))

(define-condition javascript-debugger-statement () ())

;; TODO

(defun === (x y)
  (equal x y))

(defun == (x y)
  (equal x y))

(defun as-bool (val)
  (if val :true :false))

(defun not-bool (val)
  (if val :false :true))

(defun %not (val)
  (if (eq val :true) :false :true))

(defun %bool (val)
  (if (%%bool val) :true :false))

(defun %%bool (val)
  (not (or (eq val :false) (eq val :null) (eq val :undefined) (eql val 0) (eql val :nan) (equal val ""))))

(defun %%string (val)
  (format nil "~a" val))

(defun %typeof (val)
  (case val
    ((:true :false) "boolean")
    (:undefined "undefined")
    (:null "object")
    (t (etypecase val
         (string "string")
         (hash-table "object")
         (function "function")
         (real "number")))))

(defun %instanceof (type val)
  (declare (ignore type val))
  (error "Not implemented."))

(defun %number (val)
  (or (and (realp val) val)
      (and (equal val "") 0)
      (and (stringp val)
           (let ((*read-eval* nil)
                 (num (ignore-errors (read-from-string val))))
             (and (realp num) num)))
      :nan))

(defun %< (a b)
  (as-bool (< a b)))
(defun %> (a b)
  (as-bool (> a b)))

(in-package :js-runtime)

(defun |Date| (&rest args)
  (declare (ignore args))
  (error "Not implemented."))

(defun |alert| (value)
  (print value))