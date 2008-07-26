(cl:in-package :js2cl)

(defun local-var (name &optional value)
  (let* ((name (intern name *js-package*))
         (found (assoc name *locals*)))
    (if found
        (when (and value (not (cdr found)))
          (setf (cdr found) value))
        (setf *locals* (append *locals* (list (cons name value)))))))

(defun var (name)
  (intern name *js-package*))

(compiler :do (condition body)
  `(tagbody continue-here ,(compile body) (unless (%%bool ,(compile condition)) (go break-here)) (go continue-here) break-here))

(compiler :while (condition body)
  `(tagbody continue-here (unless (%%bool ,(compile condition)) (go break-here)) ,(compile body) (go continue-here) break-here))

(compiler :for-in (var name obj body)
  (when var (local-var name))
  `(tagbody
    continue-here
      (loop :for ,(var name) :being :the :hash-keys :in ,(compile obj)
            :do ,(compile body))
    break-here))

(defun label-tag (label type)
  (intern (format nil "~a-~a" type label) *js-package*))

(defun compile-for (label init test step body)
  `(tagbody 
      ,(compile init)
    continue-here
    ,@(when label (list (label-tag label :continue)))
      (unless (%%bool ,(compile test)) (go break-here))
      ,(compile body)
      ,(compile step)
      (go continue-here)
    break-here
    ,@(when label (list (label-tag label :break)))))

(compiler :for (init test step body)
  (compile-for nil init test step body))

(compiler :label (name statement)
  (if (eq (car statement) :for)
      (apply #'compile-for name (cdr statement))
      `(tagbody ,(label-tag name :continue) ,(compile statement) ,(label-tag name :break))))

(compiler :break (name)
  `(go ,(if name (label-tag name :break) 'break-here)))

(compiler :continue (name)
  `(go ,(if name (label-tag name :continue) 'continue-here)))

(compiler :return (expr)
  `(return-from function ,(compile expr)))

(compiler :switch (expr body)
  `(let ((switchval ,(compile expr)))
     (tagbody
      break-here
        (cond ,@(loop :for form :in body :for tag :from 0
                      :when (eq (car form) :case)
                      :collect `((=== switchval ,(compile (cadr form))) (go ,tag))
                      :when (eq (car form) :default)
                      :collect `(t (go ,tag))))
        ,@(loop :for form :in body :for tag :from 0
                :when (or (eq (car form) :case) (eq (car form) :default)) :collect tag
                :else :collect (compile form)))))

(compiler :if (condition then else)
  `(progn (if (%%bool ,(compile condition)) ,(compile then) ,(compile else)) nil))

(compiler :conditional (expr yes no)
  `(if (%%bool ,(compile expr) ,(compile yes) ,(compile no))))

(compiler :seq (left right)
  `(progn ,(compile left) ,(compile right)))

(compiler :throw (expr)
  `(error 'javascript-exception :value ,(compile expr)))

(compiler :try (body catch finally)
  (let ((body (compile body)))
    (when catch
      (setf body `(handler-case ,body
                    (javascript-exception (ex)
                      (let ((,(var (car catch)) (js-exception-value ex)))
                        ,(compile (cdr catch)))))))
    (when finally
      (setf body `(unwind-protect ,body ,(compile finally))))
    body))

(compiler :debugger ()
  `(error 'javascript-debugger-statement))

(compiler :stat (exp)
  (compile exp))

(compiler :block (statements)
  (cons 'progn (mapcar #'compile statements)))

(compiler :toplevel (statements)
  (let* ((*locals* ())
         (compiled (mapcar #'compile statements)))
    (values 
     `(progn ,@(loop :for (name . val) :in *locals*
                     :when val :collect `(setf ,name ,val))
             ,@compiled)
     (mapcar #'car *locals*))))

(defun compile-func (args body)
  `(lambda ,(mapcar #'var args)
     (block function
       ,(let* ((*locals* ())
               (body (mapcar #'compile body)))
          `(let ,(mapcar #'car *locals*)
             ,(loop :for (name . val) :in *locals*
                    :when val :collect `(setf ,name ,val))
             ,@body)))))

(compiler :var (defs)
  (let ((defs (loop :for (name . def) :in defs
                    :do (local-var name)
                    :when def :collect `(setf ,(var name) ,(compile def)))))
    (if (cdr defs) (cons 'progn defs) (car defs))))

(compiler :defun (name args body)
  (local-var name (compile-func args body)))

(compiler :function (name args body)
  (declare (ignore name)) ;; Too bad
  (compile-func args body))

(compiler :atom (val)
  (if (eq val :undefined) nil val))

(compiler :num (val) val)

(compiler :name (name) (var name))

(compiler :string (val) val) ;; TODO wrap

(compiler :regexp ((regexp . mods)) ;; TODO wrap, handle 'global' mode
  (create-scanner regexp :case-insensitive-mode (find #\i mods)
                  :multi-line-mode (find #\m mods)))

(compiler :array (vals)
  `(vector ,@(mapcar #'compile vals)))

(compiler :object (props)
  `(let ((hash (make-hash-table :test 'equal)))
     ,@(loop :for (name . val) :in props
             :collect `(setf (gethash ,name hash) ,(compile val)))
     hash))

(compiler :dot (expr prop)
  `(gethash ,prop ,(compile expr)))

(compiler :sub (expr sub)
  `(gethash (%%string ,(compile sub)) ,(compile expr)))

(compiler :call (expr args)
  `(funcall ,(compile expr) ,@(mapcar #'compile args)))

(compiler :unary-prefix (op expr)
  (ecase op
    (:typeof `(%typeof ,(compile expr)))
    (:void `(progn ,(compile expr) nil))
    (:-- `(decf ,(compile expr)))
    (:++ `(incf ,(compile expr)))
    (:! `(if (%%bool ,(compile expr)) :false :true))
    (:~ `(lognot ,(compile expr)))
    (:- `(- (%number ,(compile expr))))
    (:+ `(%number ,(compile expr)))
    (:delete (ecase (car expr)
               (:name `(setf ,(var (cadr expr)) nil))
               (:dot `(remhash ,(caddr expr) ,(compile (cadr expr))))
               (:sub `(remhash (%%string ,(compile (caddr expr))) ,(compile (cadr expr))))))))

(compiler :unary-postfix (op expr)
  (let ((compiled (compile expr)))
    (ecase op
      (:-- `(prog1 ,compiled (decf ,compiled)))
      (:++ `(prog1 ,compiled (incf ,compiled))))))

(compiler :binary (op left right)
  (let ((left (compile left))
        (right (compile right)))
    (ecase op
      (:|\|\|| `(let ((left ,left)) (if (eq (%%bool ,left) :true) left ,right)))
      (:&& `(let ((left ,left)) (if (eq (%%bool left) :true) ,right left)))
      (:|\|| `(logior ,left ,right))
      (:^ `(logxor ,left ,right))
      (:& `(logand ,left ,right))
      (:== `(== ,left ,right))
      (:=== `(=== ,left ,right))
      (:!== `(not-bool (== ,left ,right)))
      (:!=== `(not-bool (=== ,left ,right)))
      (:< `(%< ,left ,right))
      (:> `(%> ,left ,right))
      (:<= `(%not (%> ,left ,right)))
      (:>= `(%not (%< ,left ,right)))
      (:in `(as-bool (nth-value 1 (gethash ,left ,right))))
      (:instanceof `(%instanceof ,left ,right))
      (:>> `(ash ,right (- ,left)))
      (:<< `(ash ,right ,left))
      (:>>> `(ash ,right (- ,left))) ;; TODO
      (:+ `(+ ,left ,right))
      (:- `(- ,left ,right))
      (:* `(* ,left ,right))
      (:/ `(/ ,left ,right))
      (:% `(nth-value 1 (ceiling ,left ,right))))))

(compiler :assign (type left right)
  (if (eq type t)
      `(setf ,(compile left) ,(compile right))
      `(setf ,(compile left) ,(compile `(:binary ,type ,left ,right)))))

(compiler :new (expr args)
  (declare (ignore expr args))
  (error "Not implemented."))
(compiler :with (obj body)
  (declare (ignore obj body))
  (error "Not implemented."))
