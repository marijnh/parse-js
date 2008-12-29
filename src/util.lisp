(in-package #:parse-js)

(defmacro with-defs (&body body)
  (loop :for form :in body
        :if (and (eq (car form) 'def) (< (length form) 4))
          :collect (cadr form) :into vars :and
          :if (caddr form) :collect `(setf ,(cadr form) ,(caddr form)) :into body :end
        :else :if (eq (car form) 'def)
          :collect (cdr form) :into funcs
        :else
          :collect form :into body
        :finally (return `(let ,vars (labels ,funcs ,@body)))))

(defmacro defun/defs (name args &body body)
  `(defun ,name ,args (with-defs ,@body)))
