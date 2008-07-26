(asdf:defsystem #:js2cl
  :depends-on (#:cl-ppcre #:parse-javascript)
  :components
  ((:module :js2cl
            :components ((:file "package")
                         (:file "runtime" :depends-on ("package"))
                         (:file "compile" :depends-on ("package"))
                         (:file "compilers" :depends-on ("runtime" "compile"))))))

