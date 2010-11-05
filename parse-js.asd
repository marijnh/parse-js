(asdf:defsystem #:parse-js
  :components
  ((:module :src
            :components ((:file "package")
                         (:file "util" :depends-on ("package"))
                         (:file "tokenize" :depends-on ("util"))
                         (:file "parse" :depends-on ("tokenize"))))))
