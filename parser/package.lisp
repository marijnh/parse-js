(cl:defpackage #:parse-javascript
  (:use #:cl)
  (:export #:lex-js #:token-type #:token-value #:token-line #:token-char #:token-newline-before
           #:parse-js #:parse-js-string))
