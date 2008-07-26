(cl:defpackage #:js-runtime
  (:use :cl :cl-ppcre)
  (:export))

(cl:defpackage #:js2cl
  (:use #:cl #:cl-ppcre #:js-runtime)
  (:shadow #:compile)
  (:export #:js-package #:compile-js #:eval-js #:compile-js-to-lisp))
