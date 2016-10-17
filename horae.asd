;;;; +----------------------------------------------------------------+
;;;; | HORAE                                                          |
;;;; +----------------------------------------------------------------+

;;; System definition

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:register-system-packages :cffi '(:cffi-sys))

(asdf:defsystem #:horae
  :description "An application run periodic tasks defined by Lisp scripts."
  :author "death <github.com/death>"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("horae/all"))
