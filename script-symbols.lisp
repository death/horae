;;;; +----------------------------------------------------------------+
;;;; | HORAE                                                          |
;;;; +----------------------------------------------------------------+

(defpackage #:horae/script-symbols
  (:use #:cl)
  (:export
   #:interval
   #:second #:seconds
   #:minute #:minutes
   #:hour #:hours
   #:day #:days
   #:week #:weeks)
  (:documentation "Symbols to be available in scripts."))

(in-package #:horae/script-symbols)

(declaim (declaration interval))
