;;;; +----------------------------------------------------------------+
;;;; | HORAE                                                          |
;;;; +----------------------------------------------------------------+

(defpackage #:horae/script-symbols
  (:documentation "Symbols to be available in scripts.")
  (:use #:cl)
  (:export
   #:interval
   #:second #:seconds
   #:minute #:minutes
   #:hour #:hours
   #:day #:days
   #:week #:weeks))

(in-package #:horae/script-symbols)

(declaim (declaration interval))
