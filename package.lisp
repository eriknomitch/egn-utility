;; ===============================================
;; UTILITY->PACKAGE ==============================
;; ===============================================
(in-package #:cl-user)

(defpackage :utility
  (:use    :cl)
  (:export :define-generic
           :define-constant
           :define-variable
           :define-function
           :define-method
           :define-macro
           :define-class
           :define-exported-generic
           :define-exported-constant
           :define-exported-variable
           :define-exported-function
           :define-exported-method
           :define-exported-macro
           :define-exported-class))

(pushnew :utility *features*)

(in-package #:utility)

