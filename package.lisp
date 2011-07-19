;;;; package.lisp

(defpackage #:utils
  (:nicknames "utils")
  (:use #:cl)
  (:export "FIRST-N"
	   "FLATTEN" 
	   "MY-APPEND"
	   "CURRY"
           "DOT-PRODUCT"
	   "COMBINATIONS"
           "ALL-COMBINATIONS"))

