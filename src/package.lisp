
(defpackage :contracts.paren
  (:use :cl :parenscript :serve.paren :alexandria :mgl-pax)
  (:export :defun/contract
           :lambda/contract
           :>>
           :>>*
           :>>i
           
           :or/c
           :and/c
           :not/c
           :listof/c
           :oneof/c
           :list/c
           :object/c
           :instanceof/c
           :maybe/c
           
           :*contracts-library*)
  
  (:shadow :switch))

(defpackage :contracts.paren-tests
  (:use :cl :contracts.paren :parenscript :lisp-unit)
  
  ;; (:import-from :contracts.paren
  ;;               :contract-variable
  ;;               :contract-contract
  ;;               :contract-type
  ;;               :build-input-contracts)
  )
