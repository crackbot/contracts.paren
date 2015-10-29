
(in-package :contracts.paren)

(defpslib "contracts"
    :package :contracts.paren
    :runtime *contracts-library*)

(defsection @main-manual (:title "Functions with contracts for Parenscript")
  (contracts.paren asdf:system)
  
  "This library is inspired by contracts found in Racket programming
langugage."
  
  "What are contracts? Here is a quote from Racket Guide."
  
  "Like a contract between two business partners, a software contract
is an agreement between two parties. The agreement specifies
obligations and guarantees for each “product” (or value) that is
handed from one party to the other.

A contract thus establishes a boundary between the two
parties. Whenever a value crosses this boundary, the contract
monitoring system performs contract checks, making sure the partners
abide by the established contract."

  "In ParenScript (unlike in Racket) contracts can be defined only at
function level. Those contracts impose constraints and provide
guarantees on the values being returned from the function."

  (@api-manual section)

  (@contracts-runtime section)
  (@contracts-combinators section)
  (@contract-types section))

(defsection @api-manual (:title "Main API")
  (lambda/contract psmacro)
  (defun/contract psmacro)
  
  "When contract is violated it will call the *VIOLATION-FUNCTION*
which you need to define inside your code base"

  (*violation-function* variable)

  (*ignore-contracts* variable))

(defparameter *ignore-contracts* nil
  "This is useful if you want to ignore contracts when compiling your
  definitions for production use and don't want to extra performance
  costs because of contracts. Setting this to t will compile
  defun/contract and lambda/contract as the usual defun and lambda.")

(defpsmacro lambda/contract (lambda-list &body body)
  "Define lambda with a contract"
  (if *ignore-contracts*
    `(lambda ,lambda-list ,@(remove-tl-contract body))
    (multiple-value-bind (docstring contract eff-body)
        (parse-function-with-combinator body)
      (let ((result (ps-gensym)))
        `(lambda ,lambda-list
           ,docstring
           ,@(build-input-checks "lambda" lambda-list contract)
           (var ,result (chain (lambda () ,@eff-body) (apply this)))
           ,(build-output-checks "lambda" lambda-list (list result) contract)
           ,result)))))

(defpsmacro defun/contract (name lambda-list &body body)
  "Define function with a contract"
  (if *ignore-contracts*
    `(defun ,name ,lambda-list ,@(remove-tl-contract body))
    (multiple-value-bind (docstring contract eff-body)
        (parse-function-with-combinator body)
      (let ((result (ps-gensym)))
        `(defun ,name ,lambda-list
           ,docstring
           ,@(build-input-checks name lambda-list contract)
           (var ,result (chain (lambda () ,@eff-body) (apply this)))
           ,(build-output-checks name lambda-list (list result) contract)
           ,result)))))
