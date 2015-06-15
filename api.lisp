
(in-package :contracts.paren)

(defpslib "contracts"
    :package :contracts.paren
    :runtime *contracts-library*)

(defsection @api-manual (:title "Functions with contracts for Parenscript")
  (lambda/contract psmacro)
  (defun/contract psmacro))

(defpsmacro lambda/contract (lambda-list &body body)
  "Define lambda with a contract"
  (multiple-value-bind (docstring contract eff-body)
      (parse-function-with-combinator body)
    (let ((result (ps-gensym)))
      `(lambda ,lambda-list
         ,docstring
         ,@(build-input-checks "lambda" lambda-list contract)
         (var ,result (chain (lambda () ,@eff-body) (apply this)))
         ,(build-output-checks "lambda" (list result) contract)
         ,result))))

(defpsmacro defun/contract (name lambda-list &body body)
  "Define function with a contract"
  (multiple-value-bind (docstring contract eff-body)
      (parse-function-with-combinator body)
    (let ((result (ps-gensym)))
      `(defun ,name ,lambda-list
         ,docstring
         ,@(build-input-checks name lambda-list contract)
         (var ,result (chain (lambda () ,@eff-body) (apply this)))
         ,(build-output-checks name (list result) contract)
         ,result))))
