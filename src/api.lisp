
(in-package :contracts.paren)

(defpslib "contracts"
    :package :contracts.paren
    :runtime *contracts-library*)

(defsection @main-manual (:title "Functions with contracts for Parenscript")
  (contracts.paren asdf:system)
  
  "This library is inspired by contracts found in Racket programming
  langugage.
  
  What are contracts? Here is a quote from Racket Guide.
  
  Like a contract between two business partners, a software contract
  is an agreement between two parties. The agreement specifies
  obligations and guarantees for each “product” (or value) that is
  handed from one party to the other.

  A contract thus establishes a boundary between the two
  parties. Whenever a value crosses this boundary, the contract
  monitoring system performs contract checks, making sure the partners
  abide by the established contract.

  In ParenScript (unlike in Racket) contracts can be defined only at
  lambda/function level. Those contracts impose constraints and
  provide guarantees on the values being returned from the
  function. There are some differences in both implementation and
  usage between ParenScript and Racket, so make sure to read the docs
  even if you're familiar with Racket contracts.

  ParenScript f*unction* definition supports positional arguments, &key,
  &optional, &rest. &optional and &keywoard does not work well
  together, also when you include &rest and &key expect &rest variable
  to capture input passed into &key, you need to remember this things
  when writing contracts.

  Simple function definition with two positional arguments looks like
  so:
  
  ```lisp
  (defun sum (x y)
    (+ x y))
  ```

  To make this a function with contract you can use DEFUN/CONTRACT form:

  ```lisp
  (defun/contract sum (x y)
    (>> intp intp intp)
    (+ x y))
  ```

  In the code above each **intp** is called a contract and **>>** sign
  is called a contract combinator. Right now there are three types of
  contract combinators, and numerous contracts that can be used."

  (@api-manual section)
  (@contract-types section)
  (@contracts-runtime section)
  (@contracts-combinators section))

(defsection @api-manual (:title "Main API")
  "API consists of two parenscript macros **defun/contract** and
**lambda/contract** both are based on core **defun** and **lambda**
taking exactly the same lambda list and body. Except first form of the
body should be a contract."
  
  (lambda/contract psmacro)
  (defun/contract psmacro)
  
  "There are a number of variables you can configure. Most important
one is *VIOLATION-FUNCTION*, it gets called when contract is
violated. You can use something basic as a starting point, like

```javascript
function blame (obj) {
    console.log(\"contract violated\");
    console.log(obj.given);
    console.log(obj.expected);
}
```
"
  (*violation-function* variable)
  (*ignore-contracts* variable))

(defparameter *ignore-contracts* nil
  "This is useful if you want to ignore contracts when compiling your
  definitions for production use and don't want to pay extra
  performance costs because of contracts. Setting this to **t** will
  compile DEFUN/CONTRACT and LAMBDA/CONTRACT as the usual defun and
  lambda forms.")

(defpsmacro lambda/contract (lambda-list &body body)
  "Define lambda with a contract.

  Works as the usual lambda form, but first form in body should be a
  contract definition. For example

  ```lisp
  (lambda/contract (x y)
    (>> intp intp intp)
    (+ x y))
  ```

  This contract will ensure that both **x** and **y** are integers and
  function returns an integer too."

  (if *ignore-contracts*
      `(lambda ,lambda-list ,@(remove-tl-contract body))
      (append '(lambda)
              (create-defun-forms "lambda" lambda-list body))))

(defpsmacro defun/contract (name lambda-list &body body)
  "Define function with a contract. Works the same way as
  lambda/contract, first body form is a contract."
  (if *ignore-contracts*
      `(defun ,name ,lambda-list ,@(remove-tl-contract body))
      (append `(defun ,name)
              (create-defun-forms name lambda-list body))))

(defun create-defun-forms (name lambda-list body)
  "Create new function based on given function name lambda-list and
body. New function will parse body looking for contracts
definitions. Each intput and output contract will be converted into
parenscript code to check the consistency between the contract and
environment during the execution of the function."
  (multiple-value-bind (docstring contract eff-body)
      (parse-function-with-combinator body)
    (let* ((result (ps-gensym))
           (input-contracts (build-input-contracts lambda-list contract))
           (output-contracts (build-output-contracts lambda-list (list result) contract))
           (env (create-contracts-env input-contracts)))
      `(,lambda-list
         ,docstring
         ,@(compile-contracts-env env)
         ,@(input-checks-for-contracts env name input-contracts)
         ,(build-pre name contract)
         (var ,result (chain (lambda () ,@eff-body) (apply this)))
         ,(check-form-for-contract env name (car output-contracts))
         ,(build-post name contract)
         ,result))))
