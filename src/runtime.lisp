
(in-package :contracts.paren)

;; [TODO]
;; class-of/c, sub-class-of/c parent-class-of/c

(defsection @contracts-runtime (:title "Contracts runtime library")
  "Some basic contracts are included with this library:"
  
  (anyp (static-ps-function *contracts-library*))
  (hasp (static-ps-function *contracts-library*))
  (emptyp (static-ps-function *contracts-library*))
  (elementp (static-ps-function *contracts-library*))
  (truep (static-ps-function *contracts-library*))
  (booleanp (static-ps-function *contracts-library*))
  (numberp function)
  (nanp (static-ps-function *contracts-library*))
  (eventp (static-ps-function *contracts-library*))
  (classp (static-ps-function *contracts-library*))
  (objectp (static-ps-function *contracts-library*))
  (nullp (static-ps-function *contracts-library*))
  (undefp (static-ps-function *contracts-library*))
  (zerop (static-ps-function *contracts-library*))
  (positivep (static-ps-function *contracts-library*))
  (negativep (static-ps-function *contracts-library*))
  
  "Using those and combinators can get you far, but when your
application grows you may require custom contracts to be
created. Defining your own contract is easy, you need to create a
function that takes one argument and returns a boolean value.

For example to create a contract that checks if argument is a DOM node
you can do the following:

```lisp
(defun nodep (obj)
  (if (eq (typeof *node) \"object\")
      (instanceof obj *node)
      (and obj
           (eq (typeof obj) \"object\")
           (eq (chain obj node-type) \"number\")
           (eq (chain obj node-name) \"string\"))))
```
")

(defparameter *contracts-library*
  '(progn
    (defun anyp ()
      "Returns t for any argument" 
      t)
    
    (defun hasp (key obj)
      "Checks if object has property key"
      (and (not (eq obj nil))
           (chain *object prototype has-own-property (call obj key))))
    
    (defun emptyp (obj)
      "True if var is empty, obj might be one of: string array
       arguments object"
      (cond ((equal obj nil) t)
            ((or (arrayp obj) (stringp obj) (argumentsp obj))
             (eq (length obj) 0))
            (t (for-in (key obj)
                       (when (hasp key obj)
                         (return f)))
               t)))
    
    (defun elementp (obj)
      "Returns true if object is a DOM element."
      (and obj (eq (@ obj node-type) 1)))
    
    (defmacro gen-pred-check (name)
      (let* ((sym-name (symbol-name name))
             (fun-name (concatenate 'string sym-name "P"))
             (str-name (concatenate 'string "[object " (string-downcase sym-name :start 1) "]"))
             (fun-sym (intern fun-name)))
        `(defun ,fun-sym (obj)
           (eq (chain *object prototype to-string (call obj))
               ,str-name))))
    
    (gen-pred-check arguments)
    (gen-pred-check function)
    (gen-pred-check string)
    (gen-pred-check number)
    (gen-pred-check date)
    (gen-pred-check array)

    (defun truep (obj)
      "Return true if OBJ equals true."
      (eq obj t))
    
    (defun booleanp (obj)
      "Return true if OBJ is boolean."
      (or (eql obj t) (eql obj f)))
    
    (defun nanp (obj)
      "Return true if OBJ is NaN value."
      (is-na-n obj))
    
    (defun eventp (obj)
      "Return true if OBJ is an instance of Event class."
      (instanceof obj *event))
    
    (defun classp (obj)
      "Return true if OBJ is a class. There is no special way to define classes in javascript, and what's called a javascript class is represented as a function. Internally this function checks if OBJ is a function."
      (functionp obj))
    
    (defun objectp (obj)
      "Return true if OBJ is of type object."
      (eql (typeof obj) "object"))
    
    (defun nullp (obj)
      "Return true if OBJ is null."
      (eql obj nil))
    
    (defun undefp (obj)
      "Return true if OBJ is udefined."
      (eql obj undefined))
    
    (defun zerop (num)
      "Return true if NUM equals to zero."
      (= num 0))

    (defun positivep (num)
      "Return true if NUM is positive."
      (> num 0))

    (defun negativep (num)
      "Return true if NUM is negative."
      (< num 0))
    
    ;; parametarized predicates
    
    ;; (defun/partial instance-of (cls obj)
    ;;   (instanceof obj cls))
    
    ;; (defun/partial type-of (obj)
    ;;   (
    
    ;; (defun/partial length-of (num obj)
    ;;   (eq num (length obj)))

    ;; (defun/partial component-of (comp obj)
    ;;  )
    )
  "Runtime with defined parenscript predicates")
