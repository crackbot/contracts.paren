
(in-package :contracts.paren)

;; [TODO]
;; class-of/c, sub-class-of/c parent-class-of/c

(defparameter *contracts-library*
  '(progn
    (defun anyp () t)
    
    (defun hasp (key obj)
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

    (defun booleanp (obj)
      (or (eql obj t) (eql obj f)))
    
    (defun nanp (obj)
      ;; return _.isNumber(obj) && obj !== +obj;
      )
    
    (defun eventp (arg)
      (instanceof arg *event))
    
    (defun classp (arg)
      (functionp arg))
    
    (defun objectp (arg)
      (eql (typeof arg) "object"))
    
    (defun nullp (arg)
      (eql arg nil))
    
    (defun undefp (arg)
      (eql arg undefined))
    
    (defun zerop (num)
      (= num 0))

    (defun positivep (num)
      (> num 0))

    (defun negativep (num)
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
