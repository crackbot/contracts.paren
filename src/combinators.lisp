
(in-package :contracts.paren)

(defsection @contract-types (:title "Contract types")
  "There are three types of contracts:
  - >> flat contracts

  - >>* full contracts, contract for functions with optional arguments
        or/and arbitrary many arguments or/and :pre :post clauses

  - >>i named dependent contracts, where names you give to contracts
      can be used in subcontracts or/and :pre :post clauses"

  (remove-tl-contract function)
  (@flat-contracts section)
  (@full-contracts section)
  (@named-contracts section))

(defsection @flat-contracts (:title "Flat contracts")
  "Flat contract is the basic building block, most of the time you
  will be using it. It suports positional and keyword arguments.

```lisp
  (defun hello (x y)
    (>> intp intp intp)
    (+ x y))
```

```lisp
  (defun hello (x y &key (add 2))
    (>> intp intp :add intp intp)
    (+ (+ x y) add))
```

  Flat contract does support all of the combinators, but not optional
  or rest arguments.")

(defsection @full-contracts (:title "Full contracts")
  "You can use this combinator type to check more advanced function
  signatures, which includes one or more of optional, keyword or
  arbitrary many arguments.

  Combinator signature is:

```lisp
  (>>* (domain contracts)
       (optional / keywords / rest)
       :pre ()
       :post ()
       range contract)
```

  Domain and range contracts are required. If you provide optional or
  keyword contract it should be a plist of variable name and
  contract, you can also skip providing those contracts for variable
  that you don't want to check, unlike domain contracts where you need
  to provide a contract for every variable. If you don't want to
  provide any contracts for those, just pass the nil

```lisp
  (>>* (intp) (:keyword floatp) intp)
```

```lisp
  (>>* (intp) nil intp)
``` 

  If :rest is present it is expected to check the &rest arguments

  :pre and :post expect a lambda which should return either a boolean
  value or a string, in case it returns **f** or a string it's treated
  as a failure and string designates an error message. :pre will be
  dispatched before any contract and :post after checking all provided
  ones. This allows you to check the function environment without any
  connection to function domain or range.

  An example, checking that function saves the return value:

```lisp
  (defun/contract sum (x y)
    (>>* (intp intp) 
         :post (lambda ()
                 (intp (chain this result)))
         intp)
    (let ((res (+ x y)))
      (setf (chain this result) res)
      res))
```")

(defsection @named-contracts (:title "Named contracts - not fully implemented")
  "Named contracts is similar to optional combinator with the
  difference that each argument and result is named and these names
  can be used in subcontracts and in pre-/post-condition clauses. In
  other words ->i combinator expresses dependencies among arguments,
  results and environment.

  Combinator signature is:

```lisp
  (>>i (domain contracts)
       (optional / keywords / rest)
       :pre () ()
       :post () ()
       (result contract))
```

  Few examples:

  Check that input X is a number and output RES is a number that is at
  least a double of X.

```
  (>>i ((x numberp))
       (res (x) (and/c numberp (>=/c (* x 2)))))
```

  Check that X is a number, Y is number greater or equal than X.
  If :A keyword is given it should be a number. If :B is also given it
  should be greater or equal than :A. RESULT is a number that is
  greater or equal than sum of X and Y.

```lisp
  (>>i ((x numberp)
        (y (x) (>=/c x)))
       (:a (a numberp)
        :b (b (a) (>=/c a)))
       (result (x y) (and/c numberp (>=/c (+ x y)))))
```

```lisp
  (>>i ((x intp)
        (y (x) (eql/c (+ x x))))
       ()
       (result intp))
```

  Note that contract's names are mandatory for both inputs and
  outputs.")

(defparameter *violation-function* 'blame
  "Function that is called when contract violation is detected, it's
  called with one argument that is an object. Exact object keys and
  values are based on combinator type.")

(defvar *supported-combinators* '(>> >>* >>i)
  "Supported combinator types")

(defstruct contract variable contract type alias deps)

(defun remove-tl-contract (forms)
  "Given forms it will remove any top-level contracts found in it"
  (if (and (listp (car forms))
           (combinator-supported-p (caar forms)))
    (rest forms)
    forms))

(defun parse-function-with-combinator (body)
  "Parse function body with cntract combinator into parts"
  (let* ((docstring (and (cdr body) (stringp (car body)) (car body)))
         (combinator (if docstring
                       (cadr body)
                       (car body)))
         (effective-body (cond ((and (not docstring) (not combinator)) body)
                               ((or (and (not combinator) docstring)
                                    (and (not docstring) combinator))
                                (cdr body))
                               (t (cddr body)))))
    (when (not (combinator-supported-p (car combinator)))
      (error "Not valid contract combinator found"))
    (values docstring combinator effective-body)))

(defun combinator-supported-p (comb)
  "Check if combinator is supported"
  (find comb *supported-combinators* :test #'eql))

(defun construct-contacts (combinator-type vars contracts &key (type :input))
  "Given list of vars and contracts it will construct a list of
contract strutures. Length of both lists should be equal. The way
contract struct is created is based on combinator type, different
combinators expect different syntax of individual contract forms."
  (iter:iter
    (iter:for var iter:in vars)
    (iter:for contract iter:in contracts)
    (let ((ctr (make-single-contract combinator-type var contract)))
      (setf (contract-type ctr) type)
      (iter:collect ctr))))

(defun parse-named-contract (contract)
  (assert (and (listp contract)
               (or (eq (length contract) 2)
                   (eq (length contract) 3))))
  (destructuring-bind (name deps &optional ctr)
      contract
    (when (not ctr)
      (setf ctr deps
            deps nil))
    (values ctr name deps)))

(defgeneric make-single-contract (combinator-type var contract)
  (:documentation "Parse single contract form"))

(defmethod make-single-contract (combinator-type var contract)
  (make-contract :variable var
                 :contract contract))

(defmethod make-single-contract ((combinator-type (eql '>>i)) var contract)
  (multiple-value-bind (cnt alias deps)
      (parse-named-contract contract)
    (make-contract :variable var
                   :contract cnt
                   :alias alias
                   :deps deps)))

(defun input-contracts-for-requireds (combinator-type combinator lambda-list construct-more-fn)
  (multiple-value-bind (requireds optionals rest? rest keys? keys allow? aux?
                                  aux more? more-context more-count key-object)
      (ps::parse-lambda-list lambda-list)
    (multiple-value-bind (ctype inputs-contracts output more-contracts)
        (parse-combinator-type combinator-type combinator)
      (declare (ignore ctype output))
      (assert (eql (length requireds) (length inputs-contracts)))
      (let ((pos/c (construct-contacts combinator-type requireds inputs-contracts))
            (additional/c (funcall construct-more-fn more-contracts optionals keys rest)))
        (append pos/c additional/c)))))

(defgeneric input-contracts (type combinator lambda-list)
  (:documentation "Construct input contracts based on combinator type"))

(defmethod input-contracts ((combinator-type (eql '>>)) combinator lambda-list)
  "PS Input contract forms for >> combinator"
  (flet ((more-contracts-fn (key-contracts optionals keys rest)
           (declare (ignore optionals rest))
           (assert (eql (length keys) (/ (length key-contracts) 2)))
           (iter:iter (iter:for key-var iter:in keys)
                      (let* ((var (if (listp key-var) (car key-var) key-var))
                             (cntr (alexandria:make-keyword var))) ;; default key val
                        (iter:collect (make-contract :variable var
                                                     :contract (getf key-contracts cntr)
                                                     :type :input))))))
    (input-contracts-for-requireds combinator-type
                                   combinator
                                   lambda-list
                                   #'more-contracts-fn)))

(defmethod input-contracts ((combinator-type (eql '>>*)) combinator lambda-list)
  "PS Input contract forms for >>* combinator"
  (flet ((more-contracts-fn (more-contracts optionals keys rest)
           "more-contracts can have optionals, keys and rest, it is a
            plist with input variable name and a contract."
           (when (not (listp rest))
             (setf rest (list rest)))
           ;; since checks for key args are optional, don't check it
           ;; (assert (eql (/ (length more-contracts) 2)
           ;;              (apply #'+ (mapcar #'length
           ;;                                 (list optionals keys rest)))))
           (let ((res (list)))
             (dolist (type (list optionals keys rest))
               (dolist (var type)
                 (when-let ((contract (getf more-contracts
                                            (alexandria:make-keyword var))))
                   (pushnew (make-single-contract combinator-type var contract)
                            res))))
             res)))
  (input-contracts-for-requireds combinator-type
                                 combinator
                                 lambda-list
                                 #'more-contracts-fn)))

(defmethod input-contracts ((combinator-type (eql '>>i)) combinator lambda-list)
  "PS Input contract forms for >>i combinator"
  (flet ((more-contracts-fn (more-contracts optionals keys rest)
           (when (not (listp rest))
             (setf rest (list rest)))
           (let ((res (list)))
             (dolist (type (list optionals keys rest))
               (dolist (var type)
                 (when-let ((contract (getf more-contracts
                                            (alexandria:make-keyword var))))
                   (pushnew (make-single-contract combinator-type var contract)
                            res))))
             res)))
    (input-contracts-for-requireds combinator-type
                                   combinator
                                   lambda-list
                                   #'more-contracts-fn)))

(defun parse-combinator (combinator)
  "Parse combinator into different values based on combinator type"
  (let ((combinator-type (car combinator)))
    (parse-combinator-type combinator-type combinator)))

(defgeneric parse-combinator-type (type combinator)
  (:documentation "Parse combinator into parts."))

(defmethod parse-combinator-type ((combinator-type (eql '>>i)) combinator)
  (parse-combinator-type '>>* combinator))

(defmethod parse-combinator-type ((combinator-type (eql '>>*)) combinator)
  (let* ((output-contract (last combinator))
         (contracts (subseq combinator 1 (- (length combinator) 1)))
         (positional-input (car contracts))
         (optional-input (cadr contracts)))
    (values combinator-type positional-input output-contract optional-input)))

(defmethod parse-combinator-type ((combinator-type (eql '>>)) combinator)
  (let ((output-contract (last combinator)))
    (multiple-value-bind (positional-input keyword-input)
        (iter:iter (iter:for item iter:in (subseq combinator 1 (- (length combinator) 1)))
          (iter:for prev previous item initially nil)
            (when (keywordp prev)
              (iter:appending (list prev item) into keyi))
                   
            (when (not (or (keywordp item)
                           (keywordp prev)))
              (iter:collect item into posi))
            (iter:finally (return (values posi keyi))))
      
        ; (inputs-contracts (subseq combinator 1 (- (length combinator) 1))))
      (values combinator-type positional-input output-contract keyword-input))))

;;;; envs

(defun build-contracts-env (contracts)
  ""
  (let ((res))
    (dolist (ctr contracts)
      (when (contract-alias ctr)
        (pushnew `(setf ,(ps-gensym)
                        ,(contract-alias ctr))
                 res)))
    res))

(defun create-contracts-env (contracts)
  "Environment right now is represented as a list of two plists.

(:x :js123)
(:hello :x)"
  (let ((aliases (list))
        (toplevel (list)))
    (dolist (ctr contracts)
      (when-let ((alias (contract-alias ctr)))
        (setf (getf aliases alias)
              (ps-gensym))
        (setf (getf toplevel (contract-variable ctr))
              alias)))
    (list toplevel aliases)))
     
(defun compile-contracts-env (env)
  (let ((res))
    (destructuring-bind (toplevel aliases)
        env
      (alexandria:doplist (key val toplevel)
        (pushnew (list 'setf
                       (getf aliases val)
                       key)
                 res)))
    res))

(defun env-compile-contract-form (env contract deps)
  (if (not env)
      contract
      (destructuring-bind (toplevel aliases)
          env
        (declare (ignore toplevel))
        (dolist (dep deps)
          (nsubst (getf aliases dep)
                  dep
                  contract))
        contract)))

;;;;

(defun blame-object (defun-name contract)
  "Creates a list with blame info, that is later converted into
javascript object and can be used for contract violation message"
  (let* ((contract-string (with-output-to-string (str)
                            (format str "~A" (contract-contract contract))))
         (defun-string-name (or (and (stringp defun-name) defun-name)
                                (symbol-name defun-name)))
         (obj (case (contract-type contract)
                (:input (list :type "domain"
                           :function defun-string-name
                           :variable (symbol-name (contract-variable contract))
                           :given (contract-variable contract)
                           :expected contract-string))
                (:output (list :type "range"
                           :function defun-string-name
                           :promised contract-string
                           :produced (contract-variable contract)))
                (t (list :type "condition"
                         :function defun-string-name
                         :check contract-string)))))
    (cons 'create obj)))

(defun flat-contractp (contract)
  "Boolean check if the contract is flat. Flat contract doesn't
include any subcontracts. For example non flat contract looks like
this:

  ```lisp
  (defun/contract (x y)
    (>> (>> intp intp)
        any
        any))
  ```"
  (if (listp (contract-contract contract))
      (not (combinator-supported-p (car (contract-contract contract))))
      t))

(defun check-flat-contract-form (env defun-name contract)
  ""
  (let* ((err (list *violation-function* (blame-object defun-name contract)))
         (contract-form (contract-check-form env contract))
         (not-form (cons 'not (list contract-form))))
    (list 'when not-form err)))

(defun setup-temp-args (num)
  ""
  (let* ((syms nil)
         (forms (iter:iter (iter:for i from 0 to (- num 1))
                  (iter:for var-sym initially (ps-gensym) then (ps-gensym))
                  (pushnew var-sym syms)
                  (iter:collect `(var ,var-sym (aref args ,i))))))
    (values (cons 'progn forms) syms)))

(defun combinator-inputs-num (contract)
  ""
  (multiple-value-bind (ctype pos-inputs output key-inputs)
      (parse-combinator (contract-contract contract))
    (length pos-inputs)))

(defun wrap-into-combinator (env defun-name contract)
  ""
  (let* ((env nil)
         (var (contract-variable contract))
         (var-fun (ps-gensym))
         (var-res (ps-gensym)))
         
    (multiple-value-bind (arg-forms arg-names)
        ; (error (combinator-inputs-num contract))
        (setup-temp-args (combinator-inputs-num contract))
      (let ((output-contracts (build-output-contracts arg-names (list var-res) (contract-contract contract))))
        `(progn
           (setf ,var-fun ,var)
           (setf ,var (lambda ()
                        (var args (chain *array prototype slice (call arguments 0)))
                        ,arg-forms
                        ,@(build-input-checks env defun-name arg-names (contract-contract contract))
                        (var ,var-res (chain ,var-fun (apply this args)))
                        ,(check-form-for-contract env defun-name (car output-contracts))
                        ,var-res)))))))

(defun build-input-contracts (lambda-list combinator)
  "Given function lambda list and function combinator build the
contract description"
  (input-contracts (car combinator) combinator lambda-list))

(defun contract-check-form (env contract)
  ""
  (list (env-compile-contract-form env
                                   (contract-contract contract)
                                   (contract-deps contract))
        (contract-variable contract)))

(defun check-form-for-contract (env defun-name contract)
  ""
  (if (flat-contractp contract)
      (check-flat-contract-form env defun-name contract)
      (progn
        (when (eql (car contract) '>>i)
          (error "not supported for >>i yet"))
        (wrap-into-combinator env defun-name contract))))

(defun build-input-checks (env defun-name lambda-list combinator)
  ""
  (mapcar (alexandria:curry #'check-form-for-contract env defun-name)
          (build-input-contracts lambda-list combinator)))

(defun input-checks-for-contracts (env defun-name contracts)
  (mapcar (alexandria:curry #'check-form-for-contract env defun-name)
          contracts))

(defun build-output-contracts (input-lambda-list output-lambda-list combinator)
  (multiple-value-bind (ctype pos-inputs output key-inputs)
      (parse-combinator combinator)
    (construct-contacts (car combinator)
                        output-lambda-list
                        output)))

(defun combinator-supports-pre-post-p (comb)
  (or (eql comb '>>*) (eql comb '>>i)))

(defun build-env-clause (defun-name clause combinator)
  ""
  (when (combinator-supports-pre-post-p (car combinator))
    (when-let ((pos (position clause combinator)))
      (let* ((contract (nth (+ 1 pos) combinator))
             (form (check-form-for-contract nil defun-name
                                            (make-contract :variable nil
                                                           :contract contract
                                                           :type clause))))
        (setf combinator (delete contract combinator))
        (setf combinator (delete clause combinator))
        form))))

(defun build-pre (defun-name combinator)
  "If combinator type supports :pre clause it will build appropriate
  verification code."
  (when (and (eql (car combinator) '>>i)
             (find :pre combinator))
    (error ":pre is not supported yet for >>i"))
  (build-env-clause defun-name :pre combinator))

(defun build-post (defun-name combinator)
  "If combinator type supports :post clause it will build appropriate
  verification code."
  (when (and (eql (car combinator) '>>i)
             (find :pre combinator))
    (error ":post is not supported yet for >>i"))
  (build-env-clause defun-name :post combinator))
