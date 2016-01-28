
(in-package :contracts.paren)

(defsection @contract-types (:title "Contract types")
  "There are three types of contracts:
  - >> flat contracts

  - >>* full contracts, contract for functions with optional arguments
        or/and arbitrary many arguments or/and :pre :post clauses

  - >>i named dependent contracts, where names you give to contracts
      can be used in subcontracts or/and :pre :post clauses"

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
       :pre () :post ()
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

(defsection @named-contracts (:title "Named contracts - not implemented")
  "Named contracts is similar to optional combinator with the
  difference that the ->i contract combinator differs from the ->*
  combinator in that each argument and result is named and these names
  can be used in the subcontracts and in the pre-/post-condition
  clauses. In other words, ->i expresses dependencies among arguments
  and results.

  Combinator signature is:

```lisp
  (>>i (domain contracts)
       (optional / keywords / rest)
       :pre () ()
       :post () ()
       (result contract))
```

```lisp
  (->i :pre () (set! c0 count)
       ((x number?)
        (y (x) (>=/c x)))
       (:a (a number?)
        :b (b (a) (>=/c a)))
       (result (x y) (and/c number? (>=/c (+ x y))))
       :post (id nn result) (string=? (name id) nn))
```lisp
")

(defparameter *violation-function* 'blame
  "Function that is called when contract violation is detected, it's
  arguments are based on combinator type")

(defvar *supported-combinators* '(>> >>* >>i)
  "Supported combinator types")

(defstruct contract variable contract type)

(defun remove-tl-contract (forms)
  "Given forms it will remove any top-level contracts found in it"
  (if (and (listp (car forms))
           (combinator-supported-p (caar forms)))
    (rest forms)
    forms))

(defun parse-function-with-combinator (body)
  "Parse function with combinator into parts"
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

(defun construct-contacts (vars contracts &key (type :input))
  "Given list of vars and contracts it will construct a list of
contract strutures. Type can be :input or :output."
  (iter:iter
    (iter:for var iter:in vars)
    (iter:for contract iter:in contracts)
    (iter:collect (make-contract :variable var
                                 :contract contract
                                 :type type))))

(defun parse-combinator (combinator)
  "Parse combinator into different values based on combinator type"
  (let ((combinator-type (car combinator)))
    (parse-combinator-type combinator-type combinator)))

(defgeneric input-contracts (type combinator lambda-list)
  (:documentation ""))

(defun input-contracts-for-requireds (combinator-type combinator lambda-list construct-more-fn)
  (multiple-value-bind (requireds optionals rest? rest keys? keys allow? aux?
                                  aux more? more-context more-count key-object)
      (ps::parse-lambda-list lambda-list)
    (multiple-value-bind (ctype inputs-contracts output more-contracts)
        (parse-combinator-type combinator-type combinator)
      (declare (ignore ctype output))
      (assert (eql (length requireds) (length inputs-contracts)))
      (let ((pos/c (construct-contacts requireds inputs-contracts))
            (additional/c (funcall construct-more-fn more-contracts optionals keys rest)))
        (append pos/c additional/c)))))

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
           ;; since checks are optional, don't check it
           ;; (assert (eql (/ (length more-contracts) 2)
           ;;              (apply #'+ (mapcar #'length
           ;;                                 (list optionals keys rest)))))
           (let ((res (list)))
             (dolist (type (list optionals keys rest))
               (dolist (var type)
                 (when-let ((contract (getf more-contracts
                                            (alexandria:make-keyword var))))
                   (pushnew (make-contract :variable var
                                           :contract contract
                                           :type :input)
                            res))))
             res)))
  (input-contracts-for-requireds combinator-type
                                 combinator
                                 lambda-list
                                 #'more-contracts-fn)))

(defmethod input-contracts ((combinator-type (eql '>>i)) combinator lambda-list)
  "PS Input contract forms for >>i combinator"
  (input-contracts '>> combinator lambda-list))

(defgeneric parse-combinator-type (type combinator)
  (:documentation ""))

(defmethod parse-combinator-type ((combinator-type (eql '>>i)) combinator)
  (parse-combinator-type '>> combinator))

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

(defun build-input-contracts (lambda-list combinator)
  "Given function lambda list and function combinator build the
contract description"
  (input-contracts (car combinator) combinator lambda-list))

(defun contract-check-form (contract)
  ""
  (list (contract-contract contract) (contract-variable contract)))

(defun blame-object (defun-name contract)
  "Creates a list with blame info, that is later converted into
javascript object and can be used for contract violation message"
  (let* ((contract-string (with-output-to-string (str)
                            (format str "~A" (contract-contract contract))))
         (defun-string-name (or (and (stringp defun-name) defun-name)
                                (symbol-name defun-name)))
         (obj (cond ((eql (contract-type contract) :input)
                     (list :type "domain"
                           :function defun-string-name
                           :variable (symbol-name (contract-variable contract))
                           :given (contract-variable contract)
                           :expected contract-string))
                    ((eql (contract-type contract) :output)
                     (list :type "range"
                           :function defun-string-name
                           :promised contract-string
                           :produced (contract-variable contract))))))
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

(defun check-flat-contract-form (defun-name contract)
  ""
  (let* ((err (list *violation-function* (blame-object defun-name contract)))
         (contract-form (contract-check-form contract))
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

(defun wrap-into-combinator (defun-name contract)
  ""
  (let* ((var (contract-variable contract))
         (var-fun (ps-gensym))
         (var-res (ps-gensym)))
    (multiple-value-bind (arg-forms arg-names)
        ; (error (combinator-inputs-num contract))
        (setup-temp-args (combinator-inputs-num contract))
      `(progn
         (setf ,var-fun ,var)
         (setf ,var (lambda ()
                      (var args (chain *array prototype slice (call arguments 0)))
                      ,arg-forms
                      ,@(build-input-checks defun-name arg-names (contract-contract contract))
                      (var ,var-res (chain ,var-fun (apply this args)))
                      ,(build-output-checks defun-name arg-names (list var-res) (contract-contract contract))
                      ,var-res))))))

(defun check-form-for-contract (defun-name contract)
  ""
  (if (flat-contractp contract)
      (check-flat-contract-form defun-name contract)
      (wrap-into-combinator defun-name contract)))

(defun build-input-checks (defun-name lambda-list combinator)
  ""
  (mapcar (alexandria:curry #'check-form-for-contract defun-name)
          (build-input-contracts lambda-list combinator)))

(defun dependant-contract-check (defun-name input-lambda-list contract)
  "Build output check forms for dependant contract >>i"
  (let ((fun (contract-contract contract))
        (contract-result-name (ps-gensym)))
  `(progn
     (let ((,contract-result-name ,fun))
       (when (not ((,contract-result-name ,@input-lambda-list) ,(contract-variable contract)))
         ,(list *violation-function* (blame-object defun-name contract)))))))

(defun build-output-checks (defun-name input-lambda-list output-lambda-list combinator)
  "Build checks for function output"
  (multiple-value-bind (ctype pos-inputs output key-inputs)
      (parse-combinator combinator)
    (declare (ignore ctype pos-inputs key-inputs))
    
    (let ((contract (make-contract :variable (car output-lambda-list)
                                   :contract (car output)
                                   :type :output)))
      (if (eql (car combinator) '>>i)
          (dependant-contract-check defun-name input-lambda-list contract)
          (check-form-for-contract defun-name contract)))))

(defun combinator-supports-pre-post (comb)
  (or (eql comb '>>*) (eql comb '>>i)))

(defun build-env-clause (defun-name clause combinator)
  (when (combinator-supports-pre-post (car combinator))
    (when-let ((pos (position clause combinator)))
      (let* ((contract (nth (+ 1 pos) combinator))
             (form (check-form-for-contract defun-name
                                            (make-contract :variable nil
                                                           :contract contract
                                                           :type :input))))
        (setf combinator (delete contract combinator))
        (setf combinator (delete clause combinator))
        form))))

(defun build-pre (defun-name combinator)
  "If combinator type supports :pre clause it will build appropriate
  verification code."
  (build-env-clause defun-name :pre combinator))

(defun build-post (defun-name combinator)
  "If combinator type supports :post clause it will build appropriate
  verification code."
  (build-env-clause defun-name :post combinator))
