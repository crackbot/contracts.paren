
(in-package :contracts.paren)

(defsection @contract-types (:title "Available contract types")
  ">> flat contracts"
  "\>\>\* contract for functions with optional arguments or arbitrary many arguments"
  "\>\>i - named dependant contracts")

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
  (flet ((more-contracts-fn (optional-contracts optionals keys rest)
           (declare (ignore keys))
           (construct-contacts (append optionals (list rest))
                                            optional-contracts)))
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
  "Boolean check if the contract is flat. Flat contract is immediatley
checkable, without any modifications to values that it checks"
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
