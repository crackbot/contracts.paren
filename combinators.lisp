
(in-package :contracts.paren)

(defsection @contracts-manual (:title "Contracts manual")
  "Contracts is a powerful way to check function inputs and outputs, this contracts library is based upon racket contracts"

  (*violation-function* variable)
  )

(defparameter *violation-function* 'blame
  "Function that is called when contract violation is detected, it's
  arguments are based on combinator type")

(defvar *supported-combinators '(>>)
  "Supported combinator types")

;; >> flat contracts
;; [TODO]
;; >>* contract for functions with optional arguments or arbitrary many arguments
;; >>i - named dependant contracts

(defstruct contract variable contract type)

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
    (when (not (eql (car combinator) '>>))
      (error "Not valid contract combinator found"))
    (values docstring combinator effective-body)))

(defun parse-combinator (combinator)
  "Parse combinator into different values based on combinator type"
  (let ((combinator-type (car combinator))
        (output-contract (last combinator)))
    (when (not (combinator-supported-p combinator-type))
      (error "Defined combinator-type is not supported"))
    
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

(defun combinator-supported-p (comb)
  "Check if combinator is supported"
  (find comb *supported-combinators))
  
(defun build-input-contracts (lambda-list combinator)
  "Given function lambda list and function combinator build the
contract description"
  (multiple-value-bind (requireds optionals rest? rest keys? keys allow? aux?
                                  aux more? more-context more-count key-object)
      (ps::parse-lambda-list lambda-list)
    (multiple-value-bind (ctype inputs-contracts output key-contracts)
        (parse-combinator combinator)
      (declare (ignore ctype output))
      
      (assert (eql (length requireds) (length inputs-contracts)))
      (assert (eql (length keys) (/ (length key-contracts) 2)))
      
      (let ((pos/c (iter:iter
                     (iter:for var iter:in requireds)
                     (iter:for contract iter:in inputs-contracts)
                     (iter:collect (make-contract :variable var
                                                  :contract contract
                                                  :type :input))))
            (key/c (iter:iter (iter:for key-var iter:in keys)
                     (iter:collect (make-contract :variable key-var
                                                  :contract (getf key-contracts (alexandria:make-keyword key-var))
                                                  :type :input)))))
        (append pos/c key/c)))))

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
      (not (eql (car (contract-contract contract)) '>>))
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
                      ,(build-output-checks defun-name (list var-res) (contract-contract contract))
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

(defun build-output-checks (defun-name lambda-list combinator)
  ""
  (multiple-value-bind (ctype pos-inputs output key-inputs)
      (parse-combinator combinator)
    (declare (ignore ctype pos-inputs key-inputs))
    (let ((contract (make-contract :variable (car lambda-list)
                                   :contract (car output)
                                   :type :output)))
      (check-form-for-contract defun-name contract))))
