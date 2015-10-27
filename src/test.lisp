
(in-package :contracts.paren-tests)

;; (define-test parse-combinator-check1
;;   (multiple-value-bind (comb in out key)
;;       (contracts.paren::parse-combinator '(>> intp :x intp intp))
;;     (assert-eq comb '>>)
;;     (assert-equal in '(intp))
;;     (assert-equal out '(intp))
;;     (assert-equal key '(:x intp))))    

;; (define-test build-intput-check1
;;   (assert-eq (length (build-input-contracts '(x) '(>> intp intp))) 1)
;;   (assert-eq (length (build-input-contracts '(x y) '(>> intp intp intp))) 2))

;; (define-test build-intput-check2
;;   (let* ((var 'x)
;;          (chk 'intp) 
;;          (contract (car (build-input-contracts (list var) (list '>> chk 'intp)))))
;;     (assert-eq (contract-variable contract) var)
;;     (assert-eq (contract-contract contract) chk)
;;     (assert-eq (contract-type contract) :input)))

;; (define-test input-contracts-keys-check1
;;   (let* ((contracts (build-input-contracts '(x &key y) '(>> intp :y intp intp)))
;;          (key-contract (cadr contracts)))
;;     (assert-eq (length contracts) 2)
;;     (assert-eq (contract-variable key-contract) 'y)
;;     (assert-eq (contract-contract key-contract) 'intp)
;;     (assert-eq (contract-type key-contract) :input)))

;; (defun hello (x)
;;   (>> intp (>> intp intp))
;;   (lambda (y)
;;     (+ y x)))

;; function hello(x) {
;;     return function (y) {
;;         return x + y;
;;     };
;; };

;; function hello(x) {
;;     function (y) {
;;         if (! intp(y)) {
            
;;         }
;;         return x + y;
;;     };
;; }
;; };

;; (defun hello (x)
;;   (>> (intp intp) intp)
;;   (x 10))

;; (hello (lambda (y) (+ y y)))

;; function hello (x) {
;;     x = function () {
;;         if () ...

;;     }
;; }  


;; >> simple contract
;; (>> intp intp)

;; (>> intp :some-key intp intp)
;; (defun text (x &key some-key)
;;   )

;; >>* contract for functions with optional arguments or arbitrary many arguments
;; (>>* (intp) (positivep) any)
;; (defun test (x &optional (p 10))
;;   )
;; (>>* (intp) (:x intp) any))
;; (defun test (y &key x)
;;   )

;; (>>* (intp) () :rest (listof/c intp) :pre (lambda () ()) :post (lambda () ()) any)
;; (defun test (x &rest ints)
;;   )

;; (>>* (domain contracts) (optional or keywords) :rest () :pre () :post () (range contract))

;; >>i - named dependant contracts
;; (>>i (x intp)
;;      (y intp)
;;      (result intp))

;; (>>i (x intp)
;;      (y (x) (>/c x y))
;;      (result (x y) (and/c numberp (>/c (+ x y)))))


;; (defun/contract hello (x)
;;   (>> numberp numberp)
;;   (+ x x))

;; (defcontract list/c (pred)
;;   (lambda (lst)
;;     (dolist (item lst)
;;       (when (not (pred item))
;;         (return nil)))
;;     t))

;; (defmacro defcontract (name lambda-list body)
  
;;   `(defpsmacro ,name ,lambda-list (quote ,body)))
