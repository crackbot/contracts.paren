
(in-package :contracts.paren)

(define-locative-type psmacro ())

(defmethod locate-object (symbol (locative-type (eql 'psmacro)) locative-args)
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'psmacro))
                                locative-args stream)

  
  
  ;(declare (ignore locative-args))
  ;(break)
  (let ((arglist (gethash symbol ps::*MACRO-TOPLEVEL-LAMBDA-LIST*)))
    
  
    (format stream "- [psmacro] *")
    (mgl-pax::print-name (prin1-to-string symbol) stream)
    (format stream "*")
    (write-char #\Space stream)
    (mgl-pax::print-arglist arglist stream)
    (write-char #\Space stream)
    (terpri stream)))
  ;; (let ((arglist (swank-backend:arglist symbol)))
  ;;   (mgl-pax::print-arglist arglist stream)
  ;;   (terpri stream)))
  ;;   (mgl-pax::with-argument-symbols ((mgl-pax::macro-arg-names arglist))
  ;;     (mgl-pax::maybe-print-docstring symbol 'function stream))))

;; (defmethod locate-and-find-source (symbol (locative-type (eql 'psmacro))
;;                                    locative-args)
;;   (declare (ignore locative-args))
;;   (find-source (macro-function symbol)))


(defmethod locate-object (symbol (locative-type (eql 'contract)) locative-args)
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'contract))
                                locative-args stream)
    (format stream "- [contract] *")
    (mgl-pax::print-name (prin1-to-string symbol) stream)
    (format stream "*")
    (terpri stream))
