
(in-package :contracts.paren)

(define-locative-type static-ps-function (var))

(defmethod locate-object (symbol (locative-type (eql 'static-ps-function)) locative-args)
  (make-reference symbol (cons locative-type locative-args)))

(defun find-defun-form (forms name)
  (dolist (form forms)
    (when (eql name (cadr form))
      (return-from find-defun-form form))))

(defmethod locate-and-document (symbol (locative-type (eql 'static-ps-function))
                                locative-args stream)
                                        ;(let ((arglist (gethash symbol ps::*MACRO-TOPLEVEL-LAMBDA-LIST*)))
  (let* ((forms (cdr (eval (car locative-args))))
         (defun-form (find-defun-form forms symbol)))
    (destructuring-bind (def name args &rest body)
        defun-form
      (let ((doc-string (if (and (> (length body) 1)
                                 (stringp (car body)))
                            (car body)
                            nil)))
        (format stream "- [contract] ")
        (format stream "~A" symbol)
        (write-char #\Space stream)
        (mgl-pax::print-arglist args stream)
        (write-char #\Space stream)
        (when doc-string
          (terpri stream)
          (format stream "~%~A" (mgl-pax::massage-docstring doc-string)))
        (terpri stream)))))

(define-locative-type psmacro ())

(defmethod locate-object (symbol (locative-type (eql 'psmacro)) locative-args)
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'psmacro))
                                locative-args stream)
  (let ((arglist (gethash symbol ps::*MACRO-TOPLEVEL-LAMBDA-LIST*)))
    (mgl-pax::locate-and-print-bullet locative-type locative-args symbol stream)
    ; (format stream "*")
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


;; (defmethod locate-object (symbol (locative-type (eql 'contract)) locative-args)
;;   (make-reference symbol (cons locative-type locative-args)))

;; (defmethod locate-and-document (symbol (locative-type (eql 'contract))
;;                                 locative-args stream)
;;     (format stream "- [contract] *")
;;     (mgl-pax::print-name (prin1-to-string symbol) stream)
;;     (format stream "*")
;;     (terpri stream))
