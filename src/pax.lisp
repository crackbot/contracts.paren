
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

;; ParenScript macro

(defparameter *docstrings* nil)

(defun get-docstrings ()
  (when (not *docstrings*)
    (setf *docstrings* (docparser:parse :contracts.paren)))
  *docstrings*)

(docparser:define-parser ps:defpsmacro (name (&rest args) &rest body)
  (let ((docstring (if (stringp (first body))
                       (first body)
                       nil)))
    (make-instance 'docparser::macro-node
                   :name name
                   :docstring docstring
                   :lambda-list args)))

(define-locative-type combinator ())

(defmethod locate-object (symbol (locative-type (eql 'combinator)) locative-args)
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'combinator))
                                locative-args stream)
  (let* ((arglist (gethash symbol ps::*MACRO-TOPLEVEL-LAMBDA-LIST*))
         (qres (docparser:query (get-docstrings)
                                :package-name :contracts.paren
                                :symbol-name symbol))
         (doc-string (docparser:node-docstring (aref qres 0))))
    (mgl-pax::locate-and-print-bullet locative-type locative-args symbol stream)
    ; (format stream "*")
    (write-char #\Space stream)
    (mgl-pax::print-arglist arglist stream)
    (write-char #\Space stream)

    (when doc-string
          (terpri stream)
          (format stream "~%~A" (mgl-pax::massage-docstring doc-string)))
    
    (terpri stream)))

;; Generate README.md

(defun generate-readme ()
  (let* ((sys-dir (asdf:system-source-directory :contracts.paren))
         (readme (merge-pathnames sys-dir "README.md")))
    (with-output-to-file (out readme :if-exists :supersede)
      (mgl-pax:document @main-manual :stream out :format :markdown))))




  
