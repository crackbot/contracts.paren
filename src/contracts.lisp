
(in-package :contracts.paren)

(defsection @contracts-combinators (:title "Contracts combinators")
  "Combinators are parenscript macros used to combine more than one
contract easily, combinator takes contracts as input and return a new
contract."
  )

(defpsmacro instanceof/c (cls)
  (let ((name (gensym)))
    `(lambda (,name)
       (instanceof ,name ,cls))))

(defpsmacro or/c (&rest preds)
  "(>> (or/c intp floatp)) value should pass at least one predicate
check"
  (let ((name (gensym)))
    `(lambda (arg)
       (let* ((res f)
              (,name (array ,@preds)))
         (dolist (pred ,name)
           (when (pred arg)
             (setf res t)
             (return)))
         res))))

(defpsmacro and/c (&rest preds)
  "(>> (and/c intp bigger-than-five)) value should pass all
predicates"
  (let ((name (gensym)))
    `(lambda (arg)
       (let ((res t)
             (,name (array ,@preds)))
         (dolist (pred ,name)
           (when (not (pred arg))
             (setf res f)
             (return)))
         res))))

(defpsmacro not/c (contract)
  `(lambda (arg)
     (not (,contract arg))))

(defpsmacro listof/c (pred)
  "(>> (listof/c intp)) value should be a list (array) and each list
value should pass a predicate"
  `(lambda (arg)
     (let ((res t))
       (dolist (item arg)
         (when (not (,pred item))
           (setf res f)
           (return)))
       res)))

(defpsmacro oneof/c (&rest preds)
  "(>> (oneof/c intp floatp)) value should be a list and pass at least
one predicate"
  (let ((name (gensym)))
    `(lambda (arg)
       (let ((res f)
             (,name (array ,@preds)))
         (dolist (val ,name)
           (when (equal val arg)
             (setf res t)
             (return)))
         res))))

(defpsmacro list/c (&rest preds)
  "(>> (list/c intp intp intp) intp) Produces a contract for a
list (array). The number of elements in the list must match the number
of arguments supplied to list/c, and each element of the list must
match the corresponding contract."
  (let ((name (gensym)))
    `(lambda (arg)
       (let ((res f)
             (,name (array ,@preds)))
         (do ((idx 0 (1+ idx)))
             ((<= idx (length ,name)))
           (let ((pred (aref ,name idx)))
             (when (not (pred (aref arg idx)))
               (setf res f)
               (return))))
         res))))

(defpsmacro object/c (&rest preds)
  "(>> (object/c :value intp :another floatp) any)"
  (let ((name (gensym)))
    `(lambda (arg)
       (let ((res t)
             (,name (array ,@preds)))
         (do ((key-idx 0 (+ 2 key))
              (val-idx 1 (+ 2 val)))
             ((>= val-idx (length ,name)))
           (let ((key (aref ,name key-idx))
                 (val (aref ,name val-idx)))
             (when (not (val (getprop arg key)))
               (setf res f)
               (return))))
         res))))

(defpsmacro maybe/c (pred)
  "(maybe/c intp) is equal to (or/c undefp intp)"
  (let ((name (gensym)))
    `(lambda (,name)
       (if (eq ,name undefined)
           t
           (,pred ,name)))))
