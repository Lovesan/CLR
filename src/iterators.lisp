(in-package #:clr.collections)

(defgeneric iter-next (iter)
  (:documentation "Moves iterator to next value."))

(defgeneric iter-current (iter)
  (:documentation "Returns current value on iterator position."))

(defgeneric iter-reset (iter)
  (:documentation "Resets an iterator to original position."))

(defgeneric iter-of (object)
  (:documentation "Returns an iterator for an object"))

(defstruct (list-iterator
            (:constructor list-iterator (head &aux (next head)))
            (:conc-name %li-))
  (head nil :type list)
  (next nil :type list)
  (current nil :type t))

(defmethod iter-next ((iter list-iterator))
  (cond
    ((%li-next iter)
     (setf (%li-current iter) (car (%li-next iter))
           (%li-next iter) (cdr (%li-next iter)))
     t)
    (t (setf (%li-current iter) nil)
       nil)))

(defmethod iter-reset ((iter list-iterator))
  (setf (%li-next iter) (%li-head iter)
        (%li-current iter) nil)
  t)

(defmethod iter-current ((iter list-iterator))
  (%li-current iter))

(defmethod iter-of ((obj list)) (list-iterator obj))

(defmacro doiter ((var iterable &optional return-form) &body body)
  (with-gensyms (iter start)
    `(prog ((,iter (iter-of ,iterable)))
        ,start
        (unless (iter-next ,iter)
          (let ((,var nil))
            (declare (ignorable ,var))
            (return ,return-form)))
        (let ((,var (iter-current ,iter)))
          ,@body)
        (go ,start))))
