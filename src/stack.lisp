(in-package #:clr.collections)

(defstruct (stack
            (:constructor %make-stack ())
            (:conc-name %st-))
  "Linked list stack"
  (head nil :type list)
  (size 0 :type index))

(defun make-stack ()
  (%make-stack))

(defun stack-push (stack value)
  (declare (type stack stack))
  (push value (%st-head stack))
  (incf (%st-size stack))
  stack)

(defun stack-pop (stack)
  (declare (type stack stack))
  (if (null (%st-head stack))
    (values nil nil)
    (let ((val (pop (%st-head stack))))
      (decf (%st-size stack))
      (values val t))))

(defun stack-peek (stack)
  (declare (type stack stack))
  (if (null (%st-head stack))
    (values nil nil)
    (values (car (%st-head stack)) t)))

(defun stack-size (stack)
  (declare (type stack stack))
  (%st-size stack))

(defun stack-empty-p (stack)
  (declare (type stack stack))
  (zerop (%st-size stack)))

(defun stack-clear (stack)
  (declare (type stack stack))
  (setf (%st-size stack) 0
        (%st-head stack) '()))

