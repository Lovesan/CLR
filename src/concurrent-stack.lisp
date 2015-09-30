(in-package #:clr.collections)

(defstruct (concurrent-stack
            (:constructor %make-cstack)
            (:predicate cstack-p)
            (:conc-name %cst-))
  (size 0 :type index)
  (head '() :type list))

(defun make-concurrent-stack ()
  (%make-cstack))

(defun cstack-push (cstack value)
  (declare (type concurrent-stack cstack))
  (loop :with new = (cons value nil)
        :for head = (%cst-head cstack) :do
          (setf (cdr new) head)
          (when (eq head (compare-exchange (%cst-head cstack) head new))
            (atomic-incf (%cst-size cstack))
            (return cstack))))

(defun cstack-pop (cstack)
  (declare (type concurrent-stack cstack))
  (loop :for head = (%cst-head cstack)
        :for next = (cdr head) :do
          (when (null head)
            (return (values nil nil)))
          (when (eq head (compare-exchange (%cst-head cstack) head next))
            (atomic-decf (%cst-size cstack))
            (return (values (car head) t)))))

(defun cstack-peek (cstack)
  (declare (type concurrent-stack cstack))
  (let ((head (%cst-head cstack)))
    (if head
      (values (car head) t)
      (values nil nil))))

(defun cstack-size (cstack)
  (declare (type concurrent-stack cstack))
  (%cst-size cstack))

(defun cstack-empty-p (cstack)
  (declare (type concurrent-stack cstack))
  (zerop (%cst-size cstack)))

(defun cstack-clear (cstack)
  (declare (type concurrent-stack cstack))
  (setf (%cst-size cstack) 0
        (%cst-head cstack) '()))
