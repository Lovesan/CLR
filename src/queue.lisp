(in-package #:clr.collections)

(defstruct (queue
            (:constructor %make-queue ())
            (:conc-name %q-))
  "Linked list queue"
  (head nil :type list)
  (tail nil :type list)
  (size 0 :type index))

(defun make-queue ()
  (%make-queue))

(defun queue-add (queue element)
  (declare (type queue queue))
  (let ((item (list element)))
    (if (%q-head queue)
      (setf (cdr (%q-tail queue)) item
            (%q-tail queue) item)
      (setf (%q-head queue) item
            (%q-tail queue) item))
    (incf (%q-size queue)))
  queue)

(defun queue-remove (queue)
  (declare (type queue queue))
  (if (%q-head queue)
    (let ((item (pop (%q-head queue))))
      (unless (%q-head queue)
        (setf (%q-tail queue) '()))
      (decf (%q-size queue))
      (values item t))
    (values nil nil)))

(defun queue-peek (queue)
  (declare (type queue queue))
  (if (%q-head queue)
    (values (car (%q-head queue)) t)
    (values nil nil)))

(defun queue-size (queue)
  (declare (type queue queue))
  (%q-size queue))

(defun queue-empty-p (queue)
  (declare (type queue queue))
  (zerop (%q-size queue)))

(defun queue-clear (queue)
  (declare (type queue queue))
  (setf (%q-head queue) '()
        (%q-tail queue) '()
        (%q-size queue) 0))
