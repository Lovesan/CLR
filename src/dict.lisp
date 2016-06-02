(in-package #:clr.collections)

(defconstant +default-dict-length+ 10)
(defconstant +dict-resize-threshold+ 0.6)

(defstruct (dict (:constructor %make-dict)
                 (:conc-name %dict-)
                 (:copier nil))
  (buckets (make-array +default-dict-length+ :initial-element nil)
           :type simple-vector)
  (count 0 :type index))

(declaim (inline dict-size))
(defun dict-size (dict)
  "Returns number of elements in DICT"
  (declare (type dict dict))
  (%dict-count dict))

(declaim (inline dict-empty-p))
(defun dict-empty-p (dict)
  "Returns T in case of DICT is empty and NIL otherwise"
  (declare (type dict dict))
  (zerop (%dict-count dict)))

(defun dict-clear (dict)
  "Clears a DICT"
  (declare (type dict dict))
  (setf (%dict-buckets dict) (make-array +default-dict-length+ :initial-element nil)
        (%dict-count dict) 0)
  (values))

(defun dict-resize (dict)
  (declare (type dict dict)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((old-buckets (%dict-buckets dict))
         (new-length (the index (1+ (* 2 (length old-buckets)))))
         (new-buckets (make-array new-length :initial-element nil)))
    (dotimes (i (length old-buckets))
      (dolist (cons (svref old-buckets i))
        (let* ((hash (the intptr (hash (car cons))))
               (j (mod hash new-length)))
          (push cons (svref new-buckets j)))))
    (setf (%dict-buckets dict) new-buckets))
  (values))

(declaim (inline dict-get-bucket))
(defun dict-get-bucket (key hash dict)
  (declare (type dict dict)
           (type intptr hash))
  (let* ((buckets (%dict-buckets dict))
         (list (svref buckets (mod hash (length buckets)))))
    (loop (when (null list) (return nil))
          (when (equals key (car (first list))) (return (first list)))
          (setf list (rest list)))))

(defun dref (dict key &optional default)
  "Returns DICT value associated with a KEY or DEFAULT value
 in case of supplied key is missing."
  (declare (type dict dict))
  (let* ((hash (the intptr (hash key)))
         (bucket (dict-get-bucket key hash dict)))
    (if (null bucket)
      (values default nil)
      (values (cdr bucket) t))))

(defun (setf dref) (new-value dict key &optional default)
  "Replaces DICT value associated with a KEY or
 allocates new bucket for the key-value pair.
 DEFAULT parameter is ignored."
  (declare (type dict dict)
           (ignore default))
  (let* ((hash (the intptr (hash key)))
         (bucket (dict-get-bucket key hash dict)))
    (if (null bucket)
      (let ((threshold (/ (1+ (coerce (%dict-count dict) 'double-float))
                          (coerce (length (%dict-buckets dict))
                                  'double-float))))
        (when (> threshold +dict-resize-threshold+)
          (dict-resize dict))
        (let* ((buckets (%dict-buckets dict))
               (i (mod hash (length buckets))))
          (push (cons key new-value)
                (svref buckets i)))
        (incf (%dict-count dict))
        (values new-value nil))
      (progn (setf (cdr bucket) new-value)
             (values new-value t)))))

(defun dict-remove (dict key)
  "Removes a KEY and associated value from DICT"
  (declare (type dict dict))
  (let* ((hash (the intptr (hash key)))
         (buckets (%dict-buckets dict))
         (pos (mod hash (length buckets)))
         (list (svref buckets pos)))
    (cond ((null list) nil)
          ((equals key (car (first list)))
           (pop (svref buckets pos))
           (decf (%dict-count dict))
           t)
          (t (let ((next (cdr list)))
               (loop (when (null next) (return nil))
                     (when (equals key (car (first next)))
                       (setf (cdr list) (cdr next))
                       (decf (%dict-count dict))
                       (return t))
                     (psetf next (cdr next)
                            list next)))))))

(defun make-dict (&optional (source '()))
  "Returns new DICT populated from SOURCE
 which is either alist or hash-table"
  (let ((dict (%make-dict)))
    (etypecase source
      (list
       (dolist (kv source)
         (setf (dref dict (car kv)) (cdr kv))))
      (hash-table
       (with-hash-table-iterator (iter source)
         (loop (multiple-value-bind
                     (more key value) (iter)
                 (unless more (return))
                 (setf (dref dict key) value))))))
    dict))

(defun dict-alist (dict)
  "Converts DICT to alist"
  (declare (type dict dict))
  (let ((list '())
        (buckets (%dict-buckets dict)))
    (dotimes (i (length buckets))
      (dolist (b (svref buckets i))
        (push (cons (car b) (cdr b)) list)))
    list))

(defun dict-hash-table (dict)
  "Converts DICT to equal-based hash-table"
  (declare (type dict dict))
  (let ((hash (make-hash-table :test 'equal))
        (buckets (%dict-buckets dict)))
    (dotimes (i (length buckets))
      (dolist (b (svref buckets i))
        (setf (gethash (car b) hash) (cdr b))))
    hash))

(defmethod print-object ((dict dict) stream)
  (print-unreadable-object (dict stream :type t :identity t)
    (format stream "~s ~a ~s ~a"
            :size (%dict-count dict)
            :buckets (length (%dict-buckets dict))))
  dict)
