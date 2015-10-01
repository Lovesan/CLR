(in-package #:clr.threading)

(defconstant +default-thread-pool-idle-timeout+ 5000)

(defstruct (thread-pool
            (:constructor %make-tpool)
            (:copier nil)
            (:predicate tpoolp)
            (:conc-name %tpool-))
  "Represents a pool of threads."
  (threads     '()             :type list) ;; list of all active threads
  (min-threads  0              :type index ;; minimum number of threads
                :read-only t)
  (max-threads  (cpu-count)    :type index ;; maximum number of threads
                :read-only t)
  (live-threads 0              :type index) ;; number of active threads
  (busy-threads 0              :type index) ;; number of busy threads
  (cv           (make-condition-variable (make-lock)) ;; condvar used to signal new jobs
                :type condition-variable
                :read-only t)
  (lock         (make-lock)    :type lock ;; pool lock
                :read-only t)
  (jobs         (make-queue)   :type queue ;; queue of jobs
                               :read-only t)
  (timeout      +default-thread-pool-idle-timeout+ ;; thread idle timeout
                :type index
                :read-only t))

(defun tpool-function (tpool)
  (declare (type thread-pool tpool))
  (with-accessors ((threads %tpool-threads)
                   (min-threads %tpool-min-threads)
                   (busy-threads %tpool-busy-threads)
                   (live-threads %tpool-live-threads)
                   (cv %tpool-cv)
                   (lock %tpool-lock)
                   (jobs %tpool-jobs)
                   (timeout %tpool-timeout))
      tpool
    (let ((thread (current-thread)))
      (with-lock (lock)
        (incf live-threads)
        (push thread threads))
      (unwind-protect
           (loop :named job-loop :do
             (let ((job (with-lock (lock)
                          (loop :while (queue-empty-p jobs) :do
                            (when (and (not (cv-wait cv timeout))
                                       (> live-threads min-threads))
                              (return-from job-loop)))
                          (incf busy-threads)
                          (queue-remove jobs))))
               (unwind-protect
                    (apply (car job) (cdr job))
                 (with-lock (lock) (decf busy-threads)))))
        (with-lock (lock)
          (decf live-threads)
          (setf threads (remove thread threads :test #'eq)))))))

(defun tpoolcall (tpool func &rest args)
  (declare (type thread-pool tpool))
  (with-accessors ((min-threads %tpool-min-threads)
                   (max-threads %tpool-max-threads)
                   (busy-threads %tpool-busy-threads)
                   (live-threads %tpool-live-threads)
                   (cv %tpool-cv)
                   (lock %tpool-lock)
                   (jobs %tpool-jobs))
      tpool
    (with-lock (lock)
      (queue-add jobs (cons func args))
      (when (or (< live-threads min-threads)
                (and (< live-threads max-threads)
                     (>= busy-threads live-threads)))
        (make-thread #'tpool-function :args (list tpool)))
      (cv-pulse cv)
      (values))))

(defun make-thread-pool (&key (min-threads 0)
                              max-threads
                              (idle-timeout +default-thread-pool-idle-timeout+))
  (declare (type index min-threads)
           (type (or null index) max-threads)
           (type index idle-timeout))
  (let* ((cpu-count (cpu-count))
         (max-threads (max min-threads
                           cpu-count
                           (or max-threads 0)))
         (lock (make-lock))
         (cv (make-condition-variable lock)))
    (%make-tpool :min-threads min-threads
                 :max-threads max-threads
                 :lock lock
                 :cv cv
                 :timeout idle-timeout)))
