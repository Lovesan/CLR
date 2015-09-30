(in-package #:clr.io)

(defconstant +iopool-timeout+ 5000)

(defstruct (io-pool
            (:constructor %make-iopool)
            (:copier nil)
            (:predicate iopoolp)
            (:conc-name %iopool-))
  "Represents a pool of I/O threads."
  (threads     '()             :type list) ;; list of all active threads
  (min-threads  0              :type index ;; minimum number of threads
                :read-only t)
  (max-threads  (cpu-count)    :type index ;; maximum number of threads
                :read-only t)
  (live-threads 0              :type index) ;; number of active threads
  (busy-threads 0              :type index) ;; number of busy threads
  (lock         (make-lock)    :type lock ;; pool lock
                :read-only t)
  (jobs         (make-hash-table :test #'eql)
                :type hash-table
                :read-only t)
  (iocp         (create-iocp)    :type handle
                :read-only t))

(defun iopool-function (iopool)
  (declare (type io-pool iopool))
  (let ((thread (current-thread)))
    (with-accessors ((threads %iopool-threads)
                     (live-threads %iopool-live-threads)
                     (busy-threads %iopool-busy-threads)
                     (min-threads %iopool-min-threads)
                     (jobs %iopool-jobs)
                     (iocp %iopool-iocp)
                     (lock %iopool-lock))
        iopool
      (with-lock (lock)
        (push thread threads)
        (incf live-threads))
      (unwind-protect
           (loop :named job-loop :do
             (multiple-value-bind
                   (job nbytes ovlpd)
                 (loop (multiple-value-bind
                             (ok nbytes key ovlpd)
                           (get-iocp-completion iocp +iopool-timeout+)
                         (declare (ignore key))
                         (with-lock (lock)
                           (if ok
                             (let* ((addr (overlapped-address ovlpd))
                                    (job (gethash addr jobs)))
                               (when job
                                 (incf busy-threads)
                                 (remhash addr jobs)
                                 (return (values job nbytes ovlpd))))
                             (when (and (> live-threads min-threads)
                                        (zerop (hash-table-count jobs)))
                               (return-from job-loop))))))
               (unwind-protect
                    (progn (free-overlapped ovlpd)
                           (apply (car job) nbytes (cdr job)))
                 (with-lock (lock)
                   (decf busy-threads)))))
        (with-lock (lock)
          (setf threads (remove thread threads :test #'eq))
          (decf live-threads))))))

(defun iopool-add (iopool ovlpd func &rest args)
  (declare (type io-pool iopool)
           (type overlapped ovlpd))
  (with-accessors ((live-threads %iopool-live-threads)
                   (busy-threads %iopool-busy-threads)
                   (min-threads %iopool-min-threads)
                   (max-threads %iopool-max-threads)
                   (jobs %iopool-jobs)
                   (iocp %iopool-iocp)
                   (lock %iopool-lock))
      iopool
    (with-lock (lock)
      (setf (gethash (overlapped-address ovlpd) jobs)
            (cons func args))
      (when (or (< live-threads min-threads)
                (and (< live-threads max-threads)
                     (>= busy-threads live-threads)))
        (make-thread #'iopool-function iopool))
      t)))

(defun make-io-pool ()
  (%make-iopool))

(defstruct (file
            (:constructor %make-file)
            (:conc-name %fl-))
  (handle (nullptr) :type handle)
  (direction :input))

(defstruct (socket
            (:include file)
            (:constructor %make-socket)))

(defconstant +file-flag-overlapped+ #x40000000)

(defun file-open (path &key (share-read t)
                            (share-write t)
                            (share-delete t)
                            (:direction :input)
                            mode)
  (let* ((path (namestring (pathname path)))
         (access (ecase direction
                   (:input #x80000000)
                   (:output #x40000000)
                   (:io (logior #x80000000 #x40000000))))
         (share (logior (or (and share-read #x00000001) 0)
                        (or (and share-write #x00000002) 0)
                        (or (and share-delete #x00000004) 0)))
         (disposition (ecase mode
                        (:supersede )
                        ))
         (flags +file-flag-overlapped+)
         (handle (create-file-native path access share disposition flags)))
    )
  )
