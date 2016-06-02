(in-package #:cl-user)

(defpackage #:clr.threading
  (:use #:cl #:clr #:clr.impl #:clr.ffi #:clr.ffi.impl #:clr.collections)
  (:export

   ;; Atomics
   #:compare-exchange
   #:atomic-incf
   #:atomic-decf
   #:atomic-exchange

   ;; Threads
   #:thread
   #:threadp
   #:make-thread
   #:current-thread
   #:thread-join
   #:thread-yield

   ;; Locks
   #:lock
   #:make-lock
   #:lockp
   #:lock-acquire
   #:lock-try-acquire
   #:lock-release
   #:with-lock

   ;; Mutex
   #:mutex
   #:mutexp
   #:make-mutex
   #:mutex-acquire
   #:mutex-try-acquire
   #:mutex-release
   #:with-mutex

   ;; Semaphore
   #:semaphore
   #:semaphorep
   #:make-semaphore
   #:semaphore-acquire
   #:semaphore-try-acquire
   #:semaphore-release
   #:semaphore-count

   ;; Condition variables
   #:condition-variable
   #:condition-variable-p
   #:make-condition-variable
   #:cv-wait
   #:cv-pulse
   #:cv-pulse-all

   ;; Thread pool
   #:make-thread-pool
   #:+default-thread-pool-idle-timeout+
   #:tpoolcall
   ))

(in-package #:clr.threading)

(defstruct (condition-variable
            (:constructor %make-cv (lock condvar))
            (:copier nil)
            (:conc-name %cv-))
  (lock (make-lock) :type lock)
  (condvar (impl-make-condvar) :type impl-condvar))

(defun make-condition-variable (lock)
  (declare (type lock lock))
  (%make-cv lock (impl-make-condvar)))

(defun cv-wait (cv &optional timeout)
  (declare (type condition-variable cv))
  (impl-cond-wait (%cv-condvar cv) (%cv-lock cv) timeout))

(defun cv-pulse (cv)
  (declare (type condition-variable cv))
  (impl-cond-pulse (%cv-condvar cv)))

(defun cv-pulse-all (cv)
  (declare (type condition-variable cv))
  (impl-cond-pulse-all (%cv-condvar cv)))

(defun make-thread (function &key (args '())
                                  #+clr:windows sta
                             &allow-other-keys)
  (declare (type list args))
  #+clr:windows
  (if sta
    (impl-make-thread
     (lambda ()
       (with-sta-apartment (apply function args))))
    (impl-make-thread
     (lambda ()
       (with-mta-apartment (apply function args)))))
  #-clr:windows
  (impl-make-thread function args))
