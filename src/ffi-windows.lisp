(in-package #:cl-user)

(defpackage #:clr.ffi.impl
  (:use #:cl #:clr #:clr.impl #:clr.ffi)
  (:export
   #:cpu-count
   #:with-sta-apartment))

(in-package #:clr.ffi.impl)

(defclib kernel32 "kernel32.dll")

(defclib ole32 "ole32.dll")

(defmacro syscall (name &rest args-and-types)
  `(ffcall (,name :library kernel32 :convention :stdcall) ,@args-and-types))

(defmacro olecall (name &rest args-and-types)
  `(ffcall (,name :library ole32 :convention :stdcall) ,@args-and-types))

(defcstruct system-info
  (oem-id :uint32)
  (page-size :uint32)
  (min-alloc :pointer)
  (max-alloc :pointer)
  (act-proc-mask :uintptr)
  (num-proc :uint32)
  (proc-type :uint32)
  (alloc-gran :uint32)
  (proc-level :uint32)
  (proc-rev :uint32))

(defun cpu-count ()
  "Retrieves current CPU count"
  (with-cptr (p system-info)
    (unless (syscall "GetSystemInfo" :pointer p :bool)
      (error "Unable to get system information"))
    (cslot p system-info num-proc)))

(defun initialize-sta ()
  (let ((rv (olecall "OleInitialize" :pointer (nullptr) :int)))
    (when (< rv 0)
      (error "Unable to initialize STA"))
    t))

(defun release-sta ()
  (olecall "OleUninitialize")
  t)

(defmacro with-sta-apartment (&body body)
  `(progn (initialize-sta)
          (unwind-protect (progn ,@body)
            (release-sta))))
