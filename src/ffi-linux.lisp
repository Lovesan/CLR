(in-package #:cl-user)

(defpackage #:clr.ffi.impl
  (:use #:cl #:clr #:clr.impl #:clr.ffi)
  (:export
   #:cpu-count))

(in-package #:clr.ffi.impl)

(defconstant +sc-nprocessors-onln+ 84)

(defun sysconf (num)
  (declare (type int num))
  (ffcall "sysconf" :int num :long))

(defun cpu-count ()
  "Retrieves current CPU count"
  (let ((rv (sysconf +sc-nprocessors-onln+)))
    (when (< rv 0)
      (error "Unable to get system information"))
    (the index rv)))
