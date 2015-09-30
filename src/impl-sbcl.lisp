(in-package #:cl-user)

(defpackage #:clr.impl
  (:use #:cl)
  (:export
   #:pointer
   #:pointerp
   #:pointer-value
   #:nullptr
   #:nullptrp
   #:ptr=
   #:ptr+
   #:deref-int8
   #:deref-uint8
   #:deref-int16
   #:deref-uint16
   #:deref-int32
   #:deref-uint32
   #:deref-int64
   #:deref-uint64
   #:deref-intptr
   #:deref-uintptr
   #:deref-float
   #:deref-double
   #:deref-char
   #:deref-ptr
   #:mem-alloc
   #:mem-free
   #:with-mem-ptr
   #:with-stack-ptr
   #:with-bytes-ptr
   #:with-string-ptr
   #:impl-ctype
   #:impl-defclib
   #:impl-ffcall
   #:impl-ptrcall
   #:compare-exchange
   #:atomic-incf
   #:atomic-decf
   #:finalize
   #:thread
   #:make-thread
   #:threadp
   #:current-thread
   #:condvar
   #:make-condvar
   #:condvarp
   #:lock
   #:make-lock
   #:lockp
   #:lock-acquire
   #:lock-release
   #:with-lock
   #:cond-wait
   #:cond-pulse
   #:cond-pulse-all
   ))

(in-package #:clr.impl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (f '(:clr
               clr:sbcl
               #+win32 clr:windows
               #-win32 clr:unix
               #+x86-64 clr:x86-64
               #+(and x86 (not x86-64)) clr:x86))
    (pushnew f *features*))
  (if (= sb-vm:n-word-bits 64)
    (pushnew 'clr:x64 *features*)
    (pushnew 'clr:x32 *features*)))

(deftype pointer () 'sb-sys:system-area-pointer)

(declaim (inline pointerp))
(defun pointerp (obj)
  (typep obj 'sb-sys:system-area-pointer))

(declaim (inline pointer-value))
(defun pointer-value (p)
  (declare (type pointer p))
  (sb-sys:sap-int p))

(declaim (inline pointer))
(defun pointer (addr)
  (declare (type (unsigned-byte #+clr:x64 64 #+clr:x32 32) addr))
  (sb-sys:int-sap addr))

(declaim (inline nullptr))
(defun nullptr () (sb-sys:int-sap 0))

(declaim (inline nullptrp))
(defun nullptrp (ptr)
  (declare (type pointer ptr))
  (zerop (sb-sys:sap-int ptr)))

(declaim (inline ptr=))
(defun ptr= (ptr1 ptr2)
  (declare (type pointer ptr1 ptr2))
  (sb-sys:sap= ptr1 ptr2))

(declaim (inline ptr+))
(defun ptr+ (ptr1 offset)
  (declare (type pointer ptr1)
           (type (integer 0 #.most-positive-fixnum) offset))
 (sb-sys:sap+ ptr1 offset))

(declaim (inline deref-int8))
(defun deref-int8 (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:signed-sap-ref-8 p offset))

(declaim (inline (setf deref-int8)))
(defun (setf deref-int8) (new-value p &optional (offset 0))
  (declare (type (signed-byte 8) new-value)
           (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (setf (sb-sys:signed-sap-ref-8 p offset) new-value))

(declaim (inline deref-uint8))
(defun deref-uint8 (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:sap-ref-8 p offset))

(declaim (inline (setf deref-uint8)))
(defun (setf deref-uint8) (new-value p &optional (offset 0))
  (declare (type (unsigned-byte 8) new-value)
           (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (setf (sb-sys:sap-ref-8 p offset) new-value))

(declaim (inline deref-int16))
(defun deref-int16 (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:signed-sap-ref-16 p offset))

(declaim (inline (setf deref-int16)))
(defun (setf deref-int16) (new-value p &optional (offset 0))
  (declare (type (signed-byte 16) new-value)
           (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (setf (sb-sys:signed-sap-ref-16 p offset) new-value))

(declaim (inline deref-uint16))
(defun deref-uint16 (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:sap-ref-16 p offset))

(declaim (inline (setf deref-uint16)))
(defun (setf deref-uint16) (new-value p &optional (offset 0))
  (declare (type (unsigned-byte 16) new-value)
           (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (setf (sb-sys:sap-ref-16 p offset) new-value))

(declaim (inline deref-int32))
(defun deref-int32 (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:signed-sap-ref-32 p offset))

(declaim (inline (setf deref-int32)))
(defun (setf deref-int32) (new-value p &optional (offset 0))
  (declare (type (signed-byte 32) new-value)
           (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (setf (sb-sys:signed-sap-ref-32 p offset) new-value))

(declaim (inline deref-uint32))
(defun deref-uint32 (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:sap-ref-32 p offset))

(declaim (inline (setf deref-uint32)))
(defun (setf deref-uint32) (new-value p &optional (offset 0))
  (declare (type (unsigned-byte 32) new-value)
           (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (setf (sb-sys:sap-ref-32 p offset) new-value))

(declaim (inline deref-int64))
(defun deref-int64 (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:signed-sap-ref-64 p offset))

(declaim (inline (setf deref-int64)))
(defun (setf deref-int64) (new-value p &optional (offset 0))
  (declare (type (signed-byte 64) new-value)
           (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (setf (sb-sys:signed-sap-ref-64 p offset) new-value))

(declaim (inline deref-uint64))
(defun deref-uint64 (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:sap-ref-64 p offset))

(declaim (inline (setf deref-uint64)))
(defun (setf deref-uint64) (new-value p &optional (offset 0))
  (declare (type (unsigned-byte 64) new-value)
           (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (setf (sb-sys:sap-ref-64 p offset) new-value))

(declaim (inline deref-intptr))
(defun deref-intptr (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:signed-sap-ref-word p offset))

(declaim (inline (setf deref-intptr)))
(defun (setf deref-intptr) (new-value p &optional (offset 0))
  (declare (type (signed-byte #+clr:x32 32 #+clr:x64 64) new-value)
           (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (setf (sb-sys:signed-sap-ref-word p offset) new-value))

(declaim (inline deref-uintptr))
(defun deref-uintptr (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:sap-ref-word p offset))

(declaim (inline (setf deref-uintptr)))
(defun (setf deref-uintptr) (new-value p &optional (offset 0))
  (declare (type (unsigned-byte #+clr:x32 32 #+clr:x64 64) new-value)
           (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (setf (sb-sys:sap-ref-word p offset) new-value))

(declaim (inline deref-float))
(defun deref-float (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:sap-ref-single p offset))

(declaim (inline (setf deref-float)))
(defun (setf deref-float) (new-value p &optional (offset 0))
  (declare (type single-float new-value)
           (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (setf (sb-sys:sap-ref-single p offset) new-value))

(declaim (inline defer-double))
(defun deref-double (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:sap-ref-double p offset))

(declaim (inline (setf deref-double)))
(defun (setf deref-double) (new-value p &optional (offset 0))
  (declare (type double-float new-value)
           (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (setf (sb-sys:sap-ref-double p offset) new-value))

(declaim (inline defer-ptr))
(defun deref-pointer (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:sap-ref-sap p offset))

(declaim (inline (setf deref-ptr)))
(defun (setf deref-ptr) (new-value p &optional (offset 0))
  (declare (type pointer new-value)
           (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (setf (sb-sys:sap-ref-sap p offset) new-value))

(declaim (inline deref-char))
(defun deref-char (p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset))
  (sb-sys:sap-ref-32 p offset))

(declaim (inline (setf deref-char)))
(defun (setf deref-char) (new-value p &optional (offset 0))
  (declare (type pointer p)
           (type (integer 0 #.most-positive-fixnum) offset)
           (type (unsigned-byte 32) new-value))
  (setf (sb-sys:sap-ref-32 p offset) new-value))

(defun mem-alloc (size)
  (declare (type (integer 0 #.most-positive-fixnum) size))
  (sb-alien:alien-sap (sb-alien:make-alien (sb-alien:unsigned 8) size)))

(defun mem-free (ptr)
  (declare (type pointer ptr))
  (sb-alien:free-alien (sb-alien:sap-alien ptr (* (array (sb-alien:unsigned 8))))))

(defmacro with-mem-ptr ((var size) &body body)
  (declare (type symbol var))
  `(let ((,var (mem-alloc ,size)))
     (unwind-protect (progn ,@body) (mem-free ,var))))

(defmacro with-stack-ptr ((var size) &body body)
  (declare (type symbol var)
           (type (integer 0 #.most-positive-fixnum) size))
  `(sb-alien:with-alien ((,var (array (sb-alien:unsigned 8) ,size)))
     (let ((,var (sb-alien:alien-sap ,var)))
       ,@body)))

(defmacro with-bytes-ptr ((var vector) &body body)
  (declare (type symbol var))
  `(let ((,var ,vector))
     (sb-sys:with-pinned-objects (,var)
       (let ((,var (sb-sys:vector-sap ,var)))
         ,@body))))

(defmacro with-string-ptr ((var string) &body body)
  (declare (type symbol var))
  `(with-bytes-ptr (,var ,string) ,@body))

(defun impl-ctype (type)
  (ecase type
    ((:* :pointer :ptr) 'sb-sys:system-area-pointer)
    ((:int8 :char) '(sb-alien:signed 8))
    ((:uint8 :byte) '(sb-alien:unsigned 8))
    ((:int16 :short) '(sb-alien:signed 16))
    ((:uint16 :ushort) '(sb-alien:unsigned 16))
    ((:int32 :int :bool :boolean #+clr:windows :long) '(sb-alien:signed 32))
    ((:uint32 :uint #+clr:windows :ulong) '(sb-alien:unsigned 32))
    ((:int64 :long-long #-clr:windows :long) '(sb-alien:signed 64))
    ((:uint64 :ulong-long #-clr:windows :ulong) '(sb-alien:unsigned 64))
    (:intptr '(sb-alien:signed #+clr:x32 32 #+clr:x64 64))
    (:uintptr '(sb-alien:unsigned #+clr:x32 32 #+clr:x64 64))
    (:float 'sb-alien:single-float)
    (:double 'sb-alien:double-float)
    (:void 'sb-alien:void)))

(defmacro impl-defclib (name pathstring)
  (declare (ignore name))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (sb-alien:load-shared-object ,pathstring)))

(defmacro impl-ffcall (name cconv library rettype (&rest types) (&rest values))
  (declare (ignore cconv library)
           (type string name))
  `(sb-alien:alien-funcall
    (sb-alien:extern-alien ,name (function ,rettype ,@types))
    ,@values))

(defmacro impl-ptrcall (ptr cconv rettype (&rest types) (&rest values))
  (declare (ignore cconv))
  `(sb-alien:alien-funcall
    (sb-alien:sap-alien ,ptr (function ,rettype ,@types))
    ,@values))

(defmacro compare-exchange (place comparand value)
  `(sb-ext:compare-and-swap ,place ,comparand ,value))

(defmacro atomic-incf (place &optional (diff 1))
  (let ((val (gensym))
        (new (gensym))
        (d (gensym)))
    `(loop :with ,d = ,diff
           :for ,val = ,place
           :for ,new = (the fixnum (+ ,d ,val)) :do
           (when (eql ,val (sb-ext:compare-and-swap ,place ,val ,new))
             (return ,new)))))

(defmacro atomic-decf (place &optional (diff 1))
  `(atomic-incf ,place (- ,diff)))

(defun finalize (object callback)
  (sb-ext:finalize object callback))

(defun make-thread (function &rest args)
  (sb-thread:make-thread function :arguments args))

(deftype thread () 'sb-thread:thread)

(defun threadp (obj) (typep obj 'thread))

(defun current-thread ()
  sb-thread:*current-thread*)

(defun make-condvar ()
  (sb-thread:make-waitqueue))

(deftype condvar () 'sb-thread:waitqueue)

(defun condvarp (obj) (typep obj 'condvar))

(defun make-lock ()
  (sb-thread:make-mutex))

(deftype lock () 'sb-thread:mutex)

(defun lockp (obj) (typep obj 'lock))

(defun lock-acquire (lock)
  (declare (type lock lock))
  (sb-thread:grab-mutex lock))

(defun lock-release (lock)
  (declare (type lock lock))
  (sb-thread:release-mutex lock))

(defmacro with-lock ((lock) &body body)
  `(sb-thread:with-mutex (,lock) ,@body))

(defun cond-wait (cv lock &optional timeout)
  (declare (type lock lock)
           (type condvar cv)
           (type (or null (integer 0 #.most-positive-fixnum)) timeout))
  (let ((rv (sb-thread:condition-wait
             cv lock :timeout (and timeout (/ timeout 1000.0d0)))))
    (when (and timeout (not rv))
      (lock-acquire lock))
    rv))

(defun cond-pulse (cv)
  (declare (type condvar cv))
  (sb-thread:condition-notify cv))

(defun cond-pulse-all (cv)
  (declare (type condvar cv))
  (sb-thread:condition-broadcast cv))
