(in-package #:cl-user)

(defpackage #:clr
  (:use #:cl)
  (:export

   ;;; features
   #:sbcl
   #:x32
   #:x64
   #:x86
   #:x86-64
   #:windows
   #:unix
   #:linux

   ;;; base types
   #:int8
   #:uint8
   #:int16
   #:short
   #:uint16
   #:ushort
   #:int32
   #:int
   #:uint32
   #:uint
   #:int64
   #:long
   #:long-long
   #:uint64
   #:ulong
   #:ulong-long
   #:intptr
   #:uintptr
   #:simple-char-string
   #:index

   ;;; base utils
   #:disposable-struct
   #:disposable-struct-p
   #:disposable-object
   #:disposedp
   #:dispose
   #:defdispose
   #:with-object
   #:with-objects
   #:hash
   #:equals
   ))
