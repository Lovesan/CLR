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
   ))
