(in-package #:clr)

(deftype simple-char-string () '(simple-array character (*)))

(deftype int8 () '(signed-byte 8))

(deftype uint8 () '(unsigned-byte 8))

(deftype int16 () '(signed-byte 16))

(deftype short () 'int16)

(deftype uint16 () '(unsigned-byte 16))

(deftype ushort () 'uint16)

(deftype int32 () '(signed-byte 32))

(deftype int () 'int32)

(deftype uint32 () '(unsigned-byte 32))

(deftype uint () 'uint32)

(deftype int64 () '(signed-byte 64))

(deftype long () #+clr:windows 'int32 #-clr:windows 'int64)

(deftype long-long () 'int64)

(deftype uint64 () '(unsigned-byte 64))

(deftype ulong () #+clr:windows 'uint32 #-clr:windows 'uint64)

(deftype ulong-long () 'uint64)

(deftype intptr () '(signed-byte #+clr:x32 32 #+clr:x64 64))

(deftype uintptr () '(unsigned-byte #+clr:x32 32 #+clr:x64 64))

(deftype index () '(integer 0 #.most-positive-fixnum))
