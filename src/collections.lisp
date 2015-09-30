(in-package #:cl-user)

(defpackage #:clr.collections
  (:use #:cl #:clr #:clr.impl #:clr.utils)
  (:export
   #:queue
   #:queue-p
   #:make-queue
   #:queue-add
   #:queue-remove
   #:queue-peek
   #:queue-clear
   #:queue-size
   #:queue-empty-p
   #:stack
   #:stack-p
   #:make-stack
   #:stack-push
   #:stack-pop
   #:stack-clear
   #:stack-peek
   #:stack-size
   #:stack-empty-p
   #:concurrent-stack
   #:make-concurrent-stack
   #:cstack-p
   #:cstack-push
   #:cstack-peek
   #:cstack-pop
   #:cstack-clear
   #:cstack-size
   #:cstack-empty-p
   ))
