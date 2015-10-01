(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((cl-file (f) (load (compile-file (truename f)))))
    (cl-file "src/clr.lisp")
    #+sbcl
    (cl-file "src/impl-sbcl.lisp")
    #-(or sbcl)
    (error "Implementation not supported")))

(flet ((cl-file (f) (load (compile-file (truename f)))))
  (cl-file "src/base-types.lisp")
  (cl-file "src/utils.lisp")
  (cl-file "src/collections.lisp")
  (cl-file "src/iterators.lisp")
  (cl-file "src/queue.lisp")
  (cl-file "src/stack.lisp")
  (cl-file "src/concurrent-stack.lisp")
  (cl-file "src/ffi.lisp")
  #+clr:windows
  (cl-file "src/ffi-windows.lisp")
  #+clr:linux
  (cl-file "src/ffi-linux.lisp")
  (cl-file "src/threading.lisp")
  (cl-file "src/thread-pool.lisp")
  )
