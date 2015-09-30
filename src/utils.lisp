(in-package #:cl-user)

(defpackage #:clr.utils
  (:use #:cl #:clr)
  (:export #:with-gensyms
           #:mklist
           #:lastcar
           ))

(in-package #:clr.utils)

(defmacro with-gensyms ((&rest names) &body body)
  (let ((bindings (mapcar (lambda (name)
                            (if (symbolp name)
                              (list name (list 'quote (gensym (string name))))
                              (destructuring-bind (var name) name
                                (list var (list 'quote (gensym (string name)))))))
                          names)))
    `(let ,bindings ,@body)))

(declaim (inline mklist))
(defun mklist (x) (if (listp x) x (list x)))

(declaim (inline lastcar))
(defun lastcar (l) (car (last l)))

