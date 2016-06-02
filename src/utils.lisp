(in-package #:cl-user)

(defpackage #:clr.utils
  (:use #:cl #:clr)
  (:export #:with-gensyms
           #:mklist
           #:lastcar
           #:nlx-protect
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

(defmacro nlx-protect (body &rest protected-forms)
  (let ((var (gensym (string '#:nlx))))
    `(let ((,var t))
       (unwind-protect (prog1 ,body (setf ,var nil))
         (when ,var ,@protected-forms)))))

(defgeneric clr:dispose (object &key &allow-other-keys)
  (:documentation "Disposes an OBJECT")
  (:method-combination progn :most-specific-first)
  (:method progn (object &key &allow-other-keys) (values)))

(defmacro clr:with-object ((var object) &body body)
  "Executes BODY inside dynamic extent where OBJECT is bound to VAR and
 is disposed before extent exits"
  `(let ((,var ,object))
     (unwind-protect (locally ,@body)
       (clr:dispose ,var))))

(defmacro clr:with-objects ((&rest bindings) &body body)
  "Executes BODY inside dynamic extent
 where BINDINGS(see CLR:WITH-OBJECT) are
 disposed before extent exists"
  (if (endp bindings)
    `(locally ,@body)
    `(clr:with-object ,(first bindings)
       (clr:with-objects ,(rest bindings)
         ,@body))))

(defmacro clr:defdispose ((object class &rest keys) &body body)
  "Defines DISPOSE method on an OBJECT of specified CLASS"
  `(defmethod clr:dispose progn ((,object ,class) ,@keys)
     ,@body))

(clr:defdispose (object stream &key (abort nil))
  (close object :abort abort))
