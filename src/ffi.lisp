(in-package #:cl-user)

(defpackage #:clr.ffi
  (:use #:cl #:clr #:clr.impl #:clr.utils)
  (:export
   #:sizeof
   #:alignof
   #:offsetof
   #:deref
   #:defclib
   #:cslot
   #:ffcall
   #:ptrcall
   #:defcstruct
   #:defcunion
   #:with-utf-16
   #:deref-utf-16
   #:utf-16-byte-count
   #:utf-16-char-count
   #:utf-16-encode
   #:utf-16-decode
   #:with-cptr
   ))

(in-package #:clr.ffi)

(defstruct (cstruct-info
            (:conc-name sti-))
  (name nil :type symbol)
  (align 1 :type index)
  (size 0 :type index)
  (slots '() :type list))

(defstruct (cslot-info
            (:conc-name sli-))
  (name nil :type symbol)
  (type nil :type symbol)
  (align 1 :type index)
  (offset 0 :type index)
  (count 1 :type index)
  (size 0 :type index))

(defvar *cstruct-defs* (make-hash-table :test #'eq))

(defun ctype-size (type)
  (case type
    ((:int8 :uint8 :char :byte) 1)
    ((:int16 :uint16 :short :ushort) 2)
    ((:int32 :uint32 :int :uint :float :bool :boolean
             #+clr:windows :long #+clr:windows :ulong)
     4)
    ((:int64 :uint64 :long-long :ulong-long :double
             #-clr:windows :long #-clr:windows :ulong)
     8)
    ((:ptr :* :pointer :intptr :uintptr :utf-16 :utf-8) #+clr:x32 4 #+clr:x64 8)
    (t (if (consp type)
         (case (first type)
           ((:struct :union)
            (nth-value 0 (process-cslot-defs
                          (rest type)
                          (eq (first type) :union)
                          nil)))
           (:array (destructuring-bind (elt-type dims) (rest type)
                     (assert (or (consp dims) (typep dims 'index))
                             (dims)
                             "Invalid :array dimensions: ~s" dims)
                     (let* ((elt-size (ctype-size elt-type))
                            (dims (if (consp dims) dims (list dims))))
                       (dolist (dim dims)
                         (unless (typep dim 'index)
                           (error "Array dimension is invalid: ~s" dim)))
                       (* elt-size (reduce #'* dims)))))
           (T (error "Unidentified C type: ~s" type)))
         (let ((def (gethash type *cstruct-defs*)))
           (unless def (error "Unidentified C value type: ~s" type))
           (sti-size def))))))

(defun ctype-align (type)
  (case type
    ((:int8 :uint8 :char :byte) 1)
    ((:int16 :uint16 :short :ushort) 2)
    ((:int32 :uint32 :int :uint :float :boolean :bool
             #+(and (not clr:windows) clr:x32) :double
             #+clr:windows :long #+clr:windows :ulong)
     4)
    ((:int64 :uint64 :long-long :ulong-long #+(or clr:windows clr:x64) :double
             #-clr:windows :long #-clr:windows :ulong)
     8)
    ((:ptr :* :pointer :intptr :uintptr :utf-16 :utf-8) #+clr:x32 4 #+clr:x64 8)
    (t (if (consp type)
         (case (first type)
           ((:struct :union)
            (nth-value 1 (process-cslot-defs
                          (rest type)
                          (eq (first type) :union)
                          nil)))
           (:array (ctype-align (second type)))
           (T (error "Unidentified C value type: ~s" type)))
         (let ((def (gethash type *cstruct-defs*)))
           (unless def (error "Unidentified C value type: ~s" type))
           (sti-align def))))))

(defun cslot-desc (type slot)
  (check-type slot symbol)
  (let ((def (if (consp type)
               (ecase (car type)
                 ((:struct :union)
                  (nth-value 2 (process-cslot-defs (rest type)
                                                   (eq :union (first type))
                                                   t))))
               (let ((def (gethash type *cstruct-defs*)))
                 (and def (sti-slots def))))))
    (unless def
      (error "Not an identified struct type: ~s" type))
    (let ((slotd (find slot def :key #'sli-name)))
      (unless slotd (error "Struct ~d has not slot named ~s" type slot))
      slotd)))

(defun cslot-offset (type slot)
  (sli-offset (cslot-desc type slot)))

(defun align-base-2 (offset align)
  (declare (type index offset align))
  (the index (logand (+ offset align -1)
                     (lognot (1- align)))))

(defun process-cslot-defs (defs &optional unionp (collect t))
  (let ((slots '())
        (max-offset 0)
        (max-align 1))
    (dolist (def defs)
      (destructuring-bind (name type &key (offset 0 offsetp)
                                          (align 1 alignp)
                                          (count 1)
                                     &aux (size (* count (ctype-size type))))
          def
        (assert (symbolp name)
                (name)
                "Slot name(~s) is not valid" name)
        (assert (typep count '(integer 1 #.(1- array-total-size-limit)))
                (count)
                "Slot ~a array count(~s) is not valid." name count)
        (if alignp
          (assert (typep align 'index)
                  (align)
                  "Slot ~a align(~s) is not valid" name align)
          (setf align (ctype-align type)))
        (if offsetp
          (assert (typep offset 'index)
                  (offset)
                  "Slot ~a offset(~s) is not valid." name offset)
          (unless unionp (setf offset (align-base-2 max-offset align))))
        (setf max-align (max max-align align)
              max-offset (max (+ offset size) max-offset))
        (when collect
          (push (make-cslot-info :name name :type type
                                 :align align :offset offset
                                 :count count :size size)
                slots))))
    (setf max-offset (align-base-2 max-offset max-align))
    (values max-offset max-align (nreverse slots))))

(defun process-cstruct-def (name slot-defs &optional unionp)
  (assert (symbolp name)
          (name)
          "~a name(~s) is not a symbol" (if unionp "Union" "Struct") name)
  (multiple-value-bind (size align slots)
      (process-cslot-defs slot-defs unionp)
    (let ((def (make-cstruct-info :name name
                                  :align align
                                  :size size
                                  :slots slots)))
      (setf (gethash name *cstruct-defs*) def)
      (values))))

(defmacro defcstruct (name &body slots)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (process-cstruct-def ',name ',slots nil)
     ',name))

(defmacro defcunion (name &body slots)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (process-cstruct-def ',name ',slots t)
     ',name))

(defmacro sizeof (ctype)
  (ctype-size ctype))

(defmacro alignof (ctype)
  (ctype-align ctype))

(defmacro offsetof (cstruct slot)
  (cslot-offset cstruct slot))

(defun get-type-deref (type ptr offset)
  (ecase type
    ((:uint8 :byte) `(deref-uint8 ,ptr ,offset))
    ((:int8 :char) `(deref-int8 ,ptr ,offset))
    ((:uint16 :ushort) `(deref-uint16 ,ptr ,offset))
    ((:int16 :short) `(deref-int16 ,ptr ,offset))
    ((:int32 :int #+clr:windows :long) `(deref-int32 ,ptr ,offset))
    ((:uint32 :uint #+clr:windows :ulong) `(deref-uint32 ,ptr ,offset))
    ((:int64 :long-long #-clr:windows :long) `(deref-int64 ,ptr ,offset))
    ((:uint64 :ulong-long #-clr:windows :ulong) `(deref-uint64 ,ptr ,offset))
    ((:intptr) `(deref-intptr ,ptr ,offset))
    ((:uintptr) `(deref-uintptr ,ptr ,offset))
    ((:float) `(deref-float ,ptr ,offset))
    ((:double) `(deref-double ,ptr ,offset))
    ((:utf-16) `(deref-utf-16 ,ptr ,offset))
    ((:boolean) `(/= 0 (deref-int ,ptr ,offset)))
    ((:* :pointer :ptr) `(deref-ptr ,ptr ,offset))))

(defmacro deref (ptr type &optional (offset 0))
  (get-type-deref type ptr offset))

(defsetf deref (ptr type &optional (offset 0)) (new-value)
  (if (member type '(:bool :boolean))
    `(setf ,(get-type-deref :int ptr offset) (or (and ,new-value 1) 0))
    `(setf ,(get-type-deref type ptr offset) ,new-value)))

(defmacro cslot (ptr type slot)
  (let ((slotd (cslot-desc type slot)))
    `(deref ,ptr ,(sli-type slotd) ,(sli-offset slotd))))

(defsetf cslot (ptr type slot) (new-value)
  (let ((slotd (cslot-desc type slot)))
    `(setf (deref ,ptr ,(sli-type slotd) ,(sli-offset slotd)) ,new-value)))

(defun utf-16-char-count (ptr &key size (offset 0) (nullt t))
  (declare (type pointer ptr)
           (type index offset)
           (type (or null index) size))
  (let ((size (or size (1- array-total-size-limit))))
    (declare (type index size))
    (do ((p (ptr+ ptr offset) (ptr+ p 2))
         (i 0 (1+ i))
         (n 0 (+ n 2)))
        ((or (>= (1- n) size)
             (and nullt
                  (<= n (- size 2))
                  (zerop (deref-uint16 p))))
         (values i n))
      (let ((c (deref-uint16 p)))
        (when (and (>= c #xD800)
                   (<= c #xDFFF))
          (incf n 2)
          (setf p (ptr+ p 2)))))))

(defun utf-16-byte-count (str &key (start 0) end (nullt t))
  (declare (type string str)
           (type index start)
           (type (or null index) end))
  (let ((end (or end (length str))))
    (do ((i 0 (+ i 2)))
        ((>= start end) (if nullt (values (+ i 2) (- end start))
                                  (values i (- end start))))
      (when (>= (char-code (elt str start)) #x10000)
        (incf i))
      (incf start))))

(defun utf-16-encode (str ptr &key (start 0) end (offset 0) size (nullt t))
  (declare (type string str)
           (type pointer ptr)
           (type index start offset)
           (type (or null index) end size))
  (let ((end (or end (length str)))
        (size (or size (1- array-total-size-limit))))
    (declare (type index end size))
    (do ((p (ptr+ ptr offset) (ptr+ p 2))
         (n 0 (+ n 2))
         (i start (1+ i)))
        ((or (>= i end) (<= (- size 2) n))
         (when (and nullt (<= n (- size 2))) (setf (deref-uint16 p) 0))
         (values n (- i start)))
      (declare (type index n))
      (let ((c (char-code (elt str i))))
        (cond ((> c #x10000)
               (let ((high (+ #xD800 (logand (ash c -10) #x3FF)))
                     (low (+ #xDC00 (logand c #x3FF))))
                 (setf (deref-uint16 p) high
                       p (ptr+ p 2)
                       n (+ n 2)
                       (deref-uint16 p) low)))
              (t (setf (deref-uint16 p) c)))))))

(defun utf-16-decode (ptr str &key (start 0) end (offset 0) size (nullt t))
  (declare (type pointer ptr)
           (type string str)
           (type index start offset)
           (type (or null index) end size))
  (let ((end (or end (length str)))
        (size (or size (1- array-total-size-limit))))
    (declare (type index end size))
    (do ((p (ptr+ ptr offset) (ptr+ p 2))
         (n 0 (+ n 2))
         (i start (1+ i)))
        ((or (>= i end)
             (and nullt (<= n (- size 2)) (zerop (deref-uint16 p))))
         (values (- i start) n))
      (let ((w1 (deref-uint16 p)))
        (if (and (>= w1 #xD800)
                 (< w1 #xDC00))
          (progn (setf p (ptr+ p 2)
                       n (+ n 2))
                 (if (<= n (- size 2))
                   (let ((w2 (deref-uint16 p)))
                     (setf (elt str i)
                           (code-char (logior (ash (- w1 #xD800) 10)
                                              (- w2 #xDC00)))))
                   (setf i (1- i)
                         n (- n 2))))
          (setf (elt str i) (code-char w1)))))))

(defun utf-16-alloc (string &optional (start 0) end)
  (declare (type string string)
           (type index start)
           (type (or null index) end))
  (let* ((size (utf-16-byte-count string :start start :end end :nullt t))
         (ptr (mem-alloc size)))
    (multiple-value-bind (nbytes nchars)
        (utf-16-encode string ptr :start start :end end :nullt t
                                  :offset 0 :size size)
      (values ptr nbytes nchars))))

(defmacro with-utf-16 ((var string &optional (start 0) end) &body body)
  `(let ((,var (utf-16-alloc ,string ,start ,end)))
     (unwind-protect (progn ,@body) (mem-free ,var))))

(defun deref-utf-16 (ptr &optional (offset 0) size)
  (declare (type pointer ptr)
           (type index offset)
           (type (or null index) size))
  (let* ((len (utf-16-char-count ptr :offset offset :size size :nullt t))
         (str (make-array len :element-type 'character)))
    (multiple-value-bind (nchars nbytes)
        (utf-16-decode ptr str :offset offset :size size :nullt t)
      (values str nchars nbytes))))

(defun check-not-aggegate (form type &optional allow-strings)
  (case type
    (:utf-16
     (unless allow-strings
       (error "Unable to return string by value: ~s" form)))
    (t (if (or (consp type) (gethash type *cstruct-defs*))
         (error "Unable to pass aggregate ~s by value in ~s" type form)))))

(defun get-args-rettype (form args)
  (let* ((evenp (evenp (length args)))
         (rettype (if evenp :void (lastcar args)))
         (args (if evenp args (butlast args))))
    (check-not-aggegate form rettype)
    (values args rettype)))

(defun translate-retval (form type)
  (if (member type '(:bool :boolean))
    `(/= 0 ,form)
    form))

(defun expand-ffcall (form name-and-options args)
  (destructuring-bind (name &key (convention :cdecl) library)
      (mklist name-and-options)
    (assert (stringp name)
            (name)
            "Foreign function name is not a symbol: ~s" name)
    (assert (member convention '(:stdcall :cdecl))
            (convention)
            "Invalid calling convention: ~s" convention)
    (multiple-value-bind (args rettype) (get-args-rettype form args)
      (labels ((expand (args itypes ivalues)
                 (if (endp args)
                   (translate-retval
                    `(impl-ffcall ,name ,convention ,library
                         ,(impl-ctype rettype)
                         ,(nreverse itypes)
                         ,(nreverse ivalues))
                    rettype)
                   (let ((type (first args))
                         (value (second args))
                         (args (cddr args)))
                     (check-not-aggegate form type t)
                     (case type
                       (:utf-16
                        (let ((var (gensym (string :utf-16))))
                          `(with-utf-16 (,var ,value)
                             ,(expand args
                                      (cons (impl-ctype :pointer) itypes)
                                      (cons var ivalues)))))
                       (t (expand args
                                  (cons (impl-ctype type) itypes)
                                  (cons value ivalues))))))))
        (expand args '() '())))))

(defun expand-ptrcall (form ptr options args)
  (destructuring-bind (&key (convention :cdecl))
      options
    (assert (member convention '(:stdcall :cdecl))
            (convention)
            "Invalid calling convention: ~s" convention)
    (multiple-value-bind (args rettype) (get-args-rettype form args)
      (labels ((expand (args itypes ivalues)
                 (if (endp args)
                   (translate-retval
                    `(impl-ptrcall ,ptr ,convention
                         ,(impl-ctype rettype)
                         ,(nreverse itypes)
                         ,(nreverse ivalues))
                    rettype)
                   (let ((type (first args))
                         (value (second args))
                         (args (cddr args)))
                     (check-not-aggegate form type t)
                     (case type
                       (:utf-16
                        (let ((var (gensym (string :utf-16))))
                          `(with-utf-16 (,var ,value)
                             ,(expand args
                                      (cons (impl-ctype :pointer) itypes)
                                      (cons var ivalues)))))
                       (t (expand args
                                  (cons (impl-ctype type) itypes)
                                  (cons value ivalues))))))))
        (expand args '() '())))))

(defmacro ffcall (&whole form name-and-options &rest args)
  (expand-ffcall form name-and-options args))

(defmacro ptrcal (&whole form pointer (&rest options) &rest args)
  (expand-ptrcall form pointer options args))

(defmacro defclib (name path)
  `(impl-defclib ,name ,path))

(defmacro with-cptr (&whole form (var type &optional (value nil value-p)) &body body)
  (case type
    (:utf-16
     (unless value-p (error "String pointers must have initial value supplied"))
     `(with-utf-16 (,var ,value) ,@body))
    (t
     (when value-p (check-not-aggegate form type))
     `(with-stack-ptr (,var ,(ctype-size type))
        ,(when value-p `(setf (deref ,var ,type) ,value))
        (let ((,var ,var))
          ,@body)))))
