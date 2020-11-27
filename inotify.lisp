;;;; +----------------------------------------------------------------+
;;;; | HORAE                                                          |
;;;; +----------------------------------------------------------------+

(defpackage #:horae/inotify
  (:documentation "Bindings for inotify.")
  (:use #:cl)
  (:import-from #:cffi #:defbitfield #:defctype
                #:translate-from-foreign #:defcfun
                #:foreign-bitfield-symbols)
  (:import-from #:alexandria #:ensure-list)
  (:import-from #:sb-sys #:fd-stream-fd #:make-fd-stream
                #:add-fd-handler #:remove-fd-handler)
  (:import-from #:cffi-sys #:native-namestring)
  (:import-from #:log4cl)
  (:export
   #:with-inotify-context
   #:add-watch
   #:remove-watch
   #:inotify-event
   #:inotify-wd
   #:inotify-mask
   #:inotify-cookie
   #:inotify-name))

(in-package #:horae/inotify)

(defbitfield (mask :ulong)
  (:access #x00000001)
  (:modify #x00000002)
  (:attrib #x00000004)
  (:close-write #x00000008)
  (:close-no-write #x00000010)
  (:close #x000000018)
  (:open #x00000020)
  (:moved-from #x00000040)
  (:moved-to #x00000080)
  (:move #x000000C0)
  (:create #x00000100)
  (:delete #x00000200)
  (:delete-self #x00000400)
  (:move-self #x00000800)
  (:unmount #x00002000)
  (:q-overflow #x00004000)
  (:ignored #x00008000)
  (:only-dir #x01000000)
  (:dont-follow #x02000000)
  (:mask-add #x20000000)
  (:is-dir #x40000000)
  (:one-shot #x80000000)
  (:all-events #x00000FFF))

(defctype int/errno :int)

(defmethod translate-from-foreign :before (value (name (eql 'int/errno)))
  (when (= -1 value)
    (sb-posix:syscall-error)))

(defcfun ("inotify_init" %inotify-init) int/errno)

(defcfun ("inotify_add_watch" %inotify-add-watch) int/errno
  (fd :int)
  (pathname :string)
  (mask mask))

(defcfun ("inotify_rm_watch" %inotify-rm-watch) int/errno
  (fd :int)
  (wd :ulong))

(defclass inotify-event ()
  ((wd :initarg :wd :reader inotify-wd)
   (mask :initarg :mask :reader inotify-mask)
   (cookie :initarg :cookie :reader inotify-cookie)
   (name :initarg :name :reader inotify-name)))

(defun read-event (stream)
  (make-instance 'inotify-event
                 :wd (read-ulong stream)
                 :mask (foreign-bitfield-symbols 'mask (read-ulong stream))
                 :cookie (read-ulong stream)
                 :name (let ((name-length (read-ulong stream)))
                         (when (plusp name-length)
                           (with-output-to-string (out)
                             (loop repeat name-length
                                   do (let ((byte (read-byte stream)))
                                        (unless (zerop byte)
                                          (write-char (code-char byte) out)))))))))

(defclass inotify-context ()
  ((watchers :initform (make-hash-table) :accessor inotify-watchers)
   (default-watcher :initarg :default-watcher :accessor inotify-default-watcher)
   (stream :initarg :stream :accessor inotify-stream)
   (handler :initarg :handler :accessor inotify-handler)))

(defun inotify-fd (context)
  (fd-stream-fd (inotify-stream context)))

(defun make-inotify-context (&key default-watcher)
  (let ((fd (%inotify-init))
        (context (make-instance 'inotify-context :default-watcher (or default-watcher #'identity))))
    (setf (inotify-stream context) (make-fd-stream fd :input t :element-type '(unsigned-byte 8)))
    (setf (inotify-handler context) (add-fd-handler fd :input (make-inotify-handler context)))
    context))

(defun make-inotify-handler (context)
  (lambda (fd)
    (declare (ignore fd))
    (loop for event = (ignore-errors (read-event (inotify-stream context)))
          while event
          do (funcall (gethash (inotify-wd event)
                               (inotify-watchers context)
                               (inotify-default-watcher context))
                      event))))

(defun destroy-inotify-context (context)
  (remove-fd-handler (inotify-handler context))
  (setf (inotify-handler context) nil)
  (close (inotify-stream context))
  (setf (inotify-stream context) nil)
  (values))

(defun add-watch (pathname mask context &key watcher)
  (let ((wd (%inotify-add-watch (inotify-fd context)
                                (native-namestring pathname)
                                (ensure-list mask))))
    (when watcher
      (setf (gethash wd (inotify-watchers context)) watcher))
    wd))

(defun remove-watch (wd context)
  (remhash wd (inotify-watchers context))
  (%inotify-rm-watch (inotify-fd context) wd))

(defmacro with-inotify-context ((var &rest args) &body forms)
  `(let ((,var (make-inotify-context ,@args)))
     (unwind-protect (progn ,@forms)
       (when ,var
         (destroy-inotify-context ,var)))))

(defun read-ulong (stream)
  (let ((x 0))
    (setf (ldb (byte 8 0) x) (read-byte stream))
    (setf (ldb (byte 8 8) x) (read-byte stream))
    (setf (ldb (byte 8 16) x) (read-byte stream))
    (setf (ldb (byte 8 24) x) (read-byte stream))
    x))
