;;;; +----------------------------------------------------------------+
;;;; | HORAE                                                          |
;;;; +----------------------------------------------------------------+

(defpackage #:horae/main
  (:documentation "The application.")
  (:use #:cl #:horae/inotify #:horae/script-symbols #:horae/tasks)
  (:import-from #:asdf #:component-pathname #:find-system)
  (:import-from #:log4cl)
  (:import-from #:cl-fad #:list-directory)
  (:import-from #:alexandria #:make-keyword #:starts-with-subseq)
  (:import-from #:constantia #:outs)
  (:import-from #:sb-sys #:serve-all-events)
  (:import-from #:uiop #:getcwd #:*command-line-arguments* #:*image-dumped-p*)
  (:export #:main))

(in-package #:horae/main)

(defvar *scripts-directory* nil
  "The directory in which the scripts are assumed to reside.")

(defun compute-scripts-directory ()
  "Return the directory in which the scripts should reside."
  (merge-pathnames
   (make-pathname :directory '(:relative "scripts"))
   (if (eq *image-dumped-p* :executable)
       (getcwd)
       (component-pathname (find-system "horae")))))

(defun main ()
  "Entry point for HORAE."
  (when (member "debug" *command-line-arguments* :test #'equal)
    (log:config :debug))
  (setf *scripts-directory* (compute-scripts-directory))
  (ensure-directories-exist *scripts-directory*)
  (log:debug *scripts-directory*)
  (reset-scripts)
  (with-inotify-context (context)
    (add-watch *scripts-directory*
               '(:close-write :delete :move)
               context
               :watcher 'scripts-directory-change)
    (loop (serve-all-events))))

(defun scripts-directory-change (event)
  "Called when there has been a change to the scripts directory."
  (handler-case
      (progn
        (log:debug "Scripts directory change mask=~A name=~A"
                   (inotify-mask event) (inotify-name event))
        (handle-script (merge-pathnames (inotify-name event) *scripts-directory*)))
    (error (e)
      (log:error "~A" e))))

(defun task-name (pathname)
  "Return the task name for the script."
  (values (make-keyword (string-upcase (pathname-name pathname)))))

(defun reset-scripts ()
  "Remove all tasks associated with scripts and create new tasks in
accordance with the scripts found in the script directory."
  (remove-all-tasks)
  (dolist (pathname (list-directory *scripts-directory*))
    (with-simple-restart (continue "Skip script ~S" pathname)
      (handle-script pathname))))

(defun handle-script (pathname)
  "If the script is dormant, do nothing; if the script file exists,
register it; otherwise, unregister it."
  (cond ((dormant-script-p pathname))
        ((probe-file pathname)
         (register-script pathname))
        (t
         (unregister-script pathname))))

(defun dormant-script-p (pathname)
  "Return true if the script should be ignored, and false otherwise."
  (starts-with-subseq "ig-" (pathname-name pathname)))

(defun register-script (pathname)
  "Create or update the task associated with the script."
  (let ((name (task-name pathname))
        (delay-function (make-task-delay-function-from-script pathname)))
    (if (member name (list-tasks))
        (set-task-delay-function name delay-function)
        (add-task name (lambda () (run-script pathname)) delay-function))))

(defun unregister-script (pathname)
  "Remove the task associated with the script."
  (remove-task (task-name pathname)))

(defun make-task-delay-function-from-script (pathname)
  "Create a task delay function from the script."
  (with-open-file (stream pathname :direction :input)
    (let ((spec (let ((*package* (load-time-value
                                  (find-package "HORAE/SCRIPT-SYMBOLS"))))
                  (read stream))))
      (if (and (consp spec)
               (eq (car spec) 'declaim)
               (consp (cadr spec))
               (eq (caadr spec) 'interval))
          (let ((interval (parse-interval (cdadr spec))))
            (constantly interval))
          (error "Unexpected delay specifier ~S for script ~S" spec pathname)))))

(defun parse-interval (spec)
  "Parse a task interval from a specification like (5 minutes 3
seconds)."
  (loop with seconds-to-wait = 0
        for (amount unit) on spec by #'cddr
        do (ecase unit
             ((second seconds)
              (incf seconds-to-wait amount))
             ((minute minutes)
              (incf seconds-to-wait (* amount 60)))
             ((hour hours)
              (incf seconds-to-wait (* amount 60 60)))
             ((day days)
              (incf seconds-to-wait (* amount 24 60 60)))
             ((week weeks)
              (incf seconds-to-wait (* amount 7 24 60 60))))
        finally (return seconds-to-wait)))

(defun run-script (pathname)
  "Run the script by loading it."
  (let ((*package* (intern-task-package (task-name pathname))))
    (load pathname)))

(defun intern-task-package (task-name)
  "Return the package appropriate for loading the task, creating it if
necessary."
  (let ((package-name (outs "HORAE/TASK-PACKAGE-" task-name)))
    (or (find-package package-name)
        (make-package package-name
                      :use (load-time-value
                            (list (find-package "COMMON-LISP")
                                  (find-package "HORAE/SCRIPT-SYMBOLS")))))))
