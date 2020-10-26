;;;; +----------------------------------------------------------------+
;;;; | HORAE                                                          |
;;;; +----------------------------------------------------------------+

(defpackage #:horae/tasks
  (:documentation "Tasks that run between delays.")
  (:use #:cl)
  (:import-from #:constantia #:outs #:out)
  (:import-from #:alexandria #:hash-table-values #:nconcf)
  (:import-from #:bordeaux-threads #:make-lock #:with-lock-held
                #:make-condition-variable #:condition-notify
                #:condition-wait #:make-thread)
  (:import-from #:log4cl)
  (:export
   #:*task-manager*
   #:task-manager
   #:add-task
   #:list-tasks
   #:remove-task
   #:set-task-delay-function
   #:remove-all-tasks))

(in-package #:horae/tasks)

;;; Task delay

(deftype delay ()
  "Represents a task delay, number of seconds."
  'integer)

;;; Task commands

;; Currently there are two commands:
;;
;; :REMOVE
;;
;;   Stop the task thread and remove the task from the task
;;   manager.
;;
;; :SET-DELAY-FUNCTION function
;;
;;   Set a new delay function for the task.  A delay function takes
;;   the owning task object as argument and returns an amount of time,
;;   in seconds, to wait until running the task.
;;

(deftype command ()
  "Represents a task command."
  'cons)

(defun command-p (object)
  "Return true if the supplied object is a command, and false
otherwise."
  (typep object 'command))

(defun command-name (command)
  "Return the name of the command."
  (check-type command command)
  (car command))

(defun command-arguments (command)
  "Return the arguments of the command."
  (check-type command command)
  (cdr command))

(defun specific-command-p (name command)
  "Return true if the command's name is the supplied name, and false
otherwise."
  (and (command-p command)
       (eq (command-name command) name)))

(defun delay-function (command)
  "Return the delay function supplied in the :SET-DELAY-FUNCTION
command."
  (assert (specific-command-p :set-delay-function command))
  (let ((delay-function (first (command-arguments command))))
    (check-type delay-function function)
    delay-function))

;;; Tasks

(defclass task ()
  ((name :initarg :name :reader task-name)
   (run-function :initarg :run-function :reader task-run-function)
   (delay-function :initarg :delay-function :accessor task-delay-function)
   (manager :initarg :manager :accessor task-manager)
   (pending-commands :initform '() :accessor task-pending-commands)
   (thread :initform nil :accessor task-thread)
   (lock :initform (make-lock) :reader task-lock)
   (has-pending-commands :initform (make-condition-variable) :reader task-has-pending-commands))
  (:documentation "Represents a task that should run between delays."))

(defmethod print-object ((object task) stream)
  (print-unreadable-object (object stream :type t)
    (out (:to stream) (slot-value object 'name)))
  object)

(defun task-queue-command (command task)
  "Add the supplied command to the task's pending commands queue."
  (check-type command command)
  (with-lock-held ((task-lock task))
    (nconcf (task-pending-commands task) (list command))
    (condition-notify (task-has-pending-commands task)))
  (values))

(defvar *task* nil
  "In a task's thread, this is the owner task.")

(defun make-task-thread (task)
  "Create a thread for the supplied task."
  (make-thread 'task-thread-function
               :name (outs "Task " (task-name task))
               :initial-bindings (list (cons '*task* task))))

(define-condition stop-task-thread ()
  ()
  (:documentation "Signaled in the task thread in order to stop
it."))

(defmacro task-log (level &body body)
  "Log something in task context."
  `(if (boundp '*task*)
       (,level "[~A] ~A" (task-name *task*) (outs ,@body))
       (error "~S called in non-task context." 'task-log)))

(defun task-thread-function ()
  "The task thread logic for the supplied task."
  (let ((task *task*))
    (handler-case
        (loop
         (handler-case
             (progn
               (task-execute-commands task)
               (when (task-wait-to-run task)
                 (task-run task)))
           (error (e)
             (task-log log:error e))))
      (stop-task-thread ()
        (remove-task/internal task (task-manager task))))))

(defun task-run (task)
  "Run the task."
  (task-log log:debug "Running task.")
  (funcall (task-run-function task)))

(defun task-execute-commands (task)
  "Execute pending commands for the task."
  (with-lock-held ((task-lock task))
    (unwind-protect
         (dolist (command (task-pending-commands task))
           (task-execute-command command task))
      (setf (task-pending-commands task) '()))))

(defun task-execute-command (command task)
  "Execute a task command."
  (task-log log:debug "Executing command " command)
  (cond ((specific-command-p :remove command)
         (signal 'stop-task-thread))
        ((specific-command-p :set-delay-function command)
         (setf (task-delay-function task) (delay-function command)))
        (t
         (task-log log:warn "Don't know how to execute this command; ignoring."))))

(defun task-wait-to-run (task)
  "Wait until it's time to run the task, or we have some pending
commands.  Return true if the task should run, and false otherwise."
  (let ((time-to-wait (funcall (task-delay-function task) task)))
    (task-log log:debug "Want to wait " time-to-wait " seconds.")
    (loop
     (when (<= time-to-wait 0)
       (task-log log:debug "Finished waiting, time to run.")
       (return-from task-wait-to-run t))
     (with-lock-held ((task-lock task))
       (let* ((wait-start-time (get-internal-real-time))
              (triggered (condition-wait (task-has-pending-commands task)
                                         (task-lock task)
                                         :timeout time-to-wait))
              (wait-end-time (get-internal-real-time))
              (wait-duration (/ (- (float wait-end-time) wait-start-time)
                                internal-time-units-per-second)))
         (task-log log:debug "Waited " wait-duration " seconds.")
         (decf time-to-wait wait-duration)
         (when triggered
           ;; If we define more commands, we may want to implement a
           ;; way to wait the remaining delay without starting over,
           ;; but for now this doesn't seem critical.
           (task-log log:debug "Got pending commands; no longer waiting.")
           (return-from task-wait-to-run nil)))))))

;;; Task manager

(defclass task-manager ()
  ((table :initform (make-hash-table :test 'equal) :reader task-manager-table)
   (lock :initform (make-lock) :reader task-manager-lock))
  (:documentation "Represents a task manager that maintains a table of
currently active tasks."))

(defvar *task-manager*
  (make-instance 'task-manager)
  "The default task manager.")

(defun find-task (name &optional (task-manager *task-manager*))
  "Return the appropriate task, or NIL if one does not exist."
  (values (gethash name (task-manager-table task-manager))))

(defun (setf find-task) (new-value task-name &optional (task-manager *task-manager*))
  "If the new value is a task, associate it with the name in the
task manager.  If the new value is NIL, remove the task from the
task manager."
  (cond ((null new-value)
         (remhash task-name (task-manager-table task-manager))
         nil)
        (t
         (setf (gethash task-name (task-manager-table task-manager))
               new-value))))

(defun remove-task/internal (task &optional (task-manager *task-manager*))
  "Internal function to remove a task from the task manager."
  (with-lock-held ((task-manager-lock task-manager))
    (assert (find-task (task-name task) task-manager))
    (assert (eq (task-manager task) task-manager))
    (setf (find-task (task-name task) task-manager) nil)
    (setf (task-manager task) nil))
  (values))

(defun add-task (name run-function delay-function &optional (task-manager *task-manager*))
  "Add a task to the task manager and activate it."
  (with-lock-held ((task-manager-lock task-manager))
    (assert (null (find-task name task-manager)))
    (let ((task (make-instance 'task
                               :name name
                               :run-function run-function
                               :delay-function delay-function
                               :manager task-manager)))
      (setf (task-thread task) (make-task-thread task))
      (setf (find-task name task-manager) task)))
  (values))

(defun list-tasks (&optional (task-manager *task-manager*))
  "Return a list of task names managed by the supplied task manager."
  (with-lock-held ((task-manager-lock task-manager))
    (mapcar #'task-name (hash-table-values (task-manager-table task-manager)))))

(defun remove-task (name &optional (task-manager *task-manager*))
  "Remove the appropriate task from the task manager."
  (with-lock-held ((task-manager-lock task-manager))
    (let ((task (find-task name task-manager)))
      (if task
          (task-queue-command '(:remove) task)
          (log:debug "Tried to remove nonexisting task with name ~S." name))))
  (values))

(defun set-task-delay-function (name new-delay-function &optional (task-manager *task-manager*))
  "Set a new delay function for the appropriate task in the task
manager."
  (with-lock-held ((task-manager-lock task-manager))
    (let ((task (find-task name task-manager)))
      (if (null task)
          (error "No task with name ~S." name)
          (task-queue-command (list :set-delay-function new-delay-function) task))))
  (values))

(defun remove-all-tasks (&optional (task-manager *task-manager*))
  "Remove all tasks from the task manager."
  (dolist (task-name (list-tasks task-manager))
    (remove-task task-name task-manager))
  (values))
