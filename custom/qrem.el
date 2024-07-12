;; -*- lexical-binding: t; -*-
(defvar q/rem--db (sqlite-open "/tmp/reminders.db"))
(defvar q/rem--timer nil)
(defvar q/rem--initialized nil)

(defun q/rem--setup-db ()
  (sqlite-execute q/rem--db "CREATE TABLE reminders (task_id INTEGER PRIMARY KEY, task_description TEXT, remind_date INTEGER)")
  (setq q/rem--initialized t))

(defun q/rem--format-task-for-show (task)
  (concat (format-time-string "%d %b, %a, %R" (nth 1 task))
          ": " (nth 0 task)))

(defun q/rem--format-for-show (tasks)
  (mapconcat #'q/rem--format-task-for-show
             tasks
             "\n"))

;;;###autoload
(defun q/notify (title body &optional timeout urgency sound)
  (notifications-notify
   :title title
   :body body
   :timeout (or timeout 3000)
   :urgency (or urgency 'low)
   :sound-name sound))

;;;###autoload
(defun q/rem-show ()
  (interactive)
  (unless q/rem--initialized
    (q/rem--setup-db))
  (let* ((query "SELECT task_description, remind_date FROM reminders WHERE remind_date > ? ORDER BY remind_date ASC")
         (current-date (format-time-string "%s"))
         (tasks (sqlite-select q/rem--db query (list current-date))))
    (q/rem--format-for-show tasks)))

;;;###autoload
(defun q/rem-all ()
  (interactive)
  (unless q/rem--initialized
    (q/rem--setup-db))
  (let* ((query "SELECT task_description, remind_date FROM reminders ORDER BY remind_date ASC")
         (tasks (sqlite-select q/rem--db query)))
    (q/rem--format-for-show tasks)))

;;;###autoload
(defun q/rem-count ()
  (interactive)
  (unless q/rem--initialized
    (q/rem--setup-db))
  (let* ((query "SELECT count(*) FROM reminders WHERE remind_date > ?")
         (current-date (format-time-string "%s"))
         (tasks (sqlite-select q/rem--db query (list current-date))))
    (caar tasks)))

;;;###autoload
(defun q/rem-clear ()
  (interactive)
  (unless q/rem--initialized
    (q/rem--setup-db))
  (sqlite-execute q/rem--db "DELETE FROM reminders")
  (message "Removed all reminders..."))

(defun q/rem--cron ()
  (let* ((query "SELECT task_id, task_description, remind_date FROM reminders WHERE remind_date <= ? ORDER BY remind_date ASC")
         (current-date (format-time-string "%s"))
         (tasks (sqlite-select q/rem--db query (list current-date))))
    (dolist (task tasks)
      (q/notify "Reminder!" (nth 1 task) 0 'normal "message")
      (sqlite-execute q/rem--db "DELETE FROM reminders WHERE task_id = ?" (list (nth 0 task))))
    (let ((count (q/rem-count)))
      (when (= 0 count)
        (q/rem-stop-cron)))))

;;;###autoload
(defun q/rem-run-cron ()
  (interactive)
  (unless q/rem--initialized
    (q/rem--setup-db))
  (unless (timerp q/rem--timer)
    (setq q/rem--timer (run-with-timer t 30 #'q/rem--cron))
    (message "Running reminder timer.")))

;;;###autoload
(defun q/rem-stop-cron ()
  (interactive)
  (unless q/rem--initialized
    (q/rem--setup-db))
  (when (timerp q/rem--timer)
    (cancel-timer q/rem--timer)
    (setq q/rem--timer nil)))

;;;###autoload
(defun q/rem-reminder (date task)
  (interactive (list (org-read-date t) (read-string "Task: ")))
  (unless q/rem--initialized
    (q/rem--setup-db))
  (let ((date (format-time-string "%s" (encode-time (parse-time-string date)))))
    (sqlite-execute q/rem--db
                    "INSERT INTO reminders (remind_date, task_description) VALUES (?, ?)"
                    (list date task))
    (q/notify "Reminder!" (format "%s: %s" (format-time-string "%R" date) task)))
  (unless (timerp q/rem--timer)
    (q/rem-run-cron)))

;;;###autoload
(defun emenu-reminder ()
  (let ((org-read-date-popup-calendar nil))
    (emenu (call-interactively #'q/rem-reminder))))

(q/rem-show)

(provide 'qrem)
