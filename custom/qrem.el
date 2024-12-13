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

(autoload #'notifications-notify "notifications" nil t)
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
  (message "Removed all reminders...")
  (when (timerp q/rem--timer)
    (cancel-timer q/rem--timer)
    (setq q/rem--timer nil)))

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

(defun q/delete-string-part (string from to)
  (if (null to) (substring string 0 from)
    (concat (substring string 0 from)
            (substring string to nil))))

(defun q/to-seconds (query)
  "Get value as seconds. QUERY could be 30m, 2h45m, 2d, 10s."
  (let ((seconds 0))
    (while (not (string-blank-p query))
      (string-match "\\([0-9]+\\)\\([smhd]\\)" query)
      (if (= (length (match-data)) 6)
          (let ((number (string-to-number (substring query (nth 2 (match-data)) (nth 3 (match-data)))))
                (timetype (string-to-char (substring query (nth 4 (match-data)) (nth 5 (match-data))))))
            (setq seconds (+ seconds (* number (cl-case timetype
                                                      (?s 1)
                                                      (?m 60)
                                                      (?h (* 60 60))
                                                      (?d (* 60 60 24))))))
            (setq query (q/delete-string-part query (nth 0 (match-data)) (nth 1 (match-data)))))
        (setq query "")))
    seconds))

;;;###autoload
(defun q/rem-reminder (date task)
  (interactive (list (read-string "Remind in: ") (read-string "Task: ")))
  (unless q/rem--initialized
    (q/rem--setup-db))
  (let ((date (format-time-string "%s" (time-add nil (q/to-seconds date)))))
    (sqlite-execute q/rem--db
                    "INSERT INTO reminders (remind_date, task_description) VALUES (?, ?)"
                    (list date task))
    (q/notify "Reminder!" (format "%s: %s" (format-time-string "%R" (string-to-number date)) task)))
  (unless (timerp q/rem--timer)
    (q/rem-run-cron)))

(require 'emenu)

;;;###autoload
(defun emenu-reminder ()
  (interactive)
  (emenu (call-interactively #'q/rem-reminder)))

(provide 'qrem)
