;;; emenu.el --- A small library/function to create a popups (like rofi, but in emacs) -*- lexical-binding: t; -*-

;;;###autoload
(defmacro emenu (fn)
  `(defun ,(intern (concat "emenu-" (symbol-name fn))) ()
       (interactive)
       (let* ((buffer (get-buffer-create "*emenu*"))
              (frame (make-frame '((auto-raise . t)
                                   (font . "Iosevka Comfy Motion 10")
                                   (top . 200)
                                   (height . 13)
                                   (width . 110)
                                   (internal-border-width . 20)
                                   (left . 0.33)
                                   (left-fringe . 0)
                                   (line-spacing . 3)
                                   (menu-bar-lines . 0)
                                   (minibuffer . only)
                                   (right-fringe . 0)
                                   (tool-bar-lines . 0)
                                   (undecorated . t)
                                   (unsplittable . t)
                                   (name . "emenu")
                                   (vertical-scroll-bar . nil)))))
         (select-frame-set-input-focus frame)
         (with-current-buffer buffer
           (condition-case nil
               (unwind-protect
                   (call-interactively (symbol-function ',fn))
                 (delete-frame frame)
                 (kill-buffer buffer))
             (quit (delete-frame frame)
                   (kill-buffer buffer)))))))

(defun wayland-window-switch ()
  (interactive)
  (let* ((windows (string-split (shell-command-to-string "wlrctl toplevel list") "\n" t))
         (selected (completing-read "Window: "
                                    windows
                                    (lambda (x) (not (string= "emacs: emenu" x)))
                                    t
                                    nil
                                    t))
         (splitted (string-split selected ":"))
         (app_id (string-trim (car splitted)))
         (title (string-trim (string-join (cdr splitted) ":"))))
    (shell-command (format "wlrctl toplevel activate app_id:\"%s\" title:\"%s\"" app_id title))))

;;;###autoload
(emenu wayland-window-switch)

(defun application-launcher ()
  (interactive)
  (let* ((apps (string-split (shell-command-to-string "dmenu-wl_path") "\n" t))
         (selected (completing-read "Run: " apps nil t)))
    (call-process-shell-command selected nil 0 nil)))

;;;###autoload
(emenu application-launcher)

(require 'proced)
(defun q/kill-process ()
  (interactive)
  (let* ((pid-width 5)
         (comm-width 30)
         (user-width 5)
         (args-width (- (window-width) (+ pid-width comm-width user-width 3)))
         (processes (proced-process-attributes))
         (candidates
          (mapcar (lambda (attributes)
                    (let* ((process (cdr attributes))
                           (args (alist-get 'args process))
                           (pid (format (format "%%%ds" pid-width) (alist-get 'pid process)))
                           (comm (format (format "%%-%ds" comm-width) (truncate-string-to-width (alist-get 'comm process) comm-width nil nil t)))
                           (user (format (format "%%-%ds" user-width) (truncate-string-to-width (alist-get 'user process) user-width nil nil t)))
                           (prompt-title (format "%s %s %s" pid user comm)))
                      (cons (if args (format "%s %s" prompt-title (truncate-string-to-width args args-width nil nil t))
                              prompt-title)
                            process)))
                  processes))
         (selected (alist-get (completing-read "Kill: " candidates nil t) candidates nil nil #'equal))
         (prompt-title (format "%s %s %s"
                               (alist-get 'pid selected)
                               (alist-get 'user selected)
                               (alist-get 'comm selected))))
    (when (y-or-n-p (format "Kill %s?" prompt-title))
      (shell-command (format "kill -9 %d" (alist-get 'pid selected))))))

;;;###autoload
(emenu q/kill-process)

(provide 'emenu)
