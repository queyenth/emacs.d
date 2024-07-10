;;;###autoload
(defmacro emenu (&rest body)
  (declare (indent 1))
  `(let* ((buffer (get-buffer-create "*emenu*"))
          (frame (make-frame '((auto-raise . t)
                               (font . "IBM Plex Mono 10")
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
                               (vertical-scroll-bar . nil)))))
     (select-frame-set-input-focus frame)
     (with-current-buffer buffer
       (condition-case nil
           (unwind-protect
               ,@body
             (delete-frame frame)
             (kill-buffer buffer))
         (quit (delete-frame frame)
               (kill-buffer buffer))))))

(defun wayland-window-switch ()
  (interactive)
  (let* ((windows (string-split (shell-command-to-string "wlrctl toplevel list") "\n" t))
         (selected (completing-read "Window: "
                                    windows
                                    (lambda (x) (not (string-match "\\*Minibuf-[0-9]+\\*" x)))
                                    t))
         (splitted (string-split selected ":"))
         (app_id (string-trim (car splitted)))
         (title (string-trim (string-join (cdr splitted) ":"))))
    (shell-command (format "wlrctl toplevel activate app_id:\"%s\" title:\"%s\"" app_id title))))

;;;###autoload
(defun emenu-window-switch ()
  (interactive)
  (emenu (wayland-window-switch)))

(defun application-launcher ()
  (interactive)
  (let* ((apps (string-split (shell-command-to-string "dmenu-wl_path") "\n" t))
         (selected (completing-read "Run: " apps nil t)))
    (call-process-shell-command selected nil 0 nil)))

;;;###autoload
(defun emenu-application-launcher ()
  (interactive)
  (emenu (application-launcher)))

(provide 'emenu)
