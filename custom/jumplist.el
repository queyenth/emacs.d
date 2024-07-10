(require 'cl-lib)

(defgroup q/jumplist
  nil
  "Jumplist group."
  :group 'convenience)

(defcustom q/jumplist-max-length
  100
  "Max length of jumplist."
  :group 'q/jumplist
  :type 'natnum)

(defcustom q/jumplist-commands
  '(end-of-buffer beginning-of-buffer switch-to-buffer find-file isearch-forward isearch-backward meow-beginning-of-thing meow-end-of-thing consult-goto-line meow-visit consult-line dired-jump)
  "Commands that should register the position in jumplist."
  :group 'q/jumplist
  :type 'list)

(defvar q/jumplist--list '()
  "Variable to store positions. Format is (buffer . position).")

(defvar q/jumplist--state nil
  "To track whether or not we are already jumping.")

(defvar q/jumplist--idx nil
  "To track the current index. nil means we were not jumping back yet.")

;;;###autoload
(define-minor-mode q/jumplist-mode
  "Jumplist mode."
  :global t
  (if q/jumplist-mode
      (q/jumplist--enable-mode)
    (q/jumplist--disable-mode)))

(defun q/jumplist--disable-mode ()
  (remove-hook 'pre-command-hook #'q/jumplist--command-hook)
  (advice-remove 'call-interactively #'q/jumplist--command-hook))

(defun q/jumplist--enable-mode ()
  (add-hook 'pre-command-hook #'q/jumplist--command-hook)
  (advice-add 'call-interactively :before #'q/jumplist--command-hook))

(defun q/jumplist--command-hook (&rest _)
  (when (and (not q/jumplist--state)
             (memq this-command q/jumplist-commands))
    (q/jumplist--save)))

(defun q/jumplist--get-curr-pos ()
  (when-let ((filename (buffer-file-name)))
    (cons filename (point-marker))))

(defun q/jumplist--get-pos (idx)
  (nth idx q/jumplist--list))

(defun q/jumplist--save ()
  (when-let ((position (q/jumplist--get-curr-pos)))
    (unless (equal q/jumplist--idx nil)
      (q/jumplist--drop q/jumplist--idx)
      (setq q/jumplist--idx nil))
    (cl-pushnew position q/jumplist--list)
    (when (> (length q/jumplist--list) q/jumplist-max-length)
      (nbutlast q/jumplist--list))))

(defun q/jumplist--jump (idx)
  (if-let ((position (q/jumplist--get-pos idx)))
      (progn
        (setq q/jumplist--state t)
        (find-file (car position))
        (goto-char (cdr position))
        (setq q/jumplist--state nil))
    (message "Can't jump there, buddy.")))

(defun q/jumplist--drop (idx)
  (setq q/jumplist--list (nthcdr idx q/jumplist--list)))

(defun q/jumplist--inc (arg)
  (setq q/jumplist--idx (min (length q/jumplist--list) (+ q/jumplist--idx arg))))

(defun q/jumplist--dec (arg)
  (setq q/jumplist--idx (max 0 (- q/jumplist--idx arg))))

;;;###autoload
(defun q/jumplist-prev ()
  "Jump back."
  (interactive)
  (unless q/jumplist--idx
    (q/jumplist--save)
    (setq q/jumplist--idx 1))
  (when (equal (q/jumplist--get-curr-pos) (q/jumplist--get-pos q/jumplist--idx))
    (q/jumplist--inc 1))
  (q/jumplist--jump q/jumplist--idx)
  (q/jumplist--inc 1))

;;;###autoload
(defun q/jumplist-next ()
  "Jump forward."
  (interactive)
  (if q/jumplist--idx
      (progn
        (when (equal (q/jumplist--get-curr-pos) (q/jumplist--get-pos q/jumplist--idx))
          (q/jumplist--dec 1))
        (q/jumplist--jump q/jumplist--idx)
        (q/jumplist--dec 1))
    (message "There's like, nowhere to go, man.")))

;;;###autoload
(defun q/jumplist-clear ()
  (interactive)
  (setq q/jumplist--idx nil)
  (setq q/jumplist--list '()))

;;;###autoload
(defun q/jumplist-consult ()
  "With universal-argument, show jumps only for the current buffer."
  (interactive)
  (let ((fn (if current-prefix-arg #'consult--mark-candidates #'consult--global-mark-candidates)))
    (consult--read
     (funcall fn (mapcar #'cdr q/jumplist--list))
     :prompt "Go to jump: "
     :annotate (consult--line-prefix)
     :category 'consult-location
     :sort nil
     :require-match t
     :lookup #'consult--lookup-location
     :history '(:input consult--line-history)
     :add-history (thing-at-point 'symbol)
     :state (consult--jump-state))))

(provide 'jumplist)
