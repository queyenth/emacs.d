(require 'init-elpa)

(show-paren-mode 1)
(global-hl-line-mode 1)

(define-key global-map (kbd "RET") 'newline-and-indent)

(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(setq auto-save-default nil)

(provide 'init-editing)
