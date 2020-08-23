(require 'init-elpa)

(show-paren-mode 1)
(global-hl-line-mode 1)

(setq-default tab-width 4)
(progn
  (setq-default indent-tabs-mode nil))

(define-key global-map (kbd "RET") 'newline-and-indent)

(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(setq auto-save-default nil)

;; Packages
(use-package company
  :ensure t
  :custom
  (company-tooltip-limit 5)
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 3)
  (company-selection-wrap-around t)
  (company-require-match 'never)
  :hook (after-init . global-company-mode))

(provide 'init-editing)
