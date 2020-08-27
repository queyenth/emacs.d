(require 'init-elpa)

(show-paren-mode 1)
(global-hl-line-mode 1)

(setq custom-tab-width 4)
(defun disable-tabs ()
  "Disable tabs."
  (setq indent-tabs-mode nil))
(defun enable-tabs ()
  "Enable tabs."
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

(add-hook 'prog-mode-hook 'enable-tabs)
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

(setq-default electric-indent-inhibit t)

(setq backward-delete-char-untabify-method 'hungry)

(setq-default evil-shift-width custom-tab-width)

;; (setq whitespace-style '(face tabs tab-mark trailing))
;; (custom-set-faces
;;  '(whitespace-tab ((t (:foreground "#636363")))))
;; (setq whitespace-display-mappings
;;       '((tab-mark 9 [124 9] [92 9])))
;; (global-whitespace-mode)

(define-key global-map (kbd "RET") 'newline-and-indent)

(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(setq auto-save-default nil)

(with-eval-after-load "ispell"
  (setenv "LANG" "en_US")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US,ru_RU")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,ru_RU")
  (setq ispell-personal-dictionary "~/.emacs.d/.hunspell_personal"))

(unless (file-exists-p "~/.emacs.d/.hunspell_personal")
  (shell-command "touch ~/.emacs.d/.hunspell_personal"))

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
