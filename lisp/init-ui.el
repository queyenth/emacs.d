(require 'init-elpa)

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq
  x-select-enable-clipboard t
  x-select-enable-primary t
  save-interprogram-paste-before-kill t
  apropos-do-all t
  mouse-yank-at-point t)

(global-display-line-numbers-mode)

(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(setq ring-bell-function 'ignore)

(use-package base16-theme
             :config
             (load-theme 'base16-tomorrow t))

(use-package minions
  :config
  (minions-mode 1))

(use-package smart-mode-line
  :init
  (setq sml/theme 'light)
  (setq sml/no-confirm-load-theme t)
  :config
  (sml/setup))

(use-package page-break-lines

(use-package all-the-icons

(use-package dashboard
  :init
  (setq dashboard-banner-logo-title "Oh, hello there!")
  (setq dashboard-startup-banner "~/.emacs.d/banner.png")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
			  (projects . 5)
			  (agenda . 1)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;;(set-default-font "Hack-10")
(set-face-attribute 'default t :font "Hack-10")

(provide 'init-ui)
