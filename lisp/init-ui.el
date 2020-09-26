(require 'init-elpa)

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(tooltip-mode -1)
(set-fringe-mode 10)

(setq
  x-select-enable-clipboard t
  x-select-enable-primary t
  save-interprogram-paste-before-kill t
  apropos-do-all t
  mouse-yank-at-point t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;(global-display-line-numbers-mode)
(column-number-mode)
(use-package linum-relative
  :init
  (setq linum-relative-backend 'display-line-numbers-mode)
  :config
  (linum-relative-global-mode))

(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(setq ring-bell-function 'ignore)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

;; (use-package gruvbox-theme
;;              :config
;;              (load-theme 'gruvbox t))

(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t))

(use-package page-break-lines)
(use-package all-the-icons)

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
(set-face-attribute 'default nil :font "Fira Mono for Powerline" :height 100)

(provide 'init-ui)
