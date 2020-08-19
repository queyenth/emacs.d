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
             :ensure t
             :config
             (load-theme 'base16-solarized-light t))

(use-package telephone-line
             :ensure t
             :config
             (telephone-line-mode 1))

;;(set-default-font "Hack-10")
(set-face-attribute 'default t :font "Hack-10")

(provide 'init-ui)
