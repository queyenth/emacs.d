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

(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(setq ring-bell-function 'ignore)

; Golden Ratio
(use-package golden-ratio
             :ensure t
             :config
             (golden-ratio-mode 1))

; Helm, important Kappa
(use-package helm
  :ensure t
  :config
  (helm-mode 1))

(use-package nord-theme
             :ensure t
             :init
             (load-theme 'nord t))

(provide 'init-ui)
