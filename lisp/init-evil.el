(require 'init-elpa)

(setq evil-want-integration t)
(setq evil-want-keybinding nil)

;; TODO: I guess we're better set mode keybindings in use-package for corresponding mode?
(use-package evil-leader
  :init
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "SPC"))

(use-package evil
  :after evil-leader
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)

  ; That doesn't work for some reason Idk
  (setq evil-want-C-u-scroll t)
  ; So we use this :(
  (evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)

  :config
  (evil-mode)

  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

  (use-package evil-goggles
    :config
    (evil-goggles-use-diff-faces)
    (evil-goggles-mode))

  (use-package evil-surround
    :commands
    (evil-surround-edit
     evil-Surround-edit
     evil-surround-region
     evil-Surround-region)
    :init
    (evil-define-key 'operator global-map "s" 'evil-surround-edit)
    (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
    (evil-define-key 'visual global-map "S" 'evil-surround-region)
    (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

  (use-package evil-commentary
    :config
    (evil-commentary-mode)
    (evil-leader/set-key "c" 'evil-commentary-line))

  (use-package org-evil))

(provide 'init-evil)
