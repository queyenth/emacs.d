(require 'init-elpa)

(use-package evil
  :ensure t
  :defer .1
  :init
  (setq evil-want-integration nil)
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode)

  (use-package evil-collection
               :after evil
               :ensure t
               :config
               (evil-collection-init))

  (use-package evil-goggles
               :ensure t
               :config
               (evil-goggles-use-diff-faces)
               (evil-goggles-mode))

  (use-package evil-surround
               :ensure t
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

  (use-package org-evil
               :ensure t)

  (message "Loading evil-mode...done"))

(provide 'init-evil)
