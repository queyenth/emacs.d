(require 'init-elpa)

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package evil-surround
             :ensure t
             :config 
             (global-evil-surround-mode 1))

(use-package org-evil
             :ensure t)

(provide 'init-evil)
