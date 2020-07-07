(require 'init-elpa)

(fset 'yes-or-no-p 'y-or-n-p)
(setq create-lockfiles nil)

(use-package fzf
  :ensure t
  :bind ("C-<SPC>" . fzf))

(provide  'init-misc)
