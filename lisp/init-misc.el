(require 'init-elpa)

(fset 'yes-or-no-p 'y-or-n-p)
(setq create-lockfiles nil)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(provide 'init-misc)
