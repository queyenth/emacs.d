(require 'init-elpa)

(fset 'yes-or-no-p 'y-or-n-p)
(setq create-lockfiles nil)

(use-package projectile
  :config
  (projectile-mode +1)
  (evil-leader/set-key "p" 'projectile-command-map))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(provide 'init-misc)
