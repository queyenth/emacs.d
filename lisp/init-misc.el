(require 'init-elpa)

(fset 'yes-or-no-p 'y-or-n-p)
(setq create-lockfiles nil)

(use-package expand-region
  :config
  (evil-leader/set-key "=" 'er/expand-region))

(use-package projectile
  :diminish
  :custom
  ((projectile-completion-system 'ivy))
  :config
  (projectile-mode +1)
  (evil-leader/set-key "p" 'projectile-command-map))

(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq enable-recursive-minibuffers t)

  (use-package counsel
    :bind (("M-x" . counsel-M-x)
           ("C-x b" . counsel-ibuffer)
           ("C-x C-f" . counsel-find-file)
           :map minibuffer-local-map
           ("C-r" . 'counsel-minibuffer-history))
    :config
    (setq ivy-initial-inputs-alist nil)

    (use-package counsel-projectile
      :config
      (counsel-projectile-mode))

  (use-package ivy-rich
    :config
    (ivy-rich-mode 1))))

(use-package hydra
  :config
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("f" nil "finished" :exit t))

  (evil-leader/set-key "t" 'hydra-text-scale/body))

(use-package which-key
    :diminish
    :config
    (which-key-mode))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; A Git Porcelain
(use-package magit
  :config
  (use-package evil-magit
    :after magit))

(provide 'init-misc)
