(server-start)

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq create-lockfiles nil)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(auto-save-visited-mode)

(column-number-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(setq read-process-output-max (* 1024 1024 3))
(setq gc-cons-threshold 100000000)

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(tooltip-mode -1)
(set-fringe-mode 0)

(setq
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t)
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(setq ring-bell-function 'ignore)

(set-face-attribute 'default nil :font "Go Mono" :height 100)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ansi-color
  :config
  (defun qqq/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . qqq/colorize-compilation-buffer))

(use-package linum-relative
  :init
  (setq linum-relative-backend 'display-line-numbers-mode)
  :hook (prog-mode . linum-relative-mode))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

; Darktooth theme
(use-package darktooth-theme
  :config
  (load-theme 'darktooth t))

(use-package writeroom-mode
  :commands writeroom-mode)

(use-package page-break-lines)
(use-package all-the-icons)

(use-package tree-sitter
  :init
  (use-package tree-sitter-langs)
  :config
  (global-tree-sitter-mode))

(setq custom-tab-width 4)
(setq-default evil-shift-width custom-tab-width)
(setq-default electric-indent-inhibit t)

(setq backward-delete-char-untabify-method 'hungry)

(defun disable-tabs ()
  "Disable tabs."
  (setq indent-tabs-mode nil))
(defun enable-tabs ()
  "Enable tabs."
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

(add-hook 'prog-mode-hook 'enable-tabs)
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(use-package evil
  :custom ((evil-undo-system 'undo-tree))
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)

  ; That doesn't work for some reason Idk
  (setq evil-want-C-u-scroll t)

  ; So we use this :(
  :config
  (evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)
  ; Backspace is so far away I just can't...
  (evil-define-key 'insert 'global (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-define-key 'normal 'global (kbd "RET") 'evil-ret-and-indent)

  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

(use-package evil-surround
  :after evil
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
  :after evil
  :config
  (evil-commentary-mode))

(use-package general
  :after evil
  :config
  (general-evil-setup t)

  (general-create-definer qqq/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer qqq/local-leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC m"
    :global-prefix "C-SPC m"))

(with-eval-after-load 'general
  (qqq/leader-keys "'" '(evil-commentary-line :which-key "comment line(s)")))

(require 'dired-x)
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-dwim-target t))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :after dired)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(with-eval-after-load 'general
  (qqq/leader-keys
    "d" '(:ignore t :which-key "Dired")
    "dd" '(dired :which-key "dwim")
    "do" '(dired-other-window :which-key "other window")))

(setq qqq/org-directory (concat (getenv "SYNCTHING") "org"))
(defun qqq/get-org-file (file)
  (concat qqq/org-directory file))
(use-package org
  :defer t
  :commands (org-capture org-agenda)
  :config
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'note)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-indent-indentation-per-level 1)
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-cycle-separator-lines 1)
  (setq org-startup-with-inline-images t)
  (setq org-directory qqq/org-directory)
  (setq org-agenda-files (list org-directory))
  (setq org-refile-targets '(org-agenda-files :level . 1))
  (setq org-todo-keywords
    '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CANCELED")))

  (setq org-return-follow-links t)

  (org-babel-do-load-languages
   'org-babel-load-languages
       '((emacs-lisp . t)
	 (python . t)))

  (setq org-confirm-babel-evaluate nil)
  (require 'ox-md)

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package org-agenda
    :ensure nil
    :after org
    :config
    (setq qqq/org-agenda-todo-view
      `(("A" "Agenda"
	((agenda ""
		 ((org-agenda-span 'day)
		  (org-deadline-warning 365)))
	 (todo "TODO"
		 ((org-agenda-overriding-header "To Refile")
		  (org-agenda-files `(,(qqq/get-org-file "/inbox.org")))))
	 (todo "NEXT"
		 ((org-agenda-overriding-header "In Progress")
		  (org-agenda-files `(,(qqq/get-org-file "/next.org")))))
	 nil))))
    (setq org-agenda-custom-commands `,qqq/org-agenda-todo-view))

(with-eval-after-load 'org
  (setq org-capture-templates
      `(("i" "inbox" entry (file ,(qqq/get-org-file "/inbox.org"))
  	  (file ,(qqq/get-org-file "/tmpl/task")))
   	("l" "link" entry (file ,(qqq/get-org-file "/inbox.org"))
  	  (file ,(qqq/get-org-file "/tmpl/link")) :immediate-finish t))))

(use-package org-drill
  :after org
  :config
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-learn-fraction 0.25)
  (setq org-drill-hint-separator "||")
  (setq org-drill-left-cloze-delimiter "<[")
  (setq org-drill-right-cloze-delimiter "]>"))

(use-package org-download
  :after org)

(use-package org-cliplink
  :after org)

(use-package org-roam
    :hook (after-init . org-roam-mode)
    :defer t
    :init
    (setq org-roam-directory "~/syncthing/data/Default/roam")
    (setq org-roam-dailies-directory "daily/")
    (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
	   #'org-roam-capture--get-point
	   "* %?"
	   :file-name "daily/%<%Y-%m-%d>"
	   :head "#+title: %<%Y-%m-%d>\n\n")))
    (require 'org-roam-protocol))

(use-package org-roam-server
:after org-roam
:config
(setq org-roam-server-host "127.0.0.1"
      org-roam-server-port 8080
      org-roam-server-authenticate nil
      org-roam-server-export-inline-images t
      org-roam-server-server-files nil
      org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
      org-roam-server-network-poll t
      org-roam-server-network-arrows nil
      org-roam-server-network-label-truncate t
      org-roam-server-network-label-truncate-length 60
      org-roam-server-network-label-wrap-length 20))

(use-package evil-org
  :after (org evil))

(qqq/leader-keys
    "o" '(:ignore t :which-key "Org")
    "oa" '(org-agenda :which-key "agenda")
    "oc" '(org-capture :which-key "capture")
    "od" '(org-drill :which-key "drill")
    "op" '(org-download-clipboard :which-key "clipboard image")
    "or" '(:ignore t :which-key "Roam")
    "ori" '(org-roam-insert :which-key "insert link")
    "oro" '(org-open-at-point :which-key "open link")
    "orf" '(org-roam-find-file :which-key "open file")
    "orb" '(org-roam :which-key "backlinks")
    "ord" '(:ignore t :which-key "Daily notes")
    "ordc" '(org-roam-dailies-capture-today :which-key "capture notes")
    "ordf" '(org-roam-dailies-find-today :which-key "open today notes"))

(with-eval-after-load "ispell"
  (setenv "LANG" "en_US")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US,ru_RU")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,ru_RU")
  (setq ispell-personal-dictionary "~/.emacs.d/.hunspell_personal"))

(unless (file-exists-p "~/.emacs.d/.hunspell_personal")
  (shell-command "touch ~/.emacs.d/.hunspell_personal"))

(use-package expand-region
  :commands er/expand-region
  :config
  (qqq/leader-keys "=" '(er/expand-region :which-key "expands region")))

(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :after selectrum
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package company
  :custom
  (company-tooltip-limit 5)
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 3)
  (company-selection-wrap-around t)
  (company-require-match 'never)
  :config
  (add-to-list 'company-backends 'company-capf)
  :hook (after-init . global-company-mode))

(use-package ripgrep)

(use-package projectile
  :diminish
  :custom
  ((projectile-completion-system 'default))
  :config
  (projectile-mode +1)
  ;(setq projectile-switch-project-action #'magit-status)
  (qqq/leader-keys "p" '(projectile-command-map :which-key "projectile")))

(use-package hydra
  :defer t
  :config
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("f" nil "finished" :exit t))

  (qqq/leader-keys "t" '(hydra-text-scale/body :which-key "scale text"))

  (defhydra hydra-writeroom-width (:timeout 4)
    "writeroom width adjustment"
    ("j" writeroom-increase-width "increase")
    ("k" writeroom-decrease-width "decrease")
    ("=" writeroom-adjust-width   "adjust (80)")
    ("f" nil "finished" :exit t))
  (qqq/leader-keys "w" '(hydra-writeroom-width/body :which-key "writeroom adjustment")))

(use-package magit
  :commands magit-status)
(qqq/leader-keys "g" '(magit-status :which-key "magit"))

(use-package flycheck
  :hook (prog-mode . global-flycheck-mode))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (use-package lsp-ui
    :after lsp
    :commands lsp-ui-mode))

(use-package web-mode)

(use-package vue-mode)

(use-package php-mode
  :hook ((php-mode . lsp-deferred)))

(use-package clojure-mode)
(use-package cider
  :commands cider)

(use-package docker)
