;; -*- lexical-binding: t -*-

(setq comp-deferred-compilation t)

(defun q/after-frame (_ &rest)
  (set-face-attribute 'default nil :font "Iosevka" :height 110))

(add-hook 'after-make-frame-functions #'q/after-frame)

(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

(setq package-enable-at-startup nil)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun q/ensure-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
(load custom-file)

(fset 'yes-or-no-p 'y-or-n-p)
(setq create-lockfiles nil)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist `(("." .,(concat user-emacs-directory "backups"))))
(setq enable-recursive-minibuffers t)

(setq
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t)
(setq-default cursor-type 'bar)
(setq ring-bell-function 'ignore)

(column-number-mode)
(show-paren-mode 1)
(savehist-mode)
(auto-save-visited-mode)
(global-auto-revert-mode)

(add-hook 'text-mode-hook #'turn-on-auto-fill)

(setq inhibit-startup-message t)
(progn
  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (tooltip-mode -1))
(set-fringe-mode 0)

(blink-cursor-mode 0)

(progn
  (q/ensure-package 'pinentry)
  (require 'pinentry)
  (pinentry-start))

(progn
  (q/ensure-package 'password-store)
  (require 'password-store))

(progn
  (q/ensure-package 'minions)
  (require 'minions)
  (minions-mode 1))

(progn
  (q/ensure-package 'which-key)
  (require 'which-key)
  (which-key-mode))

(progn
  (q/ensure-package 'rainbow-delimiters)
  (require 'rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(progn
  (q/ensure-package 'ansi-color)
  (require 'ansi-color)
  (defun q/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook #'q/colorize-compilation-buffer))

(progn
  (setq display-line-numbers-type t)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

(progn
  (q/ensure-package 'doom-themes)
  (require 'doom-themes)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one-light t)
  (doom-themes-org-config))

(progn
  (q/ensure-package 'writeroom-mode)
  (require 'writeroom-mode))

(progn
  (q/ensure-package 'all-the-icons)
  (require 'all-the-icons))

(setq custom-tab-width 4)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default evil-shift-width custom-tab-width)
(setq-default electric-indent-inhibit t)

(setq tab-always-indent 'complete)

(setq backward-delete-char-untabify-method 'hungry)

(progn
  (q/ensure-package 'undo-tree)
  (require 'undo-tree)
  (global-undo-tree-mode))

(progn
  (q/ensure-package 'general)
  (with-eval-after-load 'evil
    (require 'general)
    (general-evil-setup t)

    (general-create-definer q/leader-keys
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC")))

(progn
  (q/ensure-package 'evil)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-vsplit-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (require 'evil)
  (evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)
  (evil-define-key 'insert 'global (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-define-key 'normal 'global (kbd "RET") 'evil-ret-and-indent)
  (evil-mode))

(progn
  (q/ensure-package 'evil-collection)
  (with-eval-after-load 'evil
    (require 'evil-collection)
    (evil-collection-init)))

(progn
  (q/ensure-package 'evil-surround)
  (with-eval-after-load 'evil
    (require 'evil-surround)
    (evil-define-key 'operator global-map "s" 'evil-surround-edit)
    (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
    (evil-define-key 'visual global-map "S" 'evil-surround-region)
    (evil-define-key 'visual global-map "gS" 'evil-Surround-region)))

(progn
  (q/ensure-package 'evil-commentary)
  (with-eval-after-load 'evil
    (require 'evil-commentary)
    (evil-commentary-mode)
    (with-eval-after-load 'general
      (q/leader-keys "'" '(evil-commentary-line :which-key "comment line(s)")))))

(progn
  (setq dired-dwim-target t)
  (progn
    (q/ensure-package 'dired-single)
    (require 'dired-single)
    (with-eval-after-load 'evil-collection
      (evil-collection-define-key 'normal 'dired-mode-map
        "h" 'dired-single-up-directory
        "l" 'dired-single-buffer))))

(progn
  (q/ensure-package 'all-the-icons-dired)
  (require 'all-the-icons-dired)
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(with-eval-after-load 'general
  (q/leader-keys
    "d" '(:ignore t :which-key "Dired")
    "dd" '(dired :which-key "dwim")
    "do" '(dired-other-window :which-key "other window")))

(setq calendar-latitude 54.99244)
(setq calendar-longitude 73.36859)
(setq calendar-week-start-day 1)

(defun q/last-day-of-week-of-month-p (date day-of-week)
  (let ((today-of-week (calendar-day-of-week date))
        (month (calendar-extract-month date))
        (next-week-month (calendar-extract-month (calendar-gregorian-from-absolute (+ (calendar-absolute-from-gregorian date) 7)))))
    (and
     (eq today-of-week day-of-week)
     (not (eq next-week-month month)))))

(setq q/org-directory (concat (getenv "SYNCTHING") "org"))
(defun q/get-org-file (file)
  (concat q/org-directory file))

(progn
  (q/ensure-package 'org)
  (require 'org)
  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'note)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-indent-indentation-per-level 1)
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-cycle-separator-lines 1)
  (setq org-startup-with-inline-images t)
  (setq org-directory q/org-directory)
  (setq org-agenda-files (list org-directory))
  (setq org-refile-targets '(("next.org" :level . 0)
                             ("projects.org" :maxlevel . 1)
                             ("life.org" :level . 0)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

  (setq org-return-follow-links t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (setq org-confirm-babel-evaluate nil)
  (setq org-src-tab-acts-natively t)
  (require 'ox-md)

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(progn
  (setq q/org-agenda-todo-view
        `(("A" "Agenda"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-deadline-warning-days 365)))
            (todo "TODO"
                  ((org-agenda-overriding-header "To Refile")
                   (org-agenda-files `(,(q/get-org-file "/inbox.org")))))
            (todo "NEXT"
                  ((org-agenda-overriding-header "In Progress")
                   (org-agenda-files `(,(q/get-org-file "/next.org")
                                       ,(q/get-org-file "/projects.org")))))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting")
                   (org-agenda-files `(,(q/get-org-file "/next.org")
                                       ,(q/get-org-file "/projects.org")))))
            nil))))
  (setq org-agenda-custom-commands `,q/org-agenda-todo-view)
  (advice-add 'org-refile :after
              (lambda (&rest _)
                (org-save-all-org-buffers))))

(with-eval-after-load 'org
  (setq org-capture-templates
        `(("i" "inbox" entry (file ,(q/get-org-file "/inbox.org"))
           (file ,(q/get-org-file "/tmpl/task")) :immediate-finish t)
          ("l" "link" entry (file ,(q/get-org-file "/inbox.org"))
           (file ,(q/get-org-file "/tmpl/link")) :immediate-finish t))))

(progn
  (q/ensure-package 'org-drill)
  (with-eval-after-load 'org
    (require 'org-drill)
    (setq org-drill-add-random-noise-to-intervals-p t)
    (setq org-drill-learn-fraction 0.25)
    (setq org-drill-hint-separator "||")
    (setq org-drill-left-cloze-delimiter "<[")
    (setq org-drill-right-cloze-delimiter "]>")))

(progn
  (q/ensure-package 'org-download)
  (with-eval-after-load 'org
    (require 'org-download)))

(progn
  (q/ensure-package 'org-cliplink)
  (with-eval-after-load 'org
    (require 'org-cliplink)))

(setq q/org-roam-directory (concat (getenv "SYNCTHING") "roam"))
(progn
  (q/ensure-package 'org-roam)
  (setq org-roam-directory q/org-roam-directory)
  (setq org-roam-v2-ack t)
  (setq org-roam-dailies-directory "daily/")
  (add-to-list 'display-buffer-alist
               '(("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))
  (require 'org-roam)
  (require 'org-roam-protocol)
  (add-hook 'after-init-hook #'org-roam-setup))

(progn
  (q/ensure-package 'evil-org)
  (with-eval-after-load 'evil
    (with-eval-after-load 'org
      (require 'evil-org)
      (require 'evil-org-agenda)
      (evil-org-agenda-set-keys)))
  (add-hook 'org-mode-hook #'evil-org-mode))

(progn
  (setq org-toggl-inherit-toggl-properties t)
  (add-to-list 'load-path (concat user-emacs-directory "custom"))
  (require 'org-toggl)
  (toggl-get-projects)
  (org-toggl-integration-mode))

(q/leader-keys
  "o" '(:ignore t :which-key "Org")
  "oa" '(org-agenda :which-key "agenda")
  "oc" '(org-capture :which-key "capture")
  "od" '(org-drill :which-key "drill")
  "op" '(org-download-clipboard :which-key "clipboard image")
  "os" '(org-save-all-org-buffers :which-key "save all org buffers")
  "oz" '(org-revert-all-org-buffers :which-key "revert all org buffers")
  "ot" '(:ignore t :which-key "Time Tracking")
  "oti" '(org-clock-in :which-key "clock in")
  "oto" '(org-clock-out :which-key "clock out")
  "or" '(:ignore t :which-key "Roam")
  "ori" '(org-roam-node-insert :which-key "node insert")
  "orf" '(org-roam-node-find :which-key "node find")
  "orb" '(org-roam-buffer-toggle :which-key "backlinks")
  "org" '(org-roam-graph :which-key "graph")
  "ord" '(:ignore t :which-key "Daily notes")
  "ordc" '(org-roam-dailies-capture-today :which-key "capture notes")
  "ordf" '(org-roam-dailies-goto-today :which-key "open today notes"))

(progn
  (q/ensure-package 'expand-region)
  (require 'expand-region)
  (q/leader-keys "=" '(er/expand-region :which-key "expand region")))

(progn
  (q/ensure-package 'selectrum)
  (require 'selectrum)
  (selectrum-mode +1))

(progn
  (q/ensure-package 'selectrum-prescient)
  (with-eval-after-load 'selectrum
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1)))

(progn
  (q/ensure-package 'marginalia)
  (require 'marginalia)
  (marginalia-mode))

(progn
  (q/ensure-package 'company)
  (require 'company)
  (setq company-tooltip-limit 5)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq company-require-match 'never)
  (add-to-list 'company-backends 'company-capf)
  (add-hook 'after-init-hook #'global-company-mode))

(progn
  (q/ensure-package 'project)
  (require 'project)
  (q/leader-keys "p" '(:keymap project-prefix-map :which-key "project")))

(progn
  (q/ensure-package 'find-file-in-project)
  (require 'find-file-in-project)
  (setq ffip-use-rust-fd t))

(progn
  (q/ensure-package 'rg)
  (require 'rg)
  (with-eval-after-load 'project
    (define-key project-prefix-map "s" #'rg-project)))

(progn
  (q/ensure-package 'fzf)
  (setenv "FZF_DEFAULT_COMMAND" "fd --type f")
  (require 'fzf)
  (q/leader-keys "z" '(fzf-find-file :which-key "FZF find file"))
  (q/leader-keys "b" '(fzf-switch-buffer :which-key "FZF switch buffer"))
  (defun fzf-no-ignore ()
    (interactive)
    (setenv "FZF_DEFAULT_COMMAND" "fd --no-ignore --type f"))
  (defun fzf-ignore ()
    (interactive)
    (setenv "FZF_DEFAULT_COMMAND" "fd --type f"))
  (with-eval-after-load 'project
    (defun project-fzf ()
      (interactive)
      (let ((project (project-current t)))
        (fzf-find-file-in-dir (project-root project))))
    (define-key project-prefix-map "z" #'project-fzf)))

(progn
  (q/ensure-package 'magit)
  (require 'magit)
  (q/leader-keys "g" '(magit-status :which-key "magit"))
  (with-eval-after-load 'project
    (define-key project-prefix-map "m" #'magit-status)
    (add-to-list 'project-switch-commands '(magit-status "Magit"))))

(progn
  (q/ensure-package 'magit-todos)
  (require 'magit-todos))

(progn
  (q/ensure-package 'elfeed)
  (require 'elfeed)
  (setq elfeed-feeds '(
                       ("https://lithub.com/feed" lithub)
                       ("https://news.ycombinator.com/rss" hackernews)
                       ))
  (setq elfeed-search-filter "@7-days-ago")
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 100))

(progn
  (q/ensure-package 'flycheck)
  (require 'flycheck)
  (add-hook 'prog-mode-hook #'global-flycheck-mode))

(progn
  (add-hook 'prog-mode-hook #'electric-pair-mode))

(progn
  (q/ensure-package 'docker)
  (require 'docker))

(progn
  (q/ensure-package 'tree-sitter)
  (q/ensure-package 'tree-sitter-langs)
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(progn
  (q/ensure-package 'lsp-mode)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-intelephense-multi-root nil)
  (require 'lsp-mode)
  (with-eval-after-load 'lsp-mode
    (q/ensure-package 'lsp-ui)
    (require 'lsp-ui))
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(progn
  (q/ensure-package 'dap-mode)
  (require 'dap-mode)
  (dap-auto-configure-mode)
  (require 'dap-php))

(progn
  (q/ensure-package 'web-mode)
  (require 'web-mode))

(progn
  (q/ensure-package 'php-mode)
  (require 'php-mode)
  (add-hook 'php-mode-hook #'lsp-deferred))

(setq nxml-child-indent 4)
