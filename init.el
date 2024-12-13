;; -*- lexical-binding: t -*-
(setq user-full-name "Queyenth")
(setq user-mail-address "queyenth@gmail.com")

(add-to-list 'load-path (concat user-emacs-directory "custom"))
;; (require 'q-custom-autoloads)

(require 'emenu)
(require 'qq-mode)

(use-package qrem
  :bind ("C-c R" . q/rem-reminder))

;; (use-package modeline
;;   :config
;;   ;; (q/modeline-subtle-mode 1)
;;   (q/mode-line-set-folder-cut-each 3)
;;   (q/mode-line-set-folder-count 3))

;; (use-package jumplist
;;   :disabled
;;   :config
;;   (q/jumplist-mode 1)
;;   (defvar-keymap jumplist-repeat-keymap
;;     :repeat (:exit (q/jumplist-clear q/jumplist-consult))
;;     "j" #'q/jumplist-consult
;;     "i" #'q/jumplist-next
;;     "o" #'q/jumplist-prev
;;     "c" #'q/jumplist-clear)
;;   (keymap-global-set "C-c j" jumplist-repeat-keymap))

(register-input-method
 "jcuken-on-canary" "Russian" 'quail-use-package
 "Ф" "ЙЦУКЕН For canary layout"
 "canary")
(setq default-input-method "jcuken-on-canary")
(setq epg-pinentry-mode 'loopback)

(defun q/after-frame (&optional _)
  (interactive)
  ;; (set-face-attribute 'default nil :font "IBM Plex Mono" :height 100 :weight 'regular)
  ;; (set-face-attribute 'fixed-pitch nil :font "IBM Plex Mono" :height 100 :weight 'regular)
  ;; (set-face-attribute 'default nil :font "FantasqueSansM Nerd Font Mono" :height 110)
  ;; (set-face-attribute 'default nil :font "CozetteVector" :height 110)
  (set-face-attribute 'default nil :font "Iosevka Comfy Motion SemiLight" :height 100)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka Comfy Motion SemiLight" :height 100)
  ;; (set-face-attribute 'default nil :font "Iosevka Comfy Motion" :height 110 :weight 'regular)
  ;; (set-face-attribute 'variable-pitch nil :font "IBM Plex Sans Text")
  (set-face-attribute 'variable-pitch nil :font "Iosevka Comfy Motion Duo"))

(add-hook 'after-make-frame-functions #'q/after-frame)

(setq default-frame-alist '((alpha . (100 . 100))
                            (min-height . 1)
                            (height . 45)
                            (min-width . 1)
                            (width . 81)
                            (vertical-scroll-bars . nil)
                            (internal-border-width . 12)
                            (left-fringe . 0)
                            (right-fringe . 0)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)))

(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places t)
(window-divider-mode)
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setopt package-install-upgrade-built-in t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setenv "UID" (string-trim (shell-command-to-string "id -u")))

(setopt use-short-answers t)
(save-place-mode)
(setq save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist `(("." .,(concat user-emacs-directory "backups"))))
(setq enable-recursive-minibuffers t)
;; (setopt undo-no-redo nil)

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))

(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq frame-inhibit-implied-resize t)
(setq show-trailing-whitespace t)
(setq uniquify-buffer-name-style 'forward)
(setq set-mark-command-repeat-pop t)

(setq
 apropos-do-all t
 mouse-yank-at-point t)
(setq-default cursor-type 'box)
(setq x-stretch-cursor t)
(setq ring-bell-function 'ignore)

;; (column-number-mode)
(show-paren-mode 1)

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq history-length 500)
  (setq history-delete-duplicates t)
  (setq savehist-additional-variables '(register-alist kill-ring)))

(recentf-mode)

(auto-save-visited-mode)
(global-auto-revert-mode)
(global-so-long-mode 1)
(pixel-scroll-precision-mode)
(setopt window-resize-pixelwise t)
(setopt frame-resize-pixelwise t)

(setq inhibit-startup-message t)
(progn
  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (tooltip-mode -1))

(setq mouse-autoselect-window t)
(setq mouse-drag-and-drop-region t)
(setq mouse-drag-and-drop-region-cross-program t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(setq compilation-scroll-output 'first-error)
(setq compilation-always-kill t)

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type t))

(use-package ef-themes
  :disabled
  :config
  (setq ef-melissa-light-palette-overrides '((bg-region "#fae7b0")))
  (load-theme 'ef-melissa-light :no-confirm))

;; oxeded theme
(setq oxeded-red "#AD5C62")
(setq oxeded-green "#88B86E")
(setq oxeded-yellow "#E5CA84")
(setq oxeded-blue "#495FC2")
(setq oxeded-magenta "#905CAD")
(setq oxeded-cyan "#489FA1")

(use-package minimal-theme
  :config
  (let ((fg "grey20")
        (bg "white")
        (altbg "grey90")
        (subtle "grey50")
        (accent oxeded-blue)) ;; blue accent
    (custom-set-faces
     `(vertico-current ((t (:background ,altbg))))
     `(corfu-current ((t (:foreground ,fg :background ,altbg))))
     `(completions-common-part ((t (:foreground ,accent))))
     `(orderless-match-face-0 ((t (:foreground ,accent))))
     `(font-lock-warning-face ((t (:foreground ,accent))))
     `(flymake-warning ((t (:underline (:style wave :color ,accent)))))
     `(flymake-note ((t (:underline (:style wave :color "grey80")))))
     `(link ((t (:foreground ,oxeded-blue :underline t))))
     `(link-visited ((t (:foreground ,oxeded-magenta :underline t))))
     `(secondary-selection ((t (:background "grey80"))))
     `(org-agenda-date ((t (:foreground ,fg))))
     `(org-agenda-date-today ((t (:foreground ,fg :weight bold))))
     `(org-agenda-date-weekend ((t (:foreground ,fg :weight normal))))
     `(org-agenda-structure ((t (:foreground ,fg :weight bold))))))
  (load-theme 'minimal-light :no-confirm))

(use-package highlight-numbers
  :init
  (custom-set-faces
   `(highlight-numbers-number ((t (:foreground ,oxeded-red)))))
  (highlight-numbers-mode))

(use-package all-the-icons
  :init
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode)))

(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t)
  (setq dired-mouse-drag-files t)
  (setq dired-listing-switches "-al --dired --group-directories-first -h"))

(use-package ediff
  :defer
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(setq custom-tab-width 4)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default electric-indent-inhibit t)

(setq tab-always-indent 'complete)

(setq backward-delete-char-untabify-method 'hungry)

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

(setq zoneinfo-style-world-list '(("America/New_York" "New York")
                                  ("Europe/London" "London")
                                  ("Europe/Paris" "Paris")
                                  ("Europe/Moscow" "Moscow")
                                  ("Asia/Omsk" "Omsk")
                                  ("Asia/Tokyo" "Tokyo")))

(setq q/org-directory (concat (getenv "SYNCTHING") "org"))
(defun q/get-org-file (file)
  (concat q/org-directory file))

;; Because yank-media is broken on my system.
;; as soon as it fixed, will get rid of org-download.
(use-package org-download
  :after org
  :bind ("C-c o p" . org-download-clipboard))

(use-package org
  :defer t
  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ;; ("C-c o p" . yank-media) ;; doesn't work on emacs 29.4, org 9.7.9, pgtk, sway (wlroots)
         ("C-c o s" . org-save-all-org-buffers)
         ("C-c o z" . org-revert-all-org-buffers)
         ("C-c o t i" . org-clock-in)
         ("C-c o t o" . org-clock-out)
         ("C-c o t g" . org-clock-goto))
  :hook (org-mode . variable-pitch-mode)
  :init
  (setq org-directory q/org-directory)
  (setq org-adapt-indentation nil)
  (setq q/org-agenda-todo-view
        `(("A" "Agenda"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-deadline-warning-days 30)))
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
  (setq org-agenda-custom-commands q/org-agenda-todo-view)
  (setq org-agenda-files (list org-directory))

  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t%-6e% s")
                                   (todo . " %i %-12:c %-6e")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
  (setq org-agenda-skip-scheduled-if-done t)
  ;; (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-capture-templates
        `(("t" "todo for today" entry
           (file ,(q/get-org-file "/next.org"))
           (file ,(q/get-org-file "/tmpl/todo")) :empty-lines 1)
          ("b" "book" entry
           (file ,(q/get-org-file "/books.org"))
           (function q/org-capture-book-template) :empty-lines 1)
          ("e" "email" entry
           (file ,(q/get-org-file "/next.org"))
           "* NEXT %^{Title}\n%a\n%?\n:LOGBOOK:\n- Added: %U\n:END:" :empty-lines 1)
          ("i" "inbox task" entry
           (file ,(q/get-org-file "/inbox.org"))
           (file ,(q/get-org-file "/tmpl/task")) :empty-lines 1)
          ("n" "idea" entry
           (file ,(q/get-org-file "/ideas.org"))
           (file ,(q/get-org-file "/tmpl/idea")) :empty-lines 1)
          ("l" "link" entry
           (file ,(q/get-org-file "/inbox.org"))
           (file ,(q/get-org-file "/tmpl/link")) :empty-lines 1 :immediate-finish t)
          ("r" "region to clocked-in task" plain
           (clock)
           "%a\n%i" :empty-lines 1 :immediate-finish t)))
  (setq org-clock-clocked-in-display nil)
  (setq org-confirm-babel-evaluate nil)
  (setq org-cycle-separator-lines 1)
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)
  (setq org-indent-indentation-per-level 1)
  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'note)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(("next.org" :level . 0)
                             ("projects.org" :maxlevel . 1)
                             ("life.org" :level . 0)
                             ("inbox.org" :level . 0)))
  (setq org-refile-use-outline-path 'file)
  (setq org-return-follows-link t)
  (setq org-src-tab-acts-natively t)
  (setq org-startup-with-inline-images t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

  :config
  (custom-set-faces
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch))))
   '(org-property-value ((t (:inherit fixed-pitch))) t))

  (setq q/org-languages '(("sh" . "shell")
                          ("py" . "python")
                          ("el" . "emacs-lisp")
                          ("php" . "php")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   (mapcar (lambda (x) (cons (intern (cdr x)) t)) q/org-languages))

  (require 'org-tempo)
  (dolist (x q/org-languages)
    (add-to-list 'org-structure-template-alist `(,(car x) . ,(format "src %s" (cdr x)))))

  (org-crypt-use-before-save-magic)

  (advice-add 'org-refile :after
              (lambda (&rest _)
                (org-save-all-org-buffers)))

 (use-package org-drill
    :bind ("C-c o d" . org-drill)
    :config
    (setq org-drill-add-random-noise-to-intervals-p t
          org-drill-learn-fraction 0.25
          org-drill-hint-separator "||"
          org-drill-left-cloze-delimiter "<["
          org-drill-right-cloze-delimiter "]>"))

  (use-package org-cliplink :defer t)

  (use-package org-toggl
    :commands org-toggl-integration-mode
    :config
    (use-package request)
    (setq org-toggl-inherit-toggl-properties t)))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package orderless
  :defer 1
  :ensure t
  :init
  (defun q/toggle-kwd-orderless ()
    (interactive)
    (if (memq #'orderless-kwd-dispatch orderless-style-dispatchers)
        (progn
          (setq orderless-style-dispatchers (remove #'orderless-kwd-dispatch orderless-style-dispatchers))
          (message "Removed orderless-kwd..."))
      (add-to-list 'orderless-style-dispatchers #'orderless-kwd-dispatch)
      (message "Added orderless-kwd...")))

  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-h b" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package popper
  :disabled
  :bind (("C-<escape>" . popper-toggle)
         ("M-<escape>" . popper-cycle))
  :config
  (setq popper-reference-buffers
        '(help-mode
          compilation-mode
          "\\* docker"))
  (setq popper-group-function #'popper-group-by-project)
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package project
  :defer t
  :init
  (keymap-global-set "C-c p" project-prefix-map)
  :config
  (keymap-set project-prefix-map "z" (lambda () (interactive) (project-find-file t))))

(use-package rg
  :after project
  :config
  (keymap-set project-prefix-map "s" #'rg-project))

(setq xref-search-program 'ripgrep)

(use-package magit
  :bind (("C-c g" . magit-status)
         :map project-prefix-map
         ("m" . magit-project-status))
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(magit-project-status "Magit")))

  (with-eval-after-load 'embark
    (add-to-list 'embark-multitarget-actions #'magit-stage-file))

  (defun q/magit-my-branch-spin-off ()
    (interactive)
    (let ((clock-heading (string-replace " " "_" org-clock-heading)))
      (magit-branch-spinoff (magit-read-string-ns "Spin off branch" (format "feature/%s" clock-heading)))))

  :config
  (transient-replace-suffix 'magit-commit 'magit-commit-autofixup
    '("x" "Absorb changes" magit-commit-absorb)))

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode))

(defun q/kill-all-exec-buffers ()
  (interactive)
  (kill-matching-buffers "\* docker-compose exec" nil t))

(use-package docker :defer t)

(defun q/php-get-composer-data ()
  (let* ((root (expand-file-name (php-project-get-root-dir)))
         (composer (concat root "composer.json")))
    (with-temp-buffer
      (insert-file-contents composer)
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist))))

(require 'f)
(defun q/php-get-namespace-path ()
  "This function assumes a lot of stuff. Like it's psr-4 and stuff. A lot of trimming done just in case."
  (let* ((data (q/php-get-composer-data))
         (root (expand-file-name (php-project-get-root-dir)))
         (autoloads (alist-get 'psr-4 (alist-get 'autoload data)))
         (file-path (string-replace root "" (buffer-file-name)))
         (common-key (f-common-parent (cons file-path (mapcar #'cdr autoloads))))
         (namespace (symbol-name (car (seq-find (lambda (x) (string= (cdr x) common-key)) autoloads))))
         (inner-path (string-replace common-key "" (f-parent file-path))))
    (string-join (cons (string-trim-right namespace "\\\\") (string-split inner-path "/" t)) "\\")))

(use-package skeleton
  :defer t
  :init
  (with-eval-after-load 'php-mode
    (defun q/magento-get-namespace-path ()
      (let ((file-path (file-name-directory (or (buffer-file-name)
                                                (buffer-name (current-buffer))))))
        (string-replace "/" "\\" (and (string-match ".*/app/code/\\(.*\\)/" file-path)
                                      (match-string 1 file-path)))))
    (define-skeleton q/skel-php-namespace
      "Inserts a namespace for current file."
      nil
      "namespace " (q/php-get-namespace-path) ";\n")

    (define-skeleton q/skel-magento-namespace
      "Inserts a namespace for current magento file."
      nil
      "namespace " (q/magento-get-namespace-path) ";\n")

    (define-abbrev php-ts-mode-abbrev-table "php" "<?php declare(strict_types=1);\n")
    (define-abbrev php-ts-mode-abbrev-table "mnms" "" #'q/skel-magento-namespace)
    (define-abbrev php-ts-mode-abbrev-table "nms" "" #'q/skel-php-namespace)

    (add-hook 'php-ts-mode #'abbrev-mode)))

(use-package web-mode :defer t)

(defun q/magento (command)
  (interactive (list
                (read-string "Command: ")))
  (docker-compose-run-docker-compose-async-with-buffer "exec" '() "fpm" (concat "php -d memory_limit=4G bin/magento" " " command)))

(defun q/magento-clear-cache ()
  (interactive)
  (q/magento "cache:flush"))

(defun q/magento-di-compile ()
  (interactive)
  (q/magento "setup:di:compile"))

(defun q/magento-setup-upgrade ()
  (interactive)
  (q/magento "setup:upgrade"))

(defun q/magento-static-content-deploy (langs)
  (interactive (list
                (read-string "Locales: " nil nil "en_US en_GB")))
  (q/magento (concat "setup:static-content:deploy -f " langs)))

(setq nxml-child-indent 4)

(use-package password-store
  :bind (("C-c y y" . password-store-copy)
         ("C-c y i" . password-store-insert)
         ("C-c y e" . password-store-edit))
  :init
  (auth-source-pass-enable)
  (emenu password-store-copy))

(use-package password-store-otp
  :after password-store
  :commands password-store-otp-token-copy
  :init
  (define-advice password-store--save-field-in-kill-ring
      (:around (oldfn entry secret field) q/password-store-otp-or-pass)
    "Get OTP token if it's otpauth url."
    (if (string-prefix-p "otpauth://" secret)
        (password-store-otp-token-copy entry)
      (funcall oldfn entry secret field))))

(use-package php-mode
  :defer t)

(use-package php-ts-mode
  :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(php-mode . php-ts-mode)))

(use-package consult
  :bind (("C-c b" . consult-buffer)
         ("C-c B" . consult-buffer-other-window)
         ("C-c S" . consult-ripgrep)
         ("C-c F" . consult-fd))
  :init
  (setq register-preview-function #'consult-register-format)
  (with-eval-after-load 'project
    (keymap-set project-prefix-map "C-b" #'consult-project-buffer)
    (keymap-set project-prefix-map "C-f" #'consult-fd)
    (add-to-list 'project-switch-commands '(consult-fd "FD"))))

(use-package embark-consult
  :after (embark consult)
  :config
  (defun q/consult-embark-multi-line (buffers)
    (interactive)
    (consult-line-multi (list :include buffers)))
  (add-to-list 'embark-multitarget-actions #'q/consult-embark-multi-line)
  ;; Q because that's my favorite letter.
  (keymap-set embark-consult-search-map "Q" #'q/consult-embark-multi-line))

(defun q/mask-text (start end mask-char)
  "Overwrite the region (START to END) with the selected MASK-CHAR character."
  (interactive "@*r\ncMasking character: ")
  (let ((region-length (- end start))
        (original-point (point)))
    (delete-region start end)
    (goto-char start)
    (insert-char mask-char region-length)
    (goto-char original-point)))

(use-package repeat
  :ensure nil
  :bind ("M-z" . repeat)
  :hook (after-init . repeat-mode))

(use-package drag-stuff
  :ensure t
  :bind ((:repeat-map drag-stuff-repeat-map
                      ("n" . drag-stuff-down)
                      ("e" . drag-stuff-up)
                      ("i" . drag-stuff-right)
                      ("m" . drag-stuff-left)))
  :config
  (keymap-global-set "C-c d" drag-stuff-repeat-map))

(defvar-keymap windmove-repeat-map
  :repeat (:exit (balance-windows delete-other-windows delete-other-windows-vertically follow-delete-other-windows-and-split clone-indirect-buffer split-window-below split-window-right))
  "n" #'windmove-down
  "N" #'windmove-swap-states-down
  "e" #'windmove-up
  "E" #'windmove-swap-states-up
  "i" #'windmove-right
  "I" #'windmove-swap-states-right
  "m" #'windmove-left
  "M" #'windmove-swap-states-left
  "=" #'balance-windows
  "d" #'delete-other-windows
  "!" #'delete-other-windows-vertically
  "-" #'split-window-below
  "|" #'split-window-right
  "v" #'windmove-display-down
  "h" #'windmove-display-right
  "f" #'follow-delete-other-windows-and-split
  "c" #'clone-indirect-buffer)

(keymap-global-set "C-c w" windmove-repeat-map)

(keymap-global-set "C-c q" #'quit-window)
(keymap-global-set "C-c f" #'find-file)
(keymap-global-set "C-c k" #'kill-current-buffer)
(keymap-global-set "C-c v" #'magit)

(defvar-keymap q/mark-map
  "p" #'mark-paragraph
  "b" #'mark-whole-buffer
  "w" #'mark-word
  "d" #'mark-defun
  "f" #'mark-sexp)
(keymap-global-set "C-c m" q/mark-map)

(defvar-keymap q/transpose-map
  "p" #'transpose-paragraphs
  "c" #'transpose-chars
  "w" #'transpose-words
  "l" #'transpose-lines
  "f" #'transpose-sexps)
(keymap-global-set "C-c t" q/transpose-map)

(use-package register
  :ensure nil
  :defer 1
  :config
  (defun q/register-dwim ()
    "If region active, copy region to register, otherwise save the point."
    (interactive)
    (if (region-active-p)
        (call-interactively #'copy-to-register)
      (call-interactively #'point-to-register)))

  (defmacro q/register-macro (pred func)
    "Return a lambda that filters REGISTER-ALIST by PRED before calling FUNC."
    `(lambda ()
       (interactive)
       (let ((register-alist (seq-filter (lambda (x) (,pred (cdr x))) register-alist)))
         (call-interactively ,func))))

  (defvar-keymap q/register-map
    "'" #'q/register-dwim
    "i" (q/register-macro stringp #'insert-register)
    "j" (q/register-macro markerp #'jump-to-register))

  (setopt register-preview-delay 0)
  (keymap-global-set "C-c r" q/register-map))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package avy
  :bind (("C-z" . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-z" . avy-isearch))

  :config
  (defun q/avy-activate-mark (pt)
    "Activate a mark and then jump to PT."
    (activate-mark)
    (goto-char pt))
  (setf (alist-get ?  avy-dispatch-alist) #'q/avy-activate-mark)

  (defun q/avy-embark-act (pt)
    "Embark Act on a thing at PT."
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) #'q/avy-embark-act)

  (defun q/avy-refactor (pt)
    "Replace sexp at PT with a symbol before current point, then paste
the sexp at current point."
    (unwind-protect
        ;; Not a cute code, but saves a variable, I guess.
        (when (save-excursion
                (forward-symbol -1)
                (when-let* ((symb (thing-at-point 'symbol)))
                  (goto-char pt)
                  (kill-sexp)
                  (insert symb)
                  t))
          (yank))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?r avy-dispatch-alist) #'q/avy-refactor))

(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                                        (cmake "https://github.com/tree-sitter/tree-sitter-bash")
                                        (c "https://github.com/tree-sitter/tree-sitter-c")
                                        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                        (css "https://github.com/tree-sitter/tree-sitter-css")
                                        (go "https://github.com/tree-sitter/tree-sitter-go")
                                        (html "https://github.com/tree-sitter/tree-sitter-html")
                                        (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
                                        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                                        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                                        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                                        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
                                        (json "https://github.com/tree-sitter/tree-sitter-json")
                                        (make "https://github.com/alemuller/tree-sitter-make")
                                        (python "https://github.com/tree-sitter/tree-sitter-python"))))

(use-package puni
  :bind (("C-c (" . puni-slurp-forward)
         ("C-c )" . puni-slurp-backward)
         ("C-c }" . puni-barf-forward)
         ("C-c {" . puni-barf-backward)
         ("C-(" . puni-slurp-forward)
         ("C-)" . puni-slurp-backward)
         ("C-}" . puni-barf-forward)
         ("C-{" . puni-barf-backward))
  :init
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (add-hook 'minibuffer-setup-hook #'puni-disable-puni-mode)
  (keymap-set q/transpose-map "f" #'puni-transpose))

;; https://karthinks.com/software/a-consistent-structural-editing-interface/
;; (defvar-keymap structural-edit-map
;;   :repeat t
;;   "u" #'backward-up-list
;;   "d" #'down-list
;;   "f" #'forward-sexp
;;   "b" #'backward-sexp
;;   "K" #'kill-sexp
;;   "/" #'undo

;;   "n" #'puni-forward-sexp
;;   "p" #'puni-backward-sexp
;;   "k" #'puni-kill-line
;;   "(" #'puni-slurp-backward
;;   ")" #'puni-slurp-forward
;;   "{" #'puni-barf-backward
;;   "}" #'puni-barf-forward
;;   "s" #'puni-split
;;   "S" #'puni-squeeze
;;   "R" #'puni-raise
;;   "U" #'puni-splice
;;   "t" #'puni-transpose
;;   "x" #'eval-defun)

(defun q/narrow-region-toggle (start end)
  "Narrow from START to END or widen if narrowed."
  (interactive "r")
  (if (buffer-narrowed-p)
      (progn
        (widen)
        (recenter))
    (narrow-to-region start end)
    (deactivate-mark)))

(use-package meow
  :defer 1
  :init
  (defmacro q/call-interactive-with (prefix func)
    "Set CURRENT-PREFIX-ARG to PREFIX before calling FUNC."
    `(let ((current-prefix-arg ,prefix))
       (call-interactively ,func)))
  (defun q/meow-negative-find ()
    "MEOW-FIND backwards."
    (interactive)
    (q/call-interactive-with -1 #'meow-find))
  (defun q/meow-negative-till ()
    "MEOW-TILL backwards."
    (interactive)
    (q/call-interactive-with -1 #'meow-till))

  (defconst meow-cheatsheet-layout-canary
    '((<TLDE> "`"	"~")
      (<AE01> "1"	"!")
      (<AE02> "2"	"@")
      (<AE03> "3"	"#")
      (<AE04> "4"	"$")
      (<AE05> "5"	"%")
      (<AE06> "6"	"^")
      (<AE07> "7"	"&")
      (<AE08> "8"	"*")
      (<AE09> "9"	"(")
      (<AE10> "0"	")")
      (<AE11> "-"	"_")
      (<AE12> "="	"+")
      (<AD01> "w"	"W")
      (<AD02> "l"	"L")
      (<AD03> "y"	"Y")
      (<AD04> "p"	"P")
      (<AD05> "b"	"B")
      (<AD06> "z"	"Z")
      (<AD07> "f"	"F")
      (<AD08> "o"	"O")
      (<AD09> "u"	"u")
      (<AD10> "'"	"\"")
      (<AD11> "["	"{")
      (<AD12> "]"	"}")
      (<AC01> "c"	"C")
      (<AC02> "r"	"R")
      (<AC03> "s"	"S")
      (<AC04> "t"	"T")
      (<AC05> "g"	"G")
      (<AC06> "m"	"M")
      (<AC07> "n"	"N")
      (<AC08> "e"	"E")
      (<AC09> "i"	"I")
      (<AC10> "a"	"A")
      (<AC11> ";"	":")
      (<AB01> "q"	"Q")
      (<AB02> "j"	"J")
      (<AB03> "v"	"V")
      (<AB04> "d"	"D")
      (<AB05> "k"	"K")
      (<AB06> "x"	"X")
      (<AB07> "h"	"H")
      (<AB08> "/"	"?")
      (<AB09> ","	"<")
      (<AB10> "."	">")
      (<BKSL> "\\"      "|")))

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-canary)
    (setq meow-keypad-leader-dispatch "C-c")

    (meow-motion-overwrite-define-key
     ;; Canary
     '("n" . meow-next)
     '("e" . meow-prev)
     '("!" . repeat)
     '("<escape>" . ignore))

    (meow-leader-define-key
     ;; SPC n/e will run the original command in MOTION state.

     '("n" . "H-n")
     '("e" . "H-e")

     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))

    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)

     ;; Symbols
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("(" . meow-beginning-of-thing)
     '(")" . meow-end-of-thing)
     '("{" . backward-paragraph)
     '("}" . forward-paragraph)
     '("[" . backward-sexp)
     '("]" . forward-sexp)

     ;; TODO: Maybe a repeat keymap for structural editing/movement?
     ;; TODO: Also some prefix for some commands and maps? I guess.
     '("<" . beginning-of-defun)
     '(">" . end-of-defun)
     '("@" . backward-up-list)
     '("$" . down-list)
     '("#" . up-list)
     '("=" . meow-indent)
     '("/" . consult-line)
     '("`" . downcase-dwim)
     '("~" . upcase-dwim)
     '("?" . meow-comment)
     '("!" . repeat)
     '(":" . eval-expression)
     '("+" . puni-raise)
     '("%" . puni-splice)
     '("^" . meow-back-to-indentation)
     '("&" . meow-clipboard-save)
     (cons "*" q/mark-map)
     (cons "'" q/register-map)
     '("\"" . q/narrow-region-toggle)

     ;; Top row
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("l" . meow-line)
     '("L" . meow-goto-line)
     ;; why the hell p is yank and y is save?
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("p" . meow-yank)
     '("P" . meow-yank-pop)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("z" . meow-pop-selection)
     '("Z" . consult-imenu)
     '("f" . meow-find)
     '("F" . q/meow-negative-find)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)

     ;; Mid row
     '("c" . meow-change)
     ;; (cons "C" q/transpose-map)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-insert)
     '("S" . meow-open-above)
     '("t" . meow-till)
     '("T" . q/meow-negative-till)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("m" . meow-left)
     '("M" . meow-left-expand)
     '("n" . meow-next)
     '("N" . meow-next-expand)
     '("e" . meow-prev)
     '("E" . meow-prev-expand)
     '("i" . meow-right)
     '("I" . meow-right-expand)
     '("a" . meow-append)
     '("A" . meow-open-below)

     ;; Bottom row
     '("q" . meow-quit)
     '("Q" . consult-goto-line)
     '("j" . meow-join)
     '("J" . (lambda () (interactive) (join-line 1)))
     '("v" . meow-visit)
     '("V" . consult-yank-from-kill-ring)
     '("d" . meow-kill)
     '("D" . puni-kill-region)
     '("k" . meow-next-word)
     '("K" . meow-next-symbol)
     '("x" . meow-delete)
     '("X" . kill-sexp)
     '("h" . meow-search)
     ;; '("H" . avy-goto-char-timer)

     '("<f5>" . recompile)
     '("<escape>" . ignore)))
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package meow-tree-sitter
  :after meow
  :config
  (dolist (bind '((?a . "class")
                  (?y . "entry")
                  (?f . "function")
                  (?t . "test")
                  (?, . "parameter")
                  (?/ . "comment")))
    (meow-tree-sitter-register-thing (car bind) (cdr bind))))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (defvar-keymap flymake-repeat-map
    :repeat (:exit (flymake-show-buffer-diagnostics))
    "n" #'flymake-goto-next-error
    "e" #'flymake-goto-prev-error
    "b" #'flymake-show-buffer-diagnostics)
  (keymap-set global-map "C-c D" flymake-repeat-map)
  (setq flymake-show-diagnostics-at-end-of-line nil))

(defcustom lsp-intelephense-key ""
  "Enter the intelephense key."
  :type 'string
  :group 'eglot)

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

(use-package eglot
  :defer t
  :init
  (setq eglot-sync-connect nil)
  (setq eglot-events-buffer-size 0)
  :config
  (add-to-list 'eglot-server-programs
               `((php-mode php-ts-mode phps-mode) . ("intelephense" "--stdio" :initializationOptions (:licenseKey ,lsp-intelephense-key)))))

;; Waiting for :vc keyword in use-package.
;; https://sr.ht/~meow_king/peek/
(use-package peek
  :bind (("C-c P" . peek-overlay-dwim))
  :config
  (setq peek-overlay-window-size 10)
  (setq peek-overlay-position 'above)
  (setq peek-overlay-distance 5)
  (setq peek-live-update t)
  (global-peek-mode 1)
  (keymap-set peek-mode-keymap "M-n" #'peek-next-line)
  (keymap-set peek-mode-keymap "M-p" #'peek-prev-line))

(use-package copy-as-format
  :defer t)

(use-package typescript-ts-mode
  :after eglot
  :config
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode) . ("typescript-language-server" "--stdio"))))

(use-package dape
  :defer t
  :init
  (setq dape-buffer-window-arrangement 'right))

(use-package corfu
  :init
  (setq completion-cycle-threshold 3)
  (setq corfu-cycle t)
  ;; (setq corfu-auto nil)
  ;; (setq corfu-auto-delay 0.1)
  ;; (setq corfu-auto-prefix 4)
  (setq corfu-separator ?\s)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  (with-eval-after-load 'cc-mode
    (when (equal tab-always-indent 'complete)
      (define-key c-mode-base-map [remap c-indent-line-or-region] #'indent-for-tab-command))))

(use-package corfu-candidate-overlay
  :after corfu
  :config
  (corfu-candidate-overlay-mode))

(use-package cape
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package erc
  :defer t
  :config
  (setq erc-server "irc.libera.chat"
        erc-nick "queyenth"
        erc-user-full-name "QQQ"
        erc-track-shorten-start 5
        erc-kill-buffer-on-part t
        erc-receive-query-display 'bury))

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-user "queyenth@gmail.com")
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(setq smtpmail-stream-type 'starttls)

(setq
 mailcap-download-directory "/tmp"
 gnus-inhibit-images t
 gnus-buttonized-mime-types '("multipart/alternative" "multipart/encrypted" "multipart/signed" ".*/signed" "text/x-org" "text/richtext" "text/enriched")
 mm-html-inhibit-images t
 mm-enable-external 'ask
 mm-automatic-display '("text/plain")
 mm-discouraged-alternatives '("text/html" "text/richtext" "text/enriched" "image/.*")
 mm-inlined-types '("text/plain" "text/html")
 mm-inline-media-tests `(("text/plain" mm-inline-text identity)
                         ("text/html" mm-inline-text-html ,(lambda (_handle) mm-text-html-renderer))
                         (".*" ignore identity)))

(use-package notmuch
  :load-path "/usr/local/share/emacs/site-lisp/"
  :bind ("C-c M" . notmuch)
  :init
  (with-eval-after-load 'meow
    (setf (alist-get 'notmuch-hello-mode meow-mode-state-list) 'motion)
    (setf (alist-get 'notmuch-search-mode meow-mode-state-list) 'motion))
  :config
  (defun q/notmuch-new ()
    (interactive)
    (async-shell-command "notmuch new"))
  (setq-default notmuch-search-oldest-first nil)
  (setq notmuch-hello-sections '(notmuch-hello-insert-header
                                 notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-footer))
  (setq notmuch-saved-searches '((:name "unread" :query "tag:unread" :key "u")
                                 (:name "todo" :query "tag:todo" :key "t")
                                 (:name "sent" :query "tag:sent" :key "s")
                                 (:name "bills" :query "from:vp.ru" :key "b")
                                 (:name "kontur" :query "from:kontur.ru" :key "k"))))

(use-package notmuch-indicator
  :ensure t
  :after notmuch
  :config
  (setq notmuch-indicator-args
        '((:terms "tag:unread and tag:inbox" :label "󰇮 ")
          (:terms "tag:todo" :label " ")))
  (setq notmuch-indicator-counter-format "%s%s ")
  (notmuch-indicator-mode))

(use-package ol-notmuch)

(use-package clojure-mode :defer t)
(use-package cider :defer t)
(use-package meson-mode :defer t)
(use-package pdf-tools :defer t)

(use-package go-mode :defer t)

(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
(setq search-whitespace-regexp ".*?")

(define-abbrev text-mode-abbrev-table "qqn" "queyenth")
(define-abbrev text-mode-abbrev-table "qqgit" "https://github.com/queyenth")
(add-hook 'text-mode-hook #'abbrev-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setopt diff-default-read-only t)

(defun q/set-yuck-imenu-generic-expr ()
  "Imenu generic expressions for yuck mode."
  (setq-local imenu-generic-expression '(("Variables" "([\n\t\s]*defvar[\n\t\s]*\\([a-zA-Z0-9_-]*\\)" 1)
                                         ("Listen" "([\n\t\s]*deflisten[\n\t\s]*\\([a-zA-Z0-9_-]*\\)" 1)
                                         ("Poll" "([\n\t\s]*defpoll[\n\t\s]*\\([a-zA-Z0-9_-]*\\)" 1)
                                         ("Widget" "([\n\t\s]*defwidget[\n\t\s]*\\([a-zA-Z0-9_-]*\\)" 1)
                                         ("Window" "([\n\t\s]*defwindow[\n\t\s]*\\([a-zA-Z0-9_-]*\\)" 1))))

(use-package yuck-mode
  :hook (yuck-mode . q/set-yuck-imenu-generic-expr))

;; TODO: Extract only genius lyrics fetcher. I don't use EMMS.
(use-package lyrics-fetcher
  :defer t
  :init
  (setq lyrics-fetcher-genius-access-token (password-store-get "genius-api-token"))
  ;; TODO: Change the file format to match the ncmpcpp one.
  (setq lyrics-fetcher-lyrics-folder "~/.local/share/ncmpcpp/lyrics/")
  (defun q/get-mpris-track-info ()
    "Get track info to feed to lyrics-fetcher."
    (interactive)
    (let ((artist (shell-command-to-string "playerctl metadata artist"))
          (title (shell-command-to-string "playerctl metadata title")))
      `((info-artist . ,(string-trim artist))
        (info-title . ,(string-trim title)))))
  (setq lyrics-fetcher-current-track-method #'q/get-mpris-track-info))

(use-package atomic-chrome
  :ensure t
  :hook (after-init . atomic-chrome-start-server)
  :init
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-buffer-open-style 'frame)
  (setq atomic-chrome-extension-type-list '(ghost-text)))

;; Because I don't use bar anymore. Maybe utilize tab-bar, as I don't use tabs really? thinking...
;; Also there's a header line or something...
;; (setq display-time-24hr-format t)
;; (setq display-time-default-load-average nil)
;; (display-time-mode)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'set-goal-column 'disabled nil)
