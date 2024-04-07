;; -*- lexical-binding: t -*-
(setq user-full-name "Queyenth")
(setq user-mail-address "q@queyenth.xyz")

(setq native-comp-jit-compilation t)

(when (boundp 'x-mod3-keysym) (setopt x-mod3-keysym 'hyper))

(keymap-global-set "H-n" #'windmove-down)
(keymap-global-set "H-m" #'windmove-left)
(keymap-global-set "H-e" #'windmove-up)
(keymap-global-set "H-i" #'windmove-right)
(keymap-global-set "H-N" #'windmove-swap-states-down)
(keymap-global-set "H-M" #'windmove-swap-states-left)
(keymap-global-set "H-E" #'windmove-swap-states-up)
(keymap-global-set "H-I" #'windmove-swap-states-right)
(keymap-global-set "H-f" #'find-file)
(keymap-global-set "H-b" #'switch-to-buffer)
(keymap-global-set "H-B" #'switch-to-buffer-other-window)
(keymap-global-set "H-d" #'dired)
(keymap-global-set "H-j" #'dired-jump)
(keymap-global-set "H-J" #'dired-jump-other-window)
(keymap-global-set "H-v" #'magit)

(defun q/after-frame (&optional _)
  ;(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 110)
  ;(set-face-attribute 'default nil :font "Iosevka Comfy" :height 110)

  ;(set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font" :height 110)
  ;(set-face-attribute 'default nil :font "Monoid Nerd Font" :height 100)
  ;(set-face-attribute 'default nil :font "Hack Nerd Font" :height 100)
  ;(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 100)
  ;(set-face-attribute 'default nil :font "Inconsolata Nerd Font" :height 110)
  ;(set-face-attribute 'default nil :font "UbuntuMono Nerd Font" :height 110)
  ;(set-face-attribute 'default nil :font "AverageMono" :height 110)
  ;(set-face-attribute 'default nil :font "OverpassM Nerd Font" :height 100)
  ;(set-face-attribute 'default nil :font "0xProto Nerd Font Mono" :height 100)
  ;(set-face-attribute 'default nil :font "Agave Nerd Font Mono" :height 110)
  ;(set-face-attribute 'default nil :font "VictorMono Nerd Font Mono Medium" :height 100)
  ;(set-face-attribute 'default nil :font "CommitMono Nerd Font" :height 100)

  (set-face-attribute 'default nil :font "IBM Plex Mono Text" :height 100)

  ;This is the same as IBM Plex Mono (I think, but I can spot
  ;something a biiiiiit different, or I'm imagining things) but with
  ;icons and stuff
  ;(set-face-attribute 'default nil :font "BlexMono Nerd Font" :height 100)

  ;(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 110)
  (set-face-attribute 'variable-pitch nil :font "IBM Plex Sans Text"))

(add-hook 'after-make-frame-functions #'q/after-frame)

(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

(setq package-enable-at-startup nil)
(setopt package-install-upgrade-built-in t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun q/ensure-package (package &optional url)
  (unless (package-installed-p package)
    (if url (package-vc-install url)
      (package-install package))))

(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (concat user-emacs-directory "custom"))

(setenv "UID" (string-trim (shell-command-to-string "id -u")))

(fset 'yes-or-no-p 'y-or-n-p)
(setq create-lockfiles nil)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist `(("." .,(concat user-emacs-directory "backups"))))
(setq enable-recursive-minibuffers t)

(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq frame-inhibit-implied-resize t)
(setq show-trailing-whitespace t)

(setq
 apropos-do-all t
 mouse-yank-at-point t)
(setq-default cursor-type 'bar)
(setq ring-bell-function 'ignore)

(column-number-mode)
(show-paren-mode 1)

(progn
  (setq history-length 500)
  (setq history-delete-duplicates t)
  (setq savehist-additional-variables '(register-alist kill-ring))
  (savehist-mode))

(auto-save-visited-mode)
(global-auto-revert-mode)
(global-so-long-mode 1)
(pixel-scroll-precision-mode)

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
  (q/ensure-package 'minions)
  (setq minions-prominent-modes '(wc-goal-mode meow-normal-mode meow-insert-mode meow-beacon-mode meow-keypad-mode meow-motion-mode))
  (minions-mode 1))

(progn
  (q/ensure-package 'which-key)
  (which-key-mode))

(progn
  (q/ensure-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(progn
  (defun q/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook #'q/colorize-compilation-buffer)
  (setq compilation-scroll-output t))

(progn
  (setq display-line-numbers-type t)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

(progn
  (q/ensure-package 'ef-themes)
  (q/ensure-package 'almost-mono-themes)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-day :no-confirm))

(progn
  (q/ensure-package 'all-the-icons)
  (require 'all-the-icons))

(setq custom-tab-width 4)
(setq tab-width 4)
(setq-default indent-tabs-mode t)
(setq-default electric-indent-inhibit t)

(setq tab-always-indent 'complete)

(setq backward-delete-char-untabify-method 'hungry)

(progn
  (q/ensure-package 'all-the-icons-dired)
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(global-set-key (kbd "C-x !") 'delete-other-windows-vertically)

(progn
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-al --dired --group-directories-first -h")
  (global-set-key (kbd "C-c d") '("dired" . dired))
  (q/ensure-package 'dired-single)
  (require 'dired-single)
  (define-key dired-mode-map (kbd "h") 'dired-single-up-directory)
  (define-key dired-mode-map (kbd "l") 'dired-single-buffer))

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

(progn
  (q/ensure-package 'org)
  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'note)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-indent-indentation-per-level 1)
  (setq org-adapt-indentation nil)
  (setq org-hide-leading-stars nil)
  (setq org-hide-emphasis-markers nil)
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-cycle-separator-lines 1)
  (setq org-startup-with-inline-images t)
  (setq org-directory q/org-directory)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
  (setq org-agenda-files (list org-directory))
  (setq org-refile-targets '(("next.org" :level . 0)
                             ("projects.org" :maxlevel . 1)
                             ("life.org" :level . 0)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

  (setq org-return-follows-link t)

  (setq org-capture-templates
        `(("t" "inbox task" entry (file ,(q/get-org-file "/inbox.org"))
           (file ,(q/get-org-file "/tmpl/task")))
          ("i" "idea" entry (file ,(q/get-org-file "/ideas.org"))
           (file ,(q/get-org-file "/tmpl/idea")))
          ("l" "link" entry (file ,(q/get-org-file "/inbox.org"))
           (file ,(q/get-org-file "/tmpl/link")) :immediate-finish t)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (setq org-confirm-babel-evaluate nil)
  (setq org-src-tab-acts-natively t)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(with-eval-after-load 'org
  (org-crypt-use-before-save-magic))

(progn
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
  (setq org-agenda-custom-commands `,q/org-agenda-todo-view)
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t%-6e% s")
                                   (todo . " %i %-12:c %-6e")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))
  (advice-add 'org-refile :after
              (lambda (&rest _)
                (org-save-all-org-buffers))))

(progn
  (q/ensure-package 'org-drill)
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-learn-fraction 0.25)
  (setq org-drill-hint-separator "||")
  (setq org-drill-left-cloze-delimiter "<[")
  (setq org-drill-right-cloze-delimiter "]>"))

(progn
  (q/ensure-package 'org-download)
  (require 'org-download))
(q/ensure-package 'org-cliplink)

(global-set-key (kbd "C-c o a") '("agenda" . org-agenda))
(global-set-key (kbd "C-c o c") '("capture" . org-capture))
(global-set-key (kbd "C-c o d") '("drill" . org-drill))
(global-set-key (kbd "C-c o p") '("clipboard image" . org-download-clipboard))
(global-set-key (kbd "C-c o s") '("save all org buffers" . org-save-all-org-buffers))
(global-set-key (kbd "C-c o z") '("revert all org buffers" . org-revert-all-org-buffers))

(global-set-key (kbd "C-c o t i") '("clock in" . org-clock-in))
(global-set-key (kbd "C-c o t o") '("clock out" . org-clock-out))
(global-set-key (kbd "C-c o t g") '("clock go to" . org-clock-goto))

(progn
  (setq org-toggl-inherit-toggl-properties t)
  (with-eval-after-load 'org
    (q/ensure-package 'request)
    (require 'org-toggl)
    (org-toggl-integration-mode)))

(progn
  (q/ensure-package 'vertico)
  (setq vertico-cycle t)
  (vertico-mode))

(progn
  (q/ensure-package 'orderless)
  (require 'orderless)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(progn
  (q/ensure-package 'marginalia)
  (marginalia-mode))

(progn
  (q/ensure-package 'embark)
  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act)))
  (global-set-key (kbd "C-.") 'embark-act)
  (global-set-key (kbd "C-,") 'embark-act-noquit)
  (global-set-key (kbd "M-,") 'embark-dwim))

(progn
  (q/ensure-package 'popper)
  (require 'popper)
  (global-set-key (kbd "C-<escape>") 'popper-toggle-latest)
  (global-set-key (kbd "M-<escape>") 'popper-cycle)
  (setq popper-reference-buffers
        '(help-mode
          compilation-mode
          "\\* docker"))
  (setq popper-group-function #'popper-group-by-project)
  (popper-mode +1)
  (popper-echo-mode +1))

(progn
  (q/ensure-package 'project)
  (define-key project-prefix-map "z" (lambda () (interactive) (project-find-file t)))
  (global-set-key (kbd "C-c p") project-prefix-map))

(progn
  (q/ensure-package 'rg)
  (with-eval-after-load 'project
    (define-key project-prefix-map "s" #'rg-project)))

(setq xref-search-program 'ripgrep)

(progn
  (q/ensure-package 'magit)
  (global-set-key (kbd "C-c g") '("magit" . magit-status))

  (defun q/magit-my-branch-spin-off (branch &optional from)
    (interactive (list (magit-read-string-ns "Spin off branch" (format "feature/%s" (car (split-string org-clock-heading))))
                       (car (last (magit-region-values 'commit)))))
    (magit-branch-spinoff branch from))
  (global-set-key (kbd "C-c s") '("spinoff" . q/magit-my-branch-spin-off))

  (with-eval-after-load 'project
    (define-key project-prefix-map "m" #'magit-project-status)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit"))))

(add-hook 'prog-mode-hook #'electric-pair-mode)

(progn
  (q/ensure-package 'docker)
  (defun q/kill-all-exec-buffers ()
    (interactive)
    (kill-matching-buffers "\* docker-compose exec" nil t)))

(progn
  (q/ensure-package 'yasnippet)
  (q/ensure-package 'yasnippet-snippets)
  (yas-global-mode 1))

(q/ensure-package 's)

(defun q/yas-magento-get-namespace-path ()
  (let* ((file-path (file-name-directory (or (buffer-file-name)
                                              (buffer-name (current-buffer)))))
          (namespace (s-replace "/" "\\" (and (string-match ".*/app/code/\\(.*\\)/" file-path)
                                              (match-string 1 file-path)))))
    namespace))

(q/ensure-package 'web-mode)
(progn
  (q/ensure-package 'php-mode)
  (add-hook 'php-mode-hook #'eglot-ensure))

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

(add-hook 'typescript-ts-mode-hook #'eglot-ensure)

(progn
  (q/ensure-package 'pinentry)
  (pinentry-start))

(q/ensure-package 'password-store)
(auth-source-pass-enable)
(global-set-key (kbd "C-c y y") '("copy" . password-store-copy))
(global-set-key (kbd "C-c y p") '("insert" . password-store-insert))
(global-set-key (kbd "C-c y e") '("insert" . password-store-edit))

(global-set-key (kbd "C-c m m") '("run some command" . q/magento))
(global-set-key (kbd "C-c m c") '("clear cache" . q/magento-clear-cache))
(global-set-key (kbd "C-c m s") '("static content deploy" . q/magento-static-content-deploy))
(global-set-key (kbd "C-c m d") '("DI compile" . q/magento-di-compile))
(global-set-key (kbd "C-c m u") '("setup upgrade" . q/magento-setup-upgrade))

(progn
  (q/ensure-package 'consult))

(defun mask-text (start end mask-char)
  "Overwrite the region with the selected mask character."
  (interactive "@*r\ncMasking character: ")
  (let ((region-length (- end start))
        (original-point (point)))
    (delete-region start end)
    (goto-char start)
    (insert-char mask-char region-length)
    (goto-char original-point)))

(progn
  (q/ensure-package 'surround)
  (require 'surround))

(setopt undo-no-redo t)

(progn
  (q/ensure-package 'meow)
  (defmacro q/meow-call-negative (form)
    `(let ((current-prefix-arg -1))
       (call-interactively ,form)))
  (defun q/meow-negative-find ()
    (interactive)
    (q/meow-call-negative 'meow-find))
  (defun q/meow-negative-till ()
    (interactive)
    (q/meow-call-negative 'meow-till))
  (defun q/meow-pop-mark ()
    (interactive)
    (let ((current-prefix-arg 4))
      (call-interactively 'set-mark-command)))

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
      (<BKSL> "\\" "|")))

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-canary)
    (meow-motion-overwrite-define-key
     ;; Canary
     '("e" . meow-prev)
     '("n" . meow-next)
     ;; Qwerty
     ;'("j" . meow-next)
     ;'("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC n/e or j/k will run the original command in MOTION state.

     ;; Canary
     '("e" . "H-e")
     '("n" . "H-n")

     ;; Qwerty
     ;;'("j" . "H-j")
     ;;'("k" . "H-k")

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
     '("?" . meow-cheatsheet)

     '("b" . switch-to-buffer)
     '("j" . dired-jump)
     '("v" . magit)
     '("%" . query-replace)
     '("s" . isearch-forward)

     '("t c" . transpose-chars)
     '("t w" . transpose-words)
     '("t l" . transpose-lines)
     '("t f" . transpose-sexps)

     '("w m" . windmove-left)
     '("w n" . windmove-down)
     '("w e" . windmove-up)
     '("w i" . windmove-right)
     '("w d" . delete-other-windows)
     '("w v" . delete-other-windows-vertically)
     '("w s n" . split-window-below)
     '("w s i" . split-window-right))

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
     '("-" . negative-argument)
     (cons "_" surround-keymap)
     '(";" . meow-reverse)
     '(":" . execute-extended-command)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("(" . meow-beginning-of-thing)
     '(")" . meow-end-of-thing)
     '("C-(" . (lambda () (interactive) (surround-insert "(")))
     '("M-(" . (lambda (new) (interactive (list (char-to-string (read-char "replace ")))) (surround-change "(" new)))
     '("{" . backward-paragraph)
     '("}" . forward-paragraph)
     '("C-{" . (lambda () (interactive) (surround-insert "{")))
     '("M-{" . (lambda (new) (interactive (list (char-to-string (read-char "replace ")))) (surround-change "{" new)))
     '("[" . beginning-of-buffer)
     '("]" . end-of-buffer)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     ;'("C" . ignore)
     '("d" . meow-kill)
     '("D" . xref-find-definitions)
     '("<" . xref-go-back)
     '(">" . xref-go-forward)
     '("e" . meow-prev)
     '("E" . meow-prev-expand)
     '("f" . meow-find)
     '("F" . q/meow-negative-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-search)
     ;'("H" . ignore)
     '("i" . meow-right)
     '("I" . meow-right-expand)
     '("j" . meow-join)
     ;'("J" . ignore)
     '("k" . meow-next-word)
     '("K" . meow-next-symbol)
     '("l" . meow-line)
     '("L" . meow-goto-line)
     '("m" . meow-left)
     '("M" . meow-left-expand)
     '("n" . meow-next)
     '("N" . meow-next-expand)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("P" . meow-clipboard-yank)
     '("q" . meow-quit)
     '("Q" . consult-goto-line)
     '("r" . meow-replace)
     '("C-r" . undo-redo)
     '("R" . meow-swap-grab)
     '("s" . meow-insert)
     '("S" . meow-open-above)
     '("t" . meow-till)
     '("T" . q/meow-negative-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     ;'("V" . ignore)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-delete)
     '("X" . meow-backward-delete)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("Z" . q/meow-pop-mark)
     '("'" . (lambda () (interactive) (surround-insert "'")))
     '("\"" . (lambda () (interactive) (surround-insert "\"")))
     '("C-'" . (lambda (new) (interactive (list (char-to-string (read-char "replace ")))) (surround-change "'" new)))
     '("C-\"" . (lambda (new) (interactive (list (char-to-string (read-char "replace ")))) (surround-change "\"" new)))
     '("=" . meow-indent)
     ;'("+" . ignore)
     '("`" . downcase-dwim)
     '("~" . upcase-dwim)
     '("/" . consult-line)
     '("?" . meow-comment)
     ;'("!" . transpose-lines)
     '("@" . mark-paragraph)
     '("#" . mark-defun)
     ;'("$" . ignore)
     '("%" . exchange-point-and-mark)
     '("^" . meow-back-to-indentation)
     '("&" . repeat)
     '("C-*" . meow-start-kmacro-or-insert-counter)
     '("*" . meow-end-or-call-kmacro)
     '("<escape>" . ignore)))
  (require 'meow)
  (meow-setup)
  (meow-global-mode 1))

(progn
  (q/ensure-package 'eglot)
  (setq eglot-sync-connect nil)
  (setq eglot-events-buffer-size 0)
  (defcustom lsp-intelephense-key ""
    "Enter the intelephense key."
    :type 'string
    :group 'eglot)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((php-mode phps-mode) . ("intelephense" "--stdio" :initializationOptions (:licenseKey ,lsp-intelephense-key))))))
(progn
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((typescript-ts-mode) . ("typescript-language-server" "--stdio")))))

(progn
  (q/ensure-package 'corfu)
  (setq completion-cycle-threshold 3)
  (setq corfu-cycle t)
  ;; (setq corfu-auto nil)
  ;; (setq corfu-auto-delay 0.1)
  ;; (setq corfu-auto-prefix 4)
  (setq corfu-separator ?\s)
  (setq corfu-preview-current nil)
  ;; (setq corfu-preselect-first nil)
  (global-corfu-mode))

(progn
  (q/ensure-package 'cape)
  (add-to-list 'completion-at-point-functions #'cape-file))

(progn
  (q/ensure-package 'ace-window)
  (global-set-key (kbd "M-o") 'ace-window))


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(progn
  (q/ensure-package 'clojure-mode)
  (q/ensure-package 'cider))

(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)

(setq search-whitespace-regexp ".*?")

(load-file (concat user-emacs-directory "modeline.el"))
(require 'modeline)
(q/modeline-subtle-mode 1)

(define-abbrev text-mode-abbrev-table "qqn" "queyenth")
(define-abbrev text-mode-abbrev-table "qqgit" "https://github.com/queyenth")
(add-hook 'text-mode-hook #'abbrev-mode)
(add-hook 'org-mode-hook #'variable-pitch-mode)

