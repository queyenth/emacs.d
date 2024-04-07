;; -*- lexical-binding: t -*-
(defun q/modeline-set-faces (_theme)
  "Make THEME mode lines subtle."
  (let ((subtle (face-foreground 'shadow)))
    (custom-set-faces
     `(mode-line ((t :background unspecified :box unspecified :overline ,subtle)))
     `(mode-line-inactive ((t :background unspecified :foreground ,subtle :box unspecified :overline ,subtle))))))

(defun q/modeline-unset-faces ()
  (custom-set-faces
   '(mode-line (( )))
   '(mode-line-inactive (( )))))

(face-attribute 'mode-line :background)

(defun q/modeline--enable-mode ()
  (q/modeline-set-faces nil)
  (add-hook 'enable-theme-functions #'q/modeline-set-faces))

(defun q/modeline--disable-mode ()
  (q/modeline-unset-faces)
  (remove-hook 'enable-theme-functions #'q/modeline-set-faces))

(define-minor-mode q/modeline-subtle-mode
  "Subtle mode."
  :global t
  (if q/modeline-subtle-mode
      (q/modeline--enable-mode)
    (q/modeline--disable-mode)))

(defgroup q/modeline nil
  "Custom modeline."
  :group 'mode-line)

(defgroup q/modeline-faces nil
  "Custom modeline faces."
  :group 'q/modeline)

(defface q/modeline-indicator-blue-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for blue bg indicator."
  :group 'q/modeline-faces)

(defface q/modeline-indicator-cyan-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for cyan bg indicator."
  :group 'q/modeline-faces)

(defface q/modeline-indicator-beige-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#f2e9e1" :foreground "#575279")
    (((class color) (min-colors 88) (background dark))
     :background "#f2e9e1" :foreground "#575279")
    (t :background "beige" :foreground "white"))
  "Face for beige bg indicator."
  :group 'q/modeline-faces)

(defface q/modeline-indicator-purple-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#907aa9" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#907aa9" :foreground "white")
    (t :background "purple" :foreground "white"))
  "Face for purple bg indicator."
  :group 'q/modeline-faces)

(defface q/modeline-indicator-rose-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#d7827e" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#d7827e" :foreground "white")
    (t :background "rose" :foreground "white"))
  "Face for rose bg indicator."
  :group 'q/modeline-faces)

(defface q/modeline-indicator-pine-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#286983" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#286983" :foreground "white")
    (t :background "blue" :foreground "black"))
  "Face for pine bg indicator."
  :group 'q/modeline-faces)

(defvar-local q/mode-line-meow-state
    '(:eval
      (when (mode-line-window-selected-p)
          (cond (meow-normal-mode (propertize " NORMAL " 'face 'q/modeline-indicator-beige-bg))
                (meow-insert-mode (propertize " INSERT " 'face 'q/modeline-indicator-pine-bg))
                (meow-keypad-mode (propertize " KEYPAD " 'face 'q/modeline-indicator-purple-bg))
                (meow-motion-mode (propertize " MOTION " 'face 'q/modeline-indicator-rose-bg))
                (meow-beacon-mode (propertize " BEACON " 'face 'q/modeline-indicator-rose-bg))))))

(defvar-local q/mode-line-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'q/modeline-indicator-blue-bg))))

(defvar-local q/mode-line-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'q/modeline-indicator-cyan-bg))))

(defun q/mode-line-string-truncate-p (str)
  (and (< (window-total-width) split-width-threshold)
       (not (mode-line-window-selected-p))
       (> (length str) 10)
       (not (one-window-p :no-minibuffer))))

(defun q/mode-line-string-truncate (str &optional length)
  (if (q/mode-line-string-truncate-p str)
      (concat (substring str 0 (or length 10)) "...")
    str))

(defun q/mode-line--get-folder-path-short (length)
  (if (buffer-file-name)
      (let ((dirs (string-split default-directory "/")))
	(string-join (mapcar (lambda (x) (if length (string-limit x length) x)) (last dirs (or q/mode-line-folder-count (length dirs)))) "/"))))

(defun q/mode-line--buffer-name (length)
  (when-let ((name (buffer-name)))
    (if (mode-line-window-selected-p)
        (concat (q/mode-line--get-folder-path-short length) name)
      (q/mode-line-string-truncate name))))

(defcustom q/mode-line-folder-cut-each nil
  "Cut N characters from each folder in path.")

(defcustom q/mode-line-folder-count nil
  "How many folders to show in path.")

(defun q/mode-line-set-folder-cut-each (length)
  (interactive (list (read-number "Length: " q/mode-line-folder-cut-each)))
  (setq q/mode-line-folder-cut-each (if (and length (>= length 0)) length nil)))

(defun q/mode-line-set-folder-count (length)
  (interactive (list (read-number "Count: " q/mode-line-folder-count)))
  (setq q/mode-line-folder-count (if (and length (>= length 1)) length nil)))

(defun q/mode-line-buffer-name ()
  (let ((name (q/mode-line--buffer-name q/mode-line-folder-cut-each)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun q/mode-line-buffer-identification-face ()
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun q/mode-line-buffer-help-echo ()
  (concat
   (or (buffer-file-name)
       (format "No underlying file.\n Directory is %s" default-directory))))

(defvar-local q/mode-line-buffer-identification
    '(:eval
      (propertize (q/mode-line-buffer-name)
                  'face (q/mode-line-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (q/mode-line-buffer-help-echo))))

(defface q/modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for red indicator."
  :group 'q/modeline-faces)

(defun q/mode-line-major-mode-indicator ()
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    ((derived-mode-p 'magit-mode) (char-to-string #xE0A0))
                    (t "◦"))))
    (propertize indicator 'face 'shadow)))

(defun q/mode-line-major-mode-name ()
  (thread-last (symbol-name major-mode)
	       (string-replace "-mode" "")
	       (string-replace "-" " ")
	       (downcase)))

(defun q/mode-line-major-mode-help-echo ()
  (if-let ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local q/mode-line-major-mode
    (list
     (propertize "%[" 'face 'q/modeline-indicator-red)
     '(:eval
       (concat
        (q/mode-line-major-mode-indicator)
        " "
        (propertize
         (q/mode-line-string-truncate
          (q/mode-line-major-mode-name))
         'mouse-face 'mode-line-highlight
         'help-echo (q/mode-line-major-mode-help-echo))))
     (propertize "%]" 'face 'q/modeline-indicator-red)))

(declare-function vc-git--symbolic-ref "vc-git" (file))
(declare-function vc-git-working-revision "vc-git" (file))

(defun q/mode-line--vc-branch-name (file backend)
  (when-let ((rev (vc-working-revision file backend))
             (branch (or (vc-git--symbolic-ref file)
                         (substring rev 0 7))))
    branch))

(defvar q/modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state)))

(defun q/modeline--vc-get-face (state)
  (alist-get state q/modeline--vc-faces 'up-to-date))

(defun q/modeline--vc-face (file backend)
  (q/modeline--vc-get-face (vc-state file backend)))

(defun q/mode-line--vc-help-echo (file)
  (format "Revision: %s" (vc-working-revision file)))

(defun q/mode-line--vc-text (file brach &optional face)
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (q/mode-line--vc-help-echo file))))

(defun q/mode-line--vc-details (file branch &optional face)
  (q/mode-line-string-truncate
   (q/mode-line--vc-text file branch face)))

(defvar-local q/mode-line-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  (branch (q/mode-line--vc-branch-name file backend))
                  (face (q/modeline--vc-face file backend)))
        (q/mode-line--vc-details file branch face))))

(defun q/mode-line-flymake-counter (type)
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostics-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defmacro q/mode-line-flymake-type (type indicator &optional face)
  `(defun ,(intern (format "q/mode-line-flymake-%s" type)) ()
     (when-let ((count (q/mode-line-flymake-counter ,(intern (format ":%s" type)))))
       (concat
        (propertize ,indicator 'face 'shadow)
        (propertize count
                    'face ',(or face type)
                    'mouse-face 'mode-line-highlight)))))

(q/mode-line-flymake-type error "☣")
(q/mode-line-flymake-type warning "!")
(q/mode-line-flymake-type note "·" success)

(defvar-local q/mode-line-flymake
    `(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         '(:eval (q/mode-line-flymake-error))
         '(:eval (q/mode-line-flymake-warning))
         '(:eval (q/mode-line-flymake-note))))))

(defvar-local q/mode-line-org-clock-task
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (org-clock-is-active))
        (concat
         (propertize (char-to-string #xF43A) 'face 'shadow)
         "  "
         (q/mode-line-string-truncate org-clock-heading 15)))))

(defvar-local q/mode-line-line-number
    '(:eval
      (when (mode-line-window-selected-p)
        " %l:%C")))

(dolist (construct '(
                     q/mode-line-meow-state
                     q/mode-line-kbd-macro
                     q/mode-line-narrow
                     q/mode-line-buffer-identification
                     q/mode-line-line-number
                     q/mode-line-major-mode
                     q/mode-line-vc-branch
                     q/mode-line-flymake
                     q/mode-line-org-clock-task
                     ))
  (put construct 'risky-local-variable t))

(setq mode-line-compact nil)
(setq-default mode-line-format
              '("%e"
                q/mode-line-meow-state
                q/mode-line-kbd-macro
                q/mode-line-narrow
                " "
                q/mode-line-buffer-identification
                q/mode-line-line-number
                " "
                q/mode-line-major-mode
                " "
                q/mode-line-org-clock-task
                " "
                q/mode-line-vc-branch
                " "
                q/mode-line-flymake
                ))
(provide 'modeline)
