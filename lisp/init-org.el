(require 'init-elpa)

(use-package org
  :ensure t
  :config
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'note)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(org-agenda-files :level . 1))
  (setq org-refile-use-outline-path 'file)
  (setq org-indent-indentation-per-level 1)
  (setq org-hide-leading-stars 't)
  (setq org-hide-emphasis-markers t)
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-cycle-separator-lines 1)
  (setq org-agenda-files (list (concat (getenv "SYNCTHING") "org"))))

(provide 'init-org)
