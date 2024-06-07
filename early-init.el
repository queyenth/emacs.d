;; -*- lexical-binding: t -*-
(setq read-process-output-max (* 1024 1024 3))
(add-hook 'emacs-startup-hook
          (let ((old-threshold gc-cons-threshold))
            (lambda ()
              (setq gc-cons-threshold old-threshold))))
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections." (emacs-init-time) gcs-done)))
