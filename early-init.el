;; -*- lexical-binding: t -*-
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))
(setq gc-cons-threshold most-positive-fixnum)

(setq read-process-output-max (* 512 1024))
(setq load-prefer-newer t)
(setq package-enable-at-startup nil)

(setq native-comp-jit-compilation t)
(setq package-native-compile t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections." (emacs-init-time) gcs-done)))
