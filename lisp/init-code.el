(require 'init-elpa)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :hook ((php-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)

  (use-package lsp-ivy
    :ensure t
    :commands lsp-ivy-workspace-symbol)

  (use-package which-key
    :ensure t
    :config
    (which-key-mode)))

(provide 'init-code)
