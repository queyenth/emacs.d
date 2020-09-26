(require 'init-elpa)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :hook ((php-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (use-package lsp-ui
    :commands lsp-ui-mode)

  (use-package lsp-ivy
    :commands lsp-ivy-workspace-symbol))

(provide 'init-code)
