(require 'init-elpa)

(use-package lsp-mode
             :ensure t
             :hook(
                   (php-mode . lsp))
             :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(provide 'init-code)
