(use-package elixir-mode)
(use-package lsp-mode
  :commands lsp
  :ensure t
  :diminish lsp-mode
  :hook
  (elixir-mode . lsp)
  :init
  (add-to-list 'exec-path "~/src/elixir-ls/release")
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  (use-package exunit
    :init
    (add-hook 'elixir-mode-hook 'exunit-mode)))
