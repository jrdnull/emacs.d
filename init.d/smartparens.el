(use-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (define-key smartparens-mode-map (kbd "M-j") nil))
