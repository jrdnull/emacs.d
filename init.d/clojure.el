(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (subword-mode +1)
)

;; (use-package cider)
