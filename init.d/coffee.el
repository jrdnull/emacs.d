(use-package coffee-mode
  :config
  (unbind-key "C-c C->" coffee-mode-map)
  (unbind-key "C-c C-<" coffee-mode-map)
  (add-to-list 'coffee-args-compile "--no-header")
  (subword-mode +1)
  (setq coffee-tab-width 4))
