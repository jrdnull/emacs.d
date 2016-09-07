(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

(use-package magit-gh-pulls
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))
