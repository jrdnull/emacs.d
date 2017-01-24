(use-package god-mode
  :config
  (global-set-key (kbd "s-g") 'god-local-mode)
  (define-key god-local-mode-map (kbd "z") 'repeat)
  (define-key god-local-mode-map (kbd "i") 'god-local-mode)
  (global-set-key (kbd "C-x C-1") 'delete-other-windows)
  (global-set-key (kbd "C-x C-2") 'split-window-below)
  (global-set-key (kbd "C-x C-3") 'split-window-right)
  (global-set-key (kbd "C-x C-0") 'delete-window)
  (global-set-key (kbd "C-x C-o") 'other-window)
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor))

(defun my-update-cursor ()
  (setq cursor-type (if god-local-mode 'hollow 'box)))
