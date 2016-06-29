;; Changes for OS X
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq mac-emulate-three-button-mouse t)
  (global-set-key (kbd "M-F") 'toggle-frame-fullscreen))
