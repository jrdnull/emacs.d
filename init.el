;; Majority of config comes from Prelude or credit is given at fun
;; http://batsov.com/prelude/

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode 1)
(global-hl-line-mode +1)
(setq column-number-mode t)
(setq inhibit-startup-screen t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; ripped from github.com/bbatsov/prelude
(setq-default indent-tabs-mode nil) ; don't use tabs to indent
(setq-default tab-width 8)          ; but maintain correct appearance
(setq-default fill-column 80)

(setq require-final-newline t) ; new line EOF
(delete-selection-mode t) ; overwrite selection

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*") ; ignore special buffers

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;;-- bbatsov
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)
    (package-initialize)))
(require 'use-package)
(setq use-package-always-ensure t) ; avoid using :ensure t for everything

(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package super-save
  :config
  (super-save-mode +1))

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
;  (setq sml/theme 'respectful)
  (sml/setup))

(use-package ag)
(use-package helm
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x c o" . helm-occur))
  :init (progn
          (require 'helm-config)
          (helm-mode 1)
          (use-package helm-descbinds
            :bind (("C-h b" . helm-descbinds)
                   ("C-h w" . helm-descbinds)))
          (use-package helm-ag)))

(use-package projectile
  :config
  (projectile-global-mode)
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))

(use-package company
  :config
  (setq company-tooltip-limit 15)
  (setq company-idle-delay .25)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; source: http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

   Move point to the first non-whitespace character on this line.
   If point is already there, move to the beginning of the line.
   Effectively toggle between the first non-whitespace character and
   the beginning of the line.

   If ARG is not nil or 1, move forward ARG - 1 lines first.  If
   point reaches the beginning or end of the buffer, stop."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

(bind-key "C-x C-k" 'kill-this-buffer)

;; source: https://github.com/magnars/.emacs.d/blob/master/defuns/lisp-defuns.el
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(use-package rainbow-mode)

(setq linum-format " %d ")

(set-fringe-mode '(nil . 0))

(setq tramp-default-method "ssh")


;; Cleanup whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Setup custom to its own file
(setq custom-file "~/.emacs.d/init.d/custom.el")

(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Load all lisp files in ~/.emacs.d/init.d/
(dolist (file (directory-files "~/.emacs.d/init.d" t "\.el$"))
  (load file))
