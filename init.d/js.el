(use-package json-mode)
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
  (unbind-key "M-j" js2-mode-map)
  (use-package add-node-modules-path
    :init
    (add-hook 'js2-mode-hook #'add-node-modules-path)))

(use-package company-tern
  :config
  (add-to-list 'company-backends 'company-tern)
  (add-to-list 'tern-command "--no-port-file" 'append)
  :init
  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

(defun eslint-fix-file ()
  "Run eslint ---fix on buffer."
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (add-node-modules-path)
  (with-help-window "*eslint --fix*"
    (let ((path (expand-file-name
                 "node_modules/.bin/"
                 (locate-dominating-file buffer-file-name "node_modules"))))
      (message path)
      (princ (shell-command-to-string
              (concat path "eslint --fix " buffer-file-name))))))

(defun npm-run (script)
  "Run an npm SCRIPT and output to new buffer."
  (interactive "sscript: ")
  (with-help-window (concat "*npm run" script "*")
    (princ (shell-command-to-string (concat "npm run " script)))))

(defun npm-test ()
  (interactive)
  "Run npm test and output to new buffer"
  (npm-run "test"))
