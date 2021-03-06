;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :bind (:map go-mode-map
              ("C-c a" . go-test-current-project)
              ("C-c m" . go-test-current-file)
              ("C-c b" . go-run)
              ("C-h f" . godoc-at-point))
  :init
  (use-package company-go)
  (use-package gotest)
  (use-package go-impl)
  (use-package go-dlv)
  (use-package lsp-mode
    :init
    (use-package lsp-ui
      :init
      (add-hook 'lsp-after-initialize-hook (lambda ()
                                             (flycheck-add-next-checker 'lsp 'golangci-lint)))))
  (use-package flycheck-golangci-lint
    :ensure t
    :hook (go-mode . flycheck-golangci-lint-setup))
  :config
  (add-hook 'go-mode-hook
            '(lambda ()
               (progn
                 (set (make-local-variable 'company-backends) '(company-go))
                 (subword-mode +1))))
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (require 'gotests)
  (use-package dap-mode
    :init
    (require 'dap-go)))


(defun go-open (package)
  "Open a package in your $GOPATH"
  (interactive
   (list
    (completing-read "Package: " (directory-folders-at-depth (concat (getenv "GOPATH") "/src") 3))))
  (find-file package))

(defun directory-folders-at-depth (directory depth)
  "List sub-directories at DEPTH in DIRECTORY"
  (let* ((folder-list '())
         (current-directory-list (directory-files directory t)))
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (if (and
             (file-directory-p f)
             (not (string-equal ".." (substring f -2)))
             (not (string-equal "." (substring f -1))))
            (if (= depth 1)
                (setq folder-list (cons f folder-list))
              (setq folder-list (append folder-list (directory-folders-at-depth f (- depth 1))))))
        (setq current-directory-list (cdr current-directory-list))))
    folder-list))
