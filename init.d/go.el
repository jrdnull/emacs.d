(use-package go-mode
  :bind (:map go-mode-map
              ("C-c a" . go-test-current-project)
              ("C-c m" . go-test-current-file)
              ("C-c b" . go-run)
              ("C-h f" . godoc-at-point))
  :init
  (use-package company-go)
  (use-package go-eldoc)
  (use-package gotest)
  (use-package go-impl)
  (use-package go-dlv)
  (use-package go-rename)
  (use-package go-guru)
  :config
  (add-hook 'go-mode-hook
            '(lambda ()
               (progn
                 (let ((goimports (executable-find "goimports")))
                   (when goimports (setq gofmt-command goimports)))
                 (add-hook 'before-save-hook 'gofmt-before-save nil t)
                 (set (make-local-variable 'company-backends) '(company-go))
                 (subword-mode +1)
                 (go-eldoc-setup))))
  (require 'gotests))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

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
