(use-package go-mode
  :bind (("C-c a" . go-test-current-project)
         ("C-c m" . go-test-current-file)
         ("C-c b" . go-run)
         ("C-h f" . godoc-at-point))
  :init
  (use-package company-go)
  (use-package go-eldoc)
  (use-package gotest)
  (let ((gorename-file-path
         (concat (getenv "GOPATH") "/src/golang.org/x/tools/refactor/rename/go-rename.el")))
    (if (file-exists-p gorename-file-path)
        (progn
          (load-file gorename-file-path)
          (require 'go-rename))))
  (let ((golint-file-path
         (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs/golint.el")))
    (if (file-exists-p golint-file-path)
        (progn
          (load-file golint-file-path)
          (require 'golint))))
  (let ((go-oracle-file-path
         (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/oracle/oracle.el")))
    (if (file-exists-p go-oracle-file-path)
        (progn
          (load-file go-oracle-file-path)
          (require 'go-oracle))))
  :config
  (add-hook 'go-mode-hook
            '(lambda ()
               (progn
                 ;; Prefer goimports to gofmt if installed
                 (let ((goimports (executable-find "goimports")))
                   (when goimports (setq gofmt-command goimports)))
                 (add-hook 'before-save-hook 'gofmt-before-save nil t)
                 (set (make-local-variable 'company-backends) '(company-go))
                 (subword-mode +1)
                 (go-eldoc-setup)))))
