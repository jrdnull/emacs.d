(use-package go-mode
  :bind (("C-c a" . go-test-current-project)
         ("C-c m" . go-test-current-file)
         ("C-c b" . go-run)
         ("C-h f" . godoc-at-point))
  :init
  (use-package company-go)
  (use-package go-eldoc)
  (use-package gotest)
  (use-package go-impl)
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

(use-package go-dlv)

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
