;; Misc stuff to make working on work stuff easier

(defun godep-project (project)
  "Set the GOPATH for godep project and move into it."
  (interactive "sPackage name? ")
  (let ((orig-gopath (car (last (split-string (getenv "GOPATH") ":")))))
    (let ((project-dir (concat orig-gopath "/src/" project)))
      (setenv "GOPATH" (concat project-dir "/Godeps/_workspace:" orig-gopath))
      (find-file (concat orig-gopath "/src/" project)))))

(defun gopath-open (package)
  "Open the project inside the last segment of your $GOPATH.
 $GOPATH is set to clean it up after using a Godep project  and resetting it."
  (interactive "sPackage name? ")
  (setenv "GOPATH" (car (last (split-string (getenv "GOPATH") ":"))))
  (find-file (concat (getenv "GOPATH") "/src/" package)))

;; HTML 4 spaces
(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 4)))
