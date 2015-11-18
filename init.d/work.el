;; Misc stuff to make working on work stuff easier

;; Hardcoded path as multiple calls messed up path, fixme T_T
(defun godep-project (project)
  "Set the GOPATH for godep project and move into it"
  (interactive "sProject name? ")
  (defvar project-dir (concat (getenv "GOPATH") "/src/github.com/teamwork/"))
  (defvar orig-gopath (last (split-string (getenv "GOPATH") ":")))
  (setenv "GOPATH" (concat project-dir project "/Godeps/_workspace:" "/Users/jrd/src/go"));;orig-gopath))
  (find-file (concat project-dir project)))
