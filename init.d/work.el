;; Misc stuff to make working on work stuff easier
;; HTML 4 spaces
(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 4)))

(defun tw-reload-desk ()
  "Triggers rebuild on next request of Teamwork Desk"
  (interactive)
  (url-retrieve "http://sunbeam.teamwork.dev:9999" (lambda (status) (message "OK"))))
