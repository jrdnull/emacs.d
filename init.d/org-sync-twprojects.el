;;; org-sync-twprojects.el --- Teamwork Projects backend for org-sync.
;;
;; Copyright (C) 2016 Jordon Smith
;;
;; Author: Jordon Smith <jrd@mockra.net.com>
;; Keywords: org, teamwork projects, synchronization
;; Homepage: https://github.com/arbox/org-sync
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; This file is not part of GNU Emacs.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This package implements a backend for org-sync to synchnonize
;; issues from a Teamwork Projects task list with an org-mode buffer.
;; Read org-sync documentation for more information about it.
;;
;;; Code:
(require 'org-sync)
(require 'cl-lib)
(require 'request)
(require 'json)
(require 's)

(defvar org-sync-twprojects-backend-regexp
  "^\\(https?://\\)?\\([^\.]+?\\)\.teamwork\.com/#?tasklists/\\([0-9]+\\)"
  "Regular expression used to match backend.")

(defvar org-sync-twprojects-backend
  '((base-url      . org-sync-twprojects-base-url)
    (fetch-buglist . org-sync-twprojects-fetch-buglist)
    (send-buglist  . org-sync-twprojects-send-buglist))
  "Teamwork Projects backend.")

(add-to-list 'org-sync-backend-alist
             (cons org-sync-twprojects-backend-regexp 'org-sync-twprojects-backend))

(defvar org-sync-twprojects-auth nil
  "Teamwork Projects API Key.")

(defun org-sync-twprojects-base-url (url)
  "Return base URL from task list URL."
  (save-match-data
    (and (string-match org-sync-twprojects-backend-regexp url)
         (let ((installation (match-string 2 url))
               (task-list (match-string 3 url)))
           (concat "https://" installation ".teamwork.com/tasklists/" task-list)))))

(defun org-sync-twprojects-fetch-buglist (last-update)
  "Fetch buglist (tasks) from Teamwork Projects (anything that happened after LAST-UPDATE)."
  (let* ((response (request
                    (concat org-sync-base-url
                            "/tasks.json?filter=all"
                            (if (stringp last-update)
                                (concat "&updatedAfterDate=" last-update) ""))
                    :headers (org-sync-twprojects-req-headers)
                    :parser 'json-read
                    :sync t))
         (data (request-response-data response))
         (tasks (cdr (assoc 'todo-items data)))
         (bugs (mapcar 'org-sync-twprojects-task-to-bug tasks))
         (list-id (car (last (split-string org-sync-base-url "/"))))
         (title (if (> (length tasks) 0)
                    (concat (cdr (assoc 'todo-list-name (elt tasks 0))) " - #" list-id)
                  (concat "Task List #" list-id))))
    (message "Fetching task list")
    `(:title ,title
             :url ,org-sync-base-url
             :bugs ,bugs)))

(defun org-sync-twprojects-send-buglist (buglist)
  "Send BUGLIST (tasks) to Teamwork Projects and return updated buglist."
  (let ((new-bugs) ; todo don't bother pushing not using result
        (start-time (format-time-string "%Y%m%d%H%M00" nil t)))
    (dolist (b (org-sync-get-prop :bugs buglist))
      (cond
       ((null (org-sync-get-prop :id b)) ; new bug (no id)
        (let* ((status-code (request-response-status-code
                             (request
                              (concat org-sync-base-url "/tasks.json")
                              :type "POST"
                              :headers (org-sync-twprojects-req-headers)
                              :sync t
                              :data (org-sync-twprojects-bug-to-json b)))))
          (when (/= status-code 201)
            (error "Failed creating bug \"%s\"" (org-sync-get-prop :title b)))
          (push b new-bugs)))

       (t ; else, modified bug
        (let ((base-url
               (concat (car (split-string org-sync-base-url "tasklists/")) "tasks/")))
          ;; open first as we can't modify closed task
          (when (equal (org-sync-get-prop :status b) 'open)
            (request
             (concat base-url (number-to-string (org-sync-get-prop :id b)) "/uncomplete.json")
             :type "PUT"
             :headers (org-sync-twprojects-req-headers)
             :sync t))

          ;; update bug
          (let* ((status-code
                  (request-response-status-code
                   (request
                    (concat base-url (number-to-string (org-sync-get-prop :id b)) ".json")
                    :type "PUT"
                    :headers (org-sync-twprojects-req-headers)
                    :sync t
                    :data (org-sync-twprojects-bug-to-json b)))))
            (when (/= status-code 200)
              (error "Failed editing bug \"%s\" (%s)" (org-sync-get-prop :title b) status-code))

            ;; close it after edits
            (when (equal (org-sync-get-prop :status b) 'closed)
              (request
               (concat base-url (number-to-string (org-sync-get-prop :id b)) "/complete.json")
               :type "PUT"
               :headers (org-sync-twprojects-req-headers)
               :sync t))

            (push b new-bugs))))))
    (if (null new-bugs) nil
      (progn
        (message "Modified bugs, refetching since %s" start-time)
        (org-sync-twprojects-fetch-buglist start-time)))))

(defun org-sync-twprojects-bug-to-json (bug)
  "Return BUG as JSON."
  (let ((desc (org-sync-get-prop :desc bug)))
      (json-encode
       `((todo-item . ((content . ,(s-trim (org-sync-get-prop :title bug)))
                       (description . ,(if (null desc) "" desc))))))))

(defun org-sync-twprojects-req-headers ()
  "Return the required headers to make an API request."
  (let ((encoded (base64-encode-string (concat org-sync-twprojects-auth ":x"))))
    `(("Authorization" . ,(concat "Basic " encoded))
      ("Content-Type" . "application/json"))))

(defun org-sync-twprojects-task-to-bug (task)
  "Return TASK as a bug."
  (cl-flet* ((va (key alist) (cdr (assoc key alist)))
             (v (key) (va key task)))
    (let* ((id (v 'id))
           (author (concat (v 'creator-firstname) " " (v 'creator-lastname)))
           (status (if (string= (v 'status) "completed") 'closed 'open))
           (priority (if (string= (v 'priority) "") "none" (v 'priority)))
           (title (v 'content))
           ;; added \n to prevent false positives on modification
           (desc (if (string= (v 'description) "") nil (concat  (v 'description) "\n")))
           (ctime (org-sync-parse-date (v 'created-on)))
           (mtime (org-sync-parse-date (v 'last-changed-on)))
           (url (concat (car (split-string org-sync-base-url "tasklists/")) "#tasks/"
                        (number-to-string id))))
      `(:id ,id
            :author ,author
            :title ,title
            :desc ,desc
            :status ,status
            :priority ,priority
            :date-creation ,ctime
            :date-modification ,mtime
            :url ,url))))

(provide 'org-sync-twprojects)
;;; org-sync-twprojects.el ends here
