;;; assembla-mode.el --- A major mode allowing for interaction with Assembla.
;; Copyright (C) 2012 Dan LaManna

;; Author: Dan LaManna <dan.lamanna@gmail.com>
;; Keywords: assembla mode api

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A beta release of a work in progress, it allows interacting with Assembla,
;; but unfortunately is mostly unusable without utilizing caching in large
;; Assembla accounts. The defaults, with enabling cache are the most optimal
;; for how I currently use Assembla.

;;; Code:

(add-to-list 'load-path "./lib")

(require 'assembla-lib)
(require 'json)

(setq asm/user-table   (make-hash-table :test 'equal))
(setq asm/spaces-table (make-hash-table :test 'equal))

(defgroup assembla-mode nil
  "Interact with Assembla from Emacs."
  :prefix "assembla-mode-"
  :group  'tools)

(defcustom asm/prev-buffer-default 'assembla
  "Function called when `asm/prev-buffer' can't find a function to call."
  :group 'assembla-mode
  :type  'function)

(defcustom asm/uri-cache-durations
    `(("^spaces\\.json$" . ,(* 86400 7)) ; List of spaces
      ("^spaces\/.+\/users\\.json$" . ,(* 86400 7)) ; List of users in a space
      ("^spaces\/.+\/tickets\\.json$" . 3600) ; List of tickets in a space
      ("^spaces\/.+/tickets\/id\/.+\\.json$" . 86400) ; Individual Ticket
      ("^spaces\/.+\/tickets/\.+\/ticket_comments\\.json$" . 900)) ; Ticket Comments
    "Set of URI patterns and their corresponding cache lifetime values.
     Setting a cache value to < 1, will cause it to never be cached.  In the case of
     multiple patterns matching a URI, the smaller value will always be chosen.
     In the case a URI does not have a match, it defaults to `asl/cache-duration-default'.
     Note: This variable is completely ineffective if `asl/cache-enabled' is `nil'."
    :type  'alist
    :group 'assembla-mode)

(defun asm/get-request-cache-duration(uri type)
  "Determines how long a request should be cached based on
   `asm/uri-cache-durations'.  If a matching cache pattern can't be
   found, it defaults to `asl/cache-duration-default'."
  (setq cache-duration nil)
  (dolist (pattern asm/uri-cache-durations)
    (if (and (or (eq cache-duration nil)
		 (< (cdr pattern) cache-duration))
	     (string-match-p (car pattern) (format "%s.%s" uri type)))
	(setq cache-duration (cdr pattern))))
  (or cache-duration
      asl/cache-duration-default))

(defun asm/get(uri type callback)
  "Shorthand for calling `asl/get', while
   using `asm/get-request-cache-duration' for the cache-duration."
  (asl/get uri type callback t (asm/get-request-cache-duration uri type)))

;; mode mappings/definitions/macros
(defvar assembla-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c f") 'asm/goto-thing-at-point)
    (define-key map (kbd "C-c d") 'asm/prev-buffer)
    (define-key map (kbd "C-c c") 'asm/popup-comment)
    (define-key map (kbd "C-c F") 'asm/change-field-at-point)
      map))

(define-derived-mode assembla-mode fundamental-mode "Assembla"
  "A major mode for interacting with Assembla."
  (kill-all-local-variables)
  (use-local-map assembla-mode-map)
  (setq major-mode 'assembla-mode)
  (setq mode-name "Assembla"))

(defmacro with-assembla-buffer(asm-buffer-name heading-str &rest body)
  "Create buffer with name of ASM-BUFFER-NAME, or uses it if exists,
   preps it with readonly/erase/heading - executes `body' - then puts
   readonly back on, goes to beginning of buffer, and switches to it."
  (declare (indent 2)
	   (debug t))
  `(with-current-buffer (get-buffer-create ,asm-buffer-name)
     (toggle-read-only -1)
     (assembla-mode)
     (erase-buffer)
     (setq header-line-format ,heading-str)
     (progn ,@body)
     (goto-char (point-min))
     (toggle-read-only 1)
     (switch-to-buffer ,asm-buffer-name)))

(defun asm/val(field thing)
  (cdr (assoc field thing)))

;; init/bootstrap
(defun assembla()
  (interactive)
  (with-assembla-buffer "*assembla*" "Spaces List"
    (use-local-map assembla-mode-map)
    (asm/get "spaces" "json" 'asm/render-spaces)))

(defun asm/build-hash-tables(spaces-json)
  "Builds a global hash table of users in `asm/user-table', and spaces in
   `asm/spaces-table' by going through all spaces, and calling `asl/get'
   on /spaces/:space_id/users and adding each user to the table.
   Key is the ID, so users/spaces are only stored once."
  (let* ((spaces (json-read-from-string spaces-json))
	 (len    (length spaces)))
    (dotimes (n len)
      (lexical-let ((space-id (cdr (assoc 'id (elt spaces n)))))
	(puthash space-id (elt spaces n) asm/spaces-table)
	(asm/get (format "spaces/%s/users" space-id) "json" 'asm/--build-hash-tables)))))

(defun asm/--build-hash-tables(space-users-json)
  (let* ((space-users (json-read-from-string space-users-json))
	 (len         (length space-users)))
    (dotimes (c len)
      (puthash (cdr (assoc 'id (elt space-users c))) (elt space-users c) asm/user-table))))

(defun asm/get-user(id &optional field)
  "Gets the assembla user with an id of ID from `asm/user-table'.
   If FIELD is set, it will return that specific field from the hash-table.
   In both cases, `nil' will be returned if the user or FIELD doesn't exist."
  (let ((user (gethash id asm/user-table)))
    (when user
      (if field
	  (cdr (assoc field user))
	user))))

(defun asm/get-space(id &optional field)
  "Gets the assembla space with an id of ID from `asm/spaces-table'.
   If FIELD is set, it will return that specific field from the hash-table.
   In both cases, `nil' will be returned if the space of FIELD doesn't exist."
  (let ((space (gethash id asm/spaces-table)))
    (when space
      (if field
	  (cdr (assoc field space))
	space))))

;; doesn't support multiple args to funcall
(defun asm/prev-buffer()
  "Calls whatever function exists in `prev-buffer' text-property at
   `point-min' of current buffer.  Defaults to `asm/prev-buffer-default'.

   Currently works up to one argument if given a list."
  (interactive)
  (let ((prev-buffer-action (get-text-property (point-min) 'prev-buffer)))
    (if prev-buffer-action
	(if (listp prev-buffer-action)
	    (funcall (car prev-buffer-action) (car (cdr prev-buffer-action)))
	  (funcall prev-buffer-action))
      (funcall asm/prev-buffer-default))))

(defun asm/goto-thing-at-point()
  "This calls an action to the `assembla-thing-at-point'.

   space:  `asm/render-tickets-in-space'
   ticket: `asm/render-ticket'"
  (interactive)
  (let ((assembla-thing-at-point (get-text-property (point) 'assembla-thing-at-point)))
    (when assembla-thing-at-point
      (if (string-equal assembla-thing-at-point "space")
	  (let ((space (get-text-property (point) 'space-meta)))
	    (asm/render-tickets-in-space (cdr (assoc 'id space))))
	(if (string-equal assembla-thing-at-point "ticket")
	    (let* ((ticket (get-text-property (point) 'ticket-meta))
		   (space-id (cdr (assoc 'space_id ticket)))
		   (ticket-id (cdr (assoc 'id ticket))))
	      (asm/render-ticket space-id ticket-id)))))))

(defun asm/render-spaces(response)
  (let* ((spaces (json-read-from-string response))
	 (len    (length spaces)))
    (dotimes (n len)
      (asm/render-space (elt spaces n)))))

(defun asm/render-space(space)
  (let* ((name        (cdr (assoc 'name space)))
	 (start-point (point))
	 (str-length  (length name)))
    (insert (format "%s" name))
    (put-text-property start-point (+ start-point str-length) 'space-meta space)
    (put-text-property start-point (+ start-point str-length) 'assembla-thing-at-point "space")
    (newline)))

;;refactor
(defun asm/render-tickets-in-space(&optional space-id)
  (interactive)
  (if (not space-id) ;; assume space at point
      (let ((space (get-text-property (point) 'space-meta)))
	(if (not space)
	    (message "No space at point.")
	  (with-assembla-buffer "*assembla*" (format "Tickets in %s" (cdr (assoc 'name space)))
	    (asm/get (format "spaces/%s/tickets" (cdr (assoc 'id space))) "json" 'asm/render-tickets-list)
	    (put-text-property (point-min) (point-max) 'prev-buffer 'assembla))))
    (asm/get (format "spaces/%s/tickets" space-id) "json" 'asm/render-tickets-list)))

(defun asm/render-tickets-list(json-str)
    (let* ((tickets  (json-read-from-string json-str))
	   (len      (length tickets))
	   (space-id (cdr (assoc 'space_id (cdr (elt tickets 0))))))
      (with-assembla-buffer "*assembla*" (format "-- %s (Tickets List)" (asm/get-space space-id 'name))
	(dotimes (n len)
	  (asm/render-ticket-line (elt tickets n)))))
    (put-text-property (point-min) (point-max) 'prev-buffer 'assembla))

;; excerpt summary length @todo
(defun asm/render-ticket-line(ticket)
  (let* ((summary (cdr (assoc 'summary ticket)))
	 (start-point (point))
	 (str-length (length summary)))
   (insert (format "#%-3d %-10s %s" (cdr (assoc 'number ticket)) (format "[%s]" (cdr (assoc 'status ticket))) summary))
    (put-text-property start-point (+ start-point str-length) 'ticket-meta ticket)
    (put-text-property start-point (+ start-point str-length) 'assembla-thing-at-point "ticket")
    (newline)))

;;refactor
(defun asm/render-ticket(&optional space-id &optional ticket-id)
  (interactive)
  (if (and (not space-id)
	   (not ticket-id))
      ;; use point
      (let* ((ticket    (get-text-property (point) 'ticket-meta))
	     (space-id  (cdr (assoc 'space_id ticket)))
	     (ticket-id (cdr (assoc 'id ticket))))
	(with-assembla-buffer "*assembla*" (format "[#%d] %s (Ticket View)" (cdr (assoc 'number ticket)) (cdr (assoc 'name ticket)))
	  (asm/get (format "spaces/%s/tickets/id/%s" space-id ticket-id) "json" 'asm/render-ticket-view)
	  (put-text-property (point-min) (point-max) 'ticket-meta ticket)
	  (put-text-property (point-min) (point-max) 'prev-buffer `(asm/render-tickets-in-space ,space-id))))
    ;; use space-id/ticket-id
    (with-assembla-buffer "*assembla*" "Ticket"
      (asm/get (format "spaces/%s/tickets/id/%s" space-id ticket-id) "json" 'asm/render-ticket-view))))

(defun asm/log(message)
  (let ((buf (current-buffer)))
    (save-excursion
      (switch-to-buffer (get-buffer-create "*assembla-log*"))
      (insert (format "%s" message))
      (newline)
      (insert "----------------------------------------------------")
      (newline))
    (switch-to-buffer buf)))


(defun asm/ins-w-prop(text prop-name prop-val)
  "Inserts TEXT and places a text-property of PROP-NAME with PROP-VAL
   as it's value from the beginning to end of TEXT."
  (let* ((init-point    (point))
	 (end-point     (+ (length text) init-point)))
    (insert text)
    (put-text-property init-point end-point prop-name prop-val)))

(defun asm/insert-nl(str)
  (insert str)
  (newline))

(defun asm/render-ticket-view(json-str)
  (let* ((ticket (json-read-from-string json-str))
	 (heading (format "[#%d] %s (Ticket View)" (asm/val 'number ticket) (asm/val 'summary ticket)))
	 (desc   (if (eq "" (cdr (assoc 'description ticket))) "(no description)" (cdr (assoc 'description ticket))))
	 (status-string (format "Status:      %s\n" (asm/val 'status ticket)))
	 (priority-str  (format "Priority:    %s" (asm/ticket-priority-label (asm/val 'priority ticket))))
	 (assigned-str  (format "Assigned To: %s\n" (asm/get-user (asm/val 'assigned_to_id ticket) 'name)))
	 (due-date-str  (format "Due Date:    %s" (or (asm/val 'Due-Date (asm/val 'custom_fields ticket)) "-"))))
    (with-assembla-buffer "*assembla*" heading
      (asm/ins-w-prop status-string 'asm/field-at-point "status")
      (asm/insert-nl priority-str)
      (asm/ins-w-prop assigned-str 'asm/field-at-point "assigned-to")
      (asm/insert-nl due-date-str)
      (newline)
      (asm/insert-nl (format "%s" desc))
      (asm/insert-nl (format "---------------------------------"))
      (asm/get (format "spaces/%s/tickets/%d/ticket_comments" (cdr (assoc 'space_id ticket)) (cdr (assoc 'number ticket))) "json" 'asm/render-ticket-comments)
      (newline)
      (put-text-property (point-min) (point-max) 'ticket-meta ticket)
      (put-text-property (point-min) (point-max) 'prev-buffer `(asm/render-tickets-in-space ,(cdr (assoc 'space_id ticket)))))))

(defun asm/render-ticket-comments(json-str)
  "@todo: comments come in ordered by created_at desc, the opposite
   of how they're displayed on their site, and how we want to display them.
   So we loop through and select the comment indices in reverse, we should
   probably just have the sequence reversed."
  (with-current-buffer "*assembla*"
    (goto-char (point-max))
    (insert "%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COMMENTS ")
    (insert (make-string (- (window-width) (current-column) 1) ?%))
    (newline)
    (let* ((comments (json-read-from-string json-str))
	   (len    (length comments)))
      (dotimes (n len)
	(asm/render-comment (elt comments (- len (+ n 1))))))))

(defun asm/render-comment(comment)
  (unless (or (eq (cdr (assoc 'comment comment)) "")
	      (eq (cdr (assoc 'comment comment)) nil))
    (insert (format "%s" (asm/get-user (cdr (assoc 'user_id comment)) 'name)))
    (insert (format " (%s)" (cdr (assoc 'created_on comment))))
    (newline)
    (insert (format "%s" (cdr (assoc 'comment comment))))
    (newline)
    (insert "-----------------------------\n")))

(defun asm/ticket-priority-label(priority-int)
  (cdr (assoc priority-int '((1 . "Highest")
			     (2 . "High")
			     (3 . "Normal")
			     (4 . "Low")
			     (5 . "Lowest")))))

(defvar asm/comment-buffer-name "*assembla-comment*"
  "Buffer name for composing comments.")

(defvar asm/comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'asm/comment-submit) map))

(define-derived-mode asm/comment-mode text-mode "Assembla Comment Mode"
  (use-local-map asm/comment-mode-map))

(defun asm/change-field-at-point()
  "Finds the `asm/field-at-point' if there is one. If there is
   it calls the appropriate function to change the field.

   status:      `asm/ido-status'
   assigned-to: `asm/ido-assign-to'."
  (interactive)
  (let ((field-at-point (get-text-property (point) 'asm/field-at-point)))
    (when field-at-point
      (if (string-equal field-at-point "status")
	  (asm/ido-status)
	(if (string-equal field-at-point "assigned-to")
	    (asm/ido-assign-to))))))

(defun asm/ido-status(&optional default)
  (interactive)
  (let ((statuses '("Test" "Fixed" "New" "Accepted" "Invalid")))
    (ido-completing-read "Status: " statuses)))

(defun asm/ido-assign-to()
  "Presents an `ido' prompt with all users the ticket in *assembla* can
   be assigned to. Defaults to whoever created the ticket."
  (interactive)
  (let* ((ticket      (get-text-property (point-min) 'ticket-meta (get-buffer "*assembla*")))
	 (users-alist (gnus-hashtable-to-alist asm/user-table))
	 (names-list  (mapcar 'asm/--name-from-user users-alist))
	 (reporter-id (cdr (assoc 'reporter_id ticket)))
	 (default-val (asm/get-user reporter-id 'name)))
    (ido-completing-read "Assign To: " names-list nil nil nil nil default-val)))

(defun asm/--name-from-user(user)
  (cdr (assoc 'name user)))




(defun asm/popup-comment()
  "Creates a buffer named `asm/comment-buffer-name' and pops to it.
   No buffer will be created if the current *assembla* buffer doesn't
   have `ticket-meta'."
  (interactive)
  (let ((ticket (get-text-property (point-min) 'ticket-meta)))
    (when ticket
      (let ((buf (get-buffer-create asm/comment-buffer-name)))
	(pop-to-buffer buf)
	(asm/comment-mode)))))

(defun asm/comment-submit()
  "POSTs a comment to the ticket in *assembla* buffer."
  (interactive)
  (lexical-let* ((ticket    (get-text-property (point-min) 'ticket-meta (get-buffer "*assembla*")))
		 (space-id  (cdr (assoc 'space_id ticket)))
		 (number    (cdr (assoc 'number ticket)))
		 (comment   (buffer-string))
		 (post-list (json-encode-list `((ticket_comment . ((comment . ,comment)
								   (status  . ,(asm/ido-status)))))))
		 (uri       (format "spaces/%s/tickets/%d/ticket_comments" space-id number)))
    (insert post-list)
    (asl/post-or-put uri "json" post-list "POST" (lambda(response)
							(asl/invalidate-uri-cache uri "json")
							(kill-buffer (get-buffer asm/comment-buffer-name))
							(switch-to-buffer "*assembla*")
							(asm/render-ticket space-id (cdr (assoc 'id ticket)))))))

(asm/get "spaces" "json" 'asm/build-hash-tables)

(provide 'assembla-mode)

;;; assembla-mode.el ends here
