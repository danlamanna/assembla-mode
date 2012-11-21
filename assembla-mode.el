(add-to-list 'load-path "./assembla-lib")

(require 'assembla-lib)
(require 'json)

(setq asm/user-table (make-hash-table :test 'equal))

(defvar assembla-cache-patterns
  '(("^spaces"           . 86400) ; spaces rarely change, cache for a day
    ("spaces.*ticket"    . 3600) ; tickets change sometimes, cache for an hour
    ("*ticket_comments*" . 0)) ; comments frequently change, never cache
  "Stores the set of URI patterns and their corresponding cache
   expiration values. Setting a cache value to < 1, will cause it
   to never be cached. In the case of multiple patterns matching
   a URI, the smaller value will always be chosen.")

(defvar assembla-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'assembla-goto-thing-at-point)
    (define-key map (kbd "d") 'assembla-prev-buffer)
      map))

(defmacro with-assembla-buffer(asm-buffer-name heading-str &rest body)
  "Create buffer with name of ASM-BUFFER-NAME, or uses it if exists,
   preps it with readonly/erase/heading - executes `body' - then puts
   readonly back on, goes to beginning of buffer, and switches to it."
  (declare (indent 2))
  `(with-current-buffer (get-buffer-create ,asm-buffer-name)
     (assembla-mode)
     (erase-buffer)
     (setq header-line-format ,heading-str)
     (progn ,@body)
     (goto-char (point-min))
     (switch-to-buffer ,asm-buffer-name)))

(define-derived-mode assembla-mode fundamental-mode "Assembla"
  "A major mode for interacting with Assembla."
  (kill-all-local-variables)
  (use-local-map assembla-mode-map)
  (setq major-mode 'assembla-mode)
  (setq mode-name "Assembla"))

(defun assembla()
  (interactive)
  (with-assembla-buffer "*assembla*" "Assembla Spaces"
    (use-local-map assembla-mode-map)
    (assembla-get "spaces" "json" 'render-assembla-spaces t 86400)))

(defun asm/build-user-table(spaces-json)
  "Builds a global hash table of users in `asm/user-table' by going
   through all spaces, and calling `assembla-get' on /spaces/:space_id/users
   and adding each user to the table. Key is the user ID, so users are only
   stored once."
  (let* ((spaces (json-read-from-string spaces-json))
	 (len    (length spaces)))
    (dotimes (n len)
      (lexical-let ((space-id (cdr (assoc 'id (elt spaces n)))))
	(assembla-get (format "spaces/%s/users" space-id) "json" (lambda(space-users-json)
								   (lexical-let* ((space-users (json-read-from-string space-users-json))
										  (su-len      (length space-users)))
								     (dotimes (c su-len)
								       (puthash (cdr (assoc 'id (elt space-users c))) (elt space-users c) asm/user-table)))) t 86400)))))

(defun asm/get-user(id &optional field)
  "Gets the assembla user with an id of ID from `asm/user-table'.
   If FIELD is set, it will return that specific field from the hash-table.
   In both cases, `nil' will be returned if the user or FIELD doesn't exist."
  (let ((user (gethash id asm/user-table)))
    (when user
      (if field
	  (cdr (assoc field user))
	user))))

;; doesn't support multiple args to funcall
(defun assembla-prev-buffer()
  "Calls whatever function exists in `prev-buffer' text-property at
   `point-min' of current buffer. Defaults to `assembla'.

   Currently works up to one argument if given a list."
  (interactive)
  (let ((prev-buffer-action (get-text-property (point-min) 'prev-buffer)))
    (message (format "%s" prev-buffer-action))
    (if prev-buffer-action
	(if (listp prev-buffer-action)
	    (funcall (car prev-buffer-action) (car (cdr prev-buffer-action)))
	  (funcall prev-buffer-action))
      (assembla))))

(defun assembla-goto-thing-at-point()
  "This calls an action to the `assembla-thing-at-point'.

   space:  `assembla-render-tickets-in-space'
   ticket: `assembla-render-ticket'"
  (interactive)
  (let ((assembla-thing-at-point (get-text-property (point) 'assembla-thing-at-point)))
    (when assembla-thing-at-point
      (if (string-equal assembla-thing-at-point "space")
	  (let ((space (get-text-property (point) 'space-meta)))
	    (assembla-render-tickets-in-space (cdr (assoc 'id space))))
	(if (string-equal assembla-thing-at-point "ticket")
	    (let* ((ticket (get-text-property (point) 'ticket-meta))
		   (space-id (cdr (assoc 'space_id ticket)))
		   (ticket-id (cdr (assoc 'id ticket))))
	      (assembla-render-ticket space-id ticket-id)))))))

(defun render-assembla-spaces(response)
  (let* ((spaces (json-read-from-string response))
	 (len    (length spaces)))
    (dotimes (n len)
      (render-assembla-space (elt spaces n)))))

(defun render-assembla-space(space)
  (let* ((name        (cdr (assoc 'name space)))
	 (start-point (point))
	 (str-length  (length name)))
    (insert (format "%s" name))
    (put-text-property start-point (+ start-point str-length) 'space-meta space)
    (put-text-property start-point (+ start-point str-length) 'assembla-thing-at-point "space")
    (newline)))

;;refactor
(defun assembla-render-tickets-in-space(&optional space-id)
  (interactive)
  (if (not space-id) ;; assume space at point
      (let ((space (get-text-property (point) 'space-meta)))
	(if (not space)
	    (message "No space at point.")
	  (with-assembla-buffer "*assembla*" (format "Tickets in %s" (cdr (assoc 'name space)))
	    (assembla-get (format "spaces/%s/tickets" (cdr (assoc 'id space))) "json" 'render-assembla-tickets-list t)
	    (put-text-property (point-min) (point-max) 'prev-buffer 'assembla))))
    (assembla-get (format "spaces/%s/tickets" space-id) "json" 'render-assembla-tickets-list t)))

(defun render-assembla-tickets-list(json-str)
  (with-assembla-buffer "*assembla*" "Tickets in SPACE"
    (let* ((tickets (json-read-from-string json-str))
	   (len    (length tickets)))
      (dotimes (n len)
	(render-assembla-ticket-line (elt tickets n)))))
  (put-text-property (point-min) (point-max) 'prev-buffer 'assembla))

;; excerpt summary length @todo
(defun render-assembla-ticket-line(ticket)
  (let* ((summary (cdr (assoc 'summary ticket)))
	 (start-point (point))
	 (str-length (length summary)))
   (insert (format "#%-3d %-10s %s" (cdr (assoc 'number ticket)) (format "[%s]" (cdr (assoc 'status ticket))) summary))
    (put-text-property start-point (+ start-point str-length) 'ticket-meta ticket)
    (put-text-property start-point (+ start-point str-length) 'assembla-thing-at-point "ticket")
    (newline)))

;;refactor
(defun assembla-render-ticket(&optional space-id &optional ticket-id)
  (interactive)
  (if (and (not space-id)
	   (not ticket-id))
      ;; use point
      (let* ((ticket    (get-text-property (point) 'ticket-meta))
	     (space-id  (cdr (assoc 'space_id ticket)))
	     (ticket-id (cdr (assoc 'id ticket))))
	(with-assembla-buffer "*assembla*" (format "[%d] %s" (cdr (assoc 'number ticket)) (cdr (assoc 'name ticket)))
	  (assembla-get (format "spaces/%s/tickets/id/%s" space-id ticket-id) "json" 'render-assembla-ticket-view t)
	  (put-text-property (point-min) (point-max) 'ticket-meta ticket)
	  (put-text-property (point-min) (point-max) 'prev-buffer `(assembla-render-tickets-in-space ,space-id))))
    ;; use space-id/ticket-id
    (with-assembla-buffer "*assembla*" "Ticket"
      (assembla-get (format "spaces/%s/tickets/id/%s" space-id ticket-id) "json" 'render-assembla-ticket-view t))))

(defun render-assembla-ticket-view(json-str)
  (let* ((ticket (json-read-from-string json-str))
	 (desc   (if (eq "" (cdr (assoc 'description ticket))) "(no description)" (cdr (assoc 'description ticket)))))
      (with-assembla-buffer "*assembla*" (format "[%d] %s" (cdr (assoc 'number ticket)) (cdr (assoc 'summary ticket)))
	(insert (format "Status:      %s\n" (cdr (assoc 'status ticket))))
	(insert (format "Priority:    %s" (asm/ticket-priority-label (cdr (assoc 'priority ticket)))))
	(newline)
	(insert (format "Assigned To: %s\n" (asm/get-user (cdr (assoc 'assigned_to_id ticket)) 'name)))
	(insert (format "Due Date:    %s" (cdr (assoc 'Due-Date (cdr (assoc 'custom_fields ticket))))))
	(newline)(newline)
	(insert (format "%s" desc))
	(newline)
	(insert (format "---------------------------------"))
	(newline)
	(assembla-get (format "spaces/%s/tickets/%d/ticket_comments" (cdr (assoc 'space_id ticket)) (cdr (assoc 'number ticket))) "json" 'render-assembla-ticket-comments t 1800)
	(newline)
	(put-text-property (point-min) (point-max) 'ticket-meta ticket)
	(put-text-property (point-min) (point-max) 'prev-buffer `(assembla-render-tickets-in-space ,(cdr (assoc 'space_id ticket)))))))

(defun render-assembla-ticket-comments(json-str)
  (with-current-buffer "*assembla*"
    (goto-char (point-max))
    (insert "%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COMMENTS ")
    (insert (make-string (- (window-width) (current-column) 1) ?%))
    (newline)
    (let* ((comments (json-read-from-string json-str))
	   (len    (length comments)))
      (dotimes (n len)
	(render-assembla-comment (elt comments n))))))

(defun render-assembla-comment(comment)
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

(assembla-get "spaces" "json" 'asm/build-user-table t 86400)))

(provide 'assembla-mode)
