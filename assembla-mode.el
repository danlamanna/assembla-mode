(require 'assembla-lib)
(require 'json)

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
     (insert (format "-- %s ---------------" ,heading-str))
     (newline)
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
  (with-assembla-buffer "*assembla*" "Ticket"
    (let* ((ticket (json-read-from-string json-str))
	   (desc   (if (eq "" (cdr (assoc 'description ticket))) "(no description)" (cdr (assoc 'description ticket)))))
      (insert (format "Status: %-10s\n" (cdr (assoc 'status ticket))))
      (insert (format "Priority: %s" (cdr (assoc 'priority ticket))))
      (newline)
      (insert (format "Assigned To: %20s\n" (cdr (assoc 'assigned_to_id ticket))))
      (insert (format "Due Date: Unknown"))
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
  (let* ((comments (json-read-from-string json-str))
	 (len    (length comments)))
    (dotimes (n len)
      (render-assembla-comment (elt comments n)))))

(defun render-assembla-comment(comment)
  (unless (or (eq (cdr (assoc 'comment comment)) "")
	      (eq (cdr (assoc 'comment comment)) nil))
    (insert (cdr (assoc 'user_id comment)))
    (insert (format " (%s)" (cdr (assoc 'created_on comment))))
    (newline)
    (insert (format "%s" (cdr (assoc 'comment comment))))
    (newline)
    (insert "-----------------------------\n")))
