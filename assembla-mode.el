(require 'assembla-lib)
(require 'json)

(defvar assembla-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'assembla-render-stream)
      map))

(defmacro with-assembla-buffer(asm-buffer-name heading-str &rest body)
  "Create buffer with name of ASM-BUFFER-NAME, or uses it if exists,
   preps it with readonly/erase/heading - executes `body' - then puts
   readonly back on, goes to beginning of buffer, and switches to it."
  (declare (indent 2))
  `(with-current-buffer (get-buffer-create ,asm-buffer-name)
     (assembla-mode)
     (toggle-read-only -1)
     (erase-buffer)
     (insert (format "-- %s ---------------" ,heading-str))
     (newline)
     (progn ,@body)
     (toggle-read-only 1)
     (goto-char (point-min))
     (switch-to-buffer ,asm-buffer-name)))



(define-derived-mode assembla-mode fundamental-mode "Assembla"
  "A major mode for interacting with Assembla."
  (kill-all-local-variables)
  (setq major-mode 'assembla-mode)
  (setq mode-name "Assembla"))

(defun assembla()
  (interactive)
  (with-assembla-buffer "*assembla*" "Assembla Spaces"
    (use-local-map assembla-mode-map)
    (assembla-get "spaces" "json" 'render-assembla-spaces t 86400)))

(defun assembla-prep-buffer(&optional heading-str)
  (toggle-read-only -1)
  (erase-buffer)
  (when heading-str
    (insert (format "%s" heading-str))
    (newline)))

(defun assembla-spaces-ido()
  (assembla-get "spaces" "json" (lambda(json-str)
				  (ido-completing-read "Space: " (json-read-from-string json-str))) t))

(defun render-activity-stream(json-str)
  (with-current-buffer (get-buffer-create "assembla")
    (let* ((events (json-read-from-string json-str))
	   (len    (length events)))
      (dotimes (n len)
	(let* ((event (elt events n))
	       (operation (cdr (assoc 'operation event)))
	       (name (cdr (assoc 'author_name event)))
	       (space-name (cdr (assoc 'space_name event)))
	       (title (cdr (assoc 'title event))))
	  (insert (format "%s @ %s %s %s" name space-name operation title))
	  (newline))))))

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
    (newline)))

(defun assembla-render-tickets-in-space-at-point()
  (interactive)
  (let* ((space (get-text-property (point) 'space-meta)))
    (if (not space)
	(message "No space at point.")
      (with-assembla-buffer "*assembla*" "tickets in space"
	(assembla-get (format "spaces/%s/tickets" (cdr (assoc 'id space))) "json" 'render-assembla-tickets-list t)))))

(defun render-assembla-tickets-list(json-str)
  (let* ((tickets (json-read-from-string json-str))
	 (len    (length tickets)))
    (dotimes (n len)
      (render-assembla-ticket-line (elt tickets n)))))

(defun render-assembla-ticket-line(ticket)
  (let* ((summary (cdr (assoc 'summary ticket)))
	 (start-point (point))
	 (str-length (length summary)))
   (insert (format "#%-3d %-10s %s" (cdr (assoc 'number ticket)) (format "[%s]" (cdr (assoc 'status ticket))) summary))
    (put-text-property start-point (+ start-point str-length) 'ticket-meta ticket)
    (newline)))

(defun assembla-render-ticket-at-point()
  (interactive)
  (let* ((ticket (get-text-property (point) 'ticket-meta)))
    (if (not ticket)
	(message "No ticket at point.")
      (with-assembla-buffer "*assembla*" "single ticket"
	(let ((summary (cdr (assoc 'summary ticket)))
	      (id      (cdr (assoc 'id ticket)))
	      (space-id (cdr (assoc 'space_id ticket))))
	  (assembla-get (format "spaces/%s/tickets/id/%s" space-id id) "json" 'render-assembla-ticket-view t))))))

(defun render-assembla-ticket-view(json-str)
  (with-current-buffer (get-buffer-create "assembla-ticket-view")
    (let* ((ticket (json-read-from-string json-str))
	   (desc   (if (eq "" (cdr (assoc 'description ticket))) "(no description)" (cdr (assoc 'description ticket)))))
      (insert (format "%s" desc))
      (insert (format "---------------------------------"))
      (newline)
      (assembla-get (format "spaces/%s/tickets/%d/ticket_comments" (cdr (assoc 'space_id ticket)) (cdr (assoc 'number ticket))) "json" 'render-assembla-ticket-comments t 1800)
      (newline))))


(defun render-assembla-ticket-comments(json-str)
  (with-current-buffer (get-buffer-create "assembla-ticket-view")
    (let* ((comments (json-read-from-string json-str))
	   (len    (length comments)))
      (dotimes (n len)
	(render-assembla-comment (elt comments n))))))

(defun render-assembla-comment(comment)
  (unless (or (eq (cdr (assoc 'comment comment)) "")
	      (eq (cdr (assoc 'comment comment)) nil))
    (insert (cdr (assoc 'user_id comment)))
    (insert (format " (%s)" (cdr (assoc 'created_on comment))))
    (newline)
    (insert (format "%s" (cdr (assoc 'comment comment))))
    (newline)
    (insert "-----------------------------\n")))

 ;     (render-assembla-ticket-comments ticket))))

;(defun render-assembla-ticket-comments
