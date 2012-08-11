(require 'xml)
(require 'button)

(defvar assembla-api-url "http://www.assembla.com")

;; request response cycle
(defun assembla-api-uri (uri)
  (interactive)
  (concat assembla-api-url uri))

(defun assembla-retrieve (url callback &optional cbargs)
  (let ((url-mime-accept-string "application/xml"))
    (url-retrieve url callback cbargs)))

(defun assembla-parse-response (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (xml-parse-region url-http-end-of-headers (point-max))))

;; spaces
(defun assembla-my-spaces-xml()
  (interactive)
  (assembla-retrieve "http://www.assembla.com/spaces/my_spaces" (lambda (status)
								  (switch-to-buffer (current-buffer)))))


(defun assembla-raw-xml(url)
  (assembla-retrieve url (lambda (status)
			   (switch-to-buffer (current-buffer)))))

(defun assembla-get-spaces()
  (interactive)
  (assembla-retrieve "http://www.assembla.com/spaces/my_spaces/" (lambda (status &optional cbargs)
								   (assembla-parse-response))))


(defun assembla-my-spaces()
  (interactive)
  (assembla-retrieve "http://www.assembla.com/spaces/my_spaces/" (lambda (status &optional cbargs)
								   (let* ((root (assembla-parse-response))
									  (spaces (xml-get-children (car root) 'space)))
								     (with-current-buffer (get-buffer-create "my active tickets (assembla)")
								       (erase-buffer)
								       (dolist (space spaces)
									 (let ((id (car (xml-node-children (car (xml-get-children space 'id)))))
									       (name (car (xml-node-children (car (xml-get-children space 'name))))))
									   (insert (format "%s . %s" name id))
									 (newline))
								       (goto-char (point-min))
								       (display-buffer (current-buffer))))))))

(defun assembla-active-tickets ()
  (interactive)
  (with-current-buffer (get-buffer-create "my active tickets (Assembla)")
    (erase-buffer)
    (insert "-- Active Tickets List ----------------------------------------")
    (newline))
  (dolist (space spaces-list)
    (lexical-let ((space-name (car space))
		  (space-id   (cdr space)))
      (assembla-retrieve (format "http://www.assembla.com/spaces/%s/tickets/report/8" (cdr space))
			 (lambda (status)
			   (let* ((root (assembla-parse-response))
				  (tickets (xml-get-children (car root) 'ticket)))
			     (with-current-buffer (get-buffer-create "my active tickets (Assembla)")					
			       (when (not (eq (length tickets) 0))
				 (newline)
				 (insert space-name)
				 (newline))
			       (dolist (ticket tickets)
				 (assembla-ticket-summary-string ticket)))			     
			     (switch-to-buffer "my active tickets (Assembla)")))))))

(defun assembla-active-tickets-by-space-id (space-id)
  (assembla-retrieve (format "http://www.assembla.com/spaces/%s/tickets/report/8" space-id) (lambda (status)
											      (let* ((root (assembla-parse-response))
												     (tickets (xml-get-children (car root) 'ticket)))
												(with-current-buffer (get-buffer-create "my active tickets (Assembla)")
												  (erase-buffer)
												  (dolist (ticket tickets)
												    (assembla-ticket-summary-string ticket)))
												  (switch-to-buffer "my active tickets (Assembla)")))))


;; ticket formatting
(defun assembla-ticket-summary-string (ticket)
  (lexical-let* ((status (car (xml-node-children (car (xml-get-children ticket 'status)))))
		 (summary (car (xml-node-children (car (xml-get-children ticket 'summary)))))
		 (space-id (car (xml-node-children (car (xml-get-children ticket 'space-id)))))
		 (number (car (xml-node-children (car (xml-get-children ticket 'number))))))
    (insert-button (format "%8s - %s" (assembla-ticket-status-string status) summary) 'action (lambda (x) (assembla-single-ticket space-id number)))
    (newline)))


; should take space id, ticket id
(defun assembla-single-ticket (space-id ticket-number)
  (interactive)
  (assembla-retrieve (format "http://www.assembla.com/spaces/%s/tickets/%s" space-id ticket-number) (lambda (status)
												      (let ((ticket (car (assembla-parse-response))))
													(with-current-buffer (get-buffer-create "single ticket")
													  (erase-buffer)
													  (assembla-ticket-single ticket))
													(switch-to-buffer "single ticket")))))

(defun assembla-ticket-single (ticket)
  (let* ((status-name (car (xml-node-children (car (xml-get-children ticket 'status-name)))))
	 (summary (car (xml-node-children (car (xml-get-children ticket 'summary)))))
	  (description (car (xml-node-children (car (xml-get-children ticket 'description)))))
	   (created-on (car (xml-node-children (car (xml-get-children ticket 'created-on))))))
    ;(insert (propertize (format "%s" summary) 'bold-italic))
    (insert (format "%s (%s)" summary created-on))
    (newline)
    (insert (format "Status: %8s" status-name))
    (newline)
    (insert "______________________________________")
    (newline)
    (if (eq description nil)
	(insert "(no description)")
      (insert (format "%s" description)))
    (newline)(newline)(newline)(newline)(newline)(newline)
    (newline)
    (insert-button "Back to Active Tickets" 'action (lambda (x) (switch-to-buffer "my active tickets (Assembla)")))))

    
	   
	 
(defun assembla-ticket-status-string (status-id)  
  (cdr (assoc status-id
	      '(("0" . "New")
		("1" . "Accepted")
		("2" . "Invalid")
		("3" . "Fixed")
		("4" . "Test")))))
  
		 


;; spaces formatting
(defun assembla-space-summary-string (space)
  (let* ((id (car (xml-node-children (car (xml-get-children space 'id))))))
    (format "%s" id)))