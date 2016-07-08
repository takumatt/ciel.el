(defun ci (arg)
  
  ;; ‘S’
  ;; An interned symbol whose name is read in the minibuffer. Terminate the input with either C-j or RET.
  ;; Other characters that normally terminate a symbol (e.g., whitespace, parentheses and brackets) do not do so here. Prompt.
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Codes.html

  ;; interactive "s" can't accept close parentheses like "}", ")"
  ;; I'll find something instead of "s"
  
  (interactive "sci: ")
  (cond ((or (string= arg "(") ;; ")" "]" and  "}" are invalid in emacs lisp.
	     (string= arg "{")
	     (string= arg "["))
	 (zap-from-to-char-paren arg))
	((or (string= arg "\"")
	     (string= arg "\'"))
	     (zap-from-to-char arg)) 
	((string= arg "w") (kill-current-word))
	((string= arg "t") (zap-from-to-char "<"))
	) ;; end of cond
  ) ;; end of func
(global-set-key "\C-ci" 'ci)

;; clone of ci in vim. some behavior is completely different from original vim.
(defun zap-from-to-char (arg)
  (let ((%point (point)) (%beginning nil) (%end nil))
    (catch 'no-match-in-line-error
      (search-backward arg)
      (goto-char %point)
      (cond ((> (line-beginning-position) (match-beginning 0)) (throw 'no-match-in-line-error nil)))
      (setq %beginning (match-end 0))

      ;; (cond ((string= arg "(") (setq arg ")"))) ;; -> zap-from-to-char-paren
      (cond ((string= arg "<") (setq arg ">")))

      (search-forward arg)
      (goto-char %point)
      (cond ((< (line-end-position) (match-beginning 0)) (throw 'no-match-in-line-error nil)))
      (setq %end (match-beginning 0))
      
      (kill-region %beginning %end)
      (goto-char %beginning)
      ) ;; end of catch 
    ) ;; end of let
  ) ;; end of func

(defun zap-from-to-char-paren (arg)
  (let ((%point (point)) (%beginning (point)) (%end (point)) (%paren-n 0) (%target nil))
    (cond ((string= arg "(") (setq %target "[()]"))
	  ((string= arg "{") (setq %target "[{}]"))
	  ((string= arg "[") (setq %target "[][]"))
	  )
    (catch 'end-of-search
      (while t
	(cond ((<= %paren-n 0) ;; if
	       (goto-char %beginning)
	       (re-search-backward %target)
	       (while (nth 3 (syntax-ppss))
	       	 (re-search-backward %target))
	       (setq %beginning (match-beginning 0))
	       (cond ((string= arg (char-to-string (following-char))) (setq %paren-n (+ %paren-n 1)))
		     (t (setq %paren-n (- %paren-n 1))))
	       ) ;; if
	      (t ;; else
	       (goto-char %end)
	       (re-search-forward %target)
	       (while (nth 3 (syntax-ppss))
	       	 (re-search-forward %target))
	       (setq %end (match-end 0))
	       (cond ((string= arg (char-to-string (preceding-char))) (setq %paren-n (+ %paren-n 1)))
		     (t (setq %paren-n (- %paren-n 1))))
	       ) ;; else
	      )

	;; (message "%d" %paren-n) ;; debugging
	;; (sleep-for 5) ;; debugging
	
	(cond ((= 0 %paren-n) (throw 'end-of-search t)))
	;; (throw 'end-of-search t)
	)
      ) ;; end of catch

    ;; (message "%d" %paren-n) ;; debugging
    
    (kill-region (+ %beginning 1) (- %end 1))
    (goto-char (+ %beginning 1))
    
    ) ;; end of let
  ) ;; end of func

(defun kill-current-word ()
  (interactive)
  (backward-word 1)
  (kill-word 1))

(provide 'ci)
