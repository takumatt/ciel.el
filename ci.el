;; 中かっこ未対応
(defun ci (arg)
  (interactive "sci: ")
  (cond ((string= arg "(") (zap-from-to-char-paren))
	((string= arg "\"") (zap-from-to-char arg))
	((string= arg "w") (kill-current-word))
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

      (cond ((string= arg "(") (setq arg ")"))) ;; ()に対応 ;; ただ(()とくると真ん中の()も消される -> zap-from-to-char-paren

      (search-forward arg)
      (goto-char %point)
      (cond ((< (line-end-position) (match-beginning 0)) (throw 'no-match-in-line-error nil)))
      (setq %end (match-beginning 0))
      
      (kill-region %beginning %end)
      (goto-char %beginning)
      
      ) ;; end of catch 
    ) ;; end of let
  ) ;; end of func

(defun zap-from-to-char-paren ()
  (interactive)
  (let ((%point (point)) (%beginning (point)) (%end (point)) (%paren 0))
    (catch 'end-of-search
      (while t
	(cond ((<= %paren 0) ;; if
	       (goto-char %beginning)
	       (re-search-backward "[()]")
	       (setq %beginning (match-beginning 0))
	       (cond ((string= "(" (char-to-string (following-char))) (setq %paren (+ %paren 1)))
		     (t (setq %paren (- %paren 1))))
	       ) ;; if
	      (t ;; else
	       (goto-char %end)
	       (re-search-forward "[()]")
	       (setq %end (match-end 0))
	       (cond ((string= "(" (char-to-string (preceding-char))) (setq %paren (+ %paren 1)))
		     (t (setq %paren (- %paren 1))))
	       ) ;; else
	      )

	;; (message "%d" %paren)
	;; (sleep-for 5)
	
	(cond ((= 0 %paren) (throw 'end-of-search t)))
	;; (throw 'end-of-search t)
	)
      ) ;; end of catch

    ;; (message "%d" %paren)
    
    (kill-region (+ %beginning 1) (- %end 1))
    (goto-char (+ %beginning 1))
    
    ) ;; end of let
  ) ;; end of func

(defun kill-current-word ()
  (interactive)
  (backward-word 1)
  (kill-word 1))
