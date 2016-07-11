;; 
;; The MIT License (MIT)

;; Copyright (c) 2016 Takuma Matsushita

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.
;;

(defun ci (arg)
  
  ;; ‘S’
  ;; An interned symbol whose name is read in the minibuffer. Terminate the input with either C-j or RET.
  ;; Other characters that normally terminate a symbol (e.g., whitespace, parentheses and brackets) do not do so here. Prompt.
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Codes.html

  ;; interactive "s" can't accept close parentheses like "}", ")"
  ;; I'll find something instead of "s"
  
  (interactive "sci: ") ;; ")" "]" and  "}" are invalid in interactive "s".
  (cond ((or (string= arg "(")
	     (string= arg "{")
	     (string= arg "["))
	 (zap-from-to-char-paren-2 arg))
	((string= arg "<") (zap-from-to-char-paren arg))
	((or (string= arg "\"")
	     (string= arg "\'")
	     (string= arg "\`"))
	     (zap-from-to-char arg)) 
	((string= arg "w") (kill-current-word))
	((string= arg "t") (ci-tag)) ;; this is not completed. wait for update.
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

      ;; (cond ((string= arg "(") (setq arg ")"))) ;; -> zap-from-to-char-paren, 2
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

;; feature: catch search failed.
;; yes, zap-from-to-char-paren-2 is much easier and faster (probably) than this.
;; BUT if you don't install web-mode, then this func can be useful cuz u can't use web-mode's func.
;; ALSO this func can kill inside of <>. forward-list can't kill this.

;; zap-from-to-char-paren (search-string regexp html-flag)
(defun zap-from-to-char-paren (arg &optional %target %flag) ;; default optional value is nil
  (let ((%point (point)) (%beginning (point)) (%end (point)) (%paren-n 0))
    (when (null %target)
      (cond ((string= arg "(") (setq %target "[()]")) ;; for regexp
	    ((string= arg "{") (setq %target "[{}]"))
	    ((string= arg "[") (setq %target "[][]"))
	    ((string= arg "<") (setq %target "[<>]")) 
	    ))
    (catch 'end-of-search
      (while t
	(cond ((<= %paren-n 0) ;; if
	       (goto-char %beginning)
	       (re-search-backward %target) ;; FIX: when pointer is on <, skip-chars skip < 
	       (while (nth 3 (syntax-ppss))
	       	 (re-search-backward %target))
	       (setq %beginning (match-beginning 0))
	       
	       (cond ((null %flag)
		      (cond ((string= arg (char-to-string (following-char))) (setq %paren-n (+ %paren-n 1)))
			    (t (setq %paren-n (- %paren-n 1)))))
		     (t
		      ;; (message "arg:%s, current-tag:%s" arg (current-tag))
		      ;; (sleep-for 3)
		      (cond ((string= arg (current-tag)) (setq %paren-n (1+ %paren-n)))
			    (t (setq %paren-n (1- %paren-n)))))
		     )
	       
	       ) ;; if
	      (t ;; else
	       (goto-char %end)
	       (re-search-forward %target)
	       (while (nth 3 (syntax-ppss))
	       	 (re-search-forward %target))
	       (setq %end (match-end 0))

	       (cond ((null %flag)
		      (cond ((string= arg (char-to-string (preceding-char))) (setq %paren-n (+ %paren-n 1)))
			    (t (setq %paren-n (- %paren-n 1)))))
		     (t (cond ((string= arg (current-tag)) (setq %paren-n (1+ %paren-n)))
			      (t (setq %paren-n (1- %paren-n)))))
		     )
	       
	       ) ;; else
	      )

	;; (message "%d" %paren-n) ;; debugging
	;; (sleep-for 5) ;; debugging
	
	(cond ((= 0 %paren-n) (throw 'end-of-search t)))
	;; (throw 'end-of-search t)
	)
      ) ;; end of catch

    ;; (message "%d" %paren-n) ;; debugging

    (when (not (null %flag))
      (goto-char %beginning)
      (skip-chars-forward "^>")
      (setq %beginning (point))
      (goto-char %end)
      (skip-chars-backward "^<")
      (setq %end (point))
      (goto-char %end)
      )
    
    (kill-region (+ %beginning 1) (- %end 1))
    (goto-char (+ %beginning 1))

    ) ;; end of let
  ) ;; end of func

;; It works well. 
(defun zap-from-to-char-paren-2 (arg)
  (let ((%beginning) (%end))
    (search-backward arg)
    (setq %beginning (match-end 0))
    (forward-list)
    (setq %end (1- (point)))
    (kill-region %beginning %end)
    (goto-char (1- (point)))
    )
  )

(defun ci-tag ()
  ;; web-mode-navigate is web-mode's funcion
  ;; t case in following cond is without web-mode
  
  (cond ((derived-mode-p 'web-mode) (cit-web-mode))
	(t (cit-not-web-mode))
	) ;; end of cond
  ) ;; end of func

(defun cit-web-mode ()
  (let ((%beginning) (%end))
    (skip-chars-backward "^>")
    (setq %beginning (point))
    (skip-chars-backward "^<") ;; for searching
    (web-mode-navigate)
    (setq %end (point))
    (kill-region %beginning %end)
    ) ;; end of let
  ) ;; end of func

(defun cit-not-web-mode ()
  (let ((%target) (%point (point)))
    (skip-chars-backward "^<")
    (setq %target (concat "</?" (current-tag t) ">"))
    (goto-char %point)
    (zap-from-to-char-paren (current-tag) %target t)
    )
  )

(defun current-tag (&optional %flag)
  (let ((%beginning) (%end) (%tag) (%point (point)))
    (when (not (string= (char-to-string (following-char)) "<")) (skip-chars-backward "^<"))
    (cond ((and (null %flag) (not (string= (char-to-string (following-char)) "<"))) (setq %beginning (1- (point))))
	  (t (setq %beginning (point))))
    (skip-chars-forward "^>")
    (cond ((null %flag) (setq %end (1+ (point))))
	  (t (setq %end (point))))
    (setq %tag (buffer-substring %beginning %end))
    ;; (message "current-tag: %s" %tag) ;; debugging
    ;; (sleep-for 3)
    (goto-char %point) ;; return to init pos
    %tag
    )
  )

;; just kill a word.
(defun kill-current-word ()
  (interactive)
  (backward-word 1)
  (kill-word 1))

(provide 'ci)
