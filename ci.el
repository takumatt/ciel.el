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
  (interactive "sci: ")
  (let ((%region))
    ;; I'll merge zap-from-tochar-paren into one func.
    (cond ((or (or (string= arg "(") (string= arg ")"))
	       (or (string= arg "[") (string= arg "]"))
	       (or (string= arg "{") (string= arg "}")))
	   (setq %region (zap-from-to-char-paren-2 arg)))
	  ((or (string= arg "<") (string= arg ">")) (setq %region (zap-from-to-char-paren arg)))
	  ((or (string= arg "\"")
	       (string= arg "\'")
	       (string= arg "\`"))
	   (setq %region (zap-from-to-char arg)))
	  ((string= arg "w") (setq %region (kill-current-word)))
	  ((string= arg "t") (setq %region (ci-tag)))
	  ) ;; end of cond
    (kill-region (car %region) (cadr %region))
    )
  ) ;; end of func
(global-set-key "\C-ci" 'ci)

;; COpy inside
(defun co (arg)
  (interactive "sco: ")
  (let ((%region))
    (cond ((or (or (string= arg "(") (string= arg ")"))
	       (or (string= arg "[") (string= arg "]"))
	       (or (string= arg "{") (string= arg "}")))
	   (setq %region (zap-from-to-char-paren-2 arg)))
	  ((or (string= arg "<") (string= arg ">")) (setq %region (zap-from-to-char-paren arg)))
	  ((or (string= arg "\"")
	       (string= arg "\'")
	       (string= arg "\`"))
	   (setq %region (zap-from-to-char arg)))
	  ((string= arg "w") (setq %region (kill-current-word)))
	  ((string= arg "t") (setq %region (ci-tag)))
	  ) ;; end of cond
    (copy-region-as-kill (car %region) (cadr %region))
    )
  )
(global-set-key "\C-co" 'co)


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
      
      ;; (kill-region %beginning %end)
      (goto-char %beginning)
      (list %beginning %end)
      ) ;; end of catch 
    ) ;; end of let
  ) ;; end of func

;; zap-from-to-char-paren (search-string regexp html-flag)
;; I confirm that sometimes this function doesn't work in html-mode.
;; However I think that is not depend on this.
;; I tried this on same text but sometimes get wrong.
;; Probably it's regexp or buffering bug. (or is it bohrbug?)

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
	       (re-search-backward %target)
	       (while (nth 3 (syntax-ppss)) ;; if arg is quoted then ignore
		 (re-search-backward %target))
	       
	       (cond ((null %flag) ;; not html-mode
		      (setq %beginning (match-beginning 0))
		      (cond ((string= arg (char-to-string (following-char))) (setq %paren-n (+ %paren-n 1)))
			    (t (setq %paren-n (- %paren-n 1)))))
		     (t ;; html-mode
		      (setq %beginning (1+ (match-beginning 0)))
		      ;; (message "arg: %s, ct: %s" arg (current-tag))
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

	       (cond ((null %flag) ;; not html-mode
		      (setq %end (match-end 0))
		      (cond ((string= arg (char-to-string (preceding-char))) (setq %paren-n (+ %paren-n 1)))
			    (t (setq %paren-n (- %paren-n 1)))))
		     (t ;; html-mode
		      (setq %end (1- (match-end 0)))
		      ;; (setq %end (1- (match-beginning 0)))
		      ;; (message "arg: %s, ct: %s" arg (current-tag))
		      ;; (sleep-for 3)
		      (cond ((string= arg (current-tag)) (setq %paren-n (1+ %paren-n)))
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
    
    ;; (kill-region (+ %beginning 1) (- %end 1))
    (goto-char (+ %beginning 1))
    
    (list (1+ %beginning) (1- %end))

    ) ;; end of let
  ) ;; end of func

;; It works well. 
(defun zap-from-to-char-paren-2 (arg)
  (let ((%beginning) (%end))
    (search-backward arg)
    (setq %beginning (match-end 0))
    (forward-list)
    (setq %end (1- (point)))
    ;; (kill-region %beginning %end)
    (goto-char (1- (point)))
    (list %beginning %end)
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
    ;; (kill-region %beginning %end)
    (list %beginning %end)
    ) ;; end of let
  ) ;; end of func

(defun cit-not-web-mode ()
  (let ((%target) (%point (point)))
    (skip-chars-backward "^<")
    (setq %target (concat "</?" (current-tag t)))
    (goto-char %point)
    ;; (message "%s, %s" (current-tag) %target)
    ;; (sleep-for 3)
    (zap-from-to-char-paren (current-tag) %target t)
    )
  )

;; FIX: current tag is not always completely same. (ex. <div class=hoge> </div>)
(defun current-tag (&optional %flag)
  (let ((%beginning) (%end) (%tag) (%point (point)))
    (when (not (string= (char-to-string (following-char)) "<")) (skip-chars-backward "^<"))
    (cond ((and (null %flag) (not (string= (char-to-string (following-char)) "<"))) (setq %beginning (1- (point))))
	  (t (setq %beginning (point))))
    (skip-chars-forward "^[[:space:]>]")
    (cond ((null %flag) (setq %end (point)))
	  (t (setq %end (point))))
    (setq %tag (buffer-substring %beginning %end))
    ;; (message "current-tag: %s" %tag) ;; debugging
    ;; (sleep-for 3) ;; debugging
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
