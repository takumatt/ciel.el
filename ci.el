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
	     (string= arg "[")
	     ;; (string= arg "<") ;; also "<" is invalid.
	     )
	 (zap-from-to-char-paren arg))
	((or (string= arg "\"")
	     (string= arg "\'"))
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

;; feature: catch search failed.
;; omg I found backward-list...this big function will be deleted.
(defun zap-from-to-char-paren (arg &optional %target)
  (let ((%point (point)) (%beginning (point)) (%end (point)) (%paren-n 0))
    (when (null %target)
      (cond ((string= arg "(") (setq %target "[()]")) ;; for regexp
	    ((string= arg "{") (setq %target "[{}]"))
	    ((string= arg "[") (setq %target "[][]"))
	    ))
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

;; A function for testing backward-list
;; If this works well I will adopt
(defun zap-from-to-char-paren-2 (arg)
  )

(defun ci-tag ()
  (when (derived-mode-p 'web-mode)
    (web-mode-navigate))
  
  ;; web-mode-navigate is web-mode's funcion
  ;; following codes are without web-mode
  
  ;; (let ((%beginning) (%end) (%str) (%target))
  ;;   (skip-chars-backward "^<")
  ;;   (setq %beginning (point))
  ;;   (skip-chars-forward "^>")
  ;;   (setq %end (point))
  ;;   (setq %str (concat "<" (buffer-substring %beginning %end) ">"))
  ;;   (setq %target (concat %str "\|</" (substring %str 1)))
  ;;   (message "%s, %s" %str %target)
  
  ;;   ) ;; end of let
  ) ;; end of func

;; just kill a word.
(defun kill-current-word ()
  (interactive)
  (backward-word 1)
  (kill-word 1))

(provide 'ci)
