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
    (cond ((or (string= arg "(")
	       (string= arg "[")
	       (string= arg "{"))
	   (setq %region (zap-from-to-char-paren arg)))
	  ((or (string= arg "\"")
	       (string= arg "\'")
	       (string= arg "\`"))
	   (setq %region (zap-from-to-char arg)))
	  ((string= arg "w") (setq %region kill-current-word))
	  ) ;; end of cond
    (unless (null %region)
      (kill-region (car %region) (cadr %region)))
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
	   (setq %region (zap-from-to-char-paren arg)))
	  ((or (string= arg "\"")
	       (string= arg "\'")
	       (string= arg "\`"))
	   (setq %region (zap-from-to-char arg)))
	  ((string= arg "w") (setq %region kill-current-word)) ;; FIX: not kill! but copy
	  ) ;; end of cond
    (unless (null %region)
      (copy-region-as-kill (car %region) (cadr %region)))
    )
  )
(global-set-key "\C-co" 'co)


(defun zap-from-to-char-paren (arg)
  (let ((%beginning) (%end) (%target))
    (cond ((check-closing-paren arg)
	   (setq %end (match-beginning 0))
	   (backward-list)
	   (setq %beginning (1+ (point)))
	   (goto-char (1+ (point)))
	   (list %beginning %end))
	  (t (search-backward arg)
	     (setq %beginning (match-end 0))
	     (forward-list)
	     (setq %end (1- (point)))
	     ;; (kill-region %beginning %end)
	     (goto-char (1- (point)))
	     (list %beginning %end))
	  )
    )
  )

(defun check-closing-paren (arg)
  (let ((%target) (%point (point)))
    (cond ((string= arg "(") (setq %target "[()]")) ;; for regexp
	  ((string= arg "{") (setq %target "[{}]"))
	  ((string= arg "[") (setq %target "[][]"))
	  )
    (re-search-backward %target)
    (while (nth 3 (syntax-ppss))
      (re-search-backward %target))
    (cond ((not (string= arg (char-to-string (following-char))))
	   (goto-char %point)
	   (re-search-forward %target)
	   (while (nth 3 (syntax-ppss))
	     (re-search-forward %target))
	   (cond ((not (string= arg (char-to-string (preceding-char))))
		  (goto-char %point) (message "t") t)
		 (t (goto-char %point)
		    (message "nil")
		    nil
		    )))
	  (t (goto-char %point)
	     (message "nil")
	     nil
	     ))
    ))

(defun zap-from-to-char (arg)
  (let ((%point (point)) (%beginning nil) (%end nil))
    (catch 'no-match-in-line-error
      (search-backward arg)
      (goto-char %point)
      (cond ((> (line-beginning-position) (match-beginning 0)) (throw 'no-match-in-line-error nil)))
      (setq %beginning (match-end 0))

      (search-forward arg)
      (goto-char %point)
      (cond ((< (line-end-position) (match-beginning 0)) (throw 'no-match-in-line-error nil)))
      (setq %end (match-beginning 0))
      
      (goto-char %beginning)
      (list %beginning %end)
      ) ;; end of catch 
    ) ;; end of let
  ) ;; end of func

(defun kill-current-word ()
  (let ((%beginning) (%end) (%point (point))))
  (forward-word 1)
  (setq %beginning (point))
  (backward-word 1)
  (setq %end (point))
  (goto-char %point)
  (list %beginning %end)
(provide 'ci)
