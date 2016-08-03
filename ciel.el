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
    (cond ((or (or (string= arg "(") (string= arg ")"))
	       (or (string= arg "[") (string= arg "]"))
	       (or (string= arg "{") (string= arg "}"))) ;; ), ] and } doesn't work. I have no idea.
	   (setq %region (region-paren arg)))
	  ((or (string= arg "\"")
	       (string= arg "\'")
	       (string= arg "\`"))
	   (setq %region (region-quote arg)))
	  ((string= arg "w") (setq %region (region-word)))
	  )
    (unless (null %region)
      (kill-region (car %region) (cadr %region)))
    )
  )
(global-set-key "\C-ci" 'ci)

;; COpy inside
(defun co (arg)
  (interactive "sco: ")
  (let ((%region))
    (cond ((or (or (string= arg "(") (string= arg ")"))
	       (or (string= arg "[") (string= arg "]"))
	       (or (string= arg "{") (string= arg "}")))
	   (setq %region (region-paren arg)))
	  ((or (string= arg "\"")
	       (string= arg "\'")
	       (string= arg "\`"))
	   (setq %region (region-quote arg)))
	  ((string= arg "w") (setq %region (region-word)))
	  )
    (unless (null %region)
      (copy-region-as-kill (car %region) (cadr %region)))
    )
  )
(global-set-key "\C-co" 'co)

(defun region-paren (arg)
  (interactive "s") 
  (let ((%beginning) (%end) (%target))
    (move-to-parent-parenthesis arg)
    (setq %beginning (1+ (point)))
    (forward-list)
    (setq %end (1- (point)))
    (goto-char (1- (point)))
    (list %beginning %end)
    )
  )

;; ( %point% ) => left paren is parent.
;; ( %point% ( => left paren is parent.
;; ) %point% ) => right paren is parent.
;; ) %point% ( => find parent. the t of the second cond form is it.
(defun move-to-parent-parenthesis (arg)
  (let ((%target arg) (%init (point)) (%regexp) (%pair))
    (catch 'process 
    (cond ((string= %target "(") (setq %regexp "[()]"))
	  ((string= %target "{") (setq %regexp "[{}]"))
	  ((string= %target "[") (setq %regexp "[][]")))
    (cond ((string= %target "(") (setq %pair ")"))
	  ((string= %target "{") (setq %pair "}"))
	  ((string= %target "[") (setq %pair "]")))

    (when (string= %target (char-to-string (following-char)))
      (throw 'process "end here")
      )
    (when (string= %pair (char-to-string (preceding-char)))
      (backward-list)
      (throw 'process "end here")
      )
    
    (re-search-backward %regexp)
    (while (nth 3 (syntax-ppss)) ;; ignore commented
      (re-search-backward %regexp))
    (cond ((string= %target (char-to-string (following-char))) ;; backward is (, { or [
	   ;; do nothing cuz here is parent
	   )
	  (t
	   (goto-char %init)
	   (re-search-forward %regexp)
	   (while (nth 3 (syntax-ppss))
	     (re-search-forward %regexp)) 
	   (cond ((string= %target (char-to-string (following-char))) ;; forward is (
		  ;; do nothing
		  )
		 (t (let ((%count 0)) ;; here is in the case of ) %point (
		      (goto-char %init) 
		      (while (not (= %count 1))
			(re-search-backward %regexp)
			(while (nth 3 (syntax-ppss)) ;; ignore commented
			  (re-search-backward %regexp))
			(cond ((string= %target (char-to-string (following-char)))
			       (setq %count(1+ %count)))
			      (t (setq %count (1- %count))))
			))))
	   ))
    )))

;; find quoted area in the line
(defun region-quote (arg)
  (let ((%init (point)) (%beginning nil) (%end nil))
    (catch 'no-match-in-line-error ;; break when run into next line
      (search-backward arg)
      (goto-char %init)
      (cond ((> (line-beginning-position) (match-beginning 0)) (throw 'no-match-in-line-error nil)))
      (setq %beginning (match-end 0))

      (search-forward arg)
      (goto-char %init)
      (cond ((< (line-end-position) (match-beginning 0)) (throw 'no-match-in-line-error nil)))
      (setq %end (match-beginning 0))
      
      (goto-char %beginning)
      (list %beginning %end)
      )
    )
  )

;; just select word
(defun region-word ()
  (let ((%beginning) (%end) (%init (point)))
    (forward-word 1)
    (setq %beginning (point))
    (backward-word 1)
    (setq %end (point))
    (goto-char %init)
    (list %beginning %end)
    )
  )

(provide 'ciel)
