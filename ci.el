(defun ci (arg)
  
  ;; ‘S’
  ;; An interned symbol whose name is read in the minibuffer. Terminate the input with either C-j or RET.
  ;; Other characters that normally terminate a symbol (e.g., whitespace, parentheses and brackets) do not do so here. Prompt.
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Codes.html

  ;; `x'
  ;; 入力構文で表されたLispオブジェクト。 C-jかRETで終える。 オブジェクトは評価しない。
  ;; see 節 19.3 ミニバッファでのLispオブジェクトの読み取り。 「プロンプト」。
  ;; http://www.bookshelf.jp/texi/elisp-manual-20-2.5-jp/elisp_21.html

  ;; interactive "s" is not no longer useful
  ;; instead of this, would be happy to use "x".
  
  (interactive "sci: ")
  (cond ((or (string= arg "(") ;; ")" "]" and  "}" are invalid in emacs lisp.
	     (string= arg "{")
	     (string= arg "[")) ;; "[" is not ready, cuz idk how to use this in regexp
	 (zap-from-to-char-paren arg))
	((string= arg "\"") (zap-from-to-char arg))
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
	  ((string= arg "[") (setq %target "[[]]"))
	  )
    (catch 'end-of-search
      (while t
	(cond ((<= %paren-n 0) ;; if
	       (goto-char %beginning)
	       (re-search-backward %target)
	       (setq %beginning (match-beginning 0))
	       (cond ((string= arg (char-to-string (following-char))) (setq %paren-n (+ %paren-n 1)))
		     (t (setq %paren-n (- %paren-n 1))))
	       ) ;; if
	      (t ;; else
	       (goto-char %end)
	       (re-search-forward %target)
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

    ;; (message "%d" %paren-n)
    
    (kill-region (+ %beginning 1) (- %end 1))
    (goto-char (+ %beginning 1))
    
    ) ;; end of let
  ) ;; end of func

(defun kill-current-word ()
  (interactive)
  (backward-word 1)
  (kill-word 1))

(defun message-thing-at-point (thing)
  "thing-at-point で取得できた値を表示する"
  (interactive "Sthing:")
  (message "%s" (thing-at-point thing)))

(provide 'ci)
