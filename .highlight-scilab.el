(defvar scilab-mode-syntax-table nil "Syntax table for `scilab-mode'.")

;; Handling keywords and symbols
(setq scilab-highlights
      '(
	;; Type
	("function\\|endfunction" 						  . font-lock-type-face)

	;; Keyword
	("for\\|while\\|if\\|then\\|else\\|elseif\\|end\\|do\\|return"            . font-lock-keyword-face)
	("select\\|case\\|global\\|try\\|catch"                         	  . font-lock-keyword-face)

	;; String
	("'.'"                                                                    . font-lock-string-face)

	;; Comment
	("//.*\n"    								  . font-lock-comment-face)
	
	;; Constant / Number
	("[\s\t\n][0-9]+"               				          . font-lock-constant-face)))

;; Handling comments // ...
(setq scilab-mode-syntax-table
      	(let ( (synTable1 (make-syntax-table)))
        (modify-syntax-entry ?# "<" synTable1)
        (modify-syntax-entry ?\n ">" synTable1)
        synTable1))

;; Defining scilab-mode
(define-derived-mode scilab-mode fundamental-mode "scilab"
  "major mode for editing scilab language code."
  (setq font-lock-defaults '(scilab-highlights))
  (set-syntax-table scilab-mode-syntax-table))
