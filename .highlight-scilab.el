(defvar scilab-mode-syntax-table nil "Syntax table for `scilab-mode'.")

;; Handling keywords and symbols
(setq scilab-highlights
      '(
	("function\\|endfunction" 						  . font-lock-keyword-face)
	("for\\|while\\|if\\|else\\|end"					  . font-lock-keyword-face)
	("@"    								  . font-lock-type-face)
	("[[:blank:]\-][0-9]+"                            				  . font-lock-constant-face)))

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
