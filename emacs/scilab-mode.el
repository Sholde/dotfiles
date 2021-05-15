;; scilab mode

;; Handling keywords and symbols
(setq scilab-highlights
      '(
        ;; Comment
        ("//.*$"                                                                  . font-lock-comment-face)

        ;; Type
        ("\\<\\(function\\|endfunction\\)\\>"                                     . font-lock-type-face)

        ;; Keyword
        ("\\<\\(for\\|while\\|if\\|then\\|else\\|elseif\\|end\\|do\\|return\\|select\\|case\\|global\\|try\\|catch\\)\\>"                        . font-lock-keyword-face)

        ;; String
        ("'.'"                                                                    . font-lock-string-face)

        ;; Function
        ("\\<\\(exec\\|diff\\|comp\\|print\\|exp\\|abs\\|log\\|printf\\)\\>"      . font-lock-function-name-face)
        
        ;; Constant / Number
        ("[0-9]+\\(\\.[0-9]+\\)?\\(\\:[0-9]+\\(\\.[0-9]+\\)?\\)*"                 . font-lock-constant-face)))

;; indentation
(setq default-tab-width 4)

(defun scilab-indent-line ()
  "Indent current line for scilab-mode."
  (interactive)
  (beginning-of-line)
  (if (bobp)  ; Check for rule 1
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*\\(endfunction\\|end\\)") ; Check for rule 2
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) default-tab-width)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
                (save-excursion 
                  (while not-indented
                    (forward-line -1)
                    (if (looking-at "^[ \t]*\\(endfunction\\|end\\)") ; Check for rule 3
                        (progn
                          (setq cur-indent (current-indentation))
                          (setq not-indented nil))
                                        ; Check for rule 4
                      (if (looking-at "^[ \t]*\\(function\\|if\\|for\\|while\\)")
                          (progn
                            (setq cur-indent (+ (current-indentation) default-tab-width))
                            (setq not-indented nil))
                        (if (bobp) ; Check for rule 5
                            (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation
    

    
;; Defining scilab-mode
(define-derived-mode scilab-mode fundamental-mode "scilab"
  "major mode for editing scilab language code."
  (setq font-lock-defaults '(scilab-highlights))
  (make-local-variable 'scilab-indent-offset)
  (set (make-local-variable 'indent-line-function) 'scilab-indent-line))

