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

;; Defining scilab-mode
(define-derived-mode scilab-mode fundamental-mode "scilab"
  "major mode for editing scilab language code."
  (setq font-lock-defaults '(scilab-highlights)))
;;  (make-local-variable 'scilab-indent-offset))
;;  (set (make-local-variable 'indent-line-function) 'scilab-indent-line))

;; tab width
(defvar scilab-indent-offset 4
  "*Indentation offset for `scilab-mode'.")

;; indentaion rules
(defun scilab-indent-line ()
  "Indent current line for `scilab-mode'."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "\\<function\\>")
              (setq indent-col (+ indent-col scilab-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "\\<endfunction\\>") (>= indent-col scilab-indent-offset))
        (setq indent-col (- indent-col scilab-indent-offset))))
    (indent-line-to indent-col)))
    
