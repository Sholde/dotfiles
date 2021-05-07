(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; disable useless bar
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; begin new line above
(defun my/begin-line-above (times)
  (interactive "p") ; Calleb from M-x
  (move-beginning-of-line 1)
  (save-excursion
    (newline times)))

(global-set-key (kbd "M-o")
                'my/begin-line-above)

;; begin new line beside
(defun my/begin-line-beside (times)
  (interactive "p") ; Calleb from M-x
  (move-end-of-line 1)
  (newline times))

(global-set-key (kbd "C-o")
                'my/begin-line-beside)

;; some control
(global-set-key [(control z)] 'undo)
(global-set-key [(meta g)] 'goto-line)
(global-set-key (kbd "M-n")
                (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p")
                (lambda () (interactive) (previous-line 5)))

;; show error (and not make sound)
(setq visible-bell t)

;; enable syntax color
(global-font-lock-mode t)

;; Enable column number
(column-number-mode t)

;; Remove column number because it is redondant
;;(when (version<= "26.0.50" emacs-version)
;;  (global-display-line-numbers-mode))

;; parenthesis highlight
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; auto fill with back slash the end of line when it too long to display
(setq c-ignore-auto-fill nil)
(setq-default fill-column 80)
(setq-default fill-column-indicator 80)
(setq-default indent-tabs-mode nil)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                         (untabify (point-min) (point-max)))
            nil ))

;; Font
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-12"))

;; Make startup faster by reducing the frequency of garbage
;; collection. The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; The rest of the init file.

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; enable gnu, melpa and melpa-stable repo
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-quickstart t)


;; ensure that use-package is installed
(unless (and (fboundp 'package-installed-p)
             (package-installed-p 'use-package))
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

;; cmake
(use-package cmake-mode
  :defer t
  :mode ("CMakeLists\.txt\'" "\.cmake(.in)?\'"))

;; auto-complete
(use-package auto-complete
  :defer t
  :ensure t
  :init (ac-config-default))

;; latex completion
(load "~/.emacs.d/auto-complete-latex.el")
(use-package auto-complete-latex
  :defer 1
  :mode ("\.tex\'"))

;; mpi completion
(load "~/.emacs.d/own-mode/auto-complete-mpi.el")
(use-package auto-complete-mpi
  :defer 1
  :mode ("\.c\'" "\.h\'" "\.cc\'" "\.cpp\'" "\.c++\'" "\.hh\'"))

;; pthread completion
(load "~/.emacs.d/own-mode/auto-complete-pthread.el")
(use-package auto-complete-pthread
  :defer 1
  :mode ("\.c\'" "\.h\'" "\.cc\'" "\.cpp\'" "\.c++\'" "\.hh\'"))

;; my scilab highlighting
(load "~/.emacs.d/own-mode/scilab-mode.el")
(add-to-list 'auto-mode-alist '("\\.sci\\'" . scilab-mode))

;; theme
;;(load "~/.emacs.d/aanila-theme.el")
(use-package badwolf-theme
  :defer t
  :ensure t
  :init (load-theme 'badwolf t))

;; auto insert pair
(use-package electric-pair-mode
  :defer t
  :init (electric-pair-mode 1))

;; markdown
(use-package markdown-mode
  :defer 1
  :mode ("\.md\'"))

;; org mode
(use-package org-mode
  :defer 1
  :mode ("\.org\'"))

;; export html
(use-package htmlize
  :defer 2
  :mode ("\.org\'")
  :init (progn
          (setq org-html-htmlize-output-type 'inline-css)
          (setq org-html-validation-link nil)
          (setq org-export-html-postamble nil)
          (setq org-export-html-extension "html")
          (setq org-export-with-sub-superscripts nil)))

;; export latex
(use-package ox-latex
  :defer t
  :mode ("\.org\'")
  :config
  (unless (boundp 'org-latex-classes)
    (setq org-export-classes nil))
  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[12pt, letterpaper]{article}
         \\usepackage[document]{ragged2e}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; enable highliting src in org mode
  (setq-default org-src-fontify-natively t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (C .t)
     (fortran .t)
     (shell . t)
     (python . t)
     (R . t)
     (emacs-lisp . t)
     (lisp .t)
     (haskell . t)
     (perl . t)
     (js . t)
     )))

;;(use-package powerline
;;  :defer t
;;  :ensure t
;;  :init (powerline-default-theme))

;; enable shell-script-mode
(add-to-list 'auto-mode-alist '("\\.bash.*\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.config\\'" . shell-script-mode))

