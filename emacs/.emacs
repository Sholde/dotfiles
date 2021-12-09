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

;; PROFILING

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; SETTING VARIABLE

;; disable useless thing
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

;; Set encoding
(prefer-coding-system 'utf-8)

;; Set up the visible bell
(setq visible-bell t)

;; Enable syntax color
(global-font-lock-mode t)

;; Enable column number
(column-number-mode t)

;; Remove column number because it is redondant
;;(when (version<= "26.0.50" emacs-version)
;;  (global-display-line-numbers-mode))

;; Parenthesis highlight
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; Auto fill when the line is too long to display
(setq c-ignore-auto-fill nil)                 ;; enable autofill
(setq-default fill-column 80)                 ;; fix fill column (don't works)
(setq-default fill-column-indicator 80)       ;; fix indicator
(setq-default indent-tabs-mode nil)           ;; space > tabs
(setq-default word-wrap t)                    ;; enable word-wrap
(add-hook 'text-mode-hook 'turn-on-auto-fill) ;; auto-fill
(add-hook 'prog-mode-hook 'turn-on-auto-fill) ;; auto-fill
(add-hook 'text-mode-hook
          #'display-fill-column-indicator-mode) ;; add bar at column 80
(add-hook 'prog-mode-hook
          #'display-fill-column-indicator-mode) ;; add bar at column 80

;; If indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                         (untabify (point-min) (point-max)))
            nil ))

;; Delete trailing whitespace on save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Font
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-12"))

;; Make startup faster by reducing the frequency of garbage
;; collection. The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; The rest of the init file.

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; COMMAND

;; Begin new line above
(defun my/begin-line-above (times)
  (interactive "p") ; Calleb from M-x
  (move-beginning-of-line 1)
  (save-excursion
    (newline times)))

(global-set-key (kbd "M-o")
                'my/begin-line-above)

;; Begin new line beside
(defun my/begin-line-beside (times)
  (interactive "p") ; Calleb from M-x
  (move-end-of-line 1)
  (newline times))

(global-set-key (kbd "C-o")
                'my/begin-line-beside)

;; Some controls
(global-set-key [(control z)] 'undo)
(global-set-key [(meta g)] 'goto-line)
(global-set-key (kbd "M-n")
                (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p")
                (lambda () (interactive) (previous-line 5)))

;; PACKAGE

;; Enable gnu, melpa and melpa-stable repo
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-quickstart t)

;; Ensure that use-package is installed
(unless (and (fboundp 'package-installed-p)
             (package-installed-p 'use-package))
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

;; cmake
(use-package cmake-mode
  :defer t
  :mode ("CMakeLists.txt" "\.cmake(.in)?\'"))

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

;; Theme
;;(load "~/.emacs.d/aanila-theme.el")
(use-package badwolf-theme
  :defer t
  :ensure t
  :init (load-theme 'badwolf t))
;;(add-to-list 'default-frame-alist '(background-color . "black"))
;;(set-face-background 'default "black")
;;(add-to-list 'default-frame-alist '(display-line-numbers . "black"))
;;(setq-default customize-group '(display-line-numbers . "black"))

;; auto insert pair
(use-package electric-pair-mode
  :defer t
  :init (electric-pair-mode 1))

;; vim empty lines mode
(use-package vim-empty-lines-mode
  :defer t
  :init (global-vim-empty-lines-mode))

;; markdown
(use-package markdown-mode
  :defer 1
  :mode ("\.md\'"))

;; org mode
(use-package org-mode
  :defer 1
  :mode ("\.org\'"))

;; Export html
(use-package htmlize
  :defer 2
  :mode ("\.org\'")
  :init (progn
          (setq org-html-htmlize-output-type 'inline-css)
          (setq org-html-validation-link nil)
          (setq org-export-html-postamble nil)
          (setq org-export-html-extension "html")
          (setq org-export-with-sub-superscripts nil)))

;; Export latex
(use-package ox-latex
  :defer t
  :mode ("\.org\'")
  :config
  (unless (boundp 'org-latex-classes)
    (setq org-export-classes nil))
  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[11pt, letterpaper]{article}
         \\usepackage[document]{ragged2e}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; Enable highliting src in org mode
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

;; Jupyter Notebook in emacs
(use-package ein
  :defer 1
  :mode ("\.ipynb\'"))

;; Enable shell-script-mode
(add-to-list 'auto-mode-alist '("\\.bash.*\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.config\\'" . shell-script-mode))

;;;; nlinum
;;(use-package nlinum-relative
;;  :defer 0
;;  :ensure t
;;  :config
;;  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
;;  (setq-default nlinum-relative-redisplay-delay 0)      ;; delay
;;  (setq-default nlinum-relative-current-symbol "")      ;; or "" for display current line number
;;  (setq-default nlinum-relative-offset 0)               ;; 1 if you want 0, 2, 3...
;;  (setq-default nlinum-format "%4d\u2502")              ;; adding padding
;;  (global-nlinum-relative-mode)
;;  )

;; gnuplot
(use-package gnuplot-mode
  :defer t
  :mode ("\.pg\'"))
