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

;;;;;;;;;;;;;;;;;;;;;;
;; SETTING VARIABLE ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Disable garbage collection at startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5) ;; defer gc futher back

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

(setq inhibit-startup-message t) ;; disable startup message
(setq visible-bell t)            ;; Set up the visible bell

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Disable useless things
;; (scroll-bar-mode -1)        ; Disable visible scrollbar
;; (tool-bar-mode -1)          ; Disable the toolbar
;; (set-fringe-mode 10)        ; Give some breathing room
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar

;; Remember
(recentf-mode 1)         ;; remenber recent opened files
(setq history-length 25) ;; Save what you enter into minibuffer prompts
(savehist-mode 1)        ;;
(save-place-mode t)      ;; Save position in files

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Theme
(setq frame-background-mode 'dark)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Save backup files in a specific directory
(setq backup-directory-alist '(("." . "~/.emacs_saves")))

;; Complete paths
(ido-mode t)
;; M-x mode
(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))

;; Set encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Enable syntax color
(global-font-lock-mode t)

;; Enable column number
(column-number-mode t)
;;(when (version<= "26.0.50" emacs-version)
;;  (global-display-line-numbers-mode))
(require 'linum) ;; prefer linum over display-line-numbers
(global-linum-mode)
;; automatic lenght detection
(custom-set-variables '(linum-format 'dynamic))
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))
(set-face-foreground 'linum "gold")

;; Parenthesis highlight
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Highlight current line
(global-hl-line-mode t)
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)

;; Display file size
(size-indication-mode t)

;; Coding style
(setq-default c-default-style "bsd")
(setq-default c-basic-offset 4)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; correct spelling and typographical errors in a file
(setq ispell-program-name "hunspell")

;; Auto fill when the line is too long to display
(setq c-ignore-auto-fill nil)                 ;; enable autofill
(setq-default fill-column 80)                 ;; fix fill column (don't works)
(setq-default fill-column-indicator 80)       ;; fix indicator
(setq-default indent-tabs-mode nil)           ;; space > tabs
(setq-default word-wrap t)                    ;; enable word-wrap
(add-hook 'text-mode-hook
          #'display-fill-column-indicator-mode) ;; add bar at column 80
(add-hook 'prog-mode-hook
          #'display-fill-column-indicator-mode) ;; add bar at column 80

;; ;; If indent-tabs-mode is off, untabify before saving
;; (add-hook 'write-file-hooks
;;           (lambda () (if (not indent-tabs-mode)
;;                          (untabify (point-min) (point-max)))
;;             nil ))
;;
;; ;; Delete trailing whitespace on save
;; (add-hook 'write-file-hooks 'delete-trailing-whitespace)

;;;;;;;;;;;;;
;; COMMAND ;;
;;;;;;;;;;;;;

;; Some functions
(defun ispell-change-fr ()
  (interactive "p") ; Calleb from M-x
  (call-interactively #'(ispell-change-dictionary fr_FR)))

(defun ispell-change-en ()
  (interactive "p") ; Calleb from M-x
  (call-interactively #'ispell-change-dictionary en_US))

(defun fd-switch-dictionary()
      (interactive)
      (let* ((change (if (string= dic "francais") "english" "francais")))
        (ispell-change-dictionary change)
        (message "Dictionary switched to %s" change)
        ))

(defun my/begin-line-above (times)
  (interactive "p") ; Calleb from M-x
  (move-beginning-of-line 1)
  (save-excursion
    (newline times)))

(defun my/begin-line-beside (times)
  (interactive "p") ; Calleb from M-x
  (move-end-of-line 1)
  (newline times))

;; Some controls
(global-set-key (kbd "M-o")
                'my/begin-line-above)
(global-set-key (kbd "C-o")
                'my/begin-line-beside)
(global-set-key [(control z)] 'undo)
(global-set-key [(meta g)] 'goto-line)
(global-set-key (kbd "M-n")
                (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p")
                (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "M-:") 'dabbrev-expand)
(global-set-key (kbd "M-,") 'fill-region)

;;;;;;;;;;;;;
;; PACKAGE ;;
;;;;;;;;;;;;;

;; Enable gnu, melpa and melpa-stable repo
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

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
(load "~/.emacs.d/own-mode/auto-complete-latex.el")
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

;; cuda
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c-mode))

;; my scilab highlighting
(load "~/.emacs.d/own-mode/scilab-mode.el")
(add-to-list 'auto-mode-alist '("\\.sci\\'" . scilab-mode))

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

;; Jupyter Notebook in emacs
(use-package ein
  :defer 1
  :mode ("\.ipynb\'"))

;; Enable shell-script-mode
(add-to-list 'auto-mode-alist '("\\.bash.*\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.config\\'" . shell-script-mode))

;; gnuplot
(use-package gnuplot-mode
  :defer 0
  :mode ("\\.\\(gp\\|gnuplot\\)$"))

;;;;;;;;;;;;;;;;;;;
;; DEFER SETTING ;;
;;;;;;;;;;;;;;;;;;;

;; Make startup faster by reducing the frequency of garbage
;; collection. The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;;;;;;;;;;;;;;;
;; PROFILING ;;
;;;;;;;;;;;;;;;

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
