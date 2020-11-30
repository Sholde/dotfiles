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

;; line numbers
(column-number-mode t)
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; parenthesis highlight
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; auto fill with back slash the end of line when it too long to display
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; font
(add-to-list 'default-frame-alist
	     '(font . "DejaVu Sans Mono-11"))

;; enable melpa repo
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; badwolf theme
(load-theme 'badwolf t)

;; cmake
(require 'cmake-mode)

;; auto-complete
(require 'auto-complete)
(ac-config-default)

;; latex completion
(load "~/.emacs.d/auto-complete-latex.el")
(require 'auto-complete-latex)

;; my scilab highlighting
(load "~/.emacs.d/own-mode/scilab-mode.el")
(add-to-list 'auto-mode-alist '("\\.sci\\'" . scilab-mode))

