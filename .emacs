(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(inhibit-startup-screen t)
 '(package-selected-packages '(yaml-mode rust-mode auto-complete)))
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

;; enable melpa repo
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; my scilab support
(load "~/.emacs.d/own-mode/highlight-scilab.el")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.sci\\'" . scilab-mode))

;; auto-complete
(require 'auto-complete)
(ac-config-default)

;; enable rust-mode
(require 'rust-mode)

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
(global-set-key [f2] 'save-buffer)
(global-set-key [f3] 'find-file)
(global-set-key [f4] 'kill-this-buffer)
(global-set-key [f9] 'compile)
(global-set-key [(control z)] 'undo)
(global-set-key [(meta g)] 'goto-line)

;; show error (and not make sound)
(setq visible-bell t)

;; enable syntax color
(global-font-lock-mode t)

;; line numbers
(column-number-mode t)
(when (version<= "24.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; parenthesis highlight
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; auto fill with back slash le end of line when it too long to display
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-to-list 'default-frame-alist
	     '(font . "DejaVu Sans Mono-11"))

