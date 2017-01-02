;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; my emacs conf
(setq user-full-name "Piotr Kazmirczak")
(setq user-email-address "me@piotrkazmierczak.com")

;; setup packages
(package-initialize)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'install-packages)

(require 'evil)
(evil-mode t)

(require 'powerline)
(powerline-vim-theme)

(exec-path-from-shell-initialize)

;;; Buffer menu
(global-set-key [f5] 'buffer-menu)
(global-set-key (kbd "M-p") 'ace-window)

(global-git-gutter-mode +1)

(fset 'yes-or-no-p 'y-or-n-p)

(windmove-default-keybindings 'shift)

;;Enables awesome file-finding
(require 'ido)                      ; ido is part of emacs
(ido-mode t)                        ; for both buffers and files
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t)

(if (require 'ido-ubiquitous "ido-ubiquitous" t)
    (setf ido-ubiquitous t))

(set-face-attribute 'default nil :family "Source Code Pro for Powerline")

;;; Interface and other customizations
(setq echo-keystrokes 0.4
      debug-on-error nil
      stack-trace-on-error nil
      standard-indent 4
      tab-always-indent 'complete
      grep-scroll-output t
      ;;; Smooth scrolling
      redisplay-dont-pause t
      custom-file "~/.emacs.d/custom.el"
      scroll-margin 10
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(global-hl-line-mode +1)
(global-prettify-symbols-mode +1)
(line-number-mode 1)
(column-number-mode 1)

(setq-default comment-column 42
	      fill-column 78
	      indent-tabs-mode nil
	      tab-width 4
	      word-wrap t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(require 'rainbow-delimiters)

(global-flycheck-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'subword-mode)

;; md and json
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(require 'markdown-preview-mode)

(require 'development)

(load-theme 'FlatUI t)

(load custom-file)
