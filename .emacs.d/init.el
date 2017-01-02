;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; my emacs conf
(setq user-full-name "Piotr Kazmirczak")
(setq user-email-address "me@piotrkazmierczak.com")

;; setup packages
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package evil)
(require 'evil)
(evil-mode t)

(use-package magit
  :bind (("\C-xg" . magit-status)))
(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(use-package pastelmac-theme)

(use-package ido-ubiquitous)
(use-package smex)
(use-package color-theme)
(use-package git-timemachine)

(use-package auto-complete)
(use-package go-autocomplete
  :config
  (ac-set-trigger-key "TAB"))

;; GOLANG
(use-package go-mode)
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
(setenv "GOPATH" "/Users/piotr/devel/go-workspace")
(add-to-list 'exec-path "/Users/piotr/devel/go-workspace/bin")
(add-hook 'go-mode-hook 'my-go-mode-hook)
(with-eval-after-load 'go-mode
  (require 'go-autocomplete))
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; PYTHON
(use-package elpy
  :config
  (elpy-enable))

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

;;When opening several buffers with different names, this gives them different
;;names based on their folder
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;Super M-x, a supercharged M-x mode, much like ido, only for M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("c1afd422fab9cb7ad2422523d0491abc8f397fea76ec57bf27c70b50bfa04243" "109c37722f5b922ab2c023d364bbe3bd1e2a49e6c3267ff7bca2ccdddbf9f9c2" default)))
 '(max-lisp-eval-depth 1500)
 '(max-specpdl-size 3000)
 '(org-pretty-entities t)
 '(package-selected-packages
   (quote
    (elpy pastelmac-theme evil use-package markdown-mode)))
 '(show-paren-mode t)
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
