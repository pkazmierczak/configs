;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

(package-initialize)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(defvar my-packages
  '(company
    company-lsp
    counsel
    elpy
    evil
    exec-path-from-shell
    geiser
    go-add-tags
    go-autocomplete
    go-eldoc
    go-guru
    go-mode
    ivy
    lsp-mode
    lsp-ui
    json-mode
    magit
    git-gutter
    git-timemachine
    markdown-mode
    projectile
    rainbow-delimiters
    solarized-theme
    swiper))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))


(when (not package-archive-contents)
    (package-refresh-contents))
(package-initialize)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'evil)
(evil-mode t)

(load-theme 'solarized-light t)

(exec-path-from-shell-initialize)

(global-git-gutter-mode +1)

(fset 'yes-or-no-p 'y-or-n-p)

(windmove-default-keybindings 'shift)

;; ivy + swiper + counsel
;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)


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

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'subword-mode)

;; LSP
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;;Set up before-save hooks to format buffer and add/delete imports.
;;Make sure you don't have other gofmt/goimports hooks enabled.

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;;Optional - provides fancier overlays.

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
)

;;Company mode is a standard completion package that works well with lsp-mode.
;;company-lsp integrates company mode completion with lsp-mode.
;;completion-at-point also works out of the box but doesn't support snippets.

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; md and json
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))

;; scheme
(setq geiser-mit-binary "/usr/local/bin/mit-scheme")
(setq geiser-active-implementations '(mit))

(load custom-file)
