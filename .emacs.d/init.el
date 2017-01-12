;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; setup packages
(package-initialize)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'install-packages)

(require 'evil)
(evil-mode t)

(exec-path-from-shell-initialize)

;;; Buffer menu
(global-set-key [f5] 'buffer-menu)

(global-git-gutter-mode +1)

(fset 'yes-or-no-p 'y-or-n-p)

(windmove-default-keybindings 'shift)

;; ivy + swiper + counsel
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

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

(require 'org)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-startup-indented 1)
(setq org-capture-templates
      '(("t" "technical" entry (file "~/org/technical.org" "Tasks")
         "* %?\nEntered on %U\n")
        ("w" "work" entry (file+datetree "~/org/work-journal.org" "Work journal")
         "* %?\nEntered on %U\n"
         :emptylines 1)
        ("i" "idea" entry (file "~/org/ideas.org")
         "* %?\nEntered on %U\n")
        ("d" "doodles" entry (file "~/org/doodles.org")
         "* %? %U\n")))
(setq org-todo-keywords
      '((sequence "IDEA" "TODO" "INPROGRESS" "ON HOLD" "BLOCKED" "|" "DONE" "DELEGATED")))
(setq org-export-initial-scope
      'subtree)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t))) ; this line activates ditaa

(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-flyspell)

(load custom-file)
