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


;; helm
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x m") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
(setq helm-locate-fuzzy-match t)
;;; end helm

(set-face-attribute 'default nil :family "Source Code Pro")

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
