;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; my emacs conf
(setq user-full-name "Piotr Kazmirczak")
(setq user-email-address "me@piotrkazmierczak.com")

;; Requisites: Emacs >= 24
(require 'package)
(package-initialize)

(add-to-list 'package-archives
         '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
         '("marmalade" . "http://marmalade-repo.org/packages/") t)

(when
    (not package-archive-contents)
  (package-refresh-contents))

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(magit
        org 
        ido-ubiquitous
        smex
        undo-tree
        color-theme
        markdown-mode
        pandoc-mode
        color-theme-solarized
        git-gutter
        git-timemachine
        rainbow-delimiters
        cider
        ac-slime
        clojure-mode
        paredit
        company))

(mapc 'install-if-needed to-install)

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t
      initial-scratch-message "*scratch*\n\n")

;;; PATH
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/texbin")


;;; FONTS
(when (eq system-type 'darwin)

      ;; default Latin font (e.g. Consolas)
      (set-face-attribute 'default nil :family "Source Code Pro for Powerline")

      ;; default font size (point * 10)
      ;;
      ;; WARNING!  Depending on the default font,
      ;; if the size is not supported very well, the frame will be clipped
      ;; so that the beginning of the buffer may not be visible correctly. 
      (set-face-attribute 'default nil :height 130))


;;; Buffer menu
(global-set-key [f5] 'buffer-menu)


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

(custom-set-variables
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(max-lisp-eval-depth 1500)
 '(max-specpdl-size 3000)
 '(org-pretty-entities t)
 '(show-paren-mode t)
 '(which-function-mode t))


;; backups
(setq
    backup-by-copying t
    backup-directory-alist '(("." . "~/.emacs_backups"))
    delete-old-versions t
    kept-new-versions 6
    kept-old-versions 2
    version-control t)


;; GIT
(require 'magit)
(require 'git-gutter)
(global-set-key "\C-xg" 'magit-status)
(global-git-gutter-mode +1)

;; use shift to move around windows
(windmove-default-keybindings 'shift)



;; spellcheck
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))



;; Markdown and Pandoc
(autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(load "pandoc-mode")
(add-hook 'markdown-mode-hook 'turn-on-pandoc)



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


;; Undo!
(require 'undo-tree)
(global-undo-tree-mode)



;; COLOR THEME
(require 'color-theme)
(load-theme 'solarized t)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))
