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
        auctex
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

      (set-face-attribute 'default nil :family "Source Code Pro for Powerline")

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
(load-theme 'solarized-light t)
;; Set initial theme to "dark"
(setq dark-or-light 'dark)
(color-theme-solarized dark-or-light)
 
;; Shortcut to toggle between light and dark
(global-set-key (kbd "C-c d")
                (lambda ()
                  (interactive)
                  (if (eq dark-or-light 'light)
                      (setq dark-or-light 'dark)
                    (setq dark-or-light 'light)
                    )
                  (color-theme-solarized dark-or-light)))



;;; Clojure!
(global-set-key (kbd "C-c C-j") 'cider-jack-in)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-log-messages t)
(setq nrepl-hide-special-buffers t)
(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'company-mode)






;;; LATEX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode nil)

(require 'flymake)
(load "preview.el" nil t t)


(defun flymake-get-tex-args (file-name)
(list "pdflatex"
(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(add-hook 'LaTeX-mode-hook 'flymake-mode)

(setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
(setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

(defun turn-on-outline-minor-mode ()
(outline-minor-mode 1))

(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c \C-o") ; Or something else

(require 'tex-site)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq LaTeX-eqnarray-label "eq"
LaTeX-equation-label "eq"
LaTeX-figure-label "fig"
LaTeX-table-label "tab"
LaTeX-myChapter-label "chap"
TeX-auto-save t
TeX-newline-function 'reindent-then-newline-and-indent
TeX-parse-self t
TeX-style-path
'("style/" "auto/"
"/usr/share/emacs21/site-lisp/auctex/style/"
"/var/lib/auctex/emacs21/"
"/usr/local/share/emacs/site-lisp/auctex/style/")
LaTeX-section-hook
'(LaTeX-section-heading
LaTeX-section-title
LaTeX-section-toc
LaTeX-section-section
LaTeX-section-label))
