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

(package-refresh-contents)

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(python-mode 
        magit 
        yasnippet 
        jedi 
        auto-complete 
        autopair 
        find-file-in-repository 
        flycheck 
        flyspell 
        org 
        auctex
        multiple-cursors
        ido-ubiquitous
        latex-pretty-symbols
        smex
        undo-tree
        color-theme
        markdown-mode
        pandoc-mode
        color-theme-solarized
        langtool))

(mapc 'install-if-needed to-install)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/texbin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/usr/texbin")))


(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(setq inhibit-startup-screen t
      initial-scratch-message "*scratch*\n\n")

(setq echo-keystrokes 0.4
      debug-on-error nil
      stack-trace-on-error nil
      standard-indent 4
      tab-always-indent 'complete
      grep-scroll-output t)

(setq-default comment-column 42
	      fill-column 78
	      indent-tabs-mode nil
	      tab-width 4
	      word-wrap t)

(show-paren-mode t)

(require 'magit)
(global-set-key "\C-xg" 'magit-status)

(require 'auto-complete)
(require 'autopair)
(require 'yasnippet)
(require 'flycheck)
(global-flycheck-mode t)

(global-set-key [f7] 'find-file-in-repository)

; auto-complete mode extra settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)

;; ;; Python mode settings
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)

;; ;; Jedi settings
(require 'jedi)
;; It's also required to run "pip install --user jedi" and "pip
;; install --user epc" to get the Python side of the library work
;; correctly.
;; With the same interpreter you're using.

;; if you need to change your python intepreter, if you want to change it
(setq jedi:server-command
      '("/usr/local/bin/python2" "/Users/piotr/.emacs.d/elpa/jedi-20140204.2226/jediepcserver.py"))

(add-hook 'python-mode-hook
      (lambda ()
        (jedi:setup)
        (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)))


(add-hook 'python-mode-hook 'auto-complete-mode)

(ido-mode t)

;; -------------------- extra nice things --------------------
;; use shift to move around windows
(windmove-default-keybindings 'shift)
(show-paren-mode t)
 ; Turn beep off
(setq visible-bell nil)

(custom-set-variables
 '(LaTeX-indent-environment-list (quote (("verbatim" current-indentation) ("verbatim*" current-indentation) ("array") ("displaymath") ("eqnarray") ("eqnarray*") ("equation") ("equation*") ("picture") ("tabbing") ("table") ("table*") ("tabular") ("tabular*") ("comment"))))
 '(LaTeX-math-abbrev-prefix "<")
 '(LaTeX-paragraph-commands (quote ("comment")))
 '(ac-auto-show-menu 0.001)
 '(ac-delay 0.0)
 '(ac-modes (quote (coq-mode emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode perl-mode cperl-mode python-mode ruby-mode lua-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode ts-mode)))
 '(auto-completion-min-chars 3)
 '(auto-completion-source (quote predictive))
 '(auto-completion-syntax-alist (quote (accept . word)))
 '(completion-use-echo nil)
 '(cua-mode t nil (cua-base))
 '(dynamic-fonts-preferred-monospace-point-size 14)
 '(dynamic-fonts-preferred-proportional-point-size 14)
 '(electric-pair-pairs (quote ((36 . 36) (34 . 34))))
 '(max-lisp-eval-depth 1500)
 '(max-specpdl-size 3000)
 '(org-pretty-entities t)
 '(predictive-add-to-dict-ask nil)
 '(predictive-auto-add-min-chars 3)
 '(predictive-auto-add-to-dict t)
 '(predictive-main-dict (quote logicdict))
 '(preview-default-option-list (quote ("displaymath" "floats" "graphics" "textmath" "sections" "footnotes")))
 '(preview-default-preamble (quote ("\\RequirePackage[" ("," . preview-default-option-list) "]{preview}[2004/11/05]" "\\PreviewEnvironment{tikzpicture}")))
 '(safe-local-variable-values (quote ((eval ispell-change-dictionary "nb") (TeX-master . t))))
 '(sentence-end nil)
 '(transient-mark-mode t)
 '(which-func-modes (quote (emacs-lisp-mode c-mode c++-mode perl-mode cperl-mode python-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode diff-mode LaTeX-mode latex-mode)))
 '(which-function-mode t))

;; kinda like ST2!
(require 'multiple-cursors)
(global-set-key (kbd "S-d") 'mc/mark-all-like-this-dwim)

(setq ido-fil-extensions-order '(".tex" ".bib" ".el" ".org" ".md"))

;;Extensions to ignore when autocompleting on files
(setq ido-ignore-extensions t)
(add-to-list 'completion-ignored-extensions ".pdf")
(add-to-list 'completion-ignored-extensions ".aux")
(add-to-list 'completion-ignored-extensions ".log")
(add-to-list 'completion-ignored-extensions ".toc")
(add-to-list 'completion-ignored-extensions ".gls")
(add-to-list 'completion-ignored-extensions ".ilg")
(add-to-list 'completion-ignored-extensions ".ind")
(add-to-list 'completion-ignored-extensions ".out")
(add-to-list 'completion-ignored-extensions ".rip")
(add-to-list 'completion-ignored-extensions ".thm")
(add-to-list 'completion-ignored-extensions ".nlo")
(add-to-list 'completion-ignored-extensions ".nls")
(add-to-list 'completion-ignored-extensions ".rel")
(add-to-list 'completion-ignored-extensions ".pdfsync")
(add-to-list 'completion-ignored-extensions ".synctex.gz")

(line-number-mode 1)
(column-number-mode 1)

;; backups
(setq
    backup-by-copying t
    backup-directory-alist '(("." . "~/.emacs_backups"))
    delete-old-versions t
    kept-new-versions 6
    kept-old-versions 2
    version-control t)

;; spellcheck
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

;; grammar check
(require 'langtool)
(setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/2.3/libexec/languagetool-commandline.jar"
      langtool-mother-tongue "pl"
      langtool-disabled-rules '("WHITESPACE_RULE"
                                "EN_UNPAIRED_BRACKETS"
                                "COMMA_PARENTHESIS_WHITESPACE"
                                "EN_QUOTES"))

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

(require 'undo-tree)

(require 'color-theme)
(load-theme 'solarized-light t)


;; TeX
(require 'tex-site)
(require 'latex-pretty-symbols)
;; (setq jit-lock-defer-time 0)

;;Replaces the query-replace key to the reftex one that does query-replace in the whole document.
(add-hook 'LaTeX-mode-hook '(lambda()
                              (local-set-key (kbd "M-%") 'reftex-query-replace-document)))

;;replaces the fill-paragraph with the latex-specific one
(add-hook 'LaTeX-mode-hook '(lambda()
                              (local-set-key (kbd "M-q") 'LaTeX-fill-section)))

;; (add-hook 'LaTeX-mode-hook 'orgtbl-mode)
(add-hook 'LaTeX-mode-hook 'rainbow-delimiters-mode)
                              

;;The following makes C-c-c not ask, just do the default action. Adds C-c-a for asking
(setq TeX-command-force "")
(add-hook 'LaTeX-mode-hook
          '(lambda()
             (define-key LaTeX-mode-map "\C-c\C-a"   ; 'a' for ask, change to anything you   want
               (lambda (arg) (interactive "P")
                 (let ((TeX-command-force nil))
                   (TeX-command-master arg))))))


(setq-default TeX-master nil)
(setq TeX-parse-self t)
(setq TeX-auto-save t)

;;Enables some modes when a latex file is opened
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
(add-hook 'LaTeX-mode-hook 'abbrev-mode)

;;Compile to pdf
(setq TeX-PDF-mode t)

;;Inserts {} automaticly on _ and ^
(setq TeX-electric-sub-and-superscript t)


;;Loads and sets up reftex
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)


(setq LaTeX-eqnarray-label "eq"
      LaTeX-equation-label "eq"
      LaTeX-figure-label "fig"
      LaTeX-table-label "tab"
      LaTeX-myChapter-label "cha"
      TeX-auto-save t
      TeX-newline-function 'reindent-then-newline-and-indent
      TeX-parse-self t
      TeX-style-path '("style/" "auto/"
		       "/usr/share/emacs21/site-lisp/auctex/style/"
		       "/var/lib/auctex/emacs21/"
		       "/usr/local/share/emacs/site-lisp/auctex/style/")
      LaTeX-section-hook
      '(LaTeX-section-heading
	LaTeX-section-title
	;; LaTeX-section-toc
	LaTeX-section-section
	LaTeX-section-label))

;;Lets reftex know what new enviorments I have, and what labels it should make
(setq reftex-label-alist
      '(("lemma"   ?l "lem:"  "~\\ref{%s}" t ("lemma"   "lm.") -3)
	("proof"   ?p "proof:"  "~\\ref{%s}" t ("proof"   "pr.") -3)
	("claim"   ?m "claim:"  "~\\ref{%s}" t ("claim"   "clm.") -3)
	("fact"   ?a "fact:"  "~\\ref{%s}" t ("fact"   ) -3)
	("corollary"   ?c "cor:"  "~\\ref{%s}" t ("corollary"   "cor.") -3)
	("conjecture"   ?o "conj:"  "~\\ref{%s}" t ("conjecture"   "conj.") -3)
	("definition"   ?d "def:"  "~\\ref{%s}" t ("definition"   "def.") -3)
	("example"   ?x "ex:"  "~\\ref{%s}" t ("example"   "ex.") -3)
	("notation"   ?t "not:"  "~\\ref{%s}" t ("notation"   "not.") -3)
	("theorem" ?h "thr:" "~\\ref{%s}" t   ("theorem" "th.") -3)
	("quote"   ?d "quote:"  "~\\ref{%s}" nil ("quote" ))
	("proposition"   ?p "prop:"  "~\\ref{%s}" nil ("proposition"   "prop."))
	("axiom"   ?m "ax:"  "~\\ref{%s}" nil ("axiom"   "ax."))))
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (LaTeX-add-environments
	     '("lemma" LaTeX-env-label)
	     '("claim" LaTeX-env-label)
	     '("proof" LaTeX-env-label)
	     '("fact"     LaTeX-env-label)
	     '("corollary"  LaTeX-env-label)
	     '("conjecture" LaTeX-env-label)
	     '("definition" LaTeX-env-label)
	     '("example"	   LaTeX-env-label)
	     '("notation"   LaTeX-env-label)
	     '("theorem" LaTeX-env-label)
	     '("quote"	 LaTeX-env-label)
	     '("proposition"  LaTeX-env-label)
	     '("axiom" LaTeX-env-label))))

(setq
 ;reftex-cite-format 'natbib
 reftex-default-bibliography
 '("~/Documents/library.bib")
 reftex-extra-bindings t
 reftex-plug-into-AUCTeX t
 reftex-save-parse-info t
 reftex-sort-bibtex-matches 'author
 reftex-toc-include-labels nil
 reftex-toc-follow-mode nil)
