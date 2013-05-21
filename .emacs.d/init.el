(setq user-full-name "Piotr Kaźmierczak")
(setq user-mail-address "me@piotrkazmierczak.com")

(server-start 0)

(tool-bar-mode 0)
(menu-bar-mode 1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(desktop-save-mode 0)
(setq *have-slime* nil)

(global-auto-revert-mode 1)

(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:/usr/local/texlive/2012/bin/x86_64-darwin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path)
  (push "/usr/texbin" exec-path)
  (push "/usr/local/bin" exec-path)
  (set-face-attribute 'default nil :family "Inconsolata" :height 140)
  (global-set-key [f5] 'buffer-menu);; menu buforow
  (defun maximize-frame () 
    (interactive)
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame) 1000 1000))
  
  (global-set-key (kbd "<M-S-return>") 'maximize-frame)
  (setq ns-right-alternate-modifier nil)
)

(setq-default indent-tabs-mode nil)

(add-to-list 'load-path
	     "~/.emacs.d")

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

(require 'ido)
(ido-mode t)
(setq find-file-wildcards t)

(require 'color-theme)
(require 'color-theme-pastelmac)
(setq color-theme-is-global t);(color-theme-initialize)
(color-theme-pastelmac)

(defun toggle-night-color-theme ()
  "Switch to/from night color scheme."
  (interactive)
  (require 'color-theme)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (color-theme-snapshot) ; restore default (light) colors
    ;; create the snapshot if necessary
    (when (not (commandp 'color-theme-snapshot))
      (fset 'color-theme-snapshot (color-theme-make-snapshot)))
    (color-theme-twilight)))

(global-set-key (kbd "<f6>") 'toggle-night-color-theme)
(load-library "color-theme")

; markdown mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t) 
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

; pandoc mode
(require 'pandoc-mode)

;;;;;;;;;;;;;;;;;; org-mode

(setq load-path (cons "~/.emacs.d/org/licp" load-path))
(setq load-path (cons "~/.emacs.d/org/contrib/lisp" load-path))
(require 'org-install)
(setq org-agenda-files (list "~/Documents/org/research-todos.org"))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-font-lock-mode 1)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;(add-hook 'org-mode-hook 'turn-on-visual-line)
(setq org-log-done 'time)

(require 'org-latex)

(add-to-list 'org-export-latex-classes
          '("koma"
             "\\documentclass{scrartcl}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-export-latex-classes
             '("shortpaperen"
               "\\documentclass{article}
               \\usepackage{ucs}
               \\usepackage[utf8]{inputenc}
               \\usepackage{geometry}
               \\usepackage{hyperref}
               \\usepackage{amsfonts}
               \\usepackage{amssymb}
               \\usepackage{amsmath}
               \\usepackage{amsthm}
               \\usepackage{stmaryrd}
               \\usepackage[boxed,noend,noline]{algorithm2e}
               \\setlength{\\parindent}{0pt}
               \\setlength{\\parskip}{\\medskipamount}
               \\input{/Users/piotr/Documents/my-tex-commands.tex}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-export-latex-classes
             '("shortpaperpl"
               "\\documentclass[a4paper,10pt]{article}
                \\usepackage{ucs}
               \\usepackage[utf8x]{inputenc}
               \\usepackage[T1]{polski}
               \\usepackage{indentfirst}
               \\usepackage{amsfonts}
               \\usepackage{amssymb}
               \\usepackage{amsmath}
               \\usepackage{amsthm}
               \\usepackage{algorithmic}
               \\usepackage{times}
               %\\usepackage{ulem}
               \\usepackage{graphicx}
               \\usepackage[all]{xy}
               %\\usepackage{pstricks}
               \\usepackage{hyperref}
               \\pagestyle{empty}
               \\newcommand{\\wazne}[1]{
                 \\textsc{#1}}
               \\newtheorem{tw}{Twierdzenie}[section]
               \\newtheorem{lm}{Lemat}[section]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

; minted latex export
(setq org-export-latex-listings 'minted)
(setq org-export-latex-minted-options
      '(("frame" "lines")
        ("fontsize" "\\footnotesize")
        ("linenos" "")))
;(setq org-latex-to-pdf-process
;      '("pdflatex -shell-escape -interaction nonstopmode %s"
;        "pdflatex -shell-escape -interaction nonstopmode %s"
;        "pdflatex -shell-escape -interaction nonstopmode %s"))


;;;;; mobile-org
(setq org-directory "~/Documents")
(setq org-mobile-inbox-for-pull "~/Documents/research-todos-flagged.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")

;;;;;;;;;;;;;; TeX

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq preview-image-type (quote dvipng))

(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)
;(add-hook 'LaTeX-mode-hook (lambda ()
;                            (TeX-fold-mode 1)))

(add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)

(autoload 'LaTeX-preview-setup "preview")

(require 'latex-pretty-symbols)

;;Inserts {} automaticly on _ and ^
(setq TeX-electric-sub-and-superscript t)

;(require 'flymake)
 
;(defun flymake-get-tex-args (file-name)
;	(list "pdflatex" 
;		(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))
 
;; Konfiguracja FlySpell (sprawdzanie błędów ortograficznych w locie)
(setq ispell-program-name "aspell")
(setq ispell-dictionary "english")

; auto-fill is enabled for TeX...
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
; ...unless I work with the gang
(defun my-auto-fill-disabling-hook ()
  "Check to see if we should disable autofill."
  (save-excursion
    (when (or (re-search-forward "truls" 1000 t)
              (re-search-forward "sjur" 1000 t)
              (re-search-forward "erik" 1000 t))
      (auto-fill-mode -1))))
(add-hook 'find-file-hooks 'my-auto-fill-disabling-hook)
 
;(add-hook 'LaTeX-mode-hook 'flymake-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-buffer)

(setq flyspell-issue-message-flag nil)

; http://stackoverflow.com/questions/8847952/is-it-possible-to-disable-a-minor-mode-e-g-flyspell-on-a-per-file-basis
(defun my-no-flyspell-mode (&optional rest)
  (flyspell-mode -1))

(global-set-key [f5] 'buffer-menu);; menu buforow

;(setq TeX-outline-extra
;      '(("[ \t]*\\\\\\(bib\\)?item\\b" 7)
;("\\\\bibliography\\b" 2)))

;; Magic space by Michal Jankowski <michalj@fuw.edu.pl>
;; 'Ręcznie' można ją włączyć za pomocą M-x local-set-key SPC tex-magic-space.
;; Można też użyć funkcji tex-toggle-magic-space, przypisanej do C-c SPC.
(defun tex-magic-space ()
  "Magic-space - inserts non-breakable space after a single-letter word."
  (interactive)
  (if (string-match
       "^\\(\\s \\|~\\)?[aeiouwz]$"
       (buffer-substring (max (point-min) (- (point) 2)) (point)))
      (insert "~")
    (insert " ")
    (and auto-fill-function
         (funcall auto-fill-function))))

;; Przypisuje/wyłącza przypisanie tex-magic-space do spacji,
;; (przydatne przy pisaniu matematyki), tylko dla trybów LaTeX-owych
(defun tex-toggle-magic-space ()
  "Toggles TeX magic space, which inserts non-breakable space after a single-letter word"
  (interactive)
;   (if (local-key-binding " " 'tex-magic-space)
;      (local-unset-key " ")
;     (local-set-key " " tex-magic-space)
  (progn
    (if (equal (lookup-key TeX-mode-map " ") 'tex-magic-space)
	(progn
	  (define-key TeX-mode-map " " nil)
	  (local-unset-key " ")) ; to be sure
      (define-key TeX-mode-map " " 'tex-magic-space))
    (message "SPC is binded to %s" (lookup-key TeX-mode-map " "))))

;;Replaces the query-replace key to the reftex one that does query-replace in the whole document.
(add-hook 'LaTeX-mode-hook '(lambda()
                              (local-set-key (kbd "M-%") 'reftex-query-replace-document)))

;;replaces the fill-paragraph with the latex-specific one
;(add-hook 'LaTeX-mode-hook '(lambda()
;                              (local-set-key (kbd "M-q") 'LaTeX-fill-section)))

;;The following makes C-c-c not ask, just do the default action. Adds C-c-a for asking
(setq TeX-command-force "")
(add-hook 'LaTeX-mode-hook
          '(lambda()
             (define-key LaTeX-mode-map "\C-c\C-a"   ; 'a' for ask, change to anything you   want
               (lambda (arg) (interactive "P")
                 (let ((TeX-command-force nil))
                   (TeX-command-master arg))))))

; synctex + Skim
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)
(add-hook 'LaTeX-mode-hook
          (lambda()
            (add-to-list 'TeX-expand-list
                         '("%q" skim-make-url))))
(defun skim-make-url () (concat
                         (TeX-current-line)
                         " "
                         (expand-file-name (funcall file (TeX-output-extension) t)
                                           (file-name-directory (TeX-master-file)))
                         " "
                         (buffer-file-name)))
(setq TeX-view-program-list
      '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %q")))
(setq TeX-view-program-selection '((output-pdf "Skim")))


;; ------------------------------------------------------------------------
;; Przypisania klawiszy dla trybów LaTeX-owych

;; Automatyczne włączenie magicznej spacji (przywiązuje samotne
;; spójniki do nastepujących po nich wyrazów; przykład : a~teraz)
;; For AUC TeX
(eval-after-load "tex" '(define-key TeX-mode-map " " 'tex-magic-space))
(eval-after-load "tex" '(define-key TeX-mode-map "\C-c " 'tex-toggle-magic-space))
;; For tex-mode included in Emacs
(eval-after-load "tex-mode" '(define-key tex-mode-map " " 'tex-magic-space))
(eval-after-load "tex-mode" '(define-key tex-mode-map "\C-c " 'tex-toggle-magic-space))



;;; ====================================================================
;;; Outline mode

;; Definicja funkcji do włączenia outline mode
(defun turn-on-outline-minor-mode ()
  (outline-minor-mode 1))

;; Automatyczne włączenie outline mode dla odpowiednich trybów
(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
;(add-hook 'emacs-lisp-mode-hook 'turn-on-outline-minor-mode)
;(add-hook 'TeXinfo-mode-hook 'turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c\C-o") ; Or whatever...


;;; =====================================================================
;;; RefTeX

(require 'tex-site)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'TeX-mode-hook 'turn-on-reftex)

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
       	("theorem" ?h "thr:" "~\\ref{%s}" t   ("theorem" "th.") -3)
	("quote"   ?d "quote:"  "~\\ref{%s}" nil ("quote" ))
	("proposition"   ?p "prop:"  "~\\ref{%s}" nil ("proposition"   "prop."))
	("axiom"   ?m "ax:"  "~\\ref{%s}" nil ("axiom"   "ax."))
        ("\\todo{}" ?x nil nil t ("todo" "TODO") -3)))
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


(setq reftex-cite-format 'natbib
      reftex-default-bibliography
      '("~/Documents/library.bib")
      reftex-extra-bindings t
      reftex-plug-into-AUCTeX t
      reftex-sort-bibtex-matches 'year
      reftex-toc-mode-hook nil
      reftex-toc-include-labels t
      reftex-toc-follow-mode t
)

(defun my-org-mode-setup ()
  (when (and (buffer-file-name)
             (file-exists-p (buffer-file-name)))
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all))
    (reftex-set-cite-format
     '(
       (?b . "[[citep][%l]]")
       (?p . "%l")
       (?c . "[[citep::%l]]")
       (?n . "[[note::%l]]")
       (?f . "[[file:///Users/piotr/Documents/papers/%l-AA.pdf][%l]]"))))
  (define-key org-mode-map "\C-c\C-g" 'reftex-citation)
  )
(add-hook 'org-mode-hook 'my-org-mode-setup)


;;; =====================================================================
;;; YASnippet

(require 'yasnippet-bundle)
(yas/load-directory (expand-file-name "snippets" "~/.emacs.d"))


;;; =====================================================================
;;; Haskell

(add-to-list 'auto-mode-alist
             '("\\.hs$" . haskell-mode))
(require 'haskell-mode)

(add-to-list 'auto-mode-alist
              '("\\.l[hg]s$" . latex-lhs-mode))
(require 'latex-lhs-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(setq haskell-program-name "/usr/bin/ghci")

;(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

(require 'inf-haskell)

;(setq haskell-font-lock-symbols t)


;;; =====================================================================
;;; Scala

(add-to-list 'load-path "~/.emacs.d/scala-mode")
(require 'scala-mode)

;; (setq yas/my-directory "~/.emacs.d/scala-mode/contrib/yasnippet/snippets")
;;   (yas/load-directory yas/my-directory)

;; (add-hook 'scala-mode-hook
;;             '(lambda ()
;;                (yas/minor-mode-on)
;;                ))

;; ENSIME
(add-to-list 'load-path "~/.emacs.d/ensime/elisp/")
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;(add-hook 'ensime-source-buffer-saved-hook 'ensime-show-all-errors-and-warnings)

; http://stackoverflow.com/questions/12725667/auto-refresh-ensime-error-buffer/14082484
(add-hook 'ensime-source-buffer-saved-hook 
          'ensime-show-all-errors-and-warnings)

;;; ======================================================================
;;; Prolog

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.prolog$" . prolog-mode)
				("\\.m$" . mercury-mode))
			      auto-mode-alist))
			      
			      
;;; ======================================================================
;;;; SMV mode
(autoload 'smv-mode "smv-mode" "SMV specifications editing mode." t)
(setq auto-mode-alist 
      (append  (list '("\\.smv$" . smv-mode) '("\\.ord$" . smv-ord-mode))
	       auto-mode-alist))
(setq completion-ignored-extensions
      (cons ".ord" (cons ".opt" completion-ignored-extensions)))


;;; ======================================================================
;;;; Mercurial

(require 'mercurial)

;;; ======================================================================
;;;; Full-screen hacks and other stuff by Truls

(global-set-key [f11] 'fullscreen)

(defun fullscreen (&optional f)
  (interactive)
  (menu-bar-mode 1)
  (tool-bar-mode 0)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0))
  )

(global-set-key (kbd "s--") 'shrink-window-if-larger-than-buffer)

(global-set-key [C-s-left] (lambda () (interactive) 
                             (shrink-window-horizontally 5) ) )
(global-set-key [C-s-right] (lambda () (interactive)
                              (shrink-window-horizontally -5) ) )
(global-set-key [C-s-up] (lambda () (interactive) (shrink-window 5) ))
(global-set-key [C-s-down] (lambda () (interactive) (shrink-window -5) ) )

(global-set-key [s-left]  (lambda () (interactive) (other-window -1)) )
(global-set-key [s-right] (lambda () (interactive) (other-window  1)) )

(when (equal system-type 'darwin)
  (global-set-key [A-left]  (lambda () (interactive) (other-window -1)))
  (global-set-key [A-right] (lambda () (interactive) (other-window 1)))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Documents/notes/research-journal.org" "~/Documents/org/research-todos.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
