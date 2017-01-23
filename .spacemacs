;; -*- mode: emacs-lisp -*- ;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Uncomment some layer names and press <SPC f e R>
     ;; ----------------------------------------------------------------
     auto-completion
     syntax-checking
     emacs-lisp
     git
     ivy
     osx
     markdown
     version-control
     yaml

     (org :variables
          org-enable-github-support t
          org-startup-indented t)

     ;; languages
     haskell
     javascript
     (python :variables
             python-fill-column 99
             python-sort-imports-on-save t
             python-enable-yapf-format-on-save t)
     (go :variables
         gofmt-command "goimports"
         go-tab-width 2)

     )
   dotspacemacs-additional-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-themes '(solarized-light
                         solarized-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-use-ido nil
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   )
  )

(defun dotspacemacs/user-config ()
  (setq powerline-default-separator nil)

  ;; ORG
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
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(delete-selection-mode nil)
 '(org-agenda-files (quote ("~/org/projects.org" "~/org/work-met.org")))
 '(package-selected-packages
   (quote
    (intero hlint-refactor hindent haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern tern coffee-mode hide-comnt go-guru salt-mode mmm-jinja2 deft wgrep smex ivy-hydra counsel-projectile counsel-dash dash-functional counsel swiper ivy yapfify uuidgen rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake py-isort ox-gfm osx-dictionary org-projectile org org-download live-py-mode link-hint jinja2-mode git-link eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff dumb-jump column-enforce-mode chruby bundler inf-ruby log4e gntp request gitignore-mode fringe-helper git-gutter+ git-gutter pkg-info epl flx pos-tip pythonic yaml-mode window-numbering volatile-highlights toc-org spaceline powerline smooth-scrolling restart-emacs pyvenv persp-mode paradox hydra page-break-lines osx-trash orgit org-pomodoro org-plus-contrib open-junk-file neotree markdown-toc markdown-mode magit-gitflow leuven-theme info+ indent-guide hl-todo highlight-numbers helm-swoop helm-projectile helm-make projectile helm-descbinds helm-dash helm-company helm-c-yasnippet helm-ag google-translate gitconfig-mode git-timemachine git-messenger expand-region exec-path-from-shell evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup evil-exchange evil-escape company-anaconda buffer-move auto-yasnippet yasnippet auto-compile packed anaconda-mode aggressive-indent ace-link auto-complete avy company anzu iedit smartparens highlight flycheck git-commit with-editor go-mode helm popup helm-core async f dash s quelpa package-build use-package which-key bind-map evil spacemacs-theme ws-butler vi-tilde-fringe undo-tree stickyfunc-enhance srefactor spinner smeargle reveal-in-osx-finder rainbow-delimiters pytest pyenv-mode py-yapf puppet-mode popwin pip-requirements pcre2el pbcopy parent-mode org-repo-todo org-present org-bullets move-text mmm-mode macrostep lorem-ipsum linum-relative let-alist launchctl ido-vertical-mode hy-mode hungry-delete htmlize highlight-parentheses highlight-indentation help-fns+ helm-themes helm-pydoc helm-mode-manager helm-gitignore helm-flx goto-chg golden-ratio go-eldoc gnuplot gitattributes-mode git-gutter-fringe git-gutter-fringe+ gh-md flycheck-pos-tip flx-ido fill-column-indicator fancy-battery evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-lisp-state evil-indent-plus evil-iedit-state evil-args evil-anzu eval-sexp-fu elisp-slime-nav diminish diff-hl define-word dash-at-point cython-mode company-statistics company-quickhelp company-go clean-aindent-mode bracketed-paste bind-key auto-highlight-symbol ansible-doc ansible alert adaptive-wrap ace-window ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(tool-bar-mode nil)
 '(truncate-lines nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro for Powerline" :foundry "nil" :slant normal :weight normal :height 130 :width normal))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
