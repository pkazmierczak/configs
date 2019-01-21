;; -*- mode: emacs-lisp -*- ;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Uncomment some layer names and press <SPC f e R>
     ;; ----------------------------------------------------------------
     auto-completion
     emacs-lisp
     git
     ivy
     osx
     ranger
     syntax-checking
     (version-control :variables
                      version-control-global-margin t
                      version-control-diff-side 'left)
     yaml

     (go :variables
         go-use-metalinter t
         go-tab-width 4
         gofmt-command "goimports")
     (org :variables
          org-enable-github-support t
          org-startup-indented t)
     terraform
     )
   dotspacemacs-additional-packages '(protobuf-mode exec-path-from-shell)
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
   dotspacemacs-themes '(spacemacs-light
                         solarized-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro for Powerline"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-use-ido nil
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-mode-line-theme 'spacemacs
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
  (exec-path-from-shell-initialize)
  (setq powerline-default-separator 'arrow)
  (with-eval-after-load 'linum
    (linum-relative-toggle))

  ;; ORG
  (setq org-capture-templates
        '(("t" "technical" entry (file "~/Dropbox/docs/org/technical.org" "Tasks")
           "* %?\nEntered on %U\n")
          ("w" "work" entry (file+datetree "~/Dropbox/docs/org/work-journal.org" "Work journal")
           "* %?\nEntered on %U\n"
           :emptylines 1)
          ("i" "idea" entry (file "~/Dropbox/docs/org/ideas.org")
           "* %?\nEntered on %U\n")
          ("d" "doodles" entry (file "~/Dropbox/docs/org/doodles.org")
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

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

