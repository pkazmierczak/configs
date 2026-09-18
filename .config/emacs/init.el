;;; init.el --- entry point -*- lexical-binding: t; -*-
;;
;; Requires Emacs >= 29.
;;
;; Everything Emacs can do on its own is left to Emacs: LSP (eglot),
;; diagnostics (flymake), completion-at-point, project management
;; (project.el), file browsing (dired), version control (vc + magit),
;; tree-sitter major modes. Packages only fill the remaining gaps.
;;
;; Layout:
;;   early-init.el        -- runs before the UI, see that file
;;   init.el               -- this file
;;   lisp/init-*.el        -- the actual config, one topic per file
;;   straight/versions/default.el -- plugin lockfile, commit this

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-straight)     ; package manager bootstrap (straight.el + use-package)
(require 'init-defaults)     ; core options
(require 'init-ui)           ; theme, mode-line, which-key
(require 'init-completion)   ; vertico/orderless/corfu/consult minibuffer+completion stack
(require 'init-evil)         ; vim keybindings
(require 'init-editor)       ; editing behaviour, autocmd-equivalents, TODO highlighting, surround
(require 'init-eglot)        ; LSP client config (built into Emacs)
(require 'init-go)           ; Go
(require 'init-rust)         ; Rust
(require 'init-git)          ; magit, diff-hl
(require 'init-org)          ; org-mode, kept vanilla
(require 'init-tools)        ; browse-at-remote-like helper, test runner
(require 'init-keybindings)  ; leader-key map, tying the above together

;;; init.el ends here
