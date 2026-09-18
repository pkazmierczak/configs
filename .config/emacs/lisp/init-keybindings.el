;;; init-keybindings.el --- keymaps -*- lexical-binding: t; -*-
;;
;; `SPC' is the leader, exactly like the nvim config's `<Space>' mapleader.
;; general.el gives evil-aware leader-key definitions (the which-key group
;; labels mirror `wk.add' in lua/config/plugins.lua).
;;
;; Neovim's `K` / `grn` / `gra` / `grr` / `gri` / `grt` / `gO` map to Emacs's
;; own eldoc/eglot-rename/eglot-code-actions/xref-find-references/
;; eglot-find-implementation/eglot-find-typeDefinition/consult-imenu below --
;; nothing here is redefining a "default" the way the nvim README calls out,
;; it just wires up the closest built-in/eglot equivalent.

(use-package general
  :after evil
  :config
  (general-evil-setup) ; defines general-nmap/vmap/etc. used below

  (general-create-definer cfg-leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")

  ;; ---------------------------------------------------------------------
  ;; Basics
  ;; ---------------------------------------------------------------------
  (general-nmap
    "q" #'kill-current-buffer
    "Q" (lambda () (interactive) (kill-buffer (current-buffer)))
    "gd" #'xref-find-definitions
    "gD" #'xref-find-definitions-other-window)
  (cfg-leader
    "v" #'(lambda () (interactive) (split-window-right) (other-window 1))
    "q" #'delete-window)

  ;; ---------------------------------------------------------------------
  ;; Pickers (the vertico/consult stack) & file explorer (dired)
  ;; ---------------------------------------------------------------------
  (cfg-leader
    "SPC" #'cfg/recentf
    "f"   #'project-find-file
    "b"   #'consult-buffer
    "/"   #'consult-ripgrep
    "s/"  #'cfg/grep-current-filetype
    "'"   #'vertico-repeat)
  (general-nmap "<C-n>" #'dired-jump)
  (cfg-leader "ff" #'dired-jump)

  (cfg-leader
    "sh" #'consult-info
    "sk" #'describe-bindings
    "sc" #'execute-extended-command
    "sw" #'(lambda () (interactive) (consult-ripgrep nil (thing-at-point 'symbol)))
    "st" #'hl-todo-occur
    "ss" #'ispell-word)

  ;; ---------------------------------------------------------------------
  ;; LSP & diagnostics
  ;; ---------------------------------------------------------------------
  (cfg-leader
    "D" #'eglot-find-typeDefinition
    "r" #'eglot-rename
    "a" #'eglot-code-actions
    "e" #'(lambda () (interactive) (flymake-show-diagnostic (point))))

  (general-nmap
    "grr" #'xref-find-references
    "gri" #'eglot-find-implementation
    "gO"  #'consult-imenu)

  (cfg-leader
    "ds" #'consult-imenu
    "dd" #'flymake-show-buffer-diagnostics
    "ws" #'consult-eglot-symbols
    "wd" #'flymake-show-project-diagnostics
    "we" #'flymake-show-project-diagnostics) ; press `t' in that buffer to filter by type

  ;; ---------------------------------------------------------------------
  ;; Git (diff-hl for hunks, magit for everything else)
  ;; ---------------------------------------------------------------------
  (general-nmap
    "]c" #'diff-hl-next-hunk
    "[c" #'diff-hl-previous-hunk)
  (cfg-leader
    "ph" #'diff-hl-show-hunk
    "gs" #'diff-hl-stage-current-hunk
    "gr" #'diff-hl-revert-hunk
    "gd" #'magit-diff-buffer-file
    "gb" #'magit-blame-addition
    "gl" #'magit-log-buffer-file
    "gf" #'project-find-file
    "gc" #'magit-log-current
    "gB" #'browse-at-remote)

  ;; ---------------------------------------------------------------------
  ;; Toggles
  ;; ---------------------------------------------------------------------
  (cfg-leader
    "tb" #'cfg/toggle-theme
    "ts" #'flyspell-mode
    "tw" #'visual-line-mode
    "tf" #'cfg/toggle-autoformat
    "th" #'cfg/toggle-inlay-hints)

  ;; ---------------------------------------------------------------------
  ;; Run
  ;; ---------------------------------------------------------------------
  (general-nmap "<f10>" #'cfg/run-tests)

  ;; ---------------------------------------------------------------------
  ;; which-key group labels, mirroring `wk.add' in the nvim config.
  ;; ---------------------------------------------------------------------
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "SPC g" "git"
      "SPC s" "search"
      "SPC t" "toggle"
      "SPC d" "document"
      "SPC w" "workspace")))

(use-package consult-eglot
  :after (consult eglot))

;; Resume the last vertico/consult session, the `<leader>'` equivalent.
(use-package vertico-repeat
  :straight nil ; bundled with vertico
  :after vertico
  :config (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
