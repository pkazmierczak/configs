;;; init-evil.el --- vim keybindings -*- lexical-binding: t; -*-

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil ; evil-collection supplies these instead
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-respect-visual-line-mode t ; j/k move by display line, like the nvim config
        evil-undo-system 'undo-redo
        evil-search-module 'evil-search
        evil-symbol-word-search t)
  :config
  (evil-mode 1)
  ;; Throwaway buffers open already in a "just navigate/close" state,
  ;; matching the nvim config's quick-close autocmd (`q` closes them).
  (dolist (mode '(help-mode Man-mode compilation-mode grep-mode flymake-diagnostics-buffer-mode))
    (evil-set-initial-state mode 'motion)))

;; Extends evil bindings to the rest of Emacs (magit, dired, help, ...) so
;; every built-in package feels native instead of dropping you into emacs
;; state.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; `cs"'`, `ds"`, `ysiw)` etc. -- the mini.surround equivalent.
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

;; `gcc` / `gc` in visual mode, matching nvim's built-in `gc`.
(use-package evil-nerd-commenter
  :after evil)

;; A tiny escape hatch: `jk` in insert state leaves to normal state, on top
;; of plain <Esc>.
(use-package evil-escape
  :after evil
  :config
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.2)
  (evil-escape-mode 1))

(provide 'init-evil)
;;; init-evil.el ends here
