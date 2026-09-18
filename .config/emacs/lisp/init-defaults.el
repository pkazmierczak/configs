;;; init-defaults.el --- core editor options -*- lexical-binding: t; -*-

;; A saner GC threshold once startup is done (early-init.el set a big one).
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;; UI
(setq-default display-line-numbers-width 4)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(setq-default cursor-type 'box)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(setq scroll-margin 5
      scroll-conservatively 101 ; scroll one line at a time, never re-center
      scroll-preserve-screen-position t)
(setq use-dialog-box nil
      inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore)
(setq-default indicate-empty-lines t)
(setq-default fill-column 80)
(setq confirm-kill-emacs #'y-or-n-p) ; ask instead of silently quitting

;; Whitespace, mirroring nvim's listchars: show tabs/trailing space/nbsp.
(setq-default whitespace-style '(face tabs trailing tab-mark))
(global-whitespace-mode 1)

;; Behaviour
(setq mouse-wheel-progressive-speed nil)
(setq-default indent-tabs-mode nil)
(setq create-lockfiles nil) ; no `.#file' lock symlinks
(setq make-backup-files nil)
(setq large-file-warning-threshold (* 50 1024 1024))

;; Persistent undo, matching `undofile`.
(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode 1))

;; Real files should reload if changed on disk (matches `checktime`).
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil
      global-auto-revert-non-file-buffers t)

;; Remember where the cursor was, like nvim's last-position autocmd.
(save-place-mode 1)

;; Search
(setq case-fold-search t
      search-highlight t)

;; Indentation: Go/Rust major modes set their own via editorconfig or the
;; language server; this is just the editor-wide fallback.
(setq-default tab-width 4)

;; Completion-at-point popup (the `completion-preview-mode' / corfu equivalent
;; of nvim's built-in popup menu lives in init-completion.el).
(setq tab-always-indent 'complete)

;; editorconfig support ships with Emacs >= 30; on 29 it's still a tiny package.
(use-package editorconfig
  :config (editorconfig-mode 1))

;; Spelling is off by default but ready for prose/org (see init-org.el).
(setq ispell-dictionary "en_US")

(provide 'init-defaults)
;;; init-defaults.el ends here
