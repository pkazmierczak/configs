;;; init-eglot.el --- LSP client config -*- lexical-binding: t; -*-
;;
;; eglot and flymake ship with Emacs >= 29, so this file only says *which*
;; servers to run, how diagnostics should look, and what happens on save --
;; mirroring lua/config/lsp.lua in the nvim config.
;;
;; Adding a server = add a major-mode/program pair to `eglot-server-programs'
;; below (and install its binary). Check status with `M-x eglot-events-buffer'
;; or `M-x eglot-stderr-buffer'.

(use-package eglot
  :straight nil ; built in
  :hook ((go-ts-mode rust-ts-mode lua-mode) . eglot-ensure)
  :config
  ;; Don't let a slow server block typing.
  (setq eglot-sync-connect 1
        eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-extend-to-xref t)

  (add-to-list 'eglot-server-programs '(lua-mode . ("lua-language-server"))))

;; `eglot-workspace-configuration' is a single global plist keyed by section
;; (:gopls, :rust-analyzer, ...) -- every language file merges its own
;; section into it instead of clobbering the others.
(defun cfg-eglot-configure (section settings)
  "Merge SETTINGS (a plist) under keyword SECTION into the global
`eglot-workspace-configuration', the equivalent of after/lsp/<name>.lua."
  (setq-default eglot-workspace-configuration
                (plist-put (default-value 'eglot-workspace-configuration)
                           section settings)))

;; ---------------------------------------------------------------------------
;; Diagnostics (flymake is eglot's diagnostics backend).
;; ---------------------------------------------------------------------------

(use-package flymake
  :straight nil
  :config
  (setq flymake-fringe-indicator-position 'right-fringe
        flymake-no-changes-timeout 0.5
        flymake-wrap-around nil))

;; eldoc shows the diagnostic + hover doc for the line under the cursor,
;; the equivalent of `virtual_lines = { current_line = true }`.
(setq eldoc-echo-area-use-multiline-p 1)

;; ---------------------------------------------------------------------------
;; Format + organize-imports on save.
;; ---------------------------------------------------------------------------

(defvar-local cfg-autoformat t
  "When nil, `cfg--eglot-format-on-save' does nothing in this buffer.")

(defun cfg--eglot-format-on-save ()
  (when (and cfg-autoformat (eglot-managed-p))
    ;; `source.organizeImports' is how gopls does goimports.
    (when (derived-mode-p 'go-ts-mode)
      (ignore-errors (eglot-code-actions nil nil "source.organizeImports" t)))
    (eglot-format-buffer)))

(add-hook 'before-save-hook #'cfg--eglot-format-on-save)

(defun cfg/toggle-autoformat (&optional arg)
  "Toggle format-on-save. With ARG, toggle it for the current buffer only."
  (interactive "P")
  (if arg
      (progn (setq-local cfg-autoformat (not cfg-autoformat))
             (message "buffer autoformat: %s" cfg-autoformat))
    (setq-default cfg-autoformat (not (default-value 'cfg-autoformat)))
    (message "autoformat: %s" (default-value 'cfg-autoformat))))

;; ---------------------------------------------------------------------------
;; Inlay hints.
;;
;; `eglot-inlay-hints-mode' only exists from Emacs 30 onward; on 29 gopls and
;; rust-analyzer still send hints, Emacs just can't render them yet. Toggle
;; is a no-op with a message so `SPC t h' keeps working on 29 and starts
;; working for free after an upgrade.
;; ---------------------------------------------------------------------------

(defun cfg/toggle-inlay-hints ()
  (interactive)
  (if (fboundp 'eglot-inlay-hints-mode)
      (call-interactively #'eglot-inlay-hints-mode)
    (message "Inlay hints need Emacs >= 30 (eglot-inlay-hints-mode); this is %s"
             emacs-version)))

(provide 'init-eglot)
;;; init-eglot.el ends here
