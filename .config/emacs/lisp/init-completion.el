;;; init-completion.el --- minibuffer completion + in-buffer completion -*- lexical-binding: t; -*-
;;
;; The "vertico stack" is the closest Emacs equivalent to mini.pick/mini.extra:
;; a vertical minibuffer UI (vertico), fuzzy/flexible matching (orderless),
;; extra metadata in the candidates list (marginalia), and a set of pickers
;; for files/buffers/grep/LSP symbols (consult). corfu is the in-buffer popup
;; menu equivalent to Neovim's built-in completion popup, driven by the same
;; `completion-at-point-functions' that eglot and elisp already provide.

(use-package vertico
  ;; Pull in the extensions/ subdirectory (vertico-repeat, vertico-directory)
  ;; so they can be required directly, matching upstream's install instructions.
  :straight (vertico :files (:defaults "extensions/*"))
  :config (vertico-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :config (marginalia-mode 1))

(use-package savehist
  :straight nil
  :config (savehist-mode 1))

(use-package consult
  ;; Bindings live in init-keybindings.el, next to their nvim equivalents.
  :config
  (setq consult-narrow-key "<"))

(use-package embark
  :config (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))

;; ---------------------------------------------------------------------------
;; In-buffer completion popup (corfu), analogous to Neovim's popup menu +
;; vim.lsp.completion. `<Tab>' cycles, `<CR>' does NOT accept (so it never
;; steals a plain newline) -- mirrors the nvim config's explicit pumvisible()
;; checks.
;; ---------------------------------------------------------------------------

(use-package corfu
  :init (global-corfu-mode 1)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 1
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-quit-no-match 'separator)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)))

;; Extra completion-at-point sources (file paths, dabbrev) layered underneath
;; whatever eglot/elisp already provide.
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

;; Grep restricted to the current file's extension, the `<leader>s/`
;; equivalent. consult's ripgrep prompt accepts `#<extra rg flags>' after the
;; search pattern (see `consult-async-split-style'); we pre-seed that part.
(defun cfg/grep-current-filetype ()
  (interactive)
  (let ((ext (file-name-extension (or (buffer-file-name) ""))))
    (consult-ripgrep nil (and ext (format "#--glob=*.%s" ext)))))

(provide 'init-completion)
;;; init-completion.el ends here
