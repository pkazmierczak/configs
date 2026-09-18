;;; init-go.el --- Go support -*- lexical-binding: t; -*-
;;
;; Uses Emacs 29's built-in tree-sitter major mode (go-ts-mode) instead of the
;; older go-mode, mirroring nvim-treesitter. Install the grammar once with
;; `M-x treesit-install-language-grammar RET go RET`.
;;
;; Install: go install golang.org/x/tools/gopls@latest

(use-package go-ts-mode
  :straight nil
  :mode ("\\.go\\'" . go-ts-mode)
  :config
  (setq go-ts-mode-indent-offset 4))

;; gopls settings, the equivalent of after/lsp/gopls.lua.
(with-eval-after-load 'eglot
  (cfg-eglot-configure
   :gopls
   '(:gofumpt t
     :usePlaceholders t
     :completeUnimported t
     :staticcheck t
     :semanticTokens t
     :analyses (:nilness t :shadow t :unusedparams t :unusedwrite t :useany t)
     :hints (:assignVariableTypes t
             :compositeLiteralFields t
             :constantValues t
             :functionTypeParameters t
             :parameterNames t
             :rangeVariableTypes t))))

(provide 'init-go)
;;; init-go.el ends here
