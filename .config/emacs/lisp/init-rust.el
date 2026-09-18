;;; init-rust.el --- Rust support -*- lexical-binding: t; -*-
;;
;; Uses Emacs 29's built-in tree-sitter major mode (rust-ts-mode).
;; Install the grammar once with
;; `M-x treesit-install-language-grammar RET rust RET`.
;;
;; Install: rustup component add rust-analyzer clippy

(use-package rust-ts-mode
  :straight nil
  :mode ("\\.rs\\'" . rust-ts-mode))

;; rust-analyzer settings, the equivalent of after/lsp/rust_analyzer.lua.
(with-eval-after-load 'eglot
  (cfg-eglot-configure
   :rust-analyzer
   '(:cargo (:features "all")
     :check (:command "clippy")
     :checkOnSave t
     :procMacro (:enable t)
     :inlayHints (:closureReturnTypeHints (:enable "with_block")
                  :parameterHints (:enable t)
                  :typeHints (:enable t)))))

(provide 'init-rust)
;;; init-rust.el ends here
