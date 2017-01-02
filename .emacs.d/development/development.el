;; GOLANG
(require 'go-mode)

(exec-path-from-shell-copy-env "GOPATH")

(require 'auto-complete)
(require 'go-autocomplete)
(ac-config-default)

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook `go-mode-hook `flycheck-mode)

(require 'go-guru)
(add-hook `go-mode-hook `go-guru-hl-identifier-mode)
(set-face-attribute 'highlight nil :background "#FF0" :foreground "#000")

(require 'go-add-tags)
(global-set-key (kbd "C-c t") 'go-add-tags)

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; PYTHON
(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
;; ignoring:
;; - E501 - Try to make lines fit within --max-line-length characters.
;; - W293 - Remove trailing whitespace on blank line.
;; - W391 - Remove trailing blank lines.
;; - W690 - Fix various deprecated code (via lib2to3).
(require 'py-autopep8)
(setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(define-key global-map (kbd "RET") 'newline-and-indent)

(provide 'development)
