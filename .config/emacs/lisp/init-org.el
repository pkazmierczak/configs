;;; init-org.el --- org-mode -*- lexical-binding: t; -*-
;;
;; org-mode ships with Emacs; this replaces the nvim config's whole prose
;; stack (render-markdown.nvim + zen-mode.nvim + :Prose) and is kept
;; deliberately close to vanilla -- a handful of quality-of-life options,
;; nothing that changes org's document model or keybindings.

(use-package org
  :straight nil ; built in
  :hook (org-mode . cfg--org-mode-setup)
  :config
  (setq org-directory "~/org"
        org-agenda-files (list org-directory)
        org-startup-indented t       ; org-indent-mode: no manual asterisk indent
        org-startup-folded 'showeverything
        org-hide-emphasis-markers t  ; hide the */~/= around styled text
        org-pretty-entities t        ; render \alpha, \to, etc.
        org-ellipsis " ▾"
        org-log-done 'time           ; timestamp when a TODO is closed
        org-return-follows-link t
        org-src-fontify-natively t   ; syntax highlighting inside #+begin_src
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-confirm-babel-evaluate nil))

(defun cfg--org-mode-setup ()
  "Prose-friendly defaults for org buffers, mirroring config/prose.lua."
  (visual-line-mode 1)
  (display-line-numbers-mode -1)
  (setq-local fill-column 90))

;; Nicer bullets than the plain asterisks, still entirely optional/vanilla.
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(provide 'init-org)
;;; init-org.el ends here
