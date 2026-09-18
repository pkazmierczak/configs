;;; init-git.el --- version control -*- lexical-binding: t; -*-
;;
;; magit is the de-facto Emacs git porcelain (no nvim equivalent needed: it's
;; a full Magit-status buffer, strictly more capable than a picker). diff-hl
;; is the gutter/hunk-actions/preview equivalent of gitsigns.nvim. blamer
;; gives inline current-line blame like gitsigns' current_line_blame.

(use-package magit
  :commands (magit-status magit-log-all magit-blame-addition)
  :config
  (setq magit-diff-refine-hunks 'all))

(use-package diff-hl
  :hook ((prog-mode org-mode) . diff-hl-mode)
  :config
  (setq diff-hl-side 'left)
  ;; Fringes don't exist in a terminal; fall back to the margin there.
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  (global-diff-hl-mode 1)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; Inline "author, 3 days ago · summary" at the end of the current line.
(use-package blamer
  :config
  (setq blamer-idle-time 0.4
        blamer-min-offset 20
        blamer-view 'overlay))

(provide 'init-git)
;;; init-git.el ends here
