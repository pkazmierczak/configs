;;; init-straight.el --- package manager -*- lexical-binding: t; -*-
;;
;; straight.el bootstraps itself from git on first run and tracks exact
;; revisions in straight/versions/default.el (the lockfile — commit it).
;; use-package is built into Emacs 29; straight-use-package-by-default makes
;; every `use-package' form install its :straight recipe automatically, so
;; individual declarations below don't need to repeat `:straight t'.
;;
;;   M-x straight-pull-all       update everything
;;   M-x straight-freeze-versions   write straight/versions/default.el
;;   M-x straight-thaw-versions     reinstall exactly what the lockfile says
;;   M-x straight-remove-unused-repos

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t
      ;; Don't nag about bundled/system packages already provided elsewhere.
      straight-check-for-modifications '(check-on-save find-when-checking))

(straight-use-package 'use-package)

(provide 'init-straight)
;;; init-straight.el ends here
