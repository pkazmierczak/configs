;;; install-packages.el --- Emacs Package Installation

;; Author: Kyle W. Purdon (kpurdon)
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'package)

(defvar my-packages
  '(auto-complete
    counsel
    elpy
    evil
    exec-path-from-shell
    flycheck
    go-add-tags
    go-autocomplete
    go-eldoc
    go-guru
    go-mode
    json-mode
    magit
    git-gutter
    git-timemachine
    markdown-mode
    markdown-preview-mode
    org-bullets
    pastelmac-theme
    projectile
    py-autopep8
    rainbow-delimiters
    yaml-mode))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("org" . "http://orgmode.org/elpa/"))


(when (not package-archive-contents)
    (package-refresh-contents))
(package-initialize)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'install-packages)

;;; install-packages.el ends here
