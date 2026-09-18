;;; early-init.el --- runs before init.el, before the UI is drawn -*- lexical-binding: t; -*-

;; Requires Emacs >= 29.

;; Don't let package.el load installed packages before straight.el gets a say.
(setq package-enable-at-startup nil)

;; A bigger GC threshold makes startup (and LSP-heavy sessions) faster; reset
;; to a saner value once we're up (see lisp/init-defaults.el).
(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.6)

;; Skip file-name-handler-alist scanning for every load during startup.
(defvar cfg--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda () (setq file-name-handler-alist cfg--file-name-handler-alist)))

;; Avoid the frame being resized (and redrawn) as chrome gets stripped below.
(setq frame-inhibit-implied-resize t)

;; Strip UI chrome before the first frame is drawn, not after.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)

;; native-comp: silence and redirect noise, cache elsewhere.
(setq native-comp-async-report-warnings-errors 'silent)
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "eln-cache/" user-emacs-directory))))
