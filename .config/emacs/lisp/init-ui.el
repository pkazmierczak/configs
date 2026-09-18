;;; init-ui.el --- theme, mode-line, which-key -*- lexical-binding: t; -*-
;;
;; Colorscheme: modus-themes, built into Emacs >= 28.
;;   light -> modus-operandi
;;   dark  -> modus-vivendi
;; Light is the default; `SPC t b` (or `M-x cfg/toggle-theme`) flips it. No
;; automatic OS-based switching on purpose, mirroring the nvim config.

(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-mixed-fonts t
      modus-themes-common-palette-overrides '((border-mode-line-active bg-mode-line-active)
                                               (border-mode-line-inactive bg-mode-line-inactive)))

(defvar cfg--theme-light 'modus-operandi)
(defvar cfg--theme-dark 'modus-vivendi)

(defun cfg/set-theme (which)
  "Load WHICH ('light or 'dark), disabling every other theme first."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (if (eq which 'dark) cfg--theme-dark cfg--theme-light) t))

(defun cfg/toggle-theme ()
  "Flip between the light and dark theme."
  (interactive)
  (cfg/set-theme (if (custom-theme-enabled-p cfg--theme-dark) 'light 'dark)))

(cfg/set-theme 'light)

;; ---------------------------------------------------------------------------
;; which-key: shows available keybindings after a prefix, like the nvim config.
;; ---------------------------------------------------------------------------

(use-package which-key
  :config
  (setq which-key-idle-delay 0.4)
  (which-key-mode 1))

;; ---------------------------------------------------------------------------
;; Mode-line: a small hand-rolled one, mirroring config/statusline.lua.
;;
;;  NORMAL │  main │ init.el                    E1 W2 │ emacs-lisp │ 14:32 │ 88:12
;; ---------------------------------------------------------------------------

(defvar cfg--evil-state-names
  '((normal . "NORMAL") (insert . "INSERT") (visual . "VISUAL")
    (replace . "REPLACE") (operator . "OP-PEND") (motion . "MOTION")
    (emacs . "EMACS")))

(defun cfg--ml-mode ()
  (let ((state (and (bound-and-true-p evil-local-mode) (bound-and-true-p evil-state))))
    (propertize (format " %s " (or (cdr (assq state cfg--evil-state-names)) "EMACS"))
                'face 'mode-line-emphasis)))

(defun cfg--ml-branch ()
  (when-let ((branch (and vc-mode (substring-no-properties vc-mode))))
    (propertize (format "  %s │ " (string-trim (replace-regexp-in-string "^ Git[:-]" "" branch)))
                'face 'shadow)))

;; flymake exposes its own mode-line construct; reuse it instead of
;; reimplementing diagnostic counting.
(defun cfg--ml-diag ()
  (when (bound-and-true-p flymake-mode)
    (list flymake-mode-line-counters " │ ")))

(defun cfg--ml-left ()
  (format-mode-line
   (list (cfg--ml-mode) "│" (cfg--ml-branch) " %b" (and (buffer-modified-p) " [+]") " ")))

(defun cfg--ml-right ()
  (format-mode-line
   (list (cfg--ml-diag) mode-name " │ " (format-time-string "%H:%M") " │ %l:%c ")))

;; No `mode-line-format-right-align' before Emacs 30: pad manually instead.
(defun cfg--mode-line ()
  (let* ((left (cfg--ml-left))
         (right (cfg--ml-right))
         (pad (max 1 (- (window-width) (length left) (length right) 1))))
    (concat left (make-string pad ?\s) right)))

(setq-default mode-line-format '((:eval (cfg--mode-line))))

(provide 'init-ui)
;;; init-ui.el ends here
