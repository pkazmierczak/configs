;;; init-editor.el --- editing behaviour, autocmd-equivalents -*- lexical-binding: t; -*-

;; Recent files, backing `SPC SPC` (recent files), the equivalent of
;; MiniExtra.pickers.oldfiles.
(recentf-mode 1)
(setq recentf-max-saved-items 200)

(defun cfg/recentf ()
  "Open a recently visited file (vertico/orderless-completed)."
  (interactive)
  (find-file (completing-read "Recent file: " recentf-list nil t)))

;; Briefly highlight whatever was just yanked (`TextYankPost` in the nvim
;; config); `pulse.el` ships with Emacs.
(require 'pulse)
(defun cfg--pulse-on-yank (&rest _)
  (when (and (eq this-command 'evil-paste-after) (markerp (mark-marker)))
    (pulse-momentary-highlight-region (region-beginning) (region-end))))
(with-eval-after-load 'evil
  (advice-add 'evil-paste-after :after #'cfg--pulse-on-yank)
  (advice-add 'evil-paste-before :after #'cfg--pulse-on-yank))

;; Keep splits proportional when the frame is resized (`VimResized`).
(add-hook 'window-size-change-functions
          (lambda (_frame) (balance-windows)))

;; In throwaway windows, `q` just closes the window/buffer -- see the
;; motion-state buffer list in init-evil.el; this adds the actual binding.
(dolist (map (list 'help-mode-map 'Man-mode-map 'compilation-mode-map
                    'grep-mode-map 'flymake-diagnostics-buffer-mode-map))
  (with-eval-after-load (if (eq map 'Man-mode-map) 'man map)
    (when (boundp map)
      (define-key (symbol-value map) "q" #'quit-window))))

;; ---------------------------------------------------------------------------
;; TODO / FIXME / HACK / NOTE / WARN highlighting -- the mini.hipatterns
;; equivalent.
;; ---------------------------------------------------------------------------

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . "#dc752f") ("FIXME" . "#cc6666") ("HACK" . "#cc6666")
          ("XXX" . "#cc6666") ("WARN" . "#dc752f") ("NOTE" . "#7cafc2")))
  (global-hl-todo-mode 1))

;; Inline #rrggbb / color-name swatches, the mini.hipatterns hex_color part.
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Reduce whitespace-mode noise added in init-defaults.el to just what the
;; nvim listchars configuration shows (tabs/trailing/nbsp), not long lines.
(setq whitespace-style '(face tabs trailing tab-mark space-before-tab
                              space-after-tab))

(provide 'init-editor)
;;; init-editor.el ends here
