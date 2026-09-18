;;; init-tools.el --- small conveniences -*- lexical-binding: t; -*-
;;
;; The equivalent of lua/config/tools.lua: "open line on GitHub" and a
;; per-filetype test runner.

;; "Open line on GitHub" -- browse-at-remote does exactly this and handles
;; GitHub/GitLab/Bitbucket/sourcehut, so no hand-rolled URL parsing needed.
(use-package browse-at-remote
  :commands (browse-at-remote))

;; Run the project's test suite in a compilation buffer, bound to `<f10>` in
;; init-keybindings.el.
(defvar cfg-test-runners
  '((go-ts-mode . "go test ./...")
    (rust-ts-mode . "cargo test")
    (python-mode . "pytest")
    (python-ts-mode . "pytest")
    (js-mode . "npm test")
    (typescript-ts-mode . "npm test"))
  "Alist of (MAJOR-MODE . SHELL-COMMAND) used by `cfg/run-tests'.")

(defun cfg/run-tests ()
  "Run the test command registered for the current major mode."
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (if-let ((cmd (cdr (assq major-mode cfg-test-runners))))
      (compile cmd)
    (message "No test runner for %s" major-mode)))

(provide 'init-tools)
;;; init-tools.el ends here
