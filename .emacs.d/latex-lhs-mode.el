;;; latex-lhs-mode.el --- tex-mode features for literate-haskell-mode

;; Copyright (C) 2006-2007 Daniel Franke

;; This mode is based in part upon tex-mode.el, which is Copyright (C)
;; 1985, 86, 89, 92, 94, 95, 96, 97, 98, 1999, 2002 by the Free
;; Software Foundation, Inc. It interfaces with haskell-mode.el, which
;; is Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.,
;; and Copyright (C) 1992, 1997-1998 Simon Marlow, Graeme E Moss, and
;; Tommy Thor.  Both works are licensed under the GNU General Public
;; License.

;; latex-lhs-mode.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; latex-lhs-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Author:
;;
;; Daniel Franke <df@dfranke.us> 
;; http://www.dfranke.us
;; dfranke on #haskell.
;;
;; Purpose:
;;
;; To combine the functionality of literate-haskell-mode and tex-mode.
;; Everything that works in literate-haskell-mode should also work in
;; latex-lhs-mode.  Additionally, most features of tex-mode are available,
;; with the exception of syntax highlighting. tex-mode keys of the form
;; C-c foo are remapped to C-c C-t foo; literate-haskell-mode keys are
;; unchanged.

;; Installation:
;;
;; latex-lhs-mode depends on literate-haskell-mode, so this must be
;; installed first.
;; 
;; Put in your ~/.emacs:
;; (add-to-list 'auto-mode-alist
;;              '("\\.l[hg]s$" . latex-lhs-mode))
;; (require 'latex-lhs-mode)
;;
;; with `latex-lhs-mode.el' somewhere in your load path. 
;;
;; Customisation:
;;
;; latex-lhs-mode has no customisations of its own, but customisations for
;; literate-haskell-mode and tex-mode are honoured.
;; 
;; History:
;; 
;; Varsion 0.1a (2007-02-18): Updated contact information.
;; Version 0.1  (2006-09-10): Initial release.

;;; Code:

(require 'haskell-mode)
(require 'tex-mode)

(defun latex-lhs-indent (&optional arg)
  (if (save-excursion
	(end-of-line) ;In case we're on an \end{code} line
	(haskell-indent-within-literate-code))
      (haskell-indent-cycle)
    (save-excursion
      (beginning-of-line)
      (if (or (looking-at (regexp-quote "\\begin{code}"))
	      (looking-at (regexp-quote "\\end{code}")))
	  (indent-to-left-margin)
	(latex-indent arg)))))

(defun latex-lhs-create-index-function ()
  (latex-imenu-create-index)
  (haskell-ds-create-imenu-index))

(define-derived-mode latex-lhs-mode literate-haskell-mode
  "LaTeX+Haskell"
  "An extension of literate-haskell-mode containing some features from
latex-mode"
  (set (make-local-variable 'tex-command) latex-run-command)
  (set (make-local-variable 'tex-start-of-header) 
       "\\\\document\\(style\\|class\\)")
  (set (make-local-variable 'tex-end-of-header)
       "\\\\begin\\s-*{document}")
  (set (make-local-variable 'tex-trailer)  "\\end\\s-*{document}\n")
  (set 'imenu-create-index-function 'latex-lhs-create-index-function)
  (set 'indent-line-function 'latex-lhs-indent))

;;Mostly cribbed from tex-mode.el.
(define-key latex-lhs-mode-map "\C-c\C-t\C-k" 'tex-kill-job)
(define-key latex-lhs-mode-map "\C-c\C-t\C-l" 'tex-recenter-output-buffer)
(define-key latex-lhs-mode-map "\C-c\C-t\C-q" 'tex-show-print-queue)
(define-key latex-lhs-mode-map "\C-c\C-t\C-p" 'tex-print)
(define-key latex-lhs-mode-map "\C-c\C-t\C-v" 'tex-view)
(define-key latex-lhs-mode-map "\C-c\C-t}" 'up-list)
(define-key latex-lhs-mode-map "\C-c\C-t{" 'tex-insert-braces)
(define-key latex-lhs-mode-map "\C-c\C-t\C-r" 'tex-region)
(define-key latex-lhs-mode-map "\C-c\C-t\C-b" 'tex-buffer)
(define-key latex-lhs-mode-map "\C-c\C-t\C-f" 'tex-file)
(define-key latex-lhs-mode-map "\C-c\C-t\C-i" 'tex-bibtex-file)
(define-key latex-lhs-mode-map "\C-c\C-t\C-o" 'tex-latex-block)
(define-key latex-lhs-mode-map "\C-c\C-t\C-e" 'tex-close-latex-block)
(define-key latex-lhs-mode-map "\C-c\C-t\C-u" 
  'tex-goto-last-unclosed-latex-block)
(define-key latex-lhs-mode-map "\C-c\C-t\C-m" 'tex-feed-input)
(define-key latex-lhs-mode-map [(control return)] 'tex-feed-input)
(define-key latex-lhs-mode-map [menu-bar tex] 
  (cons "TeX" (make-sparse-keymap "Tex")))
(define-key latex-lhs-mode-map [menu-bar tex tex-kill-job]
  '(menu-item "Tex Kill" tex-kill-job :enable (tex-shell-running)))
(define-key latex-lhs-mode-map [menu-bar tex tex-recenter-output-buffer]
  '(menu-item "Tex Recenter" tex-recenter-output-buffer
	      :enable (get-buffer "*tex-shell*")))
(define-key latex-lhs-mode-map [menu-bar tex tex-show-print-queue]
  '("Show Print Queue" . tex-show-print-queue))
(define-key latex-lhs-mode-map [menu-bar tex tex-alt-print]
  '(menu-item "Tex Print (alt printer)" tex-alt-print
	      :enable (stringp tex-print-file)))
(define-key latex-lhs-mode-map [menu-bar tex tex-print]
  '(menu-item "Tex Print" tex-print :enable (stringp tex-print-file)))
(define-key latex-lhs-mode-map [menu-bar tex tex-view]
  '(menu-item "Tex View" tex-view :enable (stringp tex-print-file)))
(define-key latex-lhs-mode-map [menu-bar tex tex-bibtex-file]
  '("BibTeX File" . tex-bibtex-file))
(define-key latex-lhs-mode-map [menu-bar tex tex-validate-region]
  '(menu-item "Validate Region" tex-validate-region :enable mark-active))
(define-key latex-lhs-mode-map [menu-bar tex tex-validate-buffer]
  '("Validate Buffer" . tex-validate-buffer))
(define-key latex-lhs-mode-map [menu-bar tex tex-region]
  '(menu-item "TeX Region" tex-region :enable mark-active))
(define-key latex-lhs-mode-map [menu-bar tex tex-buffer]
      '("TeX Buffer" . tex-buffer))
(define-key latex-lhs-mode-map [menu-bar tex tex-file] 
  '("TeX File" . tex-file))

(provide 'latex-lhs-mode)
