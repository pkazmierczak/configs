;;; outlines.el --- show the outlines of a file in separate buffer

;; Copyright (C) 1997 Stephen Eglen

;; Author: Stephen Eglen <stephen@anc.ed.ac.uk>
;; Maintainer: Stephen Eglen <stephen@anc.ed.ac.uk>
;; Created: 05 Jul 1997
;; Keywords: outlines
;; location: http://www.anc.ed.ac.uk/~stephen/emacs
;; RCS: $Id: outlines.el,v 1.4 1999/04/08 09:07:40 stephen Exp $
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This library allows you to view the outlines of a source buffer in
;; a separate buffer.  It allows you to see the structure of the
;; outlines in an easier way than using the outline function
;; `hide-body'.  This has similar functionality to `occur'.

;;; Installation
;; (require 'outlines)
;;
;; To see the outlines, do M-x outlines-show.  Its best to bind this to a key, 
;; for example, you can bind to either outline-mode or outline-minor-mode:
;;
;;(define-key outline-minor-mode-map
;;  (concat outline-minor-mode-prefix "o") 'outlines-show)

;; To see the corresponding line of the source buffer, press return or
;; the middle mouse button on the line in the *Outlines* buffer.  To
;; automatically see the corresponding source entries as you move
;; across the lines in *Outlines*, use `fm-start' from fm.el (from my
;; website):
;;
;; (add-hook 'outlines-mode-hook 'fm-start)
;;
;; (You may have to press `f' to activate the highlighting.)  As you
;; move across lines in *Outlines*, the corresponding line in the
;; source buffer is highlighted.  To move to the next (previous)
;; heading of the same depth, press C-up (C-down).

;; Viewing different depths
;;
;; By default, all outlines are shown in the *Outlines* buffer.  To
;; restrict viewing to just outlines up to and including level N,
;; supply N as a numeric argument to outlines-show.  
;; e.g. C-u 3 M-x outlines-show will show outlines only up to level 3.
;; The value 0 means show all outlines.

;; Updating the *Outlines* buffer
;; 
;; If your source buffer changes, press "g" to update the *Outlines*
;; buffer.  If you also want to change the depth of outlines shown,
;; either press "N g" (where N is 0,1,2,3,4 or 5) or "M-N g" (the meta
;; key and any number N followed by g).

;;; Viewing latex documents.

;; If you use AUC TeX, support is available for viewing outlines of
;; LaTeX documents.  However, rather than using this package, I highly
;; recommend using reftex.el for generating and navigating your latex
;; documents.  Reftex should be distributed with new versions of
;; Emacs, or get it from: http://www.strw.leidenuniv.nl/~dominik/Tools/

;;; How it works.
;; We use the text property `line' attached to each line of the
;; *Outlines* buffer to indicate the corresponding line of the source
;; file (with a marker).  The `depth' property tells us the depth of the
;; outline.

;;; Versions.
;; This should work under Emacs 20 and XEmacs.  If you are using Emacs
;; 19, you probably need to explicitly load noutlines.el before
;; loading this package -- in which case, just add:
;; (require 'noutline)
;; to your .emacs before loading this file.

;;; User variables.
;; Check out the boolean variables outlines-number, outlines-hide-out
;; and outlines-flatten.

;;; TODO
;;  showing the depth in the *Outlines* buffer?

;; Using an indirect-buffer would be nicer, but this is not yet in
;; XEmacs suite.

;;; Code:

;; Must use noutline in Emacs  get outline-on-heading-p.
;(if (string-match "XEmacs" (emacs-version))
;    (require 'outline)
;  ;; else Emacs
;  (require 'noutline))

(require 'outline)

;;; User variables
(defvar outlines-number nil
  "*Non-nil to show the depth of current entry.")

(defvar outlines-hide-out nil
  "*Non-nil to hide the outline matched by `outline-regexp'.")

(defvar outlines-flatten nil
  "*Non-nil means to ignore the depth of outline.")

;;; Internal variables

(defvar outlines-mode-map ())
;; name of buffer where outlines last run.
(defvar outlines-buffer nil)

;; max depth allowed: 0 means allow all depths.
(defvar outlines-max-depth 0)

;; (setq outlines-mode-map nil) ;just for testing
(if outlines-mode-map
    ()
  (setq outlines-mode-map (make-sparse-keymap))
  (define-key outlines-mode-map [mouse-2] 'outlines-mouse-goto-line)
  (define-key outlines-mode-map "\C-c\C-c" 'outlines-goto-line)
  (define-key outlines-mode-map "\C-m" 'outlines-goto-line)
  (define-key outlines-mode-map "0" 'outlines-set-depth)
  (define-key outlines-mode-map "1" 'outlines-set-depth)
  (define-key outlines-mode-map "2" 'outlines-set-depth)
  (define-key outlines-mode-map "3" 'outlines-set-depth)
  (define-key outlines-mode-map "4" 'outlines-set-depth)
  (define-key outlines-mode-map "5" 'outlines-set-depth)
  (define-key outlines-mode-map (read-kbd-macro "C-<down>")
    'outlines-forward-same-depth)
  (define-key outlines-mode-map (read-kbd-macro "C-<up>")
    'outlines-backward-same-depth)
  (define-key outlines-mode-map "g" 'revert-buffer))

(defun outlines-set-depth ()
  "Set the value of `outlines-max-depth'."
  (interactive)
  (setq outlines-max-depth (- last-input-char 48)))

(defun outlines-mode ()
  "Major mode for output from \\[outlines-show].

  \\{outlines-mode-map}"
  (kill-all-local-variables)
  (use-local-map outlines-mode-map)
  (setq major-mode 'outlines-mode)
  (setq mode-name "Outlines")
  (make-local-variable 'revert-buffer-function)
  (setq outlines-max-depth 0)
  (setq revert-buffer-function 'outlines-revert-function)
  (run-hooks 'outlines-mode-hook)
)

(defun outlines-revert-function (ignore1 ignore2)
  "Revert the *Outlines* buffer.
Use a prefix argument to specify a maximum depth of outline."

  (let ((pos (marker-position (get-text-property (point) 'line))))
    (set-buffer outlines-buffer)
    (outlines-show (if current-prefix-arg
		       current-prefix-arg
		     outlines-max-depth)
		   pos))) 

(defun outlines-show (&optional arg pos)
  "Show the outlines in the *Outlines* buffer."
  (interactive "P")
  (let (
	(opbuf (get-buffer-create "*Outlines*"))
	(buffer (current-buffer))
	(beg nil)
	(end nil)
	(regexp outline-regexp)
	(fname buffer-file-truename)
	(depth nil)
	(text nil)
	(pt (or pos (point)))
	(this-line (point-min))
	(this-pt (point-min))
	(last-line nil)
	(last-pt nil)
	(s nil)
	(outlines-latex (and 
			 ;; AUC TeX specific.
			 (fboundp 'LaTeX-outline-level)
			 (eq major-mode 'latex-mode)))
	)
    (save-excursion

      ;; write some stuff in the *Outlines* buffer before starting.
      (set-buffer opbuf)
      (outlines-mode)
      (delete-region (point-min) (point-max))
      (setq outlines-buffer buffer)
      (if arg
	  (setq outlines-max-depth arg))
      (insert (format "Outlines depth %d for %s\n"  
		      (if outlines-max-depth
			  outlines-max-depth
			0)
		      fname ))

      ;; Parse the source buffer.
      (set-buffer buffer)
      (goto-char (point-min))

      (while (not (eobp))
	(if (outline-on-heading-p) 
	    (progn

	      (cond (
		     
		     outlines-latex
		     (setq depth (LaTeX-outline-level)))
		    ( t
		      (setq depth (outline-level))))

	      (if (or (eq outlines-max-depth 0)
		      (<= depth outlines-max-depth))
		  ;; show the outline.
		  (progn
		    (beginning-of-line) 
		    (setq tem (make-marker))
		    (set-marker tem (point))
		    (if outlines-latex
			;; do the latex stuff.
			(progn
			  (search-forward "{")
			  (setq beg (point))
			  (backward-char 1)
			  (forward-list 1)
			  (backward-char 1)			  
			  (setq end (point)))
		      ;; else not latex.
		      (if outlines-hide-out
			  (forward-char depth))
		      (setq beg (point))
		      (beginning-of-line)
		      (end-of-line)
		      (setq end (point))
		      )

		    ;; extract the text and insert it.
		    (setq text (buffer-substring beg end))
		    ;;(forward-line -1)
		    ;; add space to the string to indicate depth:
		    (setq text (concat 
				(if outlines-flatten
				    " "
				  (concat
				    (make-string depth ? ) 
				    (if outlines-number depth)))
				" "
				text "\n"))
		    (put-text-property 0 (length text) 'line tem text)
		    (put-text-property 0 (length text) 'depth depth text)
		    
		    ;; put the info in the *Outlines* buffer
		    (set-buffer opbuf)
		    (insert text)
		    (set-buffer buffer)
		    )
		)))
	(outline-next-heading)
	      )
      (switch-to-buffer opbuf)
      (setq outlines-buffer buffer)

      (goto-char (point-min))

      ;; lets try to position the point appropriately.
      (forward-line 1)
      (while (and (not s)
		  (< (point) (point-max)))
	(setq last-line this-line
	      this-line (marker-position (get-text-property (point) 'line))
	      last-pt this-pt
	      this-pt (point))
	(if (and (>= pt last-line)
		 (<= pt this-line))
	    (setq s  (if (< (- pt last-line) (- this-line pt))
			 last-pt
		       this-pt)))
	(forward-line 1)
	)
      (if s (goto-char s)
	(message "couldn't place at start"))

      )))

;; copied from occur-mode-mouse-goto in replace.el
(defun outlines-mouse-goto-line (event)
  "Visit outline under mouse in source buffer."
  (interactive "e")
  (let (buffer pos line)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq line (get-text-property (point) 'line)))
      )
    (pop-to-buffer outlines-buffer)
    (goto-char (marker-position line))
    ))

(defun outlines-goto-line ()
  "Visit outline under point in source buffer."
  (interactive)
  (let ((line (get-text-property (point) 'line)))
    (pop-to-buffer outlines-buffer)
    (goto-char (marker-position line))
    ))

(defun outlines-forward-same-depth ()
  "Move to next outline with same depth."
  (interactive)
  (let 
      ( (depth (get-text-property (point) 'depth))
	found
	trydepth)
    (save-excursion
      (while (and (not (eobp))
		  (not found))
	(forward-line 1)
	(setq trydepth (get-text-property (point) 'depth))
	(if (equal trydepth depth)
	    (setq found (point)))))
    (if found
	(goto-char found)
      (message "no next of same depth"))))

(defun outlines-backward-same-depth ()
  "Move to previous outline with same depth."
  (interactive)
  (let 
      ( (depth (get-text-property (point) 'depth))
	found
	trydepth)
    (save-excursion
      (while (and (not (bobp))
		  (not found))
	(forward-line -1)
	(setq trydepth (get-text-property (point) 'depth))
	(if (equal trydepth depth)
	    (setq found (point)))))
    (if found
	(goto-char found)
      (message "no previous of same depth"))))

(provide 'outlines)

;;; outlines.el ends here

