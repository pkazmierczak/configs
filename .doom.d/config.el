;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(require 'doom-themes)

;;; Settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t  ; if nil, italics is universally disabled

      ;; doom-one specific settings
      doom-one-brighter-modeline nil
      doom-one-brighter-comments nil)

;; Load the theme (doom-one, doom-dark, etc.)
; (load-theme 'doom-solarized-light t)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

(setq doom-font (font-spec :family "Source Code Pro for Powerline" :size 13))

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
