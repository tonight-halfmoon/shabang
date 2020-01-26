;;; package --- Summary:
;;; Use 'Theme-`Sanityinc-Tomorrow'
;; Package-Requires:
;;; Commentary:
;;
;; Start Emacs with this theme as follows:
;;
;; `env Emacs_theme=tomorrow Emacs'
;;
;; For permanent use: set environment variable:
;;
;; `export Emacs_theme=tomorrow`
;;
;; Reference:
;; [](https://github.com/purcell/color-theme-sanityinc-tomorrow)
;;
;;; Code:

(unless (package-installed-p 'smart-mode-line)
  (package-install 'smart-mode-line))
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)

(when (string-equal (getenv "emacs_theme") "tomorrow")
  (add-to-list 'package-pinned-packages '(color-theme-sanityinc-tomorrow . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'smart-mode-line)
    (package-install 'smart-mode-line))
  (unless (package-installed-p 'color-theme-sanityinc-tomorrow)
    (package-install 'color-theme-sanityinc-tomorrow))
  (load-theme 'sanityinc-tomorrow-night t)
  (add-to-list 'initial-frame-alist '(background-color . "#080808"))
  (add-to-list 'default-frame-alist '(background-color . "#080808"))
  (when (string-equal (getenv "emacs_background") "bright")
    (add-to-list 'default-frame-alist '(background-color . "#4d4d4d")))
  (setq sml/theme 'dark)
  (sml/setup)
  (message "Loading theme tomorrow...")
  (enable-theme 'sanityinc-tomorrow-night))

(provide 'themetomorrowccs)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;; ex: ts=2 sw=2 et ft=sh
;;; themetomorrowccs ends here
