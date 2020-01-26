;;; package --- Summary:
;;; Use 'Theme-`zerodark'
;; Package-Requires:
;;; Commentary:
;;
;; Start Emacs with this theme as follows:
;;
;; env Emacs_theme=zerodark Emacs
;;
;; For permanent use: set environment variable:
;;
;; export emacs_theme=zerodark
;;
;;; Code:

(when (string-equal (getenv "emacs_theme") "zerodark")
  (add-to-list 'package-pinned-packages '(async . "melpa") t)
  (add-to-list 'package-pinned-packages '(magit . "melpa") t)
  (add-to-list 'package-pinned-packages '(zerodark-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'async)
    (package-install 'async))
  (unless (package-installed-p 'magit)
    (package-install 'magit))
  (unless (package-installed-p 'zerodark-theme)
    (package-install 'zerodark-theme))
  ;; (eval-and-compile 'zerodark-theme (when (not (file-directory-p "~/.emacs.d/all-the-icons"))
  ;;                                     (mkdir "~/.emacs.d/all-the-icons" "-p"))
  ;;                   (when (not (file-exists-p "~/.emacs.d/all-the-icons/all-the-icons.ttf"))
  ;;                     (all-the-icons-install-fonts))
  ;;                   (load-theme 'zerodark t)
  ;;                   (setq inhibit-compacting-font-caches t)
  ;;                   (zerodark-setup-modeline-format)
  ;;                   (enable-theme 'zerodark))
  (load-theme 'zerodark t)
  (enable-theme 'zerodark))

(provide 'themezerodarkccs)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;; ex: ts=2 sw=2 et ft=sh
;;; themezerodarkccs ends here
