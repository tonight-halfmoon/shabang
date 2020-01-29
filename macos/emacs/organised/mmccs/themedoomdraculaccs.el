;;; package --- Summary:
;;; Use 'Theme-`doom-dracula'
;; Package-Requires:
;;; Commentary:
;;
;; Reference:
;; []()
;;
;; Start Emacs with this theme as follows:
;;
;; env Emacs_theme=ddracula Emacs
;;
;; For permanent use: set environment variable:
;;
;; export emacs_theme=ddracula
;;
;;; Code:

(defun ccs-region ()
  "Configure region highlight."
  (set-face-attribute 'region t
                      :background "#808080"
                      :foreground "#8787af"))

(defun ccs-linum ()
  "Configure linum."
  (with-eval-after-load 'linum
    ;;
    (set-face-attribute 'linum t
                        :background "#080808"
                        :foreground "#444444")))

(defun ccs-ivy()
  "Configure Ivy-mode."
  (with-eval-after-load 'ivy
    ;;
    (set-face-attribute 'ivy-current-match t
                        :background "#444444")))

(defun ccs-swiper()
  "Configure swiper."
  (require 'swiper)
  (set-face-attribute 'swiper-line-face t
                      :background "#5f5f00"))

(when (string-match "x" (getenv "emacs_theme"))
  (add-to-list 'package-pinned-packages '(doom-themes . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'doom-themes)
    (package-install 'doom-themes))
  (load-theme 'doom-dracula t)
  (message "Loading theme doom-dracula...")
  (when (string-equal system-type "berkeley-unix")
    (message "Customise doom-dracula for FreeBSD...")
    ;; (add-to-list 'default-frame-alist '((background-color . "#444444")))
    ;; (setq frame-background-mode 'light)
    (ccs-region)
    (ccs-linum)
    (ccs-ivy)
    (ccs-swiper))
  (enable-theme 'doom-dracula))

(provide 'themedoomdraculaccs)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;; ex: ts=2 sw=2 et ft=sh
;;; themedoomdraculaccs ends here
