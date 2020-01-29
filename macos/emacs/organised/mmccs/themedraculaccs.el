;;; package --- Summary:
;;; Use 'Theme-`dracula'
;; Package-Requires:
;;; Commentary:
;;
;; Reference:
;; []()
;;
;; Start Emacs with this theme as follows:
;;
;; env Emacs_theme=dracula Emacs
;;
;; For permanent use: set environment variable:
;;
;; export emacs_theme=dracula
;;
;;; Code:

(defvar ccs-gray "#1c1c1c")
(defvar ccs-gray-three30 "#303030")

(when (string-equal system-type "berkeley-unix")
  (setq ccs-gray "#444444")
  (setq ccs-gray-three30 "#444444"))

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

(defun ccs-hl-line ()
  "Configure hl-line."
  (with-eval-after-load 'hl-line
    ;;
    (set-face-background 'hl-line ccs-gray-three30)))

(when (string-match "dracula" (getenv "emacs_theme"))
  (add-to-list 'package-pinned-packages '(dracula-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'dracula-theme)
    (package-install 'dracula-theme))
  (load-theme 'dracula t)
  (message "Loading theme dracula...")
  (setq initial-frame-alist '((background-color . ccs-gray)))
  (setq default-frame-alist '((background-color . ccs-gray)))
  (add-to-list 'default-frame-alist '((background-color . ccs-gray)))
  ;; (when (string-equal system-type "berkeley-unix")
  ;;   (ccs-region)
  ;;   (ccs-ivy)
  ;;   (ccs-swiper))
  (ccs-linum)
  (ccs-hl-line)
  (enable-theme 'dracula))

(provide 'themedraculaccs)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;; ex: ts=2 sw=2 et ft=sh
;;; themedraculaccs ends here
