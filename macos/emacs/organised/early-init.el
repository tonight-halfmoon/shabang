;;; early-init.el --- Emacs init file
;;; early-init.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)

(tool-bar-mode 0)

(menu-bar-mode 0)

(scroll-bar-mode 0)

(modify-all-frames-parameters '((vertical-scroll-bars)))

(provide 'early-init)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;; ex: ts=2 sw=2 et ft=sh
;;; early-init ends here
