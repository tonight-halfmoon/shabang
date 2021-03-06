;;; package --- Summary:
;;; Commentary:
;;; Code:

(require 'package)

(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(with-eval-after-load (load-file "~/.emacs.d/features.el"))

(tool-bar-mode 0)

(menu-bar-mode 0)

(unless (not (fboundp 'scroll-bar-mode))
  (scroll-bar-mode 0))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((allout-layout . t)
                                      (tabs-width . 2)
                                      (sh-indent-comment . t)
                                      (buffer-file-coding-system . utf-8))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#f2777a")
                                 (40 . "#f99157")
                                 (60 . "#ffcc66")
                                 (80 . "#99cc99")
                                 (100 . "#66cccc")
                                 (120 . "#6699cc")
                                 (140 . "#cc99cc")
                                 (160 . "#f2777a")
                                 (180 . "#f99157")
                                 (200 . "#ffcc66")
                                 (220 . "#99cc99")
                                 (240 . "#66cccc")
                                 (260 . "#6699cc")
                                 (280 . "#cc99cc")
                                 (300 . "#f2777a")
                                 (320 . "#f99157")
                                 (340 . "#ffcc66")
                                 (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -----------
;; `CamelCase'
;; -----------
(global-subword-mode t)

;; -----------------------
;; `Screen-Configurations'
;; -----------------------
(setq inhibit-startup-screen t)
(when window-system (set-frame-size (selected-frame) 64 51))

;; ----------------------------------
;; `final-newline' Final New Line EOF
;; ----------------------------------

(setq mode-require-final-newline t)
(setq require-final-newline 'mode-require-final-newline)

;; ----------------
;; `Font-Lock' mode
;; ----------------
(font-lock-mode t)
(global-font-lock-mode 1)

;; ---------------
;; `Edit-in-place'
;; ---------------
(delete-selection-mode t)

;; ---------------
;; `Debug' support
;; ---------------
(setq debug-on-error t)

;; ---------------
;; `long-to-short'
;; ---------------
(defalias 'yes-or-no-p 'y-or-n-p)

;; --------------------
;; `Show-Column-Number'
;; --------------------
(column-number-mode 1)

(provide '.emacs)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;; ex: ts=2 sw=2 et ft=sh
;;; .emacs ends here
