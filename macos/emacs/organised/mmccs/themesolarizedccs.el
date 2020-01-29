;;; package --- Summary:
;;; Use 'Theme-`Solarized'
;; Package-Requires:
;;
;; script `git-clone-solarized'
;;
;;; Commentary:
;;
;; Start Emacs with this theme as follows:
;;
;; `env Emacs_theme=solarized Emacs`
;;
;; For permanent use: set environment variable:
;;
;; export emacs_theme=solarized
;;
;;; Code:

(defvar ccs-gray-three30 "#303030")

(when (string-equal system-type "berkeley-unix")
  (setq ccs-gray-three30 "#444444"))

(defun ccs-company-preview()
  "Configure company."
  (with-eval-after-load 'company
    (require 'company)
    (set-face-attribute 'company-preview t
                        :background "#444444"
                        :foreground "#808080")
    (set-face-attribute 'company-preview-common t
                        :background "#262626"
                        :foreground "#cd00cd")
    (set-face-attribute 'company-preview-search t
                        :background "#0000cd"
                        :foreground "#808080")
    (set-face-attribute 'company-scrollbar-bg t
                        :background "#808080")
    (set-face-attribute 'company-scrollbar-fg t
                        :background "#626262")
    (set-face-attribute 'company-tooltip t
                        :background "#444444"
                        :foreground "#808080")
    (set-face-attribute 'company-tooltip-selection t
                        :background "#bcbcbc"
                        :foreground "#00ff00"
                        :weight 'bold)))

(defun ccs-linum ()
  "Configure linum."
  (with-eval-after-load 'linum
    ;;
    (set-face-attribute 'linum t
                        :background "#080808"
                        :foreground "#444444")))

(defun ccs-hl-line ()
  "Configure hl-line."
  (with-eval-after-load 'hl-line
    ;;
    (set-face-background 'hl-line ccs-gray-three30)))

(when (string-match "solarized" (getenv "emacs_theme"))
  (call-process (getenv "SHELL") nil "git-clone-solarized" nil "-c"
                "chmod -v u+x $HOME/.emacs.d/mmccs/git-clone-solarized")
  (call-process "~/.emacs.d/mmccs/git-clone-solarized" nil "git-clone-solarized")
  (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/emacs-color-theme-solarized"))
  (load-theme 'solarized t)
  (set-variable 'solarized-termcolors 256)
  (when (string-equal system-type "berkeley-unix")
    (message "Customise theme solarized for FreeBSD...")
    (setq initial-frame-alist '((background-color . "#080808")))
    (setq default-frame-alist '((background-color . "#080808")))
    (ccs-company-preview)
    (ccs-linum)
    (ccs-hl-line))
  (enable-theme 'solarized))

(provide 'themesolarizedccs)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;; ex: ts=2 sw=2 et ft=sh
;;; themesolarizedccs ends here
