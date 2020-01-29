;;; package --- Summary:
;;; Use 'Theme-`less'
;; Package-Requires:
;;; Commentary:
;;
;; Reference:
;; []()
;;
;; Start Emacs with this theme as follows:
;;
;; env Emacs_theme=less Emacs
;;
;; For permanent use: set environment variable:
;;
;; export emacs_theme=less
;;
;;; Code:

(defvar ccs-gray-12s "#121212")

(when (string-equal system-type "berkeley-unix")
  (setq ccs-gray-12s "#d7ffd7"))

(defun ccs-hl-line ()
  "Configure hl-line."
  (with-eval-after-load 'hl-line
    ;;
    (set-face-background 'hl-line ccs-gray-12s)))

(defun ccs-highlight-indentation ()
  "Configure highlight-indentation."
  ;;
  (with-eval-after-load 'highlight-indentation
    (require 'highlight-indentation)
    (set-face-background 'highlight-indentation-face "#b2b2b2")
    (set-face-background 'highlight-indentation-current-column-face ccs-gray-12s)))

(defun ccs-highlight-changes()
  "Configure Highlight-Changes."
  (with-eval-after-load 'hilit-chg
    ;;
    (set-face-attribute 'highlight-changes nil
                        :foreground "#d7d7ff")))

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

(when (string-match "less" (getenv "emacs_theme"))
  (when (not (file-directory-p "~/.emacs.d/themes"))
    (mkdir "~/.emacs.d/themes" "-p"))
  (unless (file-exists-p "~/.emacs.d/themes/less-theme.el")
    (url-copy-file "https://jblevins.org/git/misc.git/plain/less-theme.el"
                   "~/.emacs.d/themes/less-theme.el"))
  (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes"))
  (load-theme 'less t)
  (message "Loading theme less...")
  (when (string-equal system-type "berkeley-unix")
    (message "Customise theme `less' for FreeBSD...")
    (setq initial-frame-alist '((background-color . "#5f5f5f")))
    (setq default-frame-alist '((background-color . "#5f5f5f")))
    (set-background-color "#5f5f5f")
    (ccs-highlight-changes)
    (ccs-highlight-indentation)
    (ccs-company-preview))
  (ccs-hl-line)
  (enable-theme 'less))

(provide 'themelessccs)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;; ex: ts=2 sw=2 et ft=sh
;;; themelessccs ends here
