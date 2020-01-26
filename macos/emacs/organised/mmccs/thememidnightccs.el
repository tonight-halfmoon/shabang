;;; package --- Summary:
;;; Use 'Theme-`modern-midnight'
;; Package-Requires:
;;; Commentary:
;;
;; Reference:
;; [](https://github.com/emacs-jp/replace-colorthemes)
;;
;; Start Emacs with this theme as follows:
;;
;; env Emacs_theme=midnight Emacs
;;
;; For permanent use: set environment variable:
;;
;; export emacs_theme=midnight
;;
;;; Code:

(unless (package-installed-p 'smart-mode-line)
  (package-install 'smart-mode-line))
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)

(defun ccs-the-theme()
  "Configure the theme."
  (when (string-equal (getenv "emacs_background") "black")
    (setq initial-frame-alist '((background-color . "#080808")))
    (setq default-frame-alist '((background-color . "#080808"))))
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'color-theme-modern)
    (package-install 'color-theme-modern))
  (load-theme 'midnight t t)
  (setq sml/theme 'dark)
  (sml/setup)
  (enable-theme 'midnight))

(defun ccs-mode-line()
  "`Mode-line'."
  ;;
  (set-face-attribute 'mode-line-inactive t
                      :background "#1c1c1c"
                      :inverse-video nil)
  (set-face-attribute 'mode-line t
                      :background "#444444"
                      :weight 'bold
                      :inverse-video t))

(defun ccs-highlight-indentation()
  "Configure highlight-indentation."
  ;;
  (with-eval-after-load 'highlight-indentation
    (require 'highlight-indentation)
    (set-face-background 'highlight-indentation-face "#00ff00")
    (set-face-background 'highlight-indentation-current-column-face "#00ff00")))

(defun ccs-flyspell()
  "Configure Fly spell."
  (with-eval-after-load 'flyspell
    ;;
    (set-face-attribute 'flyspell-incorrect nil
                        :underline t
                        :background "#5fff5f"
                        :foreground "#1c1c1c"
                        :weight 'bold
                        :inverse-video t)))

(defun ccs-region()
  "Configure region highlight."
  (set-face-attribute 'region t
                      :background "#808080"
                      :foreground "#8787af"))

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

(defun ccs-swiper()
  "Configure swiper."
  (require 'swiper)
  (set-face-attribute 'swiper-line-face t
                      :background "#5f5f00"))

(when (string-equal (getenv "emacs_theme") "midnight")
  (message "loading theme midnight...")
  (ccs-the-theme)
  (ccs-company-preview)
  (ccs-highlight-indentation)
  (ccs-flyspell)
  (ccs-mode-line)
  (ccs-region))

(provide 'thememidnightccs)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;; ex: ts=2 sw=2 et ft=sh
;;; thememidnightccs ends here
