;;; package --- Summary:
;;; Use 'Theme-`Twilight'
;; Package-Requires:
;;; Commentary:
;;
;; Reference:
;; []()
;;
;; Start Emacs with this theme as follows:
;;
;; env Emacs_theme=twilight Emacs
;;
;; For permanent use: set environment variable:
;;
;; export emacs_theme=twilight
;;
;;; Code:

(defun ccs-region ()
  "Configure region highlight."
  (set-face-attribute 'region t
                      :background "#808080"
                      :foreground "#8787af"))

(defun ccs-linum ()
  "Configure linum."
  (with-eval-after-load 'linum (set-face-attribute 'linum t
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

(defun ccs-highlight-indentation()
  "Configure highlight-indentation."
  ;;
  (with-eval-after-load 'highlight-indentation
    (require 'highlight-indentation)
    (set-face-background 'highlight-indentation-face "#00ff00")
    (set-face-background 'highlight-indentation-current-column-face "#00ff00")))

(when (string-match "twilight" (getenv "emacs_theme"))
  (when (not (file-directory-p "~/.emacs.d/themes"))
    (mkdir "~/.emacs.d/themes" "-p"))
  (unless (file-exists-p "~/.emacs.d/themes/twilight-theme.el")
    (url-copy-file
     "https://raw.githubusercontent.com/jrblevin/config/master/.emacs.d/themes/twilight-theme.el"
     "~/.emacs.d/themes/twilight-theme.el"))
  (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes"))
  (load-theme 'twilight t)
  (message "Loading theme Twilight...")
  (when (string-equal system-type "berkeley-unix")
    (ccs-region)
    (ccs-linum)
    (ccs-ivy)
    (ccs-swiper)
    (ccs-highlight-indentation))
  (enable-theme 'twilight))

(provide 'themetwilightccs)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;; ex: ts=2 sw=2 et ft=sh
;;; themetwilightccs ends here
