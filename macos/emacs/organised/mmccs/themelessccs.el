;;; package --- Summary
;;; Setup `Theme-Less'
;; Package-Requires:
;;; Commentary:
;; Reference:
;; [](https://jblevins.org/log/color-theme-less)
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

(defun ccs-error ()
  "Configure face error."
  (set-face-attribute 'error nil
                      :underline nil
                      :foreground "#ff00af"))

(defun ccs-warning ()
  "Configure face warning."
  (set-face-attribute 'warning nil
                      :underline nil
                      :foreground "#ff87af"))

(defun ccs-ido-first-match ()
  "Configure ido first match."
  (with-eval-after-load 'ido
    ;;
    (set-face-attribute 'ido-first-match nil
                        :foreground "#87afaf"
                        :inherit nil)))

(defun ccs-ido-subdir ()
  "Configure 'ido-subdir."
  (with-eval-after-load 'ido
    ;;
    (set-face-attribute 'ido-subdir nil
                        :foreground "#87afaf")))

(defun ccs-hl-line ()
  "Configure hl-line."
  (with-eval-after-load 'hl-line
    ;;
    (set-face-background 'hl-line "#121212")))

(defun ccs-dired ()
  "Configure dired."
  (with-eval-after-load 'dired
    ;;
    (set-face-attribute 'dired-directory t
                        :foreground "#5fd7ff")))

(defun ccs-swiper ()
  "Configure swiper."
  (with-eval-after-load 'swiper
    ;;
    (set-face-attribute 'swiper-line-face t
                        :foreground "#a8a8a8"
                        :background "#303030")))

(defun ccs-region ()
  "Configure region."
  (set-face-attribute 'region nil
                      :inherit nil
                      :weight 'bold
                      :foreground "#9e9e9e"
                      :background "#585858"))

(defun ccs-highlight-changes ()
  "Configure highlight-changes."
  (eval-when-compile
    (require 'hilit-chg))
  (with-eval-after-load 'hilit-chg
    ;;
    (setq highlight-changes-face-list nil)
    ;;
    (set-face-attribute 'highlight-changes nil
                        :foreground "#afff87")
    ;;
    (set-face-attribute 'highlight-changes-delete nil
                        :underline nil
                        :inherit nil
                        :foreground "111")))

(defun ccs-highlight-indentation ()
  "Configure highlight-indentation."
  (with-eval-after-load 'highlight-indentation
    ;;
    (set-face-background 'highlight-indentation-face "#87afaf")
    (set-face-background 'highlight-indentation-current-column-face "#87afaf")))

(defun ccs-flyspell ()
  "Configure Fly Spell."
  (eval-when-compile
    (require 'flyspell))
  (with-eval-after-load 'flyspell
    ;;
    (set-face-attribute 'flyspell-duplicate nil
                        :underline nil
                        :foreground nil
                        :background nil
                        :inherit nil
                        :inverse-video nil)
    (set-face-attribute 'flyspell-incorrect t
                        :underline nil
                        :inherit nil
                        :inverse-video t)))

(defun ccs-flycheck ()
  "Configure Flycheck."
  (with-eval-after-load 'flycheck
    ;;
    (set-face-attribute 'flycheck-error nil
                        :underline nil)
    (set-face-attribute 'flycheck-warning nil
                        :underline nil)
    (set-face-attribute 'flycheck-info nil
                        :underline nil)))

(defun ccs-theme-less ()
  "Theme less configure, load and enable."
  (when (string-match "less" (getenv "emacs_theme"))
    (when (not (file-directory-p "~/.emacs.d/themes"))
      (mkdir "~/.emacs.d/themes" "-p"))
    (when (not (file-exists-p "~/.emacs.d/themes/less-theme.el"))
      (url-copy-file "https://jblevins.org/git/misc.git/plain/less-theme.el"
                     "~/.emacs.d/themes/less-theme.el" t))
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
    (load-theme 'less t)
    (message "Loading theme Less...")
    (ccs-warning)
    (ccs-error)
    (ccs-highlight-indentation)
    (ccs-highlight-changes)
    (ccs-flyspell)
    (ccs-flycheck)
    (ccs-swiper)
    (ccs-region)
    (ccs-dired)
    (ccs-hl-line)
    (ccs-ido-subdir)
    (ccs-ido-first-match)
    (enable-theme 'less)))

(ccs-theme-less)

(provide 'themelessccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; themelessccs ends here
