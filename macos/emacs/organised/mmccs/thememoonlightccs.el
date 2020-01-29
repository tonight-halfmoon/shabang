;;; package --- Summary:
;;; Customise `Theme-Moonlight'
;; Package-Requires:
;;; Commentary:
;;
;; Choose a library
;;
;; Usage guide:
;;
;; Before starting a Emacs Server instance
;; set environment variable `theme` as follows:
;;
;; Setting Environment Variable:
;;
;; Option (1)
;; `export emacs_theme=moonlight`
;;
;; Option (2)
;;
;; `env emacs_theme=moonlight Emacs`
;;
;;; Code:

(defun ccs-linum ()
  "Configure linum."
  (with-eval-after-load 'linum (set-face-attribute 'linum t
                                                   :background "#080808"
                                                   :foreground "#444444")))

(defun ccs-region ()
  "Configure region highlight."
  (set-face-attribute 'region t
                      :background "#808080"
                      :foreground "#8787af"))

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
    (set-face-attribute 'flyspell-incorrect t
                        :underline nil
                        :background "#444444"
                        :foreground "#808080"
                        :weight 'bold
                        :inverse-video t)))

(defun ccs-highlight-changes()
  "Configure Highlight-Changes."
  (with-eval-after-load 'hilit-chg
    ;;
    (set-face-attribute 'highlight-changes nil
                        :foreground "#afd7af")))

(when (string-match "moonlight" (getenv "emacs_theme"))
  (add-to-list 'package-pinned-packages '(doom-themes . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'doom-themes)
    (package-install 'doom-themes))
  (load-theme 'doom-moonlight t)
  (message "Loading theme doom-moonlight...")
  (enable-theme 'doom-moonlight)
  (add-to-list 'default-frame-alist '(background-color . "#080808"))
  (ccs-region)
  (ccs-linum)
  (ccs-highlight-indentation)
  (ccs-flyspell)
  (ccs-highlight-changes))

(provide 'thememoonlightccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; thememoonlightccs ends here
