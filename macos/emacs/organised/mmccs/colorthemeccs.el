;;; package --- Summary:
;;; Customise `Theme'
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
;; `export emacs_theme=<theme-name>`
;;
;; Option (2)
;;
;; `env emacs_theme=<theme-name> Emacs`
;;
;;; Code:

(unless (package-installed-p 'smart-mode-line)
  (package-install 'smart-mode-line))
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)

(defun ccs-linum ()
  "Configure linum."
  (with-eval-after-load 'linum (set-face-attribute 'linum t
                                                   :background "#080808"
                                                   :foreground "#444444")))

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

(when (string-match "nimbus" (getenv "emacs_theme"))
  (add-to-list 'package-pinned-packages '(nimbus-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'nimbus-theme)
    (package-install 'nimbus-theme))
  (load-theme 'nimbus t)
  (message "Loading theme nimbus...")
  (enable-theme 'nimbus))

(when (string-match "ample" (getenv "emacs_theme"))
  (add-to-list 'package-pinned-packages '(ample-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'ample-theme)
    (package-install 'ample-theme))
  (load-theme 'ample t t)
  (message "Loading theme ample...")
  (set-face-attribute 'region t
                      :background "#808080")
  (ccs-linum)
  (enable-theme 'ample))

;; (nice to be the default theme)
(when (string-match "monochrome" (getenv "emacs_theme"))
  (add-to-list 'package-pinned-packages '(quasi-monochrome-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'quasi-monochrome-theme)
    (package-install 'quasi-monochrome-theme))
  (load-theme 'quasi-monochrome t t)
  (message "Loading theme monochrome...")
  (set-face-attribute 'region t
                      :background "#808080")
  ;;(setq sml/theme 'dark)
  ;;(sml/setup)
  (enable-theme 'quasi-monochrome))

(when (string-match "vscode-dark" (getenv "emacs_theme"))
  (when (not (file-directory-p "~/.emacs.d/themes"))
    (mkdir "~/.emacs.d/themes" "-p"))
  (url-copy-file
   "https://raw.githubusercontent.com/ianpan870102/vscode-dark-emacs-theme/master/vscode-dark-theme.el"
   "~/.emacs.d/themes/vscode-dark-theme.el" t)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (load-theme 'vscode-dark t t)
  (message "Loading theme vscode-dark...")
  (ccs-highlight-indentation)
  (enable-theme 'vscode-dark))

(when (string-match "doom" (getenv "emacs_theme"))
  (add-to-list 'package-pinned-packages '(doom-themes . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'doom-themes)
    (package-install 'doom-themes))
  (load-theme 'doom-solarized-dark t t) ;; * * * * * *
  ;;(load-theme 'doom-tomorrow-night t t) ;; * * * * *
  ;;(load-theme 'doom-dark+ t) ;; * * * *
  ;;(load-theme 'doom-one t t) ;; * * * *
  ;;(load-theme 'doom-spacegrey t t)  ;; * * *
  ;;(load-theme 'doom-gruvbox t) ;; * * * *
  ;;(load-theme 'doom-moonlight t) ;; * * * * * (see `thememoonlightccs.el')
  (message "Loading theme doom-dracula...")
  (enable-theme 'doom-solarized-dark)
  ;;(enable-theme 'doom-tomorrow-night)
  ;;(enable-theme 'doom-one)
  ;;(enable-theme 'doom-dark+)
  ;;(enable-theme 'doom-spacegrey)
  ;;(enable-theme 'doom-gruvbox)
  (add-to-list 'default-frame-alist '(background-color . "#080808"))
  (ccs-region)
  (ccs-linum)
  (ccs-highlight-indentation)
  (ccs-flyspell)
  (ccs-highlight-changes))

(provide 'colorthemeccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; colorthemeccs ends here
