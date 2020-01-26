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

(when (string-equal (getenv "emacs_theme") "nimbus")
  (add-to-list 'package-pinned-packages '(nimbus-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'nimbus-theme)
    (package-install 'nimbus-theme))
  (load-theme 'nimbus t)
  (message "Loading theme nimbus...")
  (enable-theme 'nimbus))

(when (string-equal (getenv "emacs_theme") "ample")
  (add-to-list 'package-pinned-packages '(ample-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'ample-theme)
    (package-install 'ample-theme))
  (load-theme 'ample-flat t t)
  (set-face-attribute 'region t
                      :background "#808080")
  (message "Loading theme ample...")
  (enable-theme 'ample-flat))

;; (nice to be the default theme)
(when (string-equal (getenv "emacs_theme") "monochrome")
  (add-to-list 'package-pinned-packages '(quasi-monochrome-theme . "melpa") t)
  (package-refresh-contents)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'quasi-monochrome-theme)
    (package-install 'quasi-monochrome-theme))
  (load-theme 'quasi-monochrome t t)
  (set-face-attribute 'region t
                      :background "#808080")
  (setq sml/theme 'respectful)
  (sml/setup)
  (message "Loading theme monochrome...")
  (enable-theme 'quasi-monochrome))

(when (string-equal (getenv "emacs_theme") "dracula")
  (add-to-list 'package-pinned-packages '(dracula-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'dracula-theme)
    (package-install 'dracula-theme))
  (load-theme 'dracula t t)
  (set-face-attribute 'region t
                      :background "#808080")
  (message "Loading theme dracula...")
  (enable-theme 'dracula))

(when (string-equal (getenv "emacs_theme") "vscode-dark")
  (mkdir "~/.emacs.d/themes" "-p")
  (url-copy-file
   "https://raw.githubusercontent.com/ianpan870102/vscode-dark-emacs-theme/master/vscode-dark-theme.el"
   "~/.emacs.d/themes/vscode-dark-theme.el" t)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (load-theme 'vscode-dark t t)
  (message "Loading theme vscode-dark...")
  (enable-theme 'vscode-dark))

(when (string-equal (getenv "emacs_theme") "overcast")
  (add-to-list 'package-pinned-packages '(overcast-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (package-refresh-contents)
  (unless (package-installed-p 'overcast-theme)
    (package-install 'overcast-theme))
  (load-theme 'overcast t t)
  ;;(add-to-list 'default-frame-alist '(background-color . "#080808"))
  (message "Loading theme overcast...")
  (enable-theme 'overcast))

;; (set-face-background 'mode-line "#00ff00")

;; (set-face-attribute 'mode-line-buffer-id nil
;;                     :background "#ff00ff"
;;                     :foreground "#444444")

(provide 'colorthemeccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; colorthemeccs ends here
