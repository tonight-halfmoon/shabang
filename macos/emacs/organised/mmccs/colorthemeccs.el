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
;; `export emacs_theme=modern`
;;
;; Option (2)
;;
;; `env emacs_theme=modern Emacs`
;;
;;; Code:

(when (string-equal (getenv "emacs_theme") nil)
  (setenv "emacs_theme" "tomorrow"))

;; --------------------
;; `Sanityinc-Tomorrow'
;; ====================
;;
;; Reference:
;; [](https://github.com/purcell/color-theme-sanityinc-tomorrow)

(when (string-equal (getenv "emacs_theme") "tomorrow")
  (add-to-list 'package-pinned-packages '(color-theme-sanityinc-tomorrow . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'color-theme-sanityinc-tomorrow)
    (package-install 'color-theme-sanityinc-tomorrow))
  (load-theme 'sanityinc-tomorrow-night t)
  (add-to-list 'default-frame-alist '(background-color . black))
  (when (string-equal (getenv "emacs_background") "bright")
    (add-to-list 'default-frame-alist '(background-color . "#4d4d4d")))
  (enable-theme 'sanityinc-tomorrow-night))

;; -----------------------
;; `Solarized'
;; =======================
;;
;; git clone https://github.com/sellout/emacs-color-theme-solarized.git

(when (string-equal (getenv "emacs_theme") "solarized")
  (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/emacs-color-theme-solarized"))
  (load-theme 'solarized t)
  (set-variable 'solarized-termcolors 256)
  (enable-theme 'solarized))

;; --------------------
;; `Modern'
;; ====================
;;
;; Reference:
;; [](https://github.com/emacs-jp/replace-colorthemes)
;; Start Emacs with this theme as follows:
;; `env theme=modern emacs`

(when (string-equal (getenv "emacs_theme") "modern")
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'color-theme-modern)
    (package-install 'color-theme-modern))
  (cond ((string-equal (getenv "emacs_theme_modern_mode") "tonight")
         (load-theme 'midnight t t)
         (enable-theme 'midnight))
        ((string-equal (getenv "emacs_theme_modern_mode") "dark")
         (load-theme 'tty-dark t t)
         (enable-theme 'tty-dark))
        ((string-equal (getenv "emacs_theme_modern_mode") "lethe")
         (load-theme 'lethe t t)
         (enable-theme 'lethe))
        ((string-equal (getenv "emacs_theme_modern_mode") "hober")
         (load-theme 'hober t t)
         (enable-theme 'hober))
        ((string-equal (getenv "emacs_theme_modern_mode") "charcoal")
         (load-theme 'charcoal-black t t)
         (enable-theme 'charcoal-black))
        ((string-equal (getenv "emacs_theme_modern_mode") "black-on-gray")
         (load-theme 'black-on-gray t t)
         (enable-theme 'black-on-gray))
        ((string-equal (getenv "emacs_theme_modern_mode") "clarity")
         (load-theme 'clarity t t)
         (enable-theme 'clarity))
        ((string-equal (getenv "emacs_theme_modern_mode") "desert")
         (load-theme 'desert t t)
         (enable-theme 'desert))
        ((string-equal (getenv "emacs_theme_modern_mode") "gray30")
         (load-theme 'gray30 t t)
         (enable-theme 'gray30))
        ((string-equal (getenv "emacs_theme_modern_mode") "late-night")
         (load-theme 'late-night t t)
         (enable-theme 'late-night))
        ((string-equal (getenv "emacs_theme_modern_mode") "matrix")
         (load-theme 'matrix t t)
         (enable-theme 'matrix))
        ((string-equal (getenv "emacs_theme_modern_mode") "renegade")
         (load-theme 'renegade t t)
         (enable-theme 'renegade))
        (t (load-theme 'taming-mr-arneson t t)
           (enable-theme 'taming-mr-arneson)
           (set-face-attribute 'minibuffer-prompt t
                               :foreground "#5fffff")))
  (cond ((string-equal (getenv "frame_background_mode") "light")
         (setq frame-background-mode 'light))
        (t
         (setq frame-background-mode 'dark)))
  (cond ((string-equal system-type "berkeley-unix")
         (set-face-attribute 'minibuffer-prompt nil
                             :foreground "cyan"))
        (t ()))
  ;; background colour
  (cond ((string-equal (getenv "emacs_background" ) "bright")
         (add-to-list 'default-frame-alist '(background-color . "#4d4d4d")))
        (t (add-to-list 'default-frame-alist '(background-color . black)))))

(when (string-equal (getenv "emacs_theme") "nimbus")
  (add-to-list 'package-pinned-packages '(nimbus-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'nimbus-theme)
    (package-install 'nimbus-theme))
  (load-theme 'nimbus t))

(when (string-equal (getenv "emacs_theme") "ample")
  (add-to-list 'package-pinned-packages '(ample-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'ample-theme)
    (package-install 'ample-theme))
  (load-theme 'ample-flat t t)
  (set-face-attribute 'region t
                      :background "#808080")
  (enable-theme 'ample-flat))

;; -----------
;; `Mode-line'
;; -----------

(set-face-background 'mode-line "#00ff00")

(set-face-attribute 'mode-line-buffer-id nil
                    :background "#ff00ff"
                    :foreground "#444444")

(provide 'colorthemeccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; colorthemeccs ends here
