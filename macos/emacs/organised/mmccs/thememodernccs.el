;;; package --- Summary:
;;; Use 'Theme-`modern'
;; Package-Requires:
;;; Commentary:
;;
;; Reference:
;; [](https://github.com/emacs-jp/replace-colorthemes)
;;
;; Start Emacs with this theme as follows:
;;
;; env Emacs_theme=modern Emacs
;;
;; For permanent use: set environment variable:
;;
;; export emacs_theme=modern
;;
;;; Code:

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
         (set-face-attribute 'region t
                             :background "#5f5f00")
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
        (t (add-to-list 'default-frame-alist '(background-color . "#080808")))))

(provide 'thememodernccs)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;; ex: ts=2 sw=2 et ft=sh
;;; thememodernccs ends here
