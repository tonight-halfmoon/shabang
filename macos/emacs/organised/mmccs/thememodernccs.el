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

(defvar ccs-gray "#1c1c1c")
(defvar ccs-gray-golden "#87875f")

(when (string-equal system-type "berkeley-unix")
  (setq ccs-gray "#444444")
  (setq ccs-gray-golden "#4e4e4e"))

(when (string-match "modern" (getenv "emacs_theme"))
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'color-theme-modern)
    (package-install 'color-theme-modern))
  (cond ((string-equal (getenv "emacs_theme_modern_mode") "tonight")
         (load-theme 'midnight t t)
         (enable-theme 'midnight))
        ((string-equal (getenv "emacs_theme_modern_mode") "hober")
         (load-theme 'hober t t)
         (enable-theme 'hober))
        ((string-equal (getenv "emacs_theme_modern_mode") "charcoal")
         (load-theme 'charcoal-black t t)
         (message "Loading theme charcoal-black")
         (enable-theme 'charcoal-black))
        ((string-equal (getenv "emacs_theme_modern_mode") "black-on-gray")
         (load-theme 'black-on-gray t t)
         (message "Loading theme black-on-gray")
         (add-to-list 'default-frame-alist '((background-color . ccs-gray-golden)))
         (enable-theme 'black-on-gray))
        ((string-equal (getenv "emacs_theme_modern_mode") "desert")
         (load-theme 'desert t t)
         (enable-theme 'desert))
        ((string-equal (getenv "emacs_theme_modern_mode") "gray30")
         (load-theme 'gray30 t t)
         (message "Loading theme gray30...")
         (set-face-attribute 'region t
                             :background "#5f5f00")
         (add-to-list 'default-frame-alist '((background-color . ccs-gray)))
         (enable-theme 'gray30))
        ((string-equal (getenv "emacs_theme_modern_mode") "late-night")
         (load-theme 'late-night t t)
         (enable-theme 'late-night))
        (t (load-theme 'taming-mr-arneson t t)
           (enable-theme 'taming-mr-arneson)
           (set-face-attribute 'minibuffer-prompt t
                               :foreground "#5fd7ff"))))

;; (cond ((string-equal (getenv "frame_background_mode") "light")
;;        (setq frame-background-mode 'light))
;;       (t
;;        (setq frame-background-mode 'dark)))

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
