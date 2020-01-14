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
  (progn (eval-when-compile
           (require 'package))
         (add-to-list 'package-pinned-packages '(color-theme-sanityinc-tomorrow . "melpa") t)
         (unless package-archive-contents (package-refresh-contents))
         (unless (package-installed-p 'color-theme-sanityinc-tomorrow)
           (package-install 'color-theme-sanityinc-tomorrow))
         (load-theme 'sanityinc-tomorrow-night t)
         (add-to-list 'default-frame-alist '(background-color . black))
         (when (string-equal (getenv "emacs_background") "bright")
           (add-to-list 'default-frame-alist '(background-color . "brightblack")))))

;; ------------
;; `Nord'
;; ============

(when (string-equal (getenv "emacs_theme") "nord")
  (progn (unless package-archive-contents (package-refresh-contents))
         (unless (package-installed-p 'nord-theme)
           (package-install 'nord-theme))
         (load-theme 'nord t)))

;; -----------------------
;; `Solarized' 1
;; =======================
;;
;; git clone https://github.com/sellout/emacs-color-theme-solarized.git

(when (string-equal (getenv "emacs_theme") "solarized1")
  (progn (add-to-list 'custom-theme-load-path (expand-file-name
                                               "~/.emacs.d/emacs-color-theme-solarized"))
         (load-theme 'solarized t)))

;; -----------------
;; `Solarized' 2
;; =================

(when (string-equal (getenv "emacs_theme") "solarized2")
  (progn (unless package-archive-contents (package-refresh-contents))
         (unless (package-installed-p 'solarized-theme)
           (package-install 'solarized-theme))
         (load-theme 'solarized-dark t)))

;; --------------------
;; `Modern'
;; ====================
;;
;; Reference:
;; [](https://github.com/emacs-jp/replace-colorthemes)
;; Start Emacs with this theme as follows:
;; `env theme=modern emacs`

(when (string-equal (getenv "emacs_theme") "modern")
  (progn (unless package-archive-contents (package-refresh-contents))
         (unless (package-installed-p 'color-theme-modern)
           (package-install 'color-theme-modern))
         (cond ((string-equal (getenv "emacs_theme_mode") "tonight")
                (load-theme 'midnight t t)
                (enable-theme 'midnight))
               ((string-equal (getenv "emacs_theme_mode") "dark")
                (load-theme 'tty-dark t t)
                (enable-theme 'tty-dark))
               ((string-equal (getenv "emacs_theme_mode") "lethe")
                (load-theme 'lethe t t)
                (enable-theme 'lethe))
               ((string-equal (getenv "emacs_theme_mode") "hober")
                (load-theme 'hober t t)
                (enable-theme 'hober))
               ((string-equal (getenv "emacs_theme_mode") "charcoal")
                (load-theme 'charcoal-black t t)
                (enable-theme 'charcoal-black))
               (t (load-theme 'taming-mr-arneson t t)
                  (enable-theme 'taming-mr-arneson)
                  (set-face-attribute 'minibuffer-prompt t
                                      :foreground "color-87")))
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
                (add-to-list 'default-frame-alist '(background-color . "brightblack")))
               (t (add-to-list 'default-frame-alist '(background-color . black))))))

;; -----------
;; `Mode-line'
;; -----------

(defun customise-mode-line-erlang-mode-hook ()
  "Customise Mode Line hook."
  (set-face-background 'mode-line "yellow"))

(add-hook 'erlang-mode-hook #'customise-mode-line-erlang-mode-hook)

(provide 'colorthemeccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; colorthemeccs ends here
