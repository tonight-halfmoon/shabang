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
;; `export theme=amado`
;;
;; Option (2)
;;
;; `env theme=amado Emacs`
;;
;;; Code:

(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes"))

(when (string-equal (getenv "emacs_theme") nil)
  (setenv "emacs_theme" "tomorrow"))

;; --------------------
;; `Sanityinc-Tomorrow'
;; ====================
;;
;; Reference:
;; [](https://github.com/purcell/color-theme-sanityinc-tomorrow)

(when (string-equal (getenv "emacs_theme") "tomorrow")
  (progn (add-to-list 'package-pinned-packages '(color-theme-sanityinc-tomorrow . "melpa") t)
         (unless package-archive-contents (package-refresh-contents))
         (unless (package-installed-p 'color-theme-sanityinc-tomorrow)
           (package-install 'color-theme-sanityinc-tomorrow))
         (load-theme 'sanityinc-tomorrow-night t)
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
         ;;julie ;; hober ;; ld-dark ;; oswald ;; matrix ;; 'railscast 'dark-font-lock
         (load-theme 'railscast t t)
         (enable-theme 'railscast)))

;; -------
;; `amado'
;; =======
;;
;; Reference:
;; [](http://nongnu.org/color-theme)
;;
;; wget http://download.savannah.nongnu.org/releases/color-theme/color-theme-6.6.0.tar.gz
;; `7z x color-theme-6.6.0.tar.gz` into the `color-theme' directory

(when (string-equal (getenv "emacs_theme") "amado")
  (progn (add-to-list 'load-path "~/.emacs.d/color-theme")
         (require 'color-theme)
         (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/color-theme/themes"))
         (with-eval-after-load "color-theme" '(progn (color-theme-initialize)
                                                     (color-theme-amado)))
         (custom-set-faces '(mode-line-buffer-id ((t
                                                   (:background "black"
                                                                :foreground "magenta"
                                                                :weight extra-bold
                                                                :height 0.9)))))))
;; -----------
;; `Mode-line'
;; -----------

(defun customise-mode-line-erlang-mode-hook ()
  "Customise Mode Line hook."
  (set-face-background 'mode-line "yellow"))

(add-hook 'erlang-mode-hook #'customise-mode-line-erlang-mode-hook)

;; colorthemeccs!
(provide 'colorthemeccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; colorthemeccs ends here
