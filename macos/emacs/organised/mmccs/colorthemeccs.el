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

(eval-and-compile (when (string-equal (getenv "emacs_theme") nil)
                    (add-to-list 'package-pinned-packages '(color-theme-modern . "melpa") t)
                    (unless package-archive-contents (package-refresh-contents))
                    (unless (package-installed-p 'color-theme-modern)
                      (package-install 'color-theme-modern))
                    (load-theme 'late-night t t)
                    (enable-theme 'late-night)))

;; (when (string-equal (getenv "emacs_theme") nil)
;;   (setenv "emacs_theme" "nimbus"))

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

(defun true-color-p ()
  "Return non-nil on displays that support 256 colors."
  (or (display-graphic-p)
      (= (tty-display-color-cells) 16777216)))

(when (string-equal (getenv "emacs_theme") "nimbus")
  (add-to-list 'package-pinned-packages '(nimbus-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'nimbus-theme)
    (package-install 'nimbus-theme))
  (load-theme 'nimbus t)
  (let ((class '((class color)
                 (min-colors 89)))
        (default (if (true-color-p) "#abb2bf" "#afafaf"))
        (background-darker (if (true-color-p) "#22252c" "#222222"))
        (background-ligher (if (true-color-p) "#3a3f4b" "#5f5f5f"))
        (background-blue (if (true-color-p) "#38394c" "#444444"))))
  (require 'company)
  (set-face-attribute 'company-preview t
                      :background "background-darker"
                      :foreground "default")
  (set-face-attribute 'company-preview-common t
                      :background "background-darker"
                      :foreground "purple")
  (set-face-attribute 'company-preview-search t
                      :background "blue"
                      :foreground "default")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "background-darker")
  ;; (set-face-attribute 'company-scrollbar-fg t
  ;;                     :background "background-blue")
  (set-face-attribute 'company-tooltip t
                      :background "background-darker"
                      :foreground "default")
  (set-face-attribute 'company-tooltip-selection t
                      :background "background-lighter"
                      :foreground "lightgreen"
                      :weight 'bold)
  (set-face-attribute 'company-tooltip-common t
                      :background "background-darker"
                      :foreground "purple"
                      :weight 'bold)
  (set-face-attribute 'company-tooltip-common-selection t
                      :background "background-lighter"
                      :foreground "purple"
                      :weight 'bold)
  (set-face-attribute 'company-tooltip-mouse t
                      :background "background-lighter"
                      :foreground "default"))

(when (string-equal (getenv "emacs_theme") "zerodark")
  (add-to-list 'package-pinned-packages '(async . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(zerodark-theme . "melpa-stable") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'async)
    (package-install 'async))
  (unless (package-installed-p 'magit)
    (package-install 'magit))
  (unless (package-installed-p 'zerodark-theme)
    (package-install 'zerodark-theme))
  (when (not (file-directory-p "~/.emacs.d/all-the-icons"))
    (mkdir "-p" "~/.emacs.d/all-the-icons"))
  (when (not (file-exists-p "~/.emacs.d/all-the-icons/all-the-icons.ttf"))
    (all-the-icons-install-fonts))
  (setq inhibit-compacting-font-caches t)
  (load-theme 'zerodark t)
  ;; disable smartmodelineccs before-hand
  ;;(zerodark-setup-modeline-format)
  )

(when (string-equal (getenv "emacs_theme") "vscode-dark")
  (mkdir "~/.emacs.d/themes/" "-p")
  (url-copy-file
   "https://raw.githubusercontent.com/ianpan870102/vscode-dark-emacs-theme/master/vscode-dark-theme.el"
   "~/.emacs.d/themes/vscode-dark-theme.el" t)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (load-theme 'vscode-dark t))

(when (string-equal (getenv "emacs_theme") "zeno")
  (add-to-list 'package-pinned-packages '(zeno-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'zeno-theme)
    (package-install 'zeno-theme))
  (load-theme 'zeno t))

(when (string-equal (getenv "emacs_theme") "dracula")
  (add-to-list 'package-pinned-packages '(dracula-theme . "melpa") t)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'dracula-theme)
    (package-install 'dracula-theme))
  (load-theme 'dracula t))

;; -----------
;; `Mode-line'
;; -----------

(defun customise-mode-line-erlang-mode-hook ()
  "Customise Mode Line hook."
  (set-face-background 'mode-line "brightyellow"))

(add-hook 'erlang-mode-hook #'customise-mode-line-erlang-mode-hook)

(provide 'colorthemeccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; colorthemeccs ends here
