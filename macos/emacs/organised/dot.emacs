;;; package --- Summary:
;;; Commentary:
;;; Code:

(require 'package)

(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(load-file "~/.emacs.d/mmccs/features.el")
(load "features"
      (require 'features))

;; ------
;; Nlinum
;; ______
(unless (package-installed-p 'nlinum)
  (package-install 'nlinum))
(global-nlinum-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#2d2d2d" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc"
                                   "#66cccc" "#cccccc"))
 '(custom-safe-themes (quote ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
                              default)))
 '(fci-rule-color "#515151")
 '(frame-background-mode (quote dark))
 '(mode-require-final-newline t)
 '(nlinum-format " %3i ")
 '(nlinum-highlight-current-line t)
 '(nlinum-widen t)
 '(package-selected-packages (quote (diredful nlinum yaml-mode whitespace-cleanup-mode swiper
                                              smart-mode-line projectile markdown-mode
                                              js-auto-format-mode highlight-parentheses
                                              highlight-indentation groovy-mode gitignore-mode
                                              gitconfig-mode flyspell-correct-popup flycheck-title
                                              flycheck-pos-tip flycheck-mix flycheck-color-mode-line
                                              exec-path-from-shell dockerfile-mode
                                              color-theme-modern beacon auto-complete alchemist
                                              aggressive-indent)))
 '(safe-local-variable-values (quote ((sh-indent-comment . t)
                                      (allout-layout . t))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#f2777a")
                                 (40 . "#f99157")
                                 (60 . "#ffcc66")
                                 (80 . "#99cc99")
                                 (100 . "#66cccc")
                                 (120 . "#6699cc")
                                 (140 . "#cc99cc")
                                 (160 . "#f2777a")
                                 (180 . "#f99157")
                                 (200 . "#ffcc66")
                                 (220 . "#99cc99")
                                 (240 . "#66cccc")
                                 (260 . "#6699cc")
                                 (280 . "#cc99cc")
                                 (300 . "#f2777a")
                                 (320 . "#f99157")
                                 (340 . "#ffcc66")
                                 (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t
                     (:background "color-24"
                                  :foreground "white"))))
 '(company-tooltip-common ((t
                            (:foreground "white"))))
 '(company-tooltip-selection ((t
                               (:background "cyan"
                                            :weight bold))))
 '(highlight-changes ((t
                       (:foreground "yellow"))))
 '(highlight-changes-delete ((t
                              (:foreground "yellow"
                                           :underline nil))))
 '(nlinum-current-line ((t
                         (:inherit linum
                                   :foreground "magenta"
                                   :weight bold))))
 '(whitespace-big-indent ((t nil)))
 '(whitespace-space ((t
                      (:bold t
                             :foreground "green"))))
 '(whitespace-trailing ((t
                         (:foreground "green"
                                      :weight bold)))))

;; -----------
;; `CamelCase'
;; -----------
(global-subword-mode t)

;; -----------------------
;; `Screen-Configurations'
;; -----------------------
(setq inhibit-startup-screen t)
(when window-system (set-frame-size (selected-frame) 64 51))

;; -------------------------------------
;; `company-mode' `COMP'lete `ANY'thing!
;; -------------------------------------
;; Either utilise `company-mode' or `auto-complete-mode'
;; """ To turn on for a particular major-mode, check major modes """
(unless (package-installed-p 'company)
  (package-install 'company))

;; ----------------------------------
;; `final-newline' Final New Line EOF
;; ----------------------------------
;; `mode-require-final-newline' is defined above. Check
;; `custome-set-variables'
(setq require-final-newline (quote mode-require-final-newline))

;; ----------------
;; `Font-Lock' mode
;; ----------------
(font-lock-mode t)

;; ----------------------------------
;; `diredful'  - customise dired mode
;; ----------------------------------
;; Reference:
;; [](https://www.emacswiki.org/emacs/Diredful)
;; Post-setup configurations
;; Run diredful-add.
;; generated configuration file path
;; ~/.emacs.d/diredful-conf.el
(unless (package-installed-p 'diredful)
  (package-install 'diredful))
(diredful-mode 1)

;; ---------------
;; `Edit-in-place'
;; ---------------
(delete-selection-mode t)

;; ---------------
;; `Debug' support
;; ---------------
(setq debug-on-error t)

;; emacs!
(provide '.emacs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; .emacs ends here
