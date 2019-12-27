;;; package --- Summary:
;;; Commentary:
;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(package-initialize)


(unless package-archive-contents (package-refresh-contents))
(add-to-list 'load-path "~/.emacs.d/mmccs")

(require 'clipboardccs)
(require 'colorthemeccs)
(require 'projectileccs)
(require 'keybindingsccs)
(require 'flycheckccs)
(require 'flyspellccs)
(require 'highlightchangesccs)
(require 'indentationccs)
(require 'highlightparenthesesccs)
(require 'cursorccs)
(require 'aggressiveindentccs)

;; ------------------------------------
;; `Major-Mode-Specific-Configurations'
;; ------------------------------------

(require 'emacslispccs)
(require 'exccs)
(require 'erlccs)
(require 'shccs)
(require 'mdccs)
(require 'confunixccs)
(require 'groovyccs)
(require 'fundamentalccs)
(require 'eshellccs)
(require 'dockerfileccs)
(require 'jenkinsfileccs)
(require 'yamlccs)
(require 'changelogccs)
(require 'javascriptccs)
(require 'webfccs)

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
 '(fci-rule-color "#515151")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(flycheck-highlighting-mode (quote symbols))
 '(frame-background-mode (quote dark))
 '(global-flycheck-mode t)
 '(js-auto-format-command "prettier")
 '(js-auto-format-command-args "--write --single-quote --no-semi")
 '(mode-require-final-newline t)
 '(nlinum-format " %3i ")
 '(nlinum-highlight-current-line t)
 '(nlinum-widen t)
 '(package-selected-packages (quote (highlight-parentheses beacon diredful auto-complete nlinum
                                                           exec-path-from-shell flycheck-inline
                                                           flycheck-title flycheck-pos-tip
                                                           flycheck-color-mode-line flycheck
                                                           whitespace-cleanup-mode company
                                                           projectile color-theme-modern load-dir)))
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
 '(mode-line-buffer-id ((t
                         (:background "black"
                                      :foreground "cyan"
                                      :weight extra-bold
                                      :height 0.9))))
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
(require 'company)

;; ----------------------------------
;; `final-newline' Final New Line EOF
;; ----------------------------------
;; `mode-require-final-newline' is defined above. Check
;; `custome-set-variables'
(setq require-final-newline (quote mode-require-final-newline))

;; ------------
;; `Whitespace'
;; ------------
(unless (package-installed-p 'whitespace-cleanup-mode)
  (package-install 'whitespace-cleanup-mode))
(require 'whitespace-cleanup-mode)
(require 'whitespace)

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
