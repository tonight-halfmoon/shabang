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

;; ------------------------
;; `Highlight-changes' mode
;; ------------------------
(add-hook 'text-mode-hook 'highlight-changes-mode)
(add-hook 'prog-mode-hook 'highlight-changes-mode)

(unless (package-installed-p 'load-dir)
  (package-install 'load-dir))
(require 'load-dir)
;; (load-dir-one "~/.emacs.d/mmccs")
(add-to-list 'load-path "~/.emacs.d/mmccs")

(require 'clipboardccs)
(require 'colorthemeccs)
(require 'projectileccs)
(require 'keybindingsccs)
(require 'flycheckccs)
(require 'flyspellccs)

;; ----------------------------------------
;; `Major-Mode-Specific-Configurations' ...
;; ----------------------------------------

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
 '(beacon-color "#f2777a")
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a"
                              "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d"
                              default)))
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
 '(package-selected-packages (quote (diredful auto-complete nlinum exec-path-from-shell
                                              flycheck-inline flycheck-title flycheck-pos-tip
                                              flycheck-color-mode-line flycheck
                                              whitespace-cleanup-mode company projectile
                                              color-theme-modern load-dir)))
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

;; ------------------------
;; `Screen-Configurations'
;; -------------------------
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
;; (add-hook 'after-init-hook 'global-company-mode)

;; -------------
;; `Indentation'
;; -------------
;; Major-mode-specific configurations
;; highlight-indentation
;; Reference:
;; [](https://github.com/antonj/Highlight-Indentation-for-Emacs)
(add-to-list 'load-path "~/.emacs.d/indent")
(require 'highlight-indentation)
(add-hook 'text-mode-hook 'highlight-indentation-mode)
(add-hook 'text-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'prog-mode-hook 'highlight-indentation-mode)
(add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)

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

;; -----------
;; `Mode-line'
;; -----------
(set-face-foreground 'mode-line "green")
(set-face-background 'mode-line "purple")

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

;; ---------
;; `Cursor'
;; ---------
(setq-default cursor-type 'hbar)
;; (set-cursor-color "#7F00FF")
(set-cursor-color "#6f75ff")
(setq-default x-stretch-cursor t)

;; ------------------------------------------
;; `paren' Matching pairs - Show `paren` mode
;; ------------------------------------------
(show-paren-mode 1)
;; (global-set-key "%" 'match-paren)

;; emacs!
(provide '.emacs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; .emacs ends here
