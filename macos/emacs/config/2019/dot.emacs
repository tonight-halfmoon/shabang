;;; package --- Summary:

;;; Commentary:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;(add-to-list 'load-path "/usr/local/Cellar/erlang/20.3.4/lib/erlang/lib/wrangler-1.2.0/elisp")
;;(require 'wrangler)

(add-to-list 'custom-theme-load-path "/Users/amado/.emacs.d/themes")
;;(load-theme 'odersky t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   (vector "#515151" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#cccccc"))
 '(beacon-color "#f2777a")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#515151")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(global-flycheck-mode t)
 '(linum-format " %7i ")
 '(package-selected-packages
   (quote
    (format-all docker-compose-mode docker dockerfile-mode dictionary latex-extra latex-preview-pane color-theme-sanityinc-tomorrow flycheck-kotlin company)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
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
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; MELPA
;;(add-to-list 'package-archives
;;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq-default cursor-type 'hbar)
;;(set-cursor-color "#7F00FF")
;;(set-cursor-color "#00ff00")
(set-cursor-color "#6f75ff")
(setq-default x-stretch-cursor 1)

(global-set-key (kbd "M-9") 'kill-whole-line)

(setq inhibit-startup-screen t)

(when window-system (set-frame-size (selected-frame) 64 51))

;; Note: With the following entry Meta becomes key 'cmd'
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      select-enable-clipboard t)

(add-hook 'after-init-hook 'global-company-mode)

(require 'whitespace)

;; Formatting according to the file's programming langauge

(add-hook 'before-save-hook (lambda() (format-all-mode)))

;;; Whitespace Cleanup on/before Save Hook
;;(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (whitespace-cleanup)))

;; Delete Trailing Whitespace on/before Save Hook
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; Indent according to current Major Mode on/before Save Hook
(add-hook 'before-save-hook (lambda() (indent-according-to-mode)))

;; Erlang Mode
(setq load-path (cons "/usr/local/Cellar/erlang/22.0.2/lib/erlang/lib/tools-3.2/emacs" load-path))
(setq erlang-root-dir "/usr/local/Cellar/erlang/22.0.2/lib/erlang")
(setq exec-path (cons "/usr/local/Cellar/erlang/22.0.2/lib/erlang/bin" exec-path))
(require 'erlang-start)

(add-to-list 'auto-mode-alist '("\\.config?$" . erlang-mode))

;; See method #2 follows
;;(add-hook 'before-save-hook 'erlang-indent-current-buffer)

(setq-default erlang-indent-level 2)
(setq-default allout-auto-activation t)
(setq-default erlang-indent-paranthesis 2)

;; Method #2
(defun esl-erlang-mode-before-save-hook()
  "When Major Mode is Erlang then apply the following on/before save."
  (when (eq major-mode 'erlang-mode)
    (whitespace-cleanup)
    (delete-trailing-whitespace)
    (erlang-indent-current-buffer)
    )
  )
(add-hook 'before-save-hook #'esl-erlang-mode-before-save-hook)


;; Format in erlang mode
;;(defun emacs-indent-function ()
;;  "Format the whole buffer."
;;   (erlang-mode)
;;   (indent-region (point-min) (point-max) nil)
;;   (untabify (point-min) (point-max))
;;   (delete-trailing-whitespace)
;;   (save-buffer)
;;	 )

;; Image dimensions
;; Reference: https://www.emacswiki.org/emacs/image-dimensions-minor-mode.el
;; Display the image dimensions in the mode line, when viewing an image.

(load "$HOME/.emacs.d/image-dimensions/image-dimensions-minor-mode.el")
(eval-after-load 'image-mode '(require 'image-dimensions-minor-mode))

(setq frame-title-format
      '(buffer-file-name
        ("%b (Emacs) %f" image-dimensions-minor-mode-dimensions)
        (dired-directory
         (:eval (concat (buffer-name) " (Emacs) " dired-directory))
         ("%b (Emacs)"))))


;; Format All buffer installed via package manager MELPA
;;; format-all.el --- Auto-format C, C++, JS, Python, Ruby and 25 other languages -*- lexical-binding: t -*-

;;(load "$HOME/.emacs.d/format-all/format-all.el")

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (face-remap-add-relative
             'mode-line '((:foreground "cyan" :background "black") mode-line))))

;; Kotlin Mode
;; clone or copy https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode/blob/master/kotlin-mode.el into $HOME/.emacs.d/
(load "$HOME/.emacs.d/kotlin/kotlin-mode.el")

;; Scala Mode
;; clone https://github.com/ensime/emacs-scala-mode.git into $HOME/.emacs.d/
;;(add-to-list 'load-path "$HOME/.emacs.d/emacs-scala-mode/")
;;(require 'scala-mode)

;; M-x
;; package-refresh-list
;; pacakge-install flycheck-kotlin
(require 'flycheck-kotlin)
(add-hook 'kotlin-mode-hook 'flycheck-mode)

;; Color-theme Sanityinc
;; Ref: https://github.com/purcell/color-theme-sanityinc-tomorrow
;; M-x package-install RET color-theme-sanityinc-tomorrow RET
(require 'color-theme-sanityinc-tomorrow)
;; then M-x color-theme-sanityinc-tomorrow-... /-night /-eighties -etc

;; Spell check
(add-hook 'text-mode-hook 'flyspell-mode)

;; show closing bracket
(show-paren-mode 1)
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren ${ARG}; otherwise insert."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; Default tab width is 2
(setq-default tab-width 2)

;;; Indent
(setq-default indent-tabs-mode nil)


;; Dockerfile formatting
;; format-all installed via MELPA
(load "$HOME/.emacs.d/dockerfile-mode/dockerfile-mode.el")

;; Shell script formatting
;; format-all installed via MELPA
;; brew install shfmt


                                        ;(provide .emacs)
;;; .emacs ends here
