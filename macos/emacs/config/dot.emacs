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

(setq load-path (cons "/usr/local/Cellar/erlang/21.0.8/lib/erlang/lib/tools-3.0/emacs" load-path))
(setq erlang-root-dir "/usr/local/Cellar/erlang/21.0.8/lib/erlang")
(setq exec-path (cons "/usr/local/Cellar/erlang/21.0.8/lib/erlang/bin" exec-path))
(require 'erlang-start)

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
 '(custom-enabled-themes (quote (fogus)))
 '(custom-safe-themes
   (quote
    ("3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" default)))
 '(global-flycheck-mode t)
 '(linum-format " %7i ")
 '(package-selected-packages (quote (flycheck-kotlin company))))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq-default cursor-type 'hbar)
;;(set-cursor-color "#7F00FF")
(setq-default x-stretch-cursor 1)
(global-set-key (kbd "M-9") 'kill-whole-line)
(setq inhibit-startup-screen t)
(when window-system (set-frame-size (selected-frame) 64 51))

;; Note: With the following entry Meta becomes key 'cmd'
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(add-hook 'after-init-hook 'global-company-mode)

;; Image dimensions
;; Reference: https://www.emacswiki.org/emacs/image-dimensions-minor-mode.el
;; Display the image dimensions in the mode line, when viewing an image.

(load "~/.emacs.d/image-dimensions/image-dimentsions-minor-mode.el")
(eval-after-load 'image-mode '(require 'image-dimensions-minor-mode))

 (setq frame-title-format
       '(buffer-file-name
         ("%b (Emacs) %f" image-dimensions-minor-mode-dimensions)
         (dired-directory
          (:eval (concat (buffer-name) " (Emacs) " dired-directory))
          ("%b (Emacs)"))))

;;; format-all.el --- Auto-format C, C++, JS, Python, Ruby and 25 other languages -*- lexical-binding: t -*-

(load "~/.emacs.d/format-all/format-all.el")


(set-cursor-color "#00ff00")

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (face-remap-add-relative
     'mode-line '((:foreground "cyan" :background "black") mode-line))))

;; Kotlin Mode
;; clone or copy https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode/blob/master/kotlin-mode.el into ~/.emacs.d/
(load "~/.emacs.d/kotlin/kotlin-mode.el")

;; Scala Mode
;; clone https://github.com/ensime/emacs-scala-mode.git into ~/.emacs.d/
(add-to-list 'load-path "~/.emacs.d/emacs-scala-mode/")
(require 'scala-mode)

;; MELPA
;;(add-to-list 'package-archives
;;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


;; M-x
;; package-refresh-list
;; pacakge-install flycheck-kotlin
(require 'flycheck-kotlin)
(add-hook 'kotlin-mode-hook 'flycheck-mode)

;(provide .emacs)
;;; .emacs ends here



