

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq load-path (cons "/usr/local/Cellar/erlang/21.0.8/lib/erlang/lib/tools-3.0/emacs" load-path))
(setq erlang-root-dir "/usr/local/Cellar/erlang/21.0.8/lib/erlang")
(setq exec-path (cons "/usr/local/Cellar/erlang/21.0.8/lib/erlang/bin" exec-path))
(require 'erlang-start)

;;(add-to-list 'load-path "/usr/local/Cellar/erlang/20.3.4/lib/erlang/lib/wrangler-1.2.0/elisp")
;;(require 'wrangler)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(package-selected-packages (quote (company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq-default cursor-type 'hbar)
(set-cursor-color "#7F00FF")
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

(add-to-list 'custom-theme-load-path "/Users/amado/.emacs.d/themes")
(load-theme 'spolsky t)
