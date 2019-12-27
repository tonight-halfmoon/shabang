;;; package --- Summary:
;;; Customise `Colors'
;; Package-Requires:
;;; Commentary:
;; Choose one of the options
;;; Code:

(unless package-archive-contents (package-refresh-contents))

;; --------------------------------
;; `Color-Theme-Sanityinc-Tomorrow'
;; ================================
;; (require 'color-theme-sanityinc-tomorrow)

;; --------------------
;; `Color-Theme-Modern'
;; ====================
;; Reference: [](https://github.com/emacs-jp/replace-colorthemes)
(unless (package-installed-p 'color-theme-modern)
  (package-install 'color-theme-modern))

(load-theme 'julie t t) ;; hober ;; ld-dark ;; oswald ;; matrix ;; 'railscast 'dark-font-lock
(enable-theme 'julie)
;; (load-theme 'dark-font-lock t t)
;; (enable-theme 'dark-font-lock)

;; -------------
;; `Color-Theme'
;; =============
;; Reference [](http://nongnu.org/color-theme)
;; wget http://download.savannah.nongnu.org/releases/color-theme/color-theme-6.6.0.tar.gz
;; `7z x color-theme-6.6.0.tar.gz` into the `color-theme' directory
;; (add-to-list 'load-path "~/.emacs.d/color-theme/color-theme-6.6.0")
;; (require 'color-theme)
;; ;; customised theme source "~/.emacs.d/color-theme/themes/color-theme-amado.el"
;; (eval-after-load "color-theme" '(progn (color-theme-initialize)
;;                                       (color-theme-arneson)
;;                                       (color-theme-amado)))

;; -----------
;; `Mode-line'
;; -----------

(defun customise-mode-line-hook ()
  "Customise Mode Line hook."
  (set-face-foreground 'mode-line "green")
  (set-face-background 'mode-line "purple"))
(add-hook 'erlang-mode-hook #'customise-mode-line-hook)

;; colorthemeccs!
(provide 'colorthemeccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; colorthemeccs ends here
