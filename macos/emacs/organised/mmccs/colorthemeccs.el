;;; package --- Summary:
;;; Customise `Colors'
;; Package-Requires:
;;; Commentary:
;;
;; Choose a library
;;
;;; Code:

;; --------------------------------
;; `Color-Theme-Sanityinc-Tomorrow'
;; ================================
;; (require 'color-theme-sanityinc-tomorrow)

;; --------------------
;; `Color-Theme-Modern'
;; ====================
;; Reference: [](https://github.com/emacs-jp/replace-colorthemes)

(cond ((string-equal system-type "darwin")
       (progn (unless package-archive-contents (package-refresh-contents))
              (unless (package-installed-p 'color-theme-modern)
                (package-install 'color-theme-modern))
              (load-theme 'julie t t) ;; hober ;; ld-dark ;; oswald ;; matrix ;; 'railscast 'dark-font-lock
              (enable-theme 'julie)
              ;; (load-theme 'dark-font-lock t t)
              ;; (enable-theme 'dark-font-lock)
              )))

;; -------------
;; `Color-Theme'
;; =============
;; Reference [](http://nongnu.org/color-theme)
;; wget http://download.savannah.nongnu.org/releases/color-theme/color-theme-6.6.0.tar.gz
;; `7z x color-theme-6.6.0.tar.gz` into the `color-theme' directory
(cond ((string-equal system-type "berkeley-unix")
       (progn (add-to-list 'load-path "~/.emacs.d/color-theme")
              (require 'color-theme)
              ;; customised theme source "~/.emacs.d/color-theme/themes/color-theme-amado.el"
              (with-eval-after-load "color-theme" '(progn (color-theme-initialize)
                                                          (color-theme-amado)))
              (custom-set-faces '(mode-line-buffer-id ((t
                                                        (:background "black"
                                                                     :foreground "magenta"
                                                                     :weight extra-bold
                                                                     :height 0.9))))))))

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
