;;; package --- Summary:
;;; Customise `Theme'
;; Package-Requires:
;;; Commentary:
;;
;; Choose a library
;;
;;; Code:

(require 'package)

;; ------------
;; `Nord-theme'
;; ============
(cond ((string-equal system-type "darwin")
       (progn (unless package-archive-contents (package-refresh-contents))
              (unless (package-installed-p 'nord-theme)
                (package-install 'nord-theme))
              (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
              (load-theme 'nord t))))

;; -----------------------
;; `Color-Theme-Solarized'
;; =======================
;; Requirements
;; (1) Clone the repository as follows:
;; `cd ~/.emacs.d`
;; `git clone https://github.com/sellout/emacs-color-theme-solarized.git`
;; '(custom-enabled-themes (quote (solarized)))

(cond ((string-equal system-type "berkeley-unix")
       (progn (add-to-list 'custom-theme-load-path (expand-file-name
                                                    "~/.emacs.d/emacs-color-theme-solarized"))
              (load-theme 'solarized t))))

;; --------------------
;; `Color-Theme-Modern'
;; ====================
;; Reference: [](https://github.com/emacs-jp/replace-colorthemes)

(cond ((string-equal system-type "darwin")
       (progn (unless package-archive-contents (package-refresh-contents))
              (unless (package-installed-p 'color-theme-modern)
                (package-install 'color-theme-modern))
              ;; (load-theme 'julie t t) ;; hober ;; ld-dark ;; oswald ;; matrix ;; 'railscast 'dark-font-lock
              ;; (enable-theme 'julie)
              ;; (load-theme 'dark-font-lock t t)
              ;; (enable-theme 'dark-font-lock)
              )))

;; -------------
;; `Color-Theme'
;; =============
;; Reference [](http://nongnu.org/color-theme)
;; wget http://download.savannah.nongnu.org/releases/color-theme/color-theme-6.6.0.tar.gz
;; `7z x color-theme-6.6.0.tar.gz` into the `color-theme' directory
;; To enable this theme set environment variable `theme` to "amado"

(when (and (string-equal system-type "berkeley-unix")
           (string-equal (getenv "theme") "amado"))
  (progn (add-to-list 'load-path "~/.emacs.d/color-theme")
         (require 'color-theme)
         ;; customised theme source "~/.emacs.d/color-theme/themes/color-theme-amado.el"
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
