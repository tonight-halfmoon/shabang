;;; package --- Summary:
;;; Use 'Theme-`Solarized'
;; Package-Requires:
;;
;; script `git-clone-solarized'
;;
;;; Commentary:
;;
;; Start Emacs with this theme as follows:
;;
;; `env Emacs_theme=solarized Emacs`
;;
;; For permanent use: set environment variable:
;;
;; export emacs_theme=solarized
;;
;;; Code:

(when (string-equal (getenv "emacs_theme") "solarized")
  (call-process (getenv "SHELL") nil "git-clone-solarized" nil "-c"
                "chmod -v u+x $HOME/.emacs.d/mmccs/git-clone-solarized")
  (call-process "~/.emacs.d/mmccs/git-clone-solarized" nil "git-clone-solarized")
  (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/emacs-color-theme-solarized"))
  (load-theme 'solarized t)
  (set-variable 'solarized-termcolors 256)
  (when (string-equal (getenv "emacs_background") "black")
    (setq initial-frame-alist '((background-color . "#080808")))
    (setq default-frame-alist '((background-color . "#080808")))
    (with-eval-after-load 'nlinum (set-face-attribute 'linum t
                                                      :background "#080808"
                                                      :foreground "#444444")))
  (enable-theme 'solarized))

(provide 'themesolarizedccs)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;; ex: ts=2 sw=2 et ft=sh
;;; themesolarizedccs ends here
