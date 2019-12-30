;;; package --- Summary:
;;; Customise `Spell' check
;; Package-Requires:
;; Aspell checker installed on the underlying OS
;; For a target language
;; Check your LC_ALL, and LANG
;; in FreeBSD: `.login_conf'
;; Install
;; FreeBSD:
;; cd /usr/ports/textproc/aspell && make install clean && cd
;; cd /usr/ports/textproc/en-aspell && make install clean && cd

;;; Commentary:
;;; Code:

(unless (package-installed-p 'flyspell)
  (package-install 'flyspell))

(require 'flyspell) ;; anyway, this is not necessary!

(autoload 'flyspell-mode "flyspell" "on-the-fly check spelling." t)

(set-face-attribute 'flyspell-incorrect nil
                    :underline t
                    :background "green"
                    :foreground "black")

;; For all text-derived modes, checke the following reference:
;; [](https://github.com/emacs-mirror/emacs/tree/master/lisp/textmodes)
(add-hook 'text-mode-hook 'flyspell-mode)
;; Enable flyspell-prog-mode for all programming-derived modes
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; flyspellccs!
(provide 'flyspellccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; flyspellccs ends here
