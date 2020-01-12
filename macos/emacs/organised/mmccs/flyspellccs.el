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
;;
;;; Commentary:
;;; Code:

(require 'package)

(add-to-list 'package-pinned-packages '(flyspell-correct-popup . "melpa-stable") t)

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'flyspell-correct-popup)
  (package-install 'flyspell-correct-popup))

(autoload 'flyspell-mode "flyspell" "on-the-fly check spelling." t)

(with-eval-after-load 'flyspell (set-face-attribute 'flyspell-incorrect nil
                                                    :underline t
                                                    :background "green"
                                                    :foreground "black"))

;; For all text-derived modes, checke the following reference:
;; [](https://github.com/emacs-mirror/emacs/tree/master/lisp/textmodes)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Enable flyspell-prog-mode for all programming-derived modes
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(require 'flyspell-correct-popup)

(define-key flyspell-mode-map (kbd "C-c l") 'flyspell-correct-next)

(provide 'flyspellccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; flyspellccs ends here
