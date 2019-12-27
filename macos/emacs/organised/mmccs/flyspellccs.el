;;; package --- Summary:
;;; Customise `Spell' check
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless (package-installed-p 'flyspell)
  (package-install 'flyspell))

(require 'flyspell) ;; anyway, this is not necessary!

(autoload 'flyspell-mode "flyspell" "on-the-fly check spelling." t)
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
