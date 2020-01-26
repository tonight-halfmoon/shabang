;;; package --- Summary:
;;; Customise `Nlinum' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless (package-installed-p 'nlinum)
  (package-install 'nlinum))

(global-nlinum-mode t)

(require 'nlinum)

(set-face-attribute 'nlinum-current-line nil
                    :inherit 'linum
                    :foreground "#ff00ff")

(setq nlinum-format " %3i ")
(setq nlinum-highlight-current-line t)
(setq nlinum-widen t)

(provide 'nlinumccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nlinumccs ends here
