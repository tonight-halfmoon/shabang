;;; package --- Summary:
;;; Customise `Cursor'
;; Package-Requires:
;;; Commentary:
;; The `beacon-mode' is really amazing!
;;
;; Refernce:
;; [](https://github.com/Malabarba/beacon)
;;
;;; Code:

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'beacon)
  (package-install 'beacon))

(require 'beacon)

(setq-default cursor-type 'hbar)
;; (set-cursor-color "#7F00FF")
(set-cursor-color "#6f75ff")
(setq-default x-stretch-cursor t)

(beacon-mode 1)
(setq beacon-color "#f2777a")

;; cursorccs!
(provide 'cursorccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; cursorccs ends here
