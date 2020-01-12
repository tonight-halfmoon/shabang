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

(beacon-mode 1)

(when (not (string-equal (getenv "emacs_beacon_colour") nil))
  (setq beacon-color (getenv "emacs_beacon_colour")))

(setq-default cursor-type 'hbar)
(setq-default x-stretch-cursor t)

(when (not (string-equal (getenv "emacs_cursor_colour") nil))
  (set-cursor-color (getenv "emacs_cursor_colour")))

(provide 'cursorccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; cursorccs ends here
