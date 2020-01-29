;;; package --- Summary:
;;; Use `which-key'
;; Package-Requires:
;;; Commentary:
;;; Code:

(add-to-list 'package-pinned-packages '(which-key . "melpa") t)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'which-key)
  (package-install 'which-key))

(which-key-mode)

(require 'which-key)

(which-key-setup-side-window-right)

(provide 'whichkeyccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; whichkeyccs ends here
