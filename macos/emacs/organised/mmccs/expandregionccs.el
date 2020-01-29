;;; package --- Summary:
;;; Use `expand-region'
;; Package-Requires:
;;; Commentary:
;;; Code:

(add-to-list 'package-pinned-packages '(expand-region . "melpa") t)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'expand-region)
  (package-install 'expand-region))

(require 'expand-region)

(global-set-key (kbd "C-=") 'er/expand-region)

(provide 'expandregionccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; expandregionccs ends here
