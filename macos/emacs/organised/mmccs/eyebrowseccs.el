;;; package --- Summary:
;;; Use `eyebrowse'
;; Package-Requires:
;;; Commentary:
;;; Code:

(add-to-list 'package-pinned-packages '(eyebrowse . "melpa") t)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'eyebrowse)
  (package-install 'eyebrowse))

(require 'eyebrowse)

(eyebrowse-mode t)

(provide 'eyebrowseccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; eyebrowseccs ends here
