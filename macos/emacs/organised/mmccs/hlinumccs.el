;;; package --- Summary:
;;; Customise `hlinum' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless (package-installed-p 'hlinum)
  (package-install 'hlinum))

(require 'hlinum)

(hlinum-activate)

(linum-mode)

(provide 'hlinumccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; hlinumccs ends here
