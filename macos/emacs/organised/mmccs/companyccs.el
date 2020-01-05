;;; package --- Summary:
;; `company-mode' `COMP'lete `ANY'thing!
;; Package-Requires:
;;; Commentary:
;;
;; For each major mode either utilise `company-mode' or `auto-complete-mode'
;;
;;; Code:

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'company)
  (package-install 'company))

(provide 'companyccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; companyccs ends here
