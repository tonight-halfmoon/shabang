;;; package --- Summary:
;; Customise `company-preview' - `COMP'lete `ANY'thing!
;; Package-Requires:
;;; Commentary:
;;
;; call ccs-company-preview in the target theme
;;
;;; Code:

;;;###autoload
(defun ccs-company-preview()
  "Configure company."
  (with-eval-after-load 'company
    (require 'company)
    (set-face-attribute 'company-preview t
                        :background "#444444"
                        :foreground "#808080")
    (set-face-attribute 'company-preview-common t
                        :background "#262626"
                        :foreground "#cd00cd")
    (set-face-attribute 'company-preview-search t
                        :background "#0000cd"
                        :foreground "#808080")
    (set-face-attribute 'company-scrollbar-bg t
                        :background "#808080")
    (set-face-attribute 'company-scrollbar-fg t
                        :background "#626262")
    (set-face-attribute 'company-tooltip t
                        :background "#444444"
                        :foreground "#808080")
    (set-face-attribute 'company-tooltip-selection t
                        :background "#bcbcbc"
                        :foreground "#00ff00"
                        :weight 'bold)))

(provide 'companypreviewccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; companypreviewccs ends here
