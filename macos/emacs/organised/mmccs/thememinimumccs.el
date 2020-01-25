;;; package --- Summary:
;;; Customise `Theme' at minimum accepted level
;; Package-Requires:
;;
;; No dependency
;;
;;; Commentary:
;;; Code:

(setq frame-background-mode "dark")

(setq initial-frame-alist '((background-color . "#060606")))

(setq default-frame-alist '((background-color . "#060606")))

(set-face-attribute 'region nil
                    :background "darkolivegreen")

(defun true-color-p ()
  "Return non-nil on displays that support 256 colours."
  (or (display-graphic-p)
      (= (tty-display-color-cells) 16777216)))

(let ((class '((class color)
               (min-colors 89)))
      (default (if (true-color-p) "#abb2bf" "#afafaf"))
      (background-darker (if (true-color-p) "#22252c" "#222222"))
      (background-lighter (if (true-color-p) "#3a3f4b" "#5f5f5f"))
      (background-blue (if (true-color-p) "#38394c" "#444444"))))

(require 'company)

(set-face-attribute 'company-preview t
                    :background "background-darker"
                    :foreground "default")
(set-face-attribute 'company-preview-common t
                    :background "background-darker"
                    :foreground "purple")
(set-face-attribute 'company-preview-search t
                    :background "blue"
                    :foreground "default")
(set-face-attribute 'company-scrollbar-bg nil
                    :background "background-darker")
;; (set-face-attribute 'company-scrollbar-fg t
;;                     :background "background-blue")
(set-face-attribute 'company-tooltip t
                    :background "background-darker"
                    :foreground "default")
(set-face-attribute 'company-tooltip-selection t
                    :background "background-lighter"
                    :foreground "lightgreen"
                    :weight 'bold)
(set-face-attribute 'company-tooltip-common t
                    :background "background-darker"
                    :foreground "purple"
                    :weight 'bold)
(set-face-attribute 'company-tooltip-common-selection t
                    :background "background-lighter"
                    :foreground "purple"
                    :weight 'bold)
(set-face-attribute 'company-tooltip-mouse t
                    :background "background-lighter"
                    :foreground "default")

(provide 'thememinimumccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; thememinimumccs ends here
