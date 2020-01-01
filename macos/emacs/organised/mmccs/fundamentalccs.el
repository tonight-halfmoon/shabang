;;; package --- Summary:
;;; Customise `fundamental' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))
(require 'auto-complete)

(defvar ac-override-local-map)
(defun what-fundamental-mode-hooks ()
  "Hooks for 'fundamental-mode'."
  (setq ac-auto-start 1 ac-dwim t ac-quick-help-delay 0.1)
  (setq ac-override-local-map nil ac-ignore-case t)
  (add-hook 'before-save-hook (lambda()
                                (indent-region (point-min)
                                               (point-max))
                                (whitespace-cleanup)) t t))
(add-hook 'fundamental-mode-hook #'what-fundamental-mode-hooks)

;; fundamentalccs!
(provide 'fundamentalccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; fundamentalccs ends here
