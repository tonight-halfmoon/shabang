;;; package --- Summary:
;;; Customise `groovy' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'groovy-mode)
  (package-install 'groovy-mode))

(require 'groovy-mode)
(require 'company)

(defun jeng-groovy-mode-hooks ()
  "Hooks for groovy-mode."
  (company-mode)
  (setq company-idle-delay 0.1 company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2 company-tooltip-flip-when-above t)
  (setq indent-tabs-mode nil)
  (setq groovy-indent-offset 2)
  (add-hook 'before-save-hook (lambda()
                                (indent-region (point-min)
                                               (point-max))
                                (whitespace-cleanup)) t t))

(add-hook 'groovy-mode-hook #'jeng-groovy-mode-hooks)

;; groovyccs!
(provide 'groovyccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; groovyccs ends here
