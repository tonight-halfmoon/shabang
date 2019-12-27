;;; package --- Summary:
;;; Use `Smart-mode-line'
;; Package-Requires:
;;; Commentary:
;; Reference:
;; [](https://github.com/Malabarba/smart-mode-line)
;;; Code:

(unless (package-installed-p 'smart-mode-line)
  (package-install 'smart-mode-line))

(require 'smart-mode-line)

(setq sml/no-confirm-load-theme t)
(setq sml/theme 'respectful)
(sml/setup)

;; smartmodelineccs!
(provide 'smartmodelineccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; smartmodelineccs ends here
