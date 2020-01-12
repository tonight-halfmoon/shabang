;;; package --- Summary:
;;; Customise `aggressive-indent' mode
;; Package-Requires:
;;; Commentary:
;; Reference:
;; [](https://github.com/Malabarba/aggressive-indent-mode)
;;; Code:

(unless (package-installed-p 'aggressive-indent)
  (package-install 'aggressive-indent))

(require 'aggressive-indent)

(add-hook 'prog-mode-hook #'aggressive-indent-mode)

(provide 'aggressiveindentccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; aggressiveindentccs ends here
