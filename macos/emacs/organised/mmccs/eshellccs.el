;;; package --- Summary:
;;; Customise `eshell' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'company)
  (package-install 'company))
(require 'company)

(defun beautiful-eshell-mode-hooks ()
  "Hooks for 'fundamental-mode'.")
(add-hook 'eshell-mode-hook 'beautiful-eshell-mode-hooks)

;; eshellccs!
(provide 'eshellccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; eshellccs ends here
