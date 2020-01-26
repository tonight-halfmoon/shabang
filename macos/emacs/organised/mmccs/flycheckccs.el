;;; package --- Summary:
;;; Customise `Flycheck'
;; Package-Requires:
;;; Commentary:
;;; Code:

(add-to-list 'package-pinned-packages '(flycheck . "melpa") t)
(add-to-list 'package-pinned-packages '(flycheck-color-mode-line . "melpa") t)
(add-to-list 'package-pinned-packages '(flycheck-inline . "melpa") t)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))

(unless (package-installed-p 'flycheck-color-mode-line)
  (package-install 'flycheck-color-mode-line))

(unless (package-installed-p 'flycheck-inline)
  (package-install 'flycheck-inline))

(with-eval-after-load 'flycheck (add-hook 'after-init-hook #'global-flycheck-mode))

(with-eval-after-load 'flycheck (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode))

(with-eval-after-load 'flycheck (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;; TODO - what does the following line do?
;; (setq flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))

;;(require 'flycheck-color-mode-line)

;;(setq flycheck-highlighting-mode (quote symbols))

(provide 'flycheckccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; flycheckccs ends here
