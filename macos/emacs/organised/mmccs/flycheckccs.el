;;; package --- Summary:
;;; Customise `Flycheck'
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(unless (package-installed-p 'flycheck-color-mode-line)
  (package-install 'flycheck-color-mode-line))
(unless (package-installed-p 'flycheck-pos-tip)
  (package-install 'flycheck-pos-tip))
(unless (package-installed-p 'flycheck-title)
  (package-install 'flycheck-title))
(unless (package-installed-p 'flycheck-inline)
  (package-install 'flycheck-inline))
(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(global-flycheck-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'exec-path-from-shell)
(cond ((string-equal system-type "darwin")
       (progn (unless (package-installed-p 'exec-path-from-shell)
                (package-install 'exec-path-from-shell)
                (exec-path-from-shell-initialize)
                (setq exec-path-from-shell-check-startup-files nil)))))

;; flycheckccs!
(provide 'flycheckccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; flycheckccs ends here
