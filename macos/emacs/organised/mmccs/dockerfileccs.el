;;; package --- Summary:
;;; Customise `dockerfile' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless (package-installed-p 'dockerfile-mode)
  (package-install 'dockerfile-mode))

(setq auto-mode-alist
      (append
       '(("\\dockerfileccs.el\\'" . emacs-lisp-mode))
       auto-mode-alist))

(require 'company)
(add-hook 'dockerfile-mode-hook 'company-mode t)
(defun jeng-dockerfile-mode-hooks ()
  "Hooks for Dockerfile mode."
  (setq indent-tabs-mode nil tab-width 2)
  (setq company-idle-delay 0.1 company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1 company-tooltip-flip-when-above t)
  (add-hook 'before-save-hook (lambda()
                                (whitespace-cleanup)) t t))
(add-hook 'dockerfile-mode-hook #'jeng-dockerfile-mode-hooks)


;; dockerfileccs!
(provide 'dockerfileccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; dockerfileccs ends here
