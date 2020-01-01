;;; package --- Summary:
;;; Customise `conf-unix' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))

(require 'auto-complete)

(add-hook 'conf-unix-mode-hook 'auto-complete-mode t)
(add-hook 'conf-unix-mode-hook 'flyspell-prog-mode)

(defvar ac-override-local-map)

(defun ni-config-unix-mode-hooks ()
  "Hooks for conf-unix mode."
  (setq require-final-newline (quote mode-require-final-newline))
  (setq indent-tabs-mode nil tab-width 2)
  (setq ac-auto-start 1 ac-dwim t ac-quick-help-delay 0.1)
  (setq ac-override-local-map nil ac-ignore-case t)
  (add-hook 'before-save-hook (lambda()
                                (indent-region (point-min)
                                               (point-max))
                                (whitespace-cleanup)
                                (delete-trailing-whitespace)) t t))
(add-hook 'conf-unix-mode-hook #'ni-config-unix-mode-hooks)


;; confunixccs!
(provide 'confunixccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; confunixccs ends here
