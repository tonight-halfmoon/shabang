;;; package --- Summary:
;;; Customise `sh' / `Shell-script' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))
(unless (package-installed-p 'whitespace-cleanup-mode)
  (package-install 'whitespace-cleanup-mode))

(add-hook 'sh-mode-hook 'auto-complete-mode t)
(add-hook 'emacs-lisp-mode-hook 'flyspell-mode)
(defvar sh-basic-offset)
(defvar ac-auto-start)
(defvar ac-dwim)
(defvar ac-override-local-map)
(defvar ac-quick-help-delay)
(defvar ac-ignore-case)
(defun ni-sh-mode-hooks ()
  "Hooks for sh mode."
  (setq indent-tabs-mode nil)
  (setq sh-basic-offset 2)
  (setq ac-auto-start 1 ac-dwim t ac-quick-help-delay 0.1)
  (setq ac-override-local-map nil ac-ignore-case t)
  (add-hook 'before-save-hook (lambda()
                                (indent-region (point-min)
                                               (point-max))
                                (whitespace-cleanup)
                                (delete-trailing-whitespace))))
(add-hook 'sh-mode-hook 'ni-sh-mode-hooks)

;; shccs!
(provide 'shccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; shccs ends here
