;;; package --- Summary:
;;; Customise `Emacs-Lisp' /`elisp' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

;;(setq load-path (append (list nil "~/.emacs.d/mmccs") load-path))

(load-file "~/.emacs.d/elisp-format/elisp-format.el")
(load "elisp-format"
      (require 'elisp-format))

(require 'company)

(defun ear-emacs-lisp-mode-hooks ()
  "Hooks for emacs-lisp mode."
  (setq indent-tabs-mode nil tab-width 2)
  (setq allout-auto-activation t)
  (flyspell-prog-mode)
  (company-mode)
  (setq company-idle-delay 0.1 company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1 company-tooltip-flip-when-above t)
  (add-hook 'before-save-hook (lambda()
                                (elisp-format-buffer)
                                (whitespace-cleanup)) t t))

(add-hook 'emacs-lisp-mode-hook #'ear-emacs-lisp-mode-hooks)

(defun highlight-changes-remove-after-save-hook ()
  "Remove recent change after save."
  (add-hook 'after-save-hook (lambda()) t t))

(defun ear-emacs-lisp-mode-after-save-hooks()
  "After-Save hooks for emacs-lisp mode."
  (add-hook 'after-save-hook (lambda()
                               (highlight-changes-remove-highlight (point-min)
                                                                   (point-max))) t t))

;; Example on how to remove highlight-change after-save in emacs-lisp mode
;;(add-hook 'emacs-lisp-mode-hook #'ear-emacs-lisp-mode-after-save-hooks)

;; emacslispccs!
(provide 'emacslispccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; emacslispccs ends here
