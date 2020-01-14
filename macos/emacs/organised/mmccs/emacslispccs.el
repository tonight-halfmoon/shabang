;;; package --- Summary:
;;; Customise `Emacs-Lisp' /`elisp' mode
;; Package-Requires:
;;; Commentary:
;;
;; Reference:
;; [](https://github.com/Yuki-Inoue/elisp-format)
;;
;;; Code:

(eval-when-compile
  ;; Make it available at compile-time
  'elisp-format (add-to-list
                 ;;
                 'load-path (expand-file-name "~/.emacs.d/elisp-format"))
  (require 'elisp-format))

;; Make it available and mute error
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp-format"))
(require 'elisp-format nil 'noerror)

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

(defun ear-emacs-lisp-mode-after-save-hooks()
  "After-Save hooks for emacs-lisp mode."
  (add-hook 'after-save-hook (lambda()
                               (highlight-changes-remove-highlight (point-min)
                                                                   (point-max))) t t))

;; (add-hook 'emacs-lisp-mode-hook #'ear-emacs-lisp-mode-after-save-hooks)

(provide 'emacslispccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; emacslispccs ends here
