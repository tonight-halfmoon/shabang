;;; package --- Summary:
;;; Customise `Emacs-Lisp' /`elisp' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(package-initialize)

(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'company)
  (package-install 'company))
(unless (package-installed-p 'whitespace-cleanup-mode)
  (package-install 'whitespace-cleanup-mode))

(require 'company)
(require 'whitespace-cleanup-mode)
(require 'whitespace)

(add-to-list 'load-path "~/.emacs.d/elisp-format")
(require 'elisp-format)
(add-hook 'emacs-lisp-mode-hook 'company-mode t)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(defun ear-emacs-lisp-mode-hooks ()
  "Hooks for emacs-lisp mode."
  (when (eq major-mode 'emacs-lisp-mode)
    (setq indent-tabs-mode nil tab-width 2)
    (setq allout-auto-activation t)
    (setq company-idle-delay 0.1 company-tooltip-limit 10)
    (setq company-minimum-prefix-length 1 company-tooltip-flip-when-above t)))
(add-hook 'after-init-hook 'ear-emacs-lisp-mode-hooks)

(defun ear-emacs-lisp-mode-save-hooks ()
  "Save hooks for emacs-lisp mode."
  (when (eq major-mode 'emacs-lisp-mode)
    (elisp-format-buffer)
    (whitespace-cleanup)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'ear-emacs-lisp-mode-save-hooks)

(defun highlight-changes-remove-after-save-hook()
  "Remove recent change after save."
  (add-hook 'after-save-hook (lambda())))

(defun ear-emacs-lisp-mode-after-save-hooks ()
  "After-Save hooks for emacs-lisp mode."
  (when (eq major-mode 'emacs-lisp-mode)
    (highlight-changes-remove-highlight (point-min)
                                        (point-max))))
;; Example on how to remove highlight-change after-save in emacs-lisp mode
;;(add-hook 'after-save-hook 'ear-emacs-lisp-mode-after-save-hooks)

;; emacslispccs!
(provide 'emacslispccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; emacslispccs ends here
