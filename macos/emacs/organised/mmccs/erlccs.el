;;; package --- Summary:
;;; Customise `Erlang' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(when (string-equal system-type "berkeley-unix")
  (progn
    (defvar erlang-root-dir)
    (add-to-list 'load-path (car (file-expand-wildcards "/usr/local/lib/erlang/lib/tools-*/emacs")))
    (setq erlang-root-dir (car (file-expand-wildcards "/usr/local/lib/erlang")))
    (setq exec-path (cons (car (file-expand-wildcards "/usr/local/lib/erlang/bin")) exec-path))))

(when (string-equal system-type "darwin")
  (progn (add-to-list 'load-path (car (file-expand-wildcards
                                       "/usr/local/Cellar/erlang/*/lib/erlang/lib/tools-*/emacs")))
         (defvar erlang-root-dir)
         (setq erlang-root-dir (car (file-expand-wildcards
                                     "/usr/local/Cellar/erlang/*/lib/erlang")))
         (setq exec-path (cons (car (file-expand-wildcards
                                     "/usr/local/Cellar/erlang/*/lib/erlang/bin")) exec-path))))

;;(require 'erlang-start)
(eval-when-compile 'elang-mode (add-to-list 'load-path (car (file-expand-wildcards
                                                             "/usr/local/Cellar/erlang/*/lib/erlang/lib/tools-*/emacs")))
                   (require 'erlang))

(add-to-list 'load-path (car (file-expand-wildcards
                              "/usr/local/Cellar/erlang/*/lib/erlang/lib/tools-*/emacs")))
(require 'erlang)

(add-to-list 'auto-mode-alist '("\\.config?$" . erlang-mode))

(add-hook 'erlang-mode-hook 'company-mode t)

(require 'company)

(defun seriott-erlang-mode-hook ()
  "Hooks for Erlang mode."
  (setq company-idle-delay 0.1 company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2 company-tooltip-flip-when-above t)
  (setq allout-auto-activation t)
  (setq erlang-indent-level 2)
  (local-set-key (kbd "RET") 'newline-and-indent))

(add-hook 'erlang-mode-hook #'seriott-erlang-mode-hook)

(defun seriott-erlang-mode-save-hook ()
  "Save hooks for Erlang mode."
  (add-hook 'before-save-hook (lambda()
                                (erlang-indent-current-buffer)
                                (whitespace-cleanup)) t t))

(add-hook 'erlang-mode-hook #'seriott-erlang-mode-save-hook)

(add-hook 'erlang-mode-hook 'flyspell-prog-mode)

(provide 'erlccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; erlccs ends here
