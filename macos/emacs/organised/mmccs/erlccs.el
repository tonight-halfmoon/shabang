;;; package --- Summary:
;;; Customise `Erlang' mode
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
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(cond ((string-equal system-type "darwin")
       (progn (unless (package-installed-p 'exec-path-from-shell)
                (package-install 'exec-path-from-shell)
                (exec-path-from-shell-initialize)))))

(require 'company)
(require 'whitespace-cleanup-mode)
(require 'whitespace)
(global-flycheck-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-mode)
(show-paren-mode 1)
(projectile-mode +1)
(defvar projectile-mode-map)
(define-key projectile-mode-map (kbd "C-c C-c") 'projectile-command-map)

(add-to-list 'load-path (car (file-expand-wildcards
                              "/usr/local/Cellar/erlang/*/lib/erlang/lib/tools-*/emacs")))
(setq erlang-root-dir (car (file-expand-wildcards "/usr/local/Cellar/erlang/*/lib/erlang")))
(setq exec-path (cons (car (file-expand-wildcards "/usr/local/Cellar/erlang/*/lib/erlang/bin"))
                      exec-path))
(require 'erlang-start)
;;(require 'erlang)

(add-to-list 'auto-mode-alist '("\\.config?$" . erlang-mode))

(add-hook 'erlang-mode-hook 'company-mode t)

(defun seriott-erlang-mode-hook ()
  "Hooks for Erlang mode."
  (setq company-idle-delay 0.1 company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2 company-tooltip-flip-when-above t)
  (setq allout-auto-activation t)
  (setq erlang-indent-level 2 erlang-indent-parentesis 2)
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'erlang-mode-hook 'seriott-erlang-mode-hook)


(defun seriott-erlang-mode-save-hook ()
  "Save hooks for Erlang mode."
  (add-hook 'before-save-hook (lambda()
                                (erlang-indent-current-buffer)
                                (whitespace-cleanup)
                                (delete-trailing-whitespace))))
(add-hook 'erlang-mode-hook 'seriott-erlang-mode-save-hook)
;;(add-hook 'before-save-hook 'seriott-erlang-mode-save-hook t t)


(provide 'erlccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; erlccs ends here
