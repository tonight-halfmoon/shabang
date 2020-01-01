;;; package --- Summary:
;;; Customise `Erlang' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(cond ((string-equal system-type "darwin")
       (progn (unless (package-installed-p 'exec-path-from-shell)
                (package-install 'exec-path-from-shell)
                (exec-path-from-shell-initialize)))))

(require 'company)
(require 'whitespace-cleanup-mode)
(require 'whitespace)
(global-flycheck-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

(projectile-mode +1)
(defvar projectile-mode-map)
(define-key projectile-mode-map (kbd "C-c C-c") 'projectile-command-map)

(cond ((string-equal system-type "berkeley-unix")
       (progn (add-to-list 'load-path (car (file-expand-wildcards
                                            "/usr/local/lib/erlang/lib/tools-*/emacs")))
              (setq erlang-root-dir (car (file-expand-wildcards "/usr/local/lib/erlang")))
              (setq exec-path (cons (car (file-expand-wildcards "/usr/local/lib/erlang/bin"))
                                    exec-path)))))

(cond ((string-equal system-type "darwin")
       (progn (add-to-list 'load-path (car (file-expand-wildcards
                                            "/usr/local/Cellar/erlang/*/lib/erlang/lib/tools-*/emacs")))
              (setq erlang-root-dir (car (file-expand-wildcards
                                          "/usr/local/Cellar/erlang/*/lib/erlang")))
              (setq exec-path (cons (car (file-expand-wildcards
                                          "/usr/local/Cellar/erlang/*/lib/erlang/bin"))
                                    exec-path)))))

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
(add-hook 'erlang-mode-hook #'seriott-erlang-mode-hook)


(defun seriott-erlang-mode-save-hook ()
  "Save hooks for Erlang mode."
  (add-hook 'before-save-hook (lambda()
                                (erlang-indent-current-buffer)
                                (whitespace-cleanup)
                                (delete-trailing-whitespace)) t t))
(add-hook 'erlang-mode-hook #'seriott-erlang-mode-save-hook)
;;(add-hook 'before-save-hook 'seriott-erlang-mode-save-hook t t)
(add-hook 'erlang-mode-hook 'flyspell-prog-mode)


(provide 'erlccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; erlccs ends here
