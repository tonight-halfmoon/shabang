;;; package --- Summary:
;;; Customise `Elixir' mode
;; Package-Requires:
;;; Commentary:
;; Alchemist server will start on "dev" mode
;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  ;; (add-to-list 'package-pinned-packages '(elixir-mode . "melpa") t)
  ;; (add-to-list 'package-pinned-packages '(alchemist . "melpa") t)
  )
(package-initialize)

(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))
(unless (package-installed-p 'company)
  (package-install 'company))
(unless (package-installed-p 'whitespace-cleanup-mode)
  (package-install 'whitespace-cleanup-mode))
(unless (package-installed-p 'alchemist)
  (package-install 'alchemist))
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(unless (package-installed-p 'flycheck-mix)
  (package-install 'flycheck-mix))
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
(show-paren-mode 1)
(projectile-mode +1)
(defvar projectile-mode-map)
(define-key projectile-mode-map (kbd "C-c C-c") 'projectile-command-map)
(require 'elixir-mode)
(add-to-list 'load-path "~/.emacs.d/web-mode")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.eex?\\'" . web-mode))

;; ======
;; Format
;; ======

;; repo/file `elixir-format.el` Cloned into ...
(add-to-list 'load-path "~/.emacs.d/elixir-format")
(require 'elixir-format)
;; The following hook assumes `.formatter.exs' file exists in the directory
;; containing the target source code
;; References:
;; [](https://github.com/elixir-editors/emacs-elixir#add-elixir-mode-hook-to-run-elixir-format-on-file-save)
;; [mix format](https://hexdocs.pm/mix/master/Mix.Tasks.Format.html)
(add-hook 'elixir-format-hook (lambda()
                                (setq elixir-format-arguments (list "--dot-formatter" (concat
                                                                                       (locate-dominating-file
                                                                                        buffer-file-name
                                                                                        ".formatter.exs")
                                                                                       ".formatter.exs")))
                                (setq elixir-format-arguments nil)))

;; ===
;; config completion with company on alchemist
;; ===
(require 'alchemist)
;; TODO: Check status of alchemist-server and start it
(add-hook 'elixir-mode-hook 'company-mode t)
(defun che-elixir-mode-hooks ()
  "Hooks for Elixir mode."
  (add-hook 'company-mode t)
  (setq company-idle-delay 0.1 company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2 company-tooltip-flip-when-above t)
  (add-hook 'after-init-hook 'alchemist-mode)
  (add-hook 'before-save-hook 'elixir-format nil t)
  ;; Start Alchemist Server
  (unless (alchemist-server-process-p) "Connected" (alchemist-server-start "dev")))
(add-hook 'elixir-mode-hook 'che-elixir-mode-hooks)
(add-hook 'elixir-mode-hook 'flyspell-mode)

;; More on Usage:
;; https://alchemist.readthedocs.io/en/latest/basic_usage/

(defun seriott-web-mode-hooks ()
  "Hooks for Web mode."
  (when (eq major-mode 'web-mode)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-script-padding 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-block-padding 4)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-current-column-highlight t)
    (add-hook 'local-write-file-hooks (lambda ()
                                        (whitespace-cleanup)
                                        (indent-according-to-mode)
                                        (indent-region (point-min)
                                                       (point-max) nil)
                                        (delete-trailing-whitespace) nil))))
(add-hook 'web-mode-hook 'seriott-web-mode-hooks)

;; Flycheck extension for Elixir support
;; Reference [](https://github.com/tomekowal/flycheck-mix)
(require 'flycheck-mix)
(flycheck-mix-setup)

;; exccs!
(provide 'exccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; exccs ends here
