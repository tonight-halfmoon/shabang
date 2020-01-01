;;; package --- Summary:
;;; Customise `Elixir' mode
;; Package-Requires:
;;; Commentary:
;; Alchemist server will start on "dev" mode
;;; Code:

(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))
(unless (package-installed-p 'alchemist)
  (package-install 'alchemist))
(unless (package-installed-p 'flycheck-mix)
  (package-install 'flycheck-mix))
(cond ((string-equal system-type "darwin")
       (progn (unless (package-installed-p 'exec-path-from-shell)
                (package-install 'exec-path-from-shell)
                (exec-path-from-shell-initialize)))))

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
  (setq company-idle-delay 0.1 company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2 company-tooltip-flip-when-above t)
  (add-hook 'after-init-hook #'alchemist-mode t t)
  (add-hook 'before-save-hook #'elixir-format nil t)
  ;; Start Alchemist Server
  (unless (alchemist-server-process-p) "Connected" (alchemist-server-start "dev")))
(add-hook 'elixir-mode-hook 'che-elixir-mode-hooks)
(add-hook 'elixir-mode-hook 'flyspell-prog-mode)

;; More on Usage:
;; https://alchemist.readthedocs.io/en/latest/basic_usage/

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
