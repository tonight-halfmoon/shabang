;;; package --- Summary:
;;; Customise `Elixir' mode
;; Package-Requires:
;; `mix format'
;;
;;; Commentary:
;;
;; Alchemist server will start on "dev" mode
;; More on `Usage' of `alchemist':
;; https://alchemist.readthedocs.io/en/latest/basic_usage/
;;
;; Flycheck extension for Elixir support
;; Reference [](https://github.com/tomekowal/flycheck-mix)
;;
;;; Code:

(require 'package)

(add-to-list 'package-pinned-packages '(elixir-mode . "melpa") t)
(add-to-list 'package-pinned-packages '(flycheck-mix . "melpa") t)
(add-to-list 'package-pinned-packages '(flycheck-elixir . "melpa") t)
(add-to-list 'package-pinned-packages '(alchemist . "melpa") t)

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))

(unless (package-installed-p 'alchemist)
  (package-install 'alchemist))

(unless (package-installed-p 'flycheck-mix)
  (package-install 'flycheck-mix))

(unless (package-installed-p 'flycheck-elixir)
  (package-install 'flycheck-elixir))

(add-to-list 'auto-mode-alist '("\\.eex?\\'" . web-mode))

;; ======
;; Format
;; ======
;; The following hook assumes `.formatter.exs' file exists in the directory
;; containing the target source code
;; References:
;; [](https://github.com/elixir-editors/emacs-elixir#add-elixir-mode-hook-to-run-elixir-format-on-file-save)
;; [mix format](https://hexdocs.pm/mix/master/Mix.Tasks.Format.html)
(require 'elixir-mode)

(add-hook 'elixir-format-hook (lambda()
                                (setq elixir-format-arguments (list "--dot-formatter" (concat
                                                                                       (locate-dominating-file
                                                                                        buffer-file-name
                                                                                        ".formatter.exs")
                                                                                       ".formatter.exs")))
                                (setq elixir-format-arguments nil)))

(require 'flycheck-elixir)
(require 'company)
(require 'alchemist)

(defun che-elixir-mode-hooks ()
  "Hooks for Elixir mode."

  ;; In order to enable developers write `doctest' in elixir
  ;; Check [Elixir-Doctests](https://elixir-lang.org/getting-started/mix-otp/docs-tests-and-with.html)
  ;; In order to specify an indentation of four spaces hit
  ;; C-q TAB in your @moduledoc ~S"""
  (setq tab-width 2)

  ;; Provide a default indent upon a new line
  (local-set-key (kbd "RET") 'newline-and-indent)

  ;; A TAB is a couple of spaces
  (setq indent-tabs-mode nil)
  (company-mode)
  (setq company-idle-delay 0.1 company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2 company-tooltip-flip-when-above t)
  (alchemist-mode)
  (alchemist-phoenix-mode)
  (flyspell-prog-mode)
  ;;(flycheck-mix-setup)
  ;;(flycheck-elixir)
  (add-hook 'before-save-hook (lambda()
                                (untabify (point-min)
                                          (point-max))
                                (whitespace-cleanup)) t t)
  (add-hook 'before-save-hook #'elixir-format t t)

  ;; Start Alchemist Server
  (unless (alchemist-server-process-p) "Connected" (alchemist-server-start "dev")))

(add-hook 'elixir-mode-hook #'che-elixir-mode-hooks)

(provide 'exccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; exccs ends here
