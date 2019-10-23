;;; package --- Summary:

;;; Commentary:

;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto
                                                              "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-pinned-packages '(elixir-mode . "melpa-stable")))
(package-initialize)

;; ==============================================================================================
;; pkg-install
;; ==============================================================================================
;; Consume a list of Emacs MELPA packages
;; Traverse each package in the list
;; Prompt for install
;; User answer y/n.
;; The MELPA install is not successful if missing library behind
;; on the OS is required.
;; In addition, add-to-list package remote source could be necessary
;; to tell MELPA where to look for the source. See above, the section before package-initialize.
;; Reference: [](https://stackoverflow.com/a/10095853)
(defun pkg-install
    (&rest
     tail)
  "Prompt for each package in TAIL install with y/n."
  (mapcar (lambda (next)
            (if (package-installed-p next) nil (if (y-or-n-p(format "Install packge '%s'?" next))
                                                   (package-install next) next))) tail))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(pkg-install 'travis 'yaml-mode 'docker 'docker-compose-mode 'dockerfile-mode
             ;; 'es-mode ;; elastic seach; Dependency: `brew install apache-spark`
             'auto-complete 'company 'whitespace-cleanup-mode 'flycheck 'flycheck-mix
             'flycheck-rebar3 'flycheck-color-mode-line 'flycheck-checkbashisms 'flycheck-pos-tip
             'flycheck-title 'flycheck-yamllint
             ;; 'dictionary
             'elixir-mode 'ac-alchemist
             ;; 'erlang 'erlstack-mode
             'web-mode 'js-auto-format-mode 'exec-path-from-shell 'gradle-mode 'groovy-mode
             'python-mode 'elpy 'markdown-mode
             ;; 'color-theme-sanityinc-tomorrow
             ;; 'pdf-tools
             ;; 'latex-extra
             ;; 'elisp-lint
             )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default default default italic underline success warning error])
 '(ansi-color-names-vector (vector "#515151" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc"
                                   "#66cccc" "#cccccc"))
 '(beacon-color "#f2777a")
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a"
                              default)))
 '(fci-rule-color "#515151")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(flycheck-highlighting-mode (quote symbols))
 '(frame-background-mode (quote dark))
 '(global-flycheck-mode t)
 '(linum-format " %7i ")
 '(package-selected-packages (quote (js-auto-format-mode markdown-mode elpy python-mode groovy-mode
                                                         gradle-mode exec-path-from-shell
                                                         whitespace-cleanup-mode web-mode travis
                                                         flycheck-yamllint flycheck-title
                                                         flycheck-rebar3 flycheck-pos-tip
                                                         flycheck-mix flycheck-color-mode-line
                                                         flycheck-checkbashisms dockerfile-mode
                                                         docker-compose-mode docker elixir-mode
                                                         ac-alchemist auto-complete)))
 '(safe-local-variable-values (quote ((sh-indent-comment . t)
                                      (allout-layout . t))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#f2777a")
                                 (40 . "#f99157")
                                 (60 . "#ffcc66")
                                 (80 . "#99cc99")
                                 (100 . "#66cccc")
                                 (120 . "#6699cc")
                                 (140 . "#cc99cc")
                                 (160 . "#f2777a")
                                 (180 . "#f99157")
                                 (200 . "#ffcc66")
                                 (220 . "#99cc99")
                                 (240 . "#66cccc")
                                 (260 . "#6699cc")
                                 (280 . "#cc99cc")
                                 (300 . "#f2777a")
                                 (320 . "#f99157")
                                 (340 . "#ffcc66")
                                 (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line-buffer-id ((t
                         (:background "black"
                                      :foregorund "magenta"
                                      :weight bold
                                      :height 0.9)))))

;; =============================================================================================
;; Erlang Mode
;; =============================================================================================
(add-to-list 'load-path (car (file-expand-wildcards
                              "/usr/local/Cellar/erlang/*/lib/erlang/lib/tools-*/emacs")))
(setq erlang-root-dir (car (file-expand-wildcards "/usr/local/Cellar/erlang/*/lib/erlang")))
(setq exec-path (cons (car (file-expand-wildcards "/usr/local/Cellar/erlang/*/lib/erlang/bin"))
                      exec-path))
(require 'erlang-start)
(require 'erlang)

(add-to-list 'auto-mode-alist '("\\.config?$" . erlang-mode))

(setq-default erlang-indent-level 2)
(setq-default allout-auto-activation t)
(setq-default erlang-indent-paranthesis 2)

;; See method #2 follows
;; (add-hook 'before-save-hook 'erlang-indent-current-buffer)

;; Method #2
(defun esl-erlang-mode-before-save-hook()
  "When Major Mode is Erlang then apply the following on/before save."
  (when (eq major-mode 'erlang-mode)
    ;;(whitespace-cleanup)
    (delete-trailing-whitespace)
    (erlang-indent-current-buffer)))
(add-hook 'before-save-hook #'esl-erlang-mode-before-save-hook)

;; Format in erlang mode
;; (defun emacs-indent-function ()
;;  "Format the whole buffer."
;;   (erlang-mode)
;;   (indent-region (point-min) (point-max) nil)
;;   (untabify (point-min) (point-max))
;;   (delete-trailing-whitespace)
;;   (save-buffer)
;;   )

;; ==============================================================================================
;; Wrangler
;; ==============================================================================================
;; (add-to-list 'load-path "/usr/local/Cellar/erlang/20.3.4/lib/erlang/lib/wrangler-1.2.0/elisp")
;; (require 'wrangler)

;;==========================================================
;; Distel
;;==========================================================
;; Refrence [](https://github.com/massemanet/distel.git)
;; (add-to-list 'load-path "~/.emacs.d/distel/elisp")
;; (require 'distel)
;; (distel-setup)

;;=============================================================================
;; Elixir Mode
;;=============================================================================
;; Reference [](https://github.com/elixir-editors/emacs-elixir)
;; repo/file `elixir-format.el` cloned into...
(add-to-list 'load-path "~/.emacs.d/elixir-format")
(require 'elixir-format)
;; Format upon Save when Elixir Mode is enabled
(add-hook 'elixir-mode-hook (lambda()
                              (add-hook 'before-save-hook 'elixir-format nil t)))

;; ==================================================================
;; ac alchemist // native alchemist does not support aut-complete AC
;; ==================================================================
;; Reference [](https://github.com/syohex/emacs-ac-alchemist)
(add-hook 'elixir-mode-hook 'ac-alchemist-setup)

;; ==================================================================
;; auto-complete AC mode
;; ==================================================================
(require 'auto-complete)
;; Reference [](https://github.com/auto-complete/auto-complete/issues/191)
(defun plg-ac-config()
  "Configure auto-complete AC Mode."
  (setq ac-auto-start 1)
  (setq ac-dwim t)
  (setq ac-quick-help-display 0.5)
  (setq ac-override-local-map nil)
  (setq ac-ignore-case t)
  (global-auto-complete-mode t))
(mapc (lambda(mode)
        (add-to-list 'ac-modes mode))
      '(erlang-mode latex-mode text-mode graphiz-dot-mode html-mode eshell-mode))
(plg-ac-config)

;; ===================================================================================
;; Mode-line
;; ===================================================================================
(set-face-foreground 'mode-line "green")
(set-face-background 'mode-line "purple")

(add-hook 'emacs-lisp-mode-hook ;; Customise Mode Line color in emacs-lisp mode
          (lambda ()
            (face-remap-add-relative 'mode-line
                                     '((:foreground "black"
                                                    :background "yellow") mode-line))))

;; ==========================================================
;; Cursor
;; ==========================================================
(setq-default cursor-type 'hbar)
;; (set-cursor-color "#7F00FF")
;; (set-cursor-color "#00ff00")
(set-cursor-color "#6f75ff")
(setq-default x-stretch-cursor 1)

;; ==========================================================
;; Font lock mode
;; ==========================================================
(font-lock-mode t)

;; ==========================================================
;; Color Theme Sanityinc Tomorrow
;; ==========================================================
;; Reference [](https://github.com/purcell/color-theme-sanityinc-tomorrow)
;; (require 'color-theme-sanityinc-tomorrow)

;; ==========================================================
;; Matching pairs - Show `paren` mode
;; ==========================================================
(show-paren-mode 1)
;; (global-set-key "%" 'match-paren)

;; ==========================================================
;; Screen and Frame
;; ==========================================================
(setq inhibit-startup-screen t)
(when window-system (set-frame-size (selected-frame) 64 51))

;; ==========================================================
;; Key binding
;; ==========================================================
(global-set-key (kbd "M-9") 'kill-whole-line)
(global-set-key (kbd "C-x p")
                (lambda()
                  (interactive)
                  (other-window -1)))
;; Macos bind Meta with key 'cmd'
(setq-default mac-option-modifier 'alt)
(setq-default mac-command-modifier 'meta)
(setq select-enable-clipboard t)

;; ==========================================================
;; Indentation
;; ==========================================================
;; No tabs by default. Modes that really need tabs should enable
;; indent-tabs-mode explicitly. makefile-mode already does that
(setq-default indent-tabs-mode nil)
;; Default tab width `indent size` is 2
(setq-default tab-width 2)
;; Indent according to current Major Mode on/before Save Hook
(add-hook 'before-save-hook (lambda()
                              (indent-according-to-mode)))
;; Enable auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Either Indent according to Major Mode
;; or save add hook on specific Major Mode Hook
;; (add-hook 'before-save-hook (lambda() ;; (if (not indent-tabs-mode)
;; (untabify (point-min)(point-max))
;;                              (indent-region (point-min)
;;                                             (point-max) nil)))

;; ==========================================================
;; Whitespace
;; ==========================================================
(require 'whitespace-cleanup-mode)
(require 'whitespace)
;; Whitespace Cleanup on/before Save Hook
(add-hook 'before-save-hook (lambda()
                              (whitespace-cleanup)))

;; Delete Trailing Whitespace on/before Save Hook
(add-hook 'before-save-hook (lambda()
                              (delete-trailing-whitespace)))

;; ==========================================================
;; Company Mode
;; ==========================================================
(add-hook 'after-init-hook 'global-company-mode)

;; ==========================================================
;; Image dimensions
;; ==========================================================
;; Reference: https://www.emacswiki.org/emacs/image-dimensions-minor-mode.el
;; Display the image dimensions in the mode line, when viewing an image.
(load "$HOME/.emacs.d/image-dimensions/image-dimensions-minor-mode.el")
(eval-after-load 'image-mode
  '(require 'image-dimensions-minor-mode))
(setq frame-title-format '(buffer-file-name ("%b (Emacs) %f" image-dimensions-minor-mode-dimensions)
                                            (dired-directory
                                             (:eval (concat (buffer-name) " (Emacs) "
                                                            dired-directory))
                                             ("%b (Emacs)"))))

;; ==========================================================
;; Kotlin Mode
;; ==========================================================
;; clone or copy https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode/blob/master/kotlin-mode.el into $HOME/.emacs.d/
(load "$HOME/.emacs.d/kotlin/kotlin-mode.el")

;; ==========================================================
;; Scala Mode
;; ==========================================================
;; clone https://github.com/ensime/emacs-scala-mode.git into $HOME/.emacs.d/
;; (add-to-list 'load-path "$HOME/.emacs.d/emacs-scala-mode/")
;; (require 'scala-mode)

;; ==========================================================
;; Flycheck
;; ==========================================================
;; Flycheck extension for Elixir support
;; Reference [](https://github.com/tomekowal/flycheck-mix)
(require 'flycheck-mix)
(flycheck-mix-setup)

;; Flycheck extension for Kotlin support
;; (require 'flycheck-kotlin)
;; (add-hook 'kotlin-mode-hook 'flycheck-mode)

;; ==========================================================
;; Spell check
;; ==========================================================
(autoload 'flyspell-mode "flyspell" "on-the-fly check spelling." t)
(add-hook 'LaTex-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; ==========================================================
;; Python Mode
;; ==========================================================
(require 'python-mode)
;; Elpy
;; Reference [](https://elpy.readthedocs.io/en/latest/introduction.html\#installation)
;; (elpy-enable)

;; ==========================================================
;; JavaScript Mode
;; ==========================================================
;; Dependency: ;; npm install --global eslint ;; add ~/.eslintrc with contents, e.g., { "extends": "eslint:recommended" }
;;             | prettier ;; npm install --global prettier
;; Reference [](https://github.com/ybiquitous/js-auto-format-mode)
(setq-default js-indent-level 2)
(add-hook 'js-mood-hook #'js-auto-format-mode)
(custom-set-variables '(js-auto-format-command "prettier")
                      '(js-auto-format-command-args "--write --single-quote --no-semi"))

;; Run upon Save
(defun js-auto-format-mode-before-save-hook()
  "When Major Mode is js then apply the following on/before save."
  (when (eq major-mode 'js-mode)
    (whitespace-cleanup)
    (delete-trailing-whitespace)
    (js-auto-format-execute)))
(add-hook 'before-save-hook #'js-auto-format-mode-before-save-hook)

;; ==========================================================
;; Web Mode
;; ==========================================================
(setq-default web-mode-indent-style 2)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex?\\'" . web-mode))

;; ==========================================================
;; Dockerfile linter
;; ==========================================================
;; Ref [](https://github.com/hadolint/hadolint.git)

;; ==========================================================
;; emacs-lisp `elisp` Mode
;; ==========================================================
;; Reference [](https://github.com/Yuki-Inoue/elisp-format/blob/master/elisp-format.el)
(add-to-list 'load-path "~/.emacs.d/elisp-format")
(require 'elisp-format)
(defun emacs-lisp-mode-before-save-hook()
  "When Major Mode is emacs-lisp then apply the following on/before save."
  (when (eq major-mode 'emacs-lisp-mode)
    (elisp-format-buffer)
    (whitespace-cleanup)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook #'emacs-lisp-mode-before-save-hook)

;; ==========================================================
;; markdown Mode
;; ==========================================================
(defun markdown-mode-before-save-hook()
  "When Major Mode is markdown then apply the following on/before save."
  (when (eq major-mode 'markdown-mode)
    (whitespace-cleanup)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook #'markdown-mode-before-save-hook)

(provide '.emacs)
;;; .emacs ends here
