;;; package --- Summary:
;;; Commentary:
;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-pinned-packages '(elixir-mode . "melpa-stable") t)
  ;;(add-to-list 'package-pinned-packages '(alchemist . "melpa") t)
  )
(package-initialize)

(defun ensure-package-installed
    (&rest
     tail)
  "For each package in TAIL ask for installation."
  (mapcar (lambda (package)
            (if (package-installed-p package) nil (if (y-or-n-p (format
                                                                 "Package %s is missing.  Install it? "
                                                                 package))
                                                      (package-install package) package))) tail))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'travis 'docker 'docker-compose-mode 'dockerfile-mode 'yaml-mode
                          ;; 'es-mode ;; elastic search ; brew install apache-spark
                          'auto-complete 'company 'whitespace-cleanup-mode
                          ;; 'dictionary
                          'flycheck 'flycheck-rebar3 'flycheck-yamllint 'flycheck-checkbashisms
                          'flycheck-color-mode-line ;; colorise the mode line according to the Flycheck status
                          'flycheck-pos-tip ;; shows Flycheck error messages in a graphical popup
                          'flycheck-title ;; shows Flycheck error messages in the frame title
                          ;; 'flycheck-inline ;; shows Flycheck error messages in the bugger, directly below theier origin
                          'exec-path-from-shell 'elixir-mode 'alchemist 'flycheck-mix
                          ;; AC Alchemist: Alchemist with auto-complete not company completion mode
                          ;; 'ac-alchemist
                          ;; flycheck support for Elixir
                          ;; 'erlstack-mode 'erlang
                          ;; 'web-mode ;; see web-mode section follows
                          'web-beautify 'js-auto-format-mode
                          ;; Groovy
                          'groovy-mode
                          ;; 'python-mode ;;'elpy

                          ;; 'color-theme-sanityinc-tomorrow
                          'color-theme-modern

                          ;; diredful - faces on file type
                          'diredful

                          ;; 'latex-extra
                          ;; 'pdf-tools
                          'markdown-mode 'elisp-lint
                          ;; Nlinum
                          'nlinum 'diredful
                          ;; Projectile
                          'projectile)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#2d2d2d" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc"
                                   "#66cccc" "#cccccc"))
 '(beacon-color "#f2777a")
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a"
                              "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d"
                              default)))
 '(fci-rule-color "#515151")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(flycheck-highlighting-mode (quote symbols))
 '(frame-background-mode (quote dark))
 '(global-flycheck-mode t)
 '(js-auto-format-command "prettier")
 '(js-auto-format-command-args "--write --single-quote --no-semi")
 '(mode-require-final-newline t)
 '(nlinum-format " %3i ")
 '(nlinum-highlight-current-line t)
 '(nlinum-widen t)
 '(package-selected-packages (quote (alchemist projectile nlinum elisp-lint markdown-mode diredful
                                               color-theme-modern groovy-mode js-auto-format-mode
                                               web-beautify flycheck-mix elixir-mode
                                               exec-path-from-shell flycheck-title flycheck-pos-tip
                                               flycheck-color-mode-line flycheck-checkbashisms
                                               flycheck-yamllint flycheck-rebar3 flycheck
                                               whitespace-cleanup-mode company auto-complete
                                               dockerfile-mode docker-compose-mode docker travis)))
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
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t
                     (:background "color-24"
                                  :foreground "white"))))
 '(company-tooltip-common ((t
                            (:foreground "white"))))
 '(company-tooltip-selection ((t
                               (:background "cyan"
                                            :weight bold))))
 '(mode-line-buffer-id ((t
                         (:background "black"
                                      :foreground "color-69"
                                      :weight extra-bold
                                      :height 0.9))))
 '(nlinum-current-line ((t
                         (:inherit linum
                                   :foreground "magenta"
                                   :weight bold))))
 '(whitespace-big-indent ((t nil)))
 '(whitespace-space ((t
                      (:bold t
                             :foreground "green"))))
 '(whitespace-trailing ((t
                         (:foreground "green"
                                      :weight bold)))))

;; ====================================
;; Global settings
;; ====================================

;; ===
;; CamelCase
;; ===

(global-subword-mode t)

;; ====================================================================
;; Key bindings
;; ====================================================================
(global-set-key (kbd "M-9") 'kill-whole-line)
(global-set-key (kbd "C-x p")
                (lambda()
                  (interactive)
                  (other-window -1)))

;; ====================================================================
;; Screen Configurations
;; ====================================================================
(setq inhibit-startup-screen t)

;; ====================================================================
;; company mode
;; ====================================================================
;; Either utilise `company-mode' or `auto-complete-mode'
;; """ To turn on for a particular major-mode, set dedicated variable as
;;     follows, in user-defined hooks,
;;     (global-company-mode t)
;; """
(require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)

;; ====================================================================
;; Indentation
;; ====================================================================
;; Tab Indent size 2
;; (setq-default tab-width 2)
;; Indent according to current Major Mode on/before Save Hook
;; (add-hook 'before-save-hook (lambda()
;; (indent-according-to-mode)))

;; No Tabs by default. Modes that really need tabs should enable
;; indent-tabs-mode explicitly. makefile-mode already does that
;; Indent
;; (setq-default indent-tabs-mode nil)

;; Enable Auto Indent
;; (define-key global-map (kbd "RET") 'newline-and-indent)

;;====================================
;; Final new line EOF
;; ====================================
(setq require-final-newline (quote mode-require-final-newline))

;; (add-hook 'before-save-hook (lambda() ;;(if (not indent-tabs-mode)
;; (untabify (point-min)(point-max))
;; (indent-region (point-min)
;; (point-max) nil))) ;;)

;; ====================================================================
;; Whitespace
;; ====================================================================
(require 'whitespace-cleanup-mode)
(require 'whitespace)

;; Whitespace Cleanup on/before Save Hook
;; (add-hook 'before-save-hook 'whitespace-cleanup)
;; (add-hook 'before-save-hook (lambda()
;; (whitespace-cleanup)))

;; Delete Trailing Whitespace on/before Save Hook
;; (add-hook 'before-save-hook (lambda()
;; (delete-trailing-whitespace)))

;; ====================================================================
;; Flycheck
;; ====================================================================
(global-flycheck-mode t)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (exec-path-from-shell-initialize)

;; ====================================================================
;; Font lock mode
;; ====================================================================
(font-lock-mode t)

;; ==================================
;; Major-Mode-Specific Configurations
;; ==================================

;; =============================================================================================
;; Erlang Mode
;; =============================================================================================

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
  (setq erlang-indent-level 2 erlang-indent-parenthesis 2)
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

;; ====================================================================
;; Distel
;; ====================================================================
;; https://github.com/massemanet/distel.git
;; (add-to-list 'load-path "~/.emacs.d/distel/elisp")
;; (require 'distel)
;; (distel-setup)

;; =================================================================================================
;; Elixir Mode
;; =================================================================================================
;; Reference[](https://github.com/elixir-editors/emacs-elixir)

(require 'elixir-mode)

;; ===
;; Format
;; ===

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
;; config completion with company and alchemist
;; ===
(require 'alchemist)
(add-hook 'elixir-mode-hook 'company-mode t)
(defun che-elixir-mode-hooks ()
  "Hooks for Elixir mode."
  (add-hook 'company-mode t)
  (setq company-idle-delay 0.1 company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2 company-tooltip-flip-when-above t)
  (add-hook 'after-init-hook 'alchemist-mode)
  (add-hook 'before-save-hook 'elixir-format nil t))
(add-hook 'elixir-mode-hook 'che-elixir-mode-hooks)

;; The following add-hook is a simpler but missing alchemist and company
;; (add-hook 'elixir-mode-hook (lambda()
;;                              (add-hook 'before-save-hook 'elixir-format nil t)))


;; Flycheck extension for Elixir support
;; Reference [](https://github.com/tomekowal/flycheck-mix)
(require 'flycheck-mix)
(flycheck-mix-setup)

;; ====================================================================
;; ac alchemist // native Alchemist does not support auto-complete AC
;; ====================================================================
;; Reference [](https://github.com/syohex/emacs-ac-alchemist)
;;(add-hook 'elixir-mode-hook 'ac-alchemist-setup)

;; ====================================================================
;; Color Theme Sanityinc Tomorrow
;; ====================================================================
;; (require 'color-theme-sanityinc-tomorrow)

;; ====================================================================
;; Color Theme Modern
;; ====================================================================
;; Reference: [](https://github.com/emacs-jp/replace-colorthemes)
;; (load-theme 'julie t t) ;; hober ;; ld-dark ;; oswald ;; matrix ;; 'railscast
;; (enable-theme 'julie)

;; ====================================================================
;; Color Theme
;; ====================================================================
;; Reference [](http://nongnu.org/color-theme)
;; wget http://download.savannah.nongnu.org/releases/color-theme/color-theme-6.6.0.tar.gz
;; `7z x color-theme-6.6.0.tar.gz` into the `color-theme' directory
(add-to-list 'load-path "~/.emacs.d/color-theme/color-theme-6.6.0")
(require 'color-theme)
;; customised theme source "~/.emacs.d/color-theme/themes/color-theme-amado.el"
(eval-after-load "color-theme" '(progn (color-theme-initialize)
                                       (color-theme-arneson)
                                       (color-theme-amado)))

;; =======================================================
;; diredful - customise face on file type in dired mode
;; =======================================================
;; Reference: [](https://www.emacswiki.org/emacs/Diredful)
(diredful-mode 1)


;; ====================================================================
;; Cursor conf
;; ====================================================================
(setq-default cursor-type 'hbar)

;; ====================================================================
;; Matching pairs - Show Paren Mode
;; ====================================================================
(show-paren-mode 1)
;; (global-set-key "%" 'match-paren)

;; ====================================================================
;; Python mode
;; ====================================================================
;; elpy
;; https://elpy.readthedocs.io/en/latest/introduction.html\#installation
;; (elpy-enable)

;; ====================================================================
;; JavaScript mode
;; ====================================================================
;; Dependency: Either `eslint' or `prettier'
;; Option (1)
;; npm install --global eslint
;; add ~/.eslintrc with contents, e.g., { "extends": "eslint:recommended" }
;; Option (2)
;; npm install -g prettier
;; check configuration above wiht `cusomt-set-variables'
(setq-default js-indent-level 2)

(defun ser-js-af-hooks ()
  "Hooks for js mode with js auto-format."
  (when (eq major-mode 'js-mode)
    (js-auto-format-mode)))
(add-hook 'after-init-hook (lambda()
                             (ser-js-af-hooks)))
;; Run upon Save
(defun seriott-js-mode-save-hook ()
  "Save hooks for js mode."
  (when (eq major-mode 'js-mode)
    (whitespace-cleanup)
    (delete-trailing-whitespace)
    (js-auto-format-execute)))
(add-hook 'before-save-hook #'seriott-js-mode-save-hook)

;; Dependency on cl-lib
;; (add-to-list 'load-path "~/.emacs.d/cl-lib")
;; (require 'cl-lib)

;; ====================================================================
;; emacs-lisp `elisp` Mode
;; ====================================================================
(add-to-list 'load-path "~/.emacs.d/elisp-format")
(require 'elisp-format)
(add-hook 'emacs-lisp-mode-hook 'company-mode t)
(defun ear-emacs-lisp-mode-hooks ()
  "Hooks for emacs-lisp mode."
  (when (eq major-mode 'emacs-lisp-mode)
    (setq indent-tabs-mode nil tab-width 2)
    (setq allout-auto-activation t)
    (setq company-idle-delay 0.1 company-tooltip-limit 10)
    (setq company-minimum-prefix-length 2 company-tooltip-flip-when-above t)))
(add-hook 'after-init-hook 'ear-emacs-lisp-mode-hooks)

(defun ear-emacs-lisp-mode-save-hooks ()
  "Save hooks for emacs-lisp mode."
  (when (eq major-mode 'emacs-lisp-mode)
    (elisp-format-buffer)
    (whitespace-cleanup)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'ear-emacs-lisp-mode-save-hooks)

;; ====================================================================
;; Web mode : configured for html and eex.html
;; ====================================================================
;; Reference [](http://web-mode.org/)
(add-to-list 'load-path "~/.emacs.d/web-mode")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex?\\'" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.json?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
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

;; ====================================================================
;; web-beautify formatter
;; ====================================================================
;; Dependency: $ npm install -g js-beautify
;; Passing formatting configurations:
;; `Option-1'
;; add file `~/.jsbeautifyrc` with contents {"indent-size": 2, "end_with_newline": true}
;; `Option-2'
;; via environment variable
;; Other options, check: [](https://github.com/beautify-web/js-beautify#options)
;; Reference: [](https://github.com/yasuyk/web-beautify)
(defun halloween-web-beautify-save-hook ()
  "Hooks with web-beautify."
  ;; (!) Be careful!! Add one and only one for the same major mode!
  (setenv "jsbeautify_indent_size" "2")
  (setenv "jsbeautify_end_with_newline" "true")
  (setq indent-tabs-mode nil)
  (add-hook 'before-save-hook (lambda()
                                (web-beautify-js-buffer)
                                (whitespace-cleanup))))
(add-hook 'json-mode-hook 'halloween-web-beautify-save-hook)

;; ====================================================================
;; Dockerfile mode
;; ====================================================================
(add-hook 'dockerfile-mode-hook 'company-mode t)
(defun jeng-dockerfile-mode-hooks ()
  "Hooks for Dockerfile mode."
  (setq indent-tabs-mode nil tab-width 2)
  (setq company-idle-delay 0.1 company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1 company-tooltip-flip-when-above t)
  (add-hook 'before-save-hook (lambda()
                                (whitespace-cleanup))))
(add-hook 'dockerfile-mode-hook 'jeng-dockerfile-mode-hooks)


;; ====================================================================
;; Dockerfile linter
;; ====================================================================
;; Referemce [](https://github.com/hadolint/hadolint)

;; ====================================================================
;; auto-complete AC mode
;; ====================================================================
(require 'auto-complete)
;; Reference [](https://github.com/auto-complete/auto-complete/issues/191)
(defun plug-ac ()
  "Configure auto-complete AC Mode."
  (setq ac-auto-start 1 ac-dwim t ac-quick-help-delay 0.1)
  (setq ac-override-local-map nil ac-ignore-case t)
  (global-auto-complete-mode t))
;; (mapc (lambda(mode)
;;         (add-to-list 'ac-modes mode))
;;       '(web-mode js-mode markdown-mode latex-mode text-mode eshell-mode yaml-mode fundamental-mode))
;; (plug-ac)

;; ====================================================================
;; markdown mode
;; ====================================================================
(defun ser-markdown-mode-hooks ()
  "Hooks for Markdown mode."
  (setq indent-tabs-mode nil tab-width 2)
  (setq allout-auto-activation t)
  (setq markdown-indent-on-enter t)
  (add-hook 'local-write-file-hooks (lambda()
                                      (indent-according-to-mode)
                                      (whitespace-cleanup)
                                      (delete-trailing-whitespace) nil)))
(add-hook 'markdown-mode-hook #'ser-markdown-mode-hooks)

;; =============================================================================================
;; Nlinum
;; =============================================================================================
(global-nlinum-mode t)

;; ===================================================================================
;; Projectile mode
;; ===================================================================================
;; Reference [](https://github.com/bbatsov/projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c C-c") 'projectile-command-map)

;; ====================================================================
;; shell-script mode
;; ====================================================================

(add-hook 'sh-mode-hook 'auto-complete-mode t)
(defun ni-sh-mode-hooks ()
  "Hooks for sh mode."
  (setq indent-tabs-mode nil)
  (setq sh-basic-offset 2)
  (setq ac-auto-start 1 ac-dwim t ac-quick-help-delay 0.1)
  (setq ac-override-local-map nil ac-ignore-case t)
  (add-hook 'before-save-hook (lambda()
                                (indent-region (point-min)
                                               (point-max))
                                (whitespace-cleanup)
                                (delete-trailing-whitespace))))
(add-hook 'sh-mode-hook 'ni-sh-mode-hooks)

;; =======================================================
;; `conf-unix' mode
;; =======================================================
(defun ni-config-unix-mode-hooks ()
  "Hooks for conf-unix mode."
  (setq require-final-newline (quote mode-require-final-newline))
  (setq indent-tabs-mode nil tab-width 2)
  (add-hook 'before-save-hook (lambda()
                                (indent-region (point-min)
                                               (point-max))
                                (whitespace-cleanup)
                                (delete-trailing-whitespace))))
(add-hook 'conf-unix-mode-hook 'ni-config-unix-mode-hooks)

;; =========================================================================================
;; groovy mode
;; =========================================================================================
(add-hook 'groovy-mode-hook 'company-mode t)
(defun jeng-groovy-mode-hooks ()
  "Hooks for groovy-mode."
  (setq indent-tabs-mode nil)
  (setq groovy-indent-offset 2)
  (add-hook 'before-save-hook (lambda()
                                (indent-region (point-min)
                                               (point-max))
                                (whitespace-cleanup))))

(add-hook 'groovy-mode-hook 'jeng-groovy-mode-hooks)

;; =========================================================================================
;; Jenkins file mode (scriptive)
;; =========================================================================================
;; wget https://raw.githubusercontent.com/john2x/jenkinsfile-mode/master/jenkinsfile-mode.el
(add-to-list 'load-path "~/.emacs.d/jenkinsfile-mode")
(require 'jenkinsfile-mode)

;; ===================================================================================
;; yaml mode
;; ===================================================================================
(require 'yaml-mode)
(add-hook 'yaml-mode-hook 'auto-complete-mode t)
(defun mor-yaml-mode-hooks()
  "Hooks for yaml-mode."
  (setq ac-auto-start 1 ac-dwim t ac-quick-help-delay 0.1)
  (setq ac-override-local-map nil ac-ignore-case t)
  (setq indent-tabs-mode nil)
  (setq yaml-indent-offset 2)
  (define-key yaml-mode-map "\C-m" 'newline-and-indent)
  (add-hook 'before-save-hook (lambda()
                                (whitespace-cleanup))))
(add-hook 'yaml-mode-hook 'mor-yaml-mode-hooks)

;; ====================================================================
;; Change Log mode
;; ====================================================================
(defun glaz-change-log-mode-hooks ()
  "Hooks for changel-log mode."
  (setq indent-line-function 'indent-relative)
  (add-hook 'before-save-hook (lambda()
                                (indent-region 1 73 0)
                                (whitespace-cleanup)
                                (delete-trailing-whitespace))))
(add-hook 'change-log-mode-hook 'glaz-change-log-mode-hooks)


;;============================================
;; copy to macos clip-board pbcopy & pbpaste
;;============================================
;; Reference:
;; [](https://gist.github.com/the-kenny/267162)

(defun copy-from-osx ()
  "Copy from osx."
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  "Copy `TEXT` to osx as `PUSH`."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; check OS type
(cond ((string-equal system-type "windows-nt") ; Microsoft Windows
       (progn (message "Microsoft Windows")))
      ((string-equal system-type "gnu/linux") ; linux
       (progn (message "Linux")))
      ((string-equal system-type "darwin") ; Mac OS X
       (progn (message "Mac OS X")
              ;; ====================================
              ;; solve `ls does not support --dired;'
              ;; ====================================
              (setq dired-use-ls-dired nil)
              ;; Integrate pbcopy
              (setq interprogram-cut-function 'paste-to-osx)
              (setq interprogram-paste-function 'copy-from-osx))))

;; emacs!
(provide '.emacs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; .emacs ends here
