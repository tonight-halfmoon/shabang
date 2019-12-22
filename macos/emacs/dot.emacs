;;; package --- Summary:
;;; Commentary:
;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  ;;(add-to-list 'package-pinned-packages '(alchemist . "melpa") t)
  ;;(add-to-list 'package-pinned-packages '(elixir-mode . "melpa") t)
  )
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
  (unless package-archive-contents (package-refresh-contents))
  (mapcar (lambda (next)
            (if (package-installed-p next) nil (if (y-or-n-p (format "Install packge '%s'? " next))
                                                   (package-install next) next))) tail))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(pkg-install 'travis 'yaml-mode 'docker 'docker-compose-mode 'dockerfile-mode
             ;; 'es-mode ;; elastic seach; Dependency: `brew install apache-spark`
             'auto-complete 'company 'whitespace-cleanup-mode
             ;; 'dictionary
             'flycheck 'flycheck-rebar3 'flycheck-color-mode-line ;; colourise the mode line according to the Flycheck status
             'flycheck-checkbashisms 'flycheck-pos-tip ;; shows Flycheck error message in a graphical popup
             'flycheck-title ;; shows Flycheck error messge in the frame title
             'flycheck-yamllint
             ;; 'flycheck-inline ;; shows Flycheck error message in the bugger, directly below their origin
             'exec-path-from-shell ;;
             'elixir-mode 'alchemist 'flycheck-mix
             ;; AC Alchemist auto-complete not company alchemist
             ;; 'ac-alchemist
             ;; 'erlang 'erlstack-mode
             ;; 'web-mode ;; see web-mode section follows
             'web-beautify 'js-auto-format-mode
             ;; gradle mode
             'gradle-mode
             ;; groovy mode
             'groovy-mode
             ;; 'python-mode ;;'elpy

             ;; Markdown
             'markdown-mode
             ;; Elisp linter
             'elisp-lint

             ;; Color theme
             'color-theme-modern
             ;; 'color-theme-sanityinc-tomorrow
             ;; pdf tools
             ;; 'pdf-tools
             ;; LaTeX
             ;; 'latex-extra
             'nlinum ;; Emacs's buffer line number column display
             ;; customise dired mode
             'diredful
             ;; Projectile (directory project style)
             'projectile)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat)))
 '(fci-rule-color "#515151")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(flycheck-highlighting-mode (quote symbols))
 '(frame-background-mode (quote dark))
 '(global-flycheck-mode t)
 '(js-auto-format-command "prettier")
 '(js-auto-format-command-args "--write --single-quote --no-semi")
 '(mode-require-final-newline t)
 '(nlinum-format " %3i ")
 '(nlinum-hightlight-current-line t)
 '(nlinum-widen t)
 '(package-selected-packages (quote (projectile diredful nlinum color-theme-modern elisp-lint
                                                markdown-mode groovy-mode gradle-mode
                                                js-auto-format-mode web-beautify flycheck-mix
                                                alchemist elixir-mode exec-path-from-shell
                                                flycheck-yamllint flycheck-title flycheck-pos-tip
                                                flycheck-checkbashisms flycheck-color-mode-line
                                                flycheck-rebar3 flycheck whitespace-cleanup-mode
                                                company auto-complete dockerfile-mode
                                                docker-compose-mode docker yaml-mode travis)))
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
 '(mode-line-buffer-id ((t
                         (:background "black"
                                      :foregorund "color-63"
                                      :weight extra-bold
                                      :height 0.9))))
 '(nlinum-current-line ((t
                         (:inherit linum
                                   :background "magenta"
                                   :foreground "green"
                                   :weight ultra-light))))
 '(whitespace-big-indent ((t nil)))
 '(whitespace-space ((t
                      (:bold t
                             :foreground "green"))))
 '(whitespace-trailing ((t
                         (:foreground "green"
                                      :weight bold)))))

;; ==================================================================
;; `Global-Settings'
;; ==================================================================

;; ===========
;; `CamelCase'
;; ===========

(global-subword-mode t)

;; =============================================================================
;; Mode-line
;; =============================================================================
(set-face-foreground 'mode-line "green")
(set-face-background 'mode-line "purple")

;; ==========================================================
;; `Cursor'
;; ==========================================================
(setq-default cursor-type 'hbar)
;; (set-cursor-color "#7F00FF")
(set-cursor-color "#6f75ff")
(setq-default x-stretch-cursor t)

;; ==========================================================
;; `Font-lock' Font Lock mode
;; ==========================================================
(font-lock-mode t)

;; =============================================================================================
;; `Nlinum' - Buffer Line Numbers Column
;; =============================================================================================
(global-nlinum-mode t)

;; ==========================================================
;; `Color-themes' Color Theme Sanityinc Tomorrow
;; ==========================================================
;; Reference [](https://github.com/purcell/color-theme-sanityinc-tomorrow)
;; (require 'color-theme-sanityinc-tomorrow)
;; Check `color-theme-modern' or `Color-Theme'

;; ==================================================================
;; `Color-Theme'
;; ==================================================================
;; Reference [](http://nongnu.org/color-theme)
;; wget http://download.savannah.nongnu.org/release/color-theme/color-theme-6.6.0.tar.gz
;; `7z x color-themr-6.6.0.tar.gz` into  '~/.emacs.d/color-theme' directory
;; (add-to-list 'load-path "~/.emacs.d/color-theme/color-theme-6.6.0")
;; (require 'color-theme)
;; ;; customised theeme source "~/.emacs.d/color-theme/themes/color-theme-amado.el"
;; (eval-after-load "color-theme" '(progn (color-theme-initialize)
;;                                        (color-theme-arneson)
;;                                       (color-theme-amado)))

;; ==================================================================
;; `Color-Theme-Modern'
;; ==================================================================
;; Referene: [](https://github.com/emacs-jp/replace-colorthemes)
;; (load-theme 'railscast t t) ;; 'hober ;; 'ld-dark ;; 'oswald ;; 'matrix
;; ;; 'railscast 'julie 'dark-font-lock
;; (enable-theme 'railscast)

;; ==========================================================
;; `paren' Matching pairs - Show `paren` mode
;; ==========================================================
(show-paren-mode 1)
;; (global-set-key "%" 'match-paren)

;; ==========================================================
;; `Screen' Configurations and Frame
;; ==========================================================
(setq inhibit-startup-screen t)
(when window-system (set-frame-size (selected-frame) 64 51))

;; ==========================================================
;; `Key-binding'
;; ==========================================================
(global-set-key (kbd "M-9") 'kill-whole-line)
(global-set-key (kbd "C-x p")
                (lambda()
                  (interactive)
                  (other-window -1)))

;; Macos bind Meta with key 'cmd'
;; (setq-default mac-option-modifier 'alt)
;; (setq-default mac-command-modifier 'meta)
;; (setq select-enable-clipboard t)

;; ==========================================================
;; `Indentation'
;; ==========================================================
;; No tabs by default. Modes that really need tabs should enable
;; indent-tabs-mode explicitly. makefile-mode already does that
;; (setq-default indent-tabs-mode nil)
;; Default tab width `indent size` is 2
;; (setq-default tab-width 2)
;; Indent according to current Major Mode on/before Save Hook
;; (add-hook 'before-save-hook (lambda()
;;                              (indent-according-to-mode)))

;; Enable auto indent
;; (define-key global-map (kbd "RET") 'newline-and-indent)

;; ==================================
;; `final-newline' Final New Line EOF
;; ==================================
;; `mode-require-final-newline' is defined above. Check
;; `custome-set-variables'
(setq require-final-newline (quote mode-require-final-newline))

;; Either Indent according to Major Mode
;; or save add hook on specific Major Mode Hook
;; (add-hook 'before-save-hook (lambda() ;; (if (not indent-tabs-mode)
;; (untabify (point-min)(point-max))
;;                              (indent-region (point-min)
;;                                             (point-max) nil)))

;; ==========================================================
;; `Whitespace'
;; ==========================================================
(require 'whitespace-cleanup-mode)
(require 'whitespace)
;; Whitespace Cleanup on/before Save Hook
;; (add-hook 'before-save-hook (lambda()
;;                              (whitespace-cleanup)))

;; Delete Trailing Whitespace on/before Save Hook
;; (add-hook 'before-save-hook (lambda()
;;                              (delete-trailing-whitespace)))

;; ==========================================================
;; `Company-mode' COMPlete ANYthing !
;; ==========================================================
;; Reference: [](http://company-mode.github.io/)
;; Either utilise `company-mode' or `auto-complete-mode'
;; """ To turn on for a particular major-mode, set dedicated variable as
;;     follows, in user-defined hooks,
;;     (global-company-mode t) ;; with melpa
;;      or
;;     (add-hook 'after-init-hook 'global-company-mode) ;; with stable-melpa
;; """
;; (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)

;; ==========================================================
;; `Flycheck'
;; ==========================================================
;; See at the top: therefore the following two entries are not
;; necessary
(global-flycheck-mode t)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ==================================================================
;; `auto-complete' AC mode
;; ==================================================================
;; Loading AC mode but enable it only for particular/selective
;; major-mode list
;; If a target major-mode prefers company, then do not that
;; major-mode to the list of AC mode. Instead, configure company mode
;; for that mojore-mode
(require 'auto-complete)
;; Reference [](https://github.com/auto-complete/auto-complete/issues/191)
(defun plg-ac-config()
  "Configure auto-complete AC Mode."
  (setq ac-auto-start 1)
  (setq ac-dwim t)
  ;; (setq ac-quick-help-display 0.5)
  ;; (setq ac-override-local-map nil)
  (setq ac-ignore-case t)
  ;;(global-auto-complete-mode t)
  )
;; ad auto-complete for particular major mode
(mapc (lambda(mode)
        (add-to-list 'ac-modes mode))
      '(latex-mode text-mode graphiz-dot-mode html-mode))
;; eshell-mode
;; add the following evaluation to the defined hook in
;; target major-mode hooks
;; (plg-ac-config)

;; ==============
;; `diredful' customise dired mode
;; ==============
;; Reference:
;; [](https://www.emacswiki.org/emacs/Diredful)
;; Post-setup configurations
;; Run diredful-add.
;; generated configuration file path
;; ~/.emacs.d/diredful-conf.el
(diredful-mode 1)

;; ==================================================================
;; `Projectil'
;; ==================================================================
;; Reference [](https://guithub.com/bbatsov/projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c C-c") 'projectile-command-map)

;; =============================================================================================
;; `Erlang' erlang mode
;; =============================================================================================
(add-to-list 'load-path (car (file-expand-wildcards
                              "/usr/local/Cellar/erlang/*/lib/erlang/lib/tools-*/emacs")))
(setq erlang-root-dir (car (file-expand-wildcards "/usr/local/Cellar/erlang/*/lib/erlang")))
(setq exec-path (cons (car (file-expand-wildcards "/usr/local/Cellar/erlang/*/lib/erlang/bin"))
                      exec-path))
(require 'erlang-start)
(require 'erlang)

(add-to-list 'auto-mode-alist '("\\.config?$" . erlang-mode))

(defun seriott-erlang-mode-hooks ()
  "Hooks for Erlang mode."
  (setq erlang-indent-level 2)
  (setq allout-auto-activation t)
  ;;(setq erlang-indent-paranthesis 2)
  ;; (setq company-idle-delay 0.1)
  ;; (setq company-tooltip-limit 10)
  ;; (setq company-minimum-prefix-length 2)
  ;; (setq company-tooltip-flip-when-above t)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (global-company-mode t)
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'before-save-hook (lambda()
                                (whitespace-cleanup)
                                (delete-trailing-whitespace)
                                (erlang-indent-current-buffer))))
(add-hook 'erlang-mode-hook 'seriott-erlang-mode-hooks)

;; ==============================================================================================
;; `Wrangler'
;; ==============================================================================================
;; (add-to-list 'load-path "/usr/local/Cellar/erlang/20.3.4/lib/erlang/lib/wrangler-1.2.0/elisp")
;; (require 'wrangler)

;;==========================================================
;; `Distel'
;;==========================================================
;; Refrence [](https://github.com/massemanet/distel.git)
;; (add-to-list 'load-path "~/.emacs.d/distel/elisp")
;; (require 'distel)
;; (distel-setup)

;;=============================================================================
;; `Elixir' elixir mode
;;=============================================================================
;; Reference [](https://github.com/elixir-editors/emacs-elixir)
(require 'elixir-mode)

;; ===
;; Format
;; ===

;; repo/file `elixir-format.el` cloned into...
(add-to-list 'load-path "~/.emacs.d/elixir-format")
(require 'elixir-format)
;; The following hook assumes file `.formatter.exs' exists in the directory
;; of work source code
;; References:
;; [](https://github.com/elixir-editors/emacs-elixir#add-elixir-mode-hook-to-run-exlixir-format-on-file-save)
;; [mix format](https://hexdocs.pm/mix/master/Mix.Tasks.Format.html)
(add-hook 'elixir-format-hook (lambda()
                                (setq elixir-format-arguments (list "--dot-formtatter" (concat
                                                                                        (locate-dominating-file
                                                                                         buffer-file-name
                                                                                         ".formatter.exs")
                                                                                        ".formatter.exs")))
                                (setq elixir-format-arguments nil)))

;; ===
;; Config completiong with company and alchemist
;; ===
;; Reference: [](https://github.com/tonini/alchemist.el)
;; TODO: Check status of alchemist-server and start it
(require 'company)
(require 'alchemist)
(defun che-elixir-mode-hooks ()
  "Hooks for Elixir mode."
  (global-company-mode t)
  (add-hook 'after-init-hook 'global-company-mode 'alchemist-mode)
  (setq company-idle-delay 0.1)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-flip-when-above t)
  (add-hook 'before-save-hook 'elixir-format nil t))
(add-hook 'elixir-mode-hook 'che-elixir-mode-hooks)
(add-hook 'alchemist-iex-mode-hook (lambda()
                                     (local-set-key (kbd "<tab>") 'company-complete)))

;; The following add-hook is simpler but missing alchemist and company
;; (add-hook 'elixir-mode-hook (lambda()
;;                              (add-hook 'before-save-hook 'elixir-format nil t)))

;; Flycheck extension for Elixir support
;; Reference [](https://github.com/tomekowal/flycheck-mix)
(require 'flycheck-mix)
(flycheck-mix-setup)

;; ====================================================================
;; `ac-alchemist' // native alchemist does not support aut-complete AC
;; ====================================================================
;; Reference [](https://github.com/syohex/emacs-ac-alchemist)
;; (add-hook 'elixir-mode-hook 'ac-alchemist-setup)
;; Check Elixir mode above configured with alchemist to utilise company

;; ==========================================================
;; `Image' dimensions
;; ==========================================================
;; Reference: [](https://www.emacswiki.org/emacs/image-dimensions-minor-mode.el)
;; Display the image dimensions in the mode line, when viewing an image.
(add-to-list 'load-path "~/.emacs.d/image-dimensions")
(eval-after-load 'image-mode
  '(require 'image-dimensions-minor-mode))
(setq frame-title-format '(buffer-file-name ("%b (Emacs) %f" image-dimensions-minor-mode-dimensions)
                                            (dired-directory
                                             (:eval (concat (buffer-name) " (Emacs) "
                                                            dired-directory))
                                             ("%b (Emacs)"))))

;; ==========================================================
;; `Kotlin'
;; ==========================================================
;; clone or copy https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode/blob/master/kotlin-mode.el into ~/.emacs.d/
;; (load "~/.emacs.d/kotlin/kotlin-mode.el")

;; ==========================================================
;; `Scala' Mode
;; ==========================================================
;; clone https://github.com/ensime/emacs-scala-mode.git into ~/.emacs.d/
;; (add-to-list 'load-path "~/.emacs.d/emacs-scala-mode/")
;; (require 'scala-mode)

;; Flycheck extension for Kotlin support
;; (require 'flycheck-kotlin)
;; (add-hook 'kotlin-mode-hook 'flycheck-mode)

;; ==========================================================
;; `Spell' check
;; ==========================================================
(autoload 'flyspell-mode "flyspell" "on-the-fly check spelling." t)
(add-hook 'LaTex-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; ==========================================================
;; `Python'
;; ==========================================================
;; (require 'python-mode)
;; Elpy
;; Reference [](https://elpy.readthedocs.io/en/latest/introduction.html\#installation)
;; (elpy-enable)

;; ==========================================================
;; `JavaScript'
;; ==========================================================
;; Dependency: ;; npm install --global eslint ;; add ~/.eslintrc with contents, e.g., { "extends": "eslint:recommended" }
;;             | prettier ;; npm install --global prettier
;; Reference [](https://github.com/ybiquitous/js-auto-format-mode)
;; Configure formatting engine (either eslint or prettier), as follows
;; Configure with `prettier` (which already evaluated at the top - see above):
;; (custom-set-variables
;;  '(js-auto-format-command "prettier")
;;  '(js-auto-format-command-args "--write --single-quote --no-semi"))

(setq-default js-indent-level 2)
;; The following line is conflicting with the save hook
;; (add-hook 'js-mood-hook #'js-auto-format-mode)

;; Run upon Save
(defun seriott-js-mode-save-hook()
  "Save hooks for js mode."
  (when (eq major-mode 'js-mode)
    (whitespace-cleanup)
    (delete-trailing-whitespace)
    (js-auto-format-execute)))
(add-hook 'before-save-hook #'seriott-js-mode-save-hook)

;; ==========================================================
;; `Web' web-mode
;; ==========================================================
;; Reference: [](http://web-mode.org/)
;; Download it as follows
;; $ wget https://raw.githubusercontent.com/fxbois/web-mode/master/web-mode.el
(add-to-list 'load-path "~/.emacs.d/web-mode")
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex?\\'" . web-mode))

;; Configure engine(s) when file extension is too general or unknown
(setq web-mode-engines-alist '(("php"    . "\\.phtml\\'")
                               ("blade"  . "\\.blade\\.")))

;; Configure
(add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/some/react/path/.*\\.js[x]?\\'" . web-mode))

;; associate a file path with content type
(setq web-mode-content-types-alist '(("json" . "/some/path/.*\\.api\\'")
                                     ("xml"  . "/other/path/.*\\.api\\'")
                                     ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))

;; (setq-default web-mode-indent-style 2)

(defun seriott-web-mode-hooks ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-block-padding 4)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (add-hook 'local-write-file-hooks (lambda()
                                      (whitespace-cleanup)
                                      (indent-according-to-mode)
                                      (indent-region (point-min)
                                                     (point-max) nil)
                                      (delete-trailing-whitespace) nil)))
(add-hook 'web-mode-hook 'seriott-web-mode-hooks)

;; =============================================================================================
;; `web-beautify' formatter when web mode
;; =============================================================================================
;; (add-to-list 'load-path "~/.emacs.d/web-format")
;; (require 'web-beautify) ;; Dependency: $ npm -g install js-beautify;; add file `~/.jsbeautifyrc` with contents {"indent-size": 2, "end_with_newline": true}
;; Reference: [](https://github.com/yasuyk/web-beautify)

;; (defun seriott-web-beautify-save-hooks ()
;; "Save hooks with web-beautify for web mode."
;;  ;; (!) Becarefull! Add one and only one for the same major mode.
;; (web-beautify-html-buffer))
;; (add-hook 'web-mode-hook (lambda()
;;                            (add-hook 'before-save-hook 'seriott-web-beautify-save-hooks t t)))

;; ==========================================================
;; `Dockerfile' linter
;; ==========================================================
;; Ref [](https://github.com/hadolint/hadolint.git)

;; ==========================================================
;; `emacs-lisp' `elisp' Mode
;; ==========================================================
;; Reference [](https://github.com/Yuki-Inoue/elisp-format/blob/master/elisp-format.el)
(add-to-list 'load-path "~/.emacs.d/elisp-format")
(require 'elisp-format)
(defun ear-emacs-lisp-mode-hooks ()
  "Hooks for emacs-lisp mode."
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq allout-auto-activation t)
  (global-company-mode t)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.1)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-flip-when-above t)
  ;; Customise Mode Line color in emacs-lisp mode
  (face-remap-add-relative 'mode-line
                           '((:foreground "black"
                                          :background "green"))) ;;)
  (add-hook 'before-save-hook (lambda()
                                (elisp-format-buffer)
                                (whitespace-cleanup)
                                (delete-trailing-whitespace))))

(add-hook 'emacs-lisp-mode-hook 'ear-emacs-lisp-mode-hooks)

;; ==========================================================
;; `Markdown'
;; ==========================================================
(defun ser-markdown-mode-hooks ()
  "Hooks for markdown mode."
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (setq allout-auto-activation t)
  (setq markdown-indent-on-enter t)
  (add-hook 'local-write-file-hooks (lambda()
                                      (indent-according-to-mode)
                                      (whitespace-cleanup)
                                      (delete-trailing-whitespace) nil)))
(add-hook 'markdown-mode-hook #'ser-markdown-mode-hooks)


;; ==================================================================
;; `YAML' Mode
;; ==================================================================
(require 'yaml-mode)
(defun mor-yaml-mode-hooks()
  "Hooks for yaml-mode."
  (setq indent-tabs-mode nil)
  (setq yaml-indent-offset 2)
  (define-key yaml-mode-map "\C-m" 'newline-and-indent)
  (add-hook 'before-save-hook (lambda()
                                (whitespace-cleanup))))
(add-hook 'yaml-mode-hook 'mor-yaml-mode-hooks)

;; ==================================================================
;; `Change-log-mode'
;; ==================================================================
;; In this mode I do not want indent by defualt upon key enter
;; Also, indent to first column for the first 73 point in emacs buffer
;; as auto-format upon save
;; If you want to auto-indent for the complete file on-save, then
;; change to (indent-region (point-min) (point-max) 0)
(defun glaz-change-log-mode-hooks()
  "Hooks for change-log mode."
  (setq indent-line-function 'indent-relative)
  (add-hook 'before-save-hook (lambda()
                                (indent-region 1 73 0)
                                (whitespace-cleanup)
                                (delete-trailing-whitespace))))
(add-hook 'change-log-mode-hook 'glaz-change-log-mode-hooks)

;; ==================================================================
;; `Clip-board' Copy - Paste- from- / to MAC OS
;; ==================================================================
;; Reference:
;; [](https:gist.github.com/the-kenny/267162)

(defun copy-from-osx ()
  "Copy from osx."
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  "Copy `TEXT` to osx as `PUSH`."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; Check OS type
(cond ((string-equal system-type "window-nt")
       (progn (message "Microsoft Windows")))
      ((string-equal system-type "gnu/linux")
       (progn (message "Linux")))
      ((string-equal system-type "darwin")
       (progn (message "Mac OS X")
              ;; ====================================
              ;; solve `ls does not support --dired;`
              ;; ====================================
              (setq dired-use-ls-dired nil)

              ;; ======
              ;; Integrate pbcopy
              ;; =====
              (setq interprogram-cut-function 'paste-to-osx)
              (setq interprogram-paste-function 'copy-from-osx))))

;; =============================================================================================
;; =============================================================================================
;;   (indent-region (point-min) (point-max) nil)
;;   (untabify (point-min) (point-max))
;;   (delete-trailing-whitespace)
;;   (save-buffer)

;; emacs!
(provide '.emacs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; .emacs ends here
