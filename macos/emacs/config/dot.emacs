;;; package --- Summary:

;;; Commentary:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/"))))

  ;; Elixir Mode package source
  (add-to-list 'package-pinned-packages '(elixir-mode . "melpa-stable"))
  )
(package-initialize)

;; Wrangler
;;(add-to-list 'load-path "/usr/local/Cellar/erlang/20.3.4/lib/erlang/lib/wrangler-1.2.0/elisp")
;;(require 'wrangler)

;; Additional themes
;;(add-to-list 'custom-theme-load-path "/Users/amado/.emacs.d/themes")
;;(load-theme 'odersky t)

;; MELPA
;;(add-to-list 'package-archives
;;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; pkg_install
;; Consume a list of Emacs MELPA packages
;; Traverse each package in the list
;; Prompt for install
;; User answer y/n.
;; The MELPA install is not successful if missing library behind
;; on the OS is required.
;; In addition, add-to-list package remote source could be necessary
;; to tell MELPA where to look for the source. See above, the section before package-initialize.
;; Reference: [](https://stackoverflow.com/a/10095853)
(defun pkg_install  (&rest tail)
  "Prompt for MELPA package install with y/n."
  (mapcar
   (lambda (next)
     (if (package-installed-p next)
         nil
       (if (y-or-n-p (format "Do you want to install packge '%s'?" next))
           (package-install next)
         next)))
   tail))

(or (file-exists-p package-user-dir)
    (package-refresh-contents)
    )

(pkg_install
 ;; My package list
 'general 'dictionary

 'travis 'yaml-mode 'docker 'docker-compose-mode 'dockerfile-mode

 'company 'company-distel

 'whitespace-cleanup-mode
 'smart-hungry-delete
 'auto-complete 'auto-complete-distel
 'format-all
 'flycheck 'flycheck-dialyzer 'flycheck-rebar3
 'flycheck-gradle 'flycheck-kotlin
 'flycheck-pycheckers 'flycheck-pyflakes
 'flycheck-color-mode-line

 'gradle-mode
 'groovy-mode
 'es-mode 'js-format 'js-auto-format-mode 'js-auto-beautify

 'erlang 'erlstack-mode
 ;;'elixir-mode
 ;;'elixir-yasnippets

 'html5-schema 'html-to-markdown 'xml+
 'markdown-mode
 ;;'pdf-mode
 ;;'latex-extra 'latex-preview-mode

 ;;'python-mode 'python-environment 'elpy

 ;;'scala-mode
 ;;'kotlin-mode

 'babel

 'color-theme-sanityinc-tomorrow
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   (vector "#515151" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#cccccc"))
 '(beacon-color "#f2777a")
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#515151")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(global-flycheck-mode t)
 '(linum-format " %7i ")
 '(package-selected-packages
   (quote
    (elixir-mode ## erlstack-mode js-auto-beautify js-auto-format-mode js-format es-mode gradle-mode flycheck-color-mode-line flycheck-gradle flycheck-rebar3 flycheck-dialyzer auto-complete-distel smart-hungry-delete whitespace-cleanup-mode company-distel travis general groovy-mode babel format-all docker-compose-mode docker dockerfile-mode dictionary color-theme-sanityinc-tomorrow company)))
 '(safe-local-variable-values (quote ((sh-indent-comment . t) (allout-layout . t))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
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
 '(mode-line-buffer-id ((t (:background "black" :foregorund "magenta" :weight bold :height 0.9)))))

;;==========================================================
;; Mode-line
;;==========================================================
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (face-remap-add-relative
             'mode-line '((:foreground "cyan" :background "black") mode-line))))

;;==========================================================
;; Cursor
;;==========================================================
(setq-default cursor-type 'hbar)
;;(set-cursor-color "#7F00FF")
;;(set-cursor-color "#00ff00")
(set-cursor-color "#6f75ff")
(setq-default x-stretch-cursor 1)

;;==========================================================
;; Font lock mode
;;==========================================================
(font-lock-mode t)

;;==========================================================
;; Color-theme Sanityinc
;;==========================================================
;; Ref: https://github.com/purcell/color-theme-sanityinc-tomorrow
;; M-x package-install RET color-theme-sanityinc-tomorrow RET
(require 'color-theme-sanityinc-tomorrow)
;; then M-x color-theme-sanityinc-tomorrow-... /-night /-eighties -etc

;;==========================================================
;; show closing bracket
;;==========================================================
(show-paren-mode 1)
(global-set-key "%" 'match-paren)

;;==========================================================
;; Screen and Frame
;;==========================================================
(setq inhibit-startup-screen t)
(when window-system (set-frame-size (selected-frame) 64 51))

;;==========================================================
;; Key binding
;;==========================================================
(global-set-key (kbd "M-9") 'kill-whole-line)

;; Macos bind Meta with key 'cmd'
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      select-enable-clipboard t)

;;==========================================================
;; Whitespace
;;==========================================================
(require 'whitespace-cleanup-mode)
(require 'whitespace)
;;; Whitespace Cleanup on/before Save Hook
;;(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (whitespace-cleanup)))

;; Delete Trailing Whitespace on/before Save Hook
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

(add-hook 'before-save-hook
          (lambda() (if (not indent-tabs-mode)
                        (untabify (point-min)(point-max))))
          )

;;==========================================================
;; Indentation
;;==========================================================
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Indent according to current Major Mode on/before Save Hook
(add-hook 'before-save-hook (lambda() (indent-according-to-mode)))

;; Default tab width is 2
(setq-default tab-width 2)

(setq-default indent-tabs-mode nil)

;;==========================================================
;; Company Mode
;;==========================================================
(add-hook 'after-init-hook 'global-company-mode)

;;==========================================================
;; Format
;;==========================================================
;; Format w.r.t to Major Mode (programming language)
(add-hook 'before-save-hook (lambda() (format-all-mode)))

;; Note
;; Shell script formatting
;; format-all installed via MELPA
;; brew install shfmt

;;==========================================================
;; Erlang Mode
;;==========================================================
(add-to-list
 'load-path
 (car (file-expand-wildcards "/usr/local/Cellar/erlang/*/lib/erlang/lib/tools-*/emacs")))

;;(setq load-path (cons "/usr/local/Cellar/erlang/22.1.3/lib/erlang/lib/tools-3.2.1/emacs" load-path))
(setq erlang-root-dir (car (file-expand-wildcards "/usr/local/Cellar/erlang/*/lib/erlang")));;"/usr/local/Cellar/erlang/22.1.3/lib/erlang")
(setq exec-path (cons (car (file-expand-wildcards "/usr/local/Cellar/erlang/*/lib/erlang/bin")) exec-path));;(cons "/usr/local/Cellar/erlang/22.1.3/lib/erlang/bin" exec-path))
(require 'erlang-start)
(require 'erlang)

(add-to-list 'auto-mode-alist '("\\.config?$" . erlang-mode))

(setq-default erlang-indent-level 2)
(setq-default allout-auto-activation t)
(setq-default erlang-indent-paranthesis 2)

;; See method #2 follows
;;(add-hook 'before-save-hook 'erlang-indent-current-buffer)

;; Method #2
(defun esl-erlang-mode-before-save-hook()
  "When Major Mode is Erlang then apply the following on/before save."
  (when (eq major-mode 'erlang-mode)
    (whitespace-cleanup)
    (delete-trailing-whitespace)
    (erlang-indent-current-buffer)
    )
  )
(add-hook 'before-save-hook #'esl-erlang-mode-before-save-hook)

;; Format in erlang mode
;;(defun emacs-indent-function ()
;;  "Format the whole buffer."
;;   (erlang-mode)
;;   (indent-region (point-min) (point-max) nil)
;;   (untabify (point-min) (point-max))
;;   (delete-trailing-whitespace)
;;   (save-buffer)
;;   )

;;==========================================================
;; Elixir Mode
;;==========================================================
;;(unless (package-installed-p 'elixir-mode)
;;  (package-install 'elixir-mode)
;;  )

;; Format upon Save when Elixir Mode
;;(add-hook 'elixir-mode-hook
;;          (lambda() (add-hook 'before-save-hook 'elixir-format nil t))
;;          )

;;==========================================================
;; Distel
;;==========================================================
;; https://github.com/massemanet/distel.git
;;(add-to-list 'load-path "~/.emacs.d/distel/elisp")
;;(require 'distel)
;;(distel-setup)

;;==========================================================
;; Image dimensions
;;==========================================================
;; Reference: https://www.emacswiki.org/emacs/image-dimensions-minor-mode.el
;; Display the image dimensions in the mode line, when viewing an image.
(load "$HOME/.emacs.d/image-dimensions/image-dimensions-minor-mode.el")
(eval-after-load 'image-mode '(require 'image-dimensions-minor-mode))
(setq frame-title-format
      '(buffer-file-name
        ("%b (Emacs) %f" image-dimensions-minor-mode-dimensions)
        (dired-directory
         (:eval (concat (buffer-name) " (Emacs) " dired-directory))
         ("%b (Emacs)"))))

;;==========================================================
;; Gradle Mode
;;==========================================================
(require 'gradle-mode)

;;==========================================================
;; Kotlin Mode
;;==========================================================
;; clone or copy https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode/blob/master/kotlin-mode.el into $HOME/.emacs.d/
(load "$HOME/.emacs.d/kotlin/kotlin-mode.el")

;;==========================================================
;; Scala Mode
;;==========================================================
;; clone https://github.com/ensime/emacs-scala-mode.git into $HOME/.emacs.d/
;;(add-to-list 'load-path "$HOME/.emacs.d/emacs-scala-mode/")
;;(require 'scala-mode)

;;==========================================================
;; Flycheck
;;==========================================================
(global-flycheck-mode t)

;; M-x
;; package-refresh-list
;; pacakge-install flycheck-kotlin
(require 'flycheck-kotlin)
(add-hook 'kotlin-mode-hook 'flycheck-mode)

;;==========================================================
;; Spell check
;;==========================================================
(add-hook 'text-mode-hook 'flyspell-mode)

;;==========================================================
;;==========================================================
(defun match-paren (arg)
  "Go to the matching paren if on a paren ${ARG}; otherwise insert."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;==========================================================
;; Python Mode
;;==========================================================
;;(elpy-enable)

;;==========================================================
;; JavaScript Mode
;;==========================================================
;;(setq-default js-indent-level 2)

(provide '.emacs)
;;; .emacs ends here
