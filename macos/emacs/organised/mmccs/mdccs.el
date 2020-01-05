;;; package --- Summary:
;;; Customise `markdown' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

;; --------------------------
;; Underlying Configurations
;; --------------------------
;; `brew install markdown`
;; but at least this does not support rendering a table.
;; Use `pandoc':
;; `brew install pandoc`
;; Check variable `markdown-command' in emacs
;; M-x describe-variable RET markdown-command RET
;; By default, its value is `markdown`.
;; Now, choose to customising it!
;; Customise it in a way to utilising a target markdown converter.
;; I'd like to utilise`pandoc'.
;; See the configuration of the variable follows.
;;
;; References:
;; [](https://github.com/jrblevin/markdown-mode/)
;; [](https://pandoc.org/installing.html#macos)
;; [](https://pandoc.org/MANUAL.html)
;; [](http://endwan.com/blog/2013/10/06/about-emacs-calling-shell-command/)
;; [](https://ronn-bundgaard.dk/blog/convert-docx-to-markdown-with-pandoc/)

(require 'package)

(add-to-list 'package-pinned-packages '(markdown-mode . "melpa") t)

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

(require 'markdown-mode)

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

;;(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;;(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defun ser-markdown-mode-hooks ()
  "Hooks for Markdown mode."
  (setq markdown-command "pandoc --from markdown --to html --ascii")
  (setq markdown-indent-on-enter t)
  (setq indent-tabs-mode nil tab-width 2)
  (setq allout-auto-activation t)
  (add-hook 'before-save-hook (lambda()
                                (indent-according-to-mode)
                                (whitespace-cleanup)) t t))

(add-hook 'markdown-mode-hook #'ser-markdown-mode-hooks)

;; mdccs!
(provide 'mdccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; mdccs ends here
