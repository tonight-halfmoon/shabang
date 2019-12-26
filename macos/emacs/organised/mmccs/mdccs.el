;;; package --- Summary:
;;; Customise `markdown' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))
(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))
(unless (package-installed-p 'whitespace-cleanup-mode)
  (package-install 'whitespace-cleanup-mode))
(unless (package-installed-p 'flyspell)
  (package-install 'flyspell))

(require 'auto-complete)

;;(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
;;(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;;(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook 'auto-complete-mode t)
(add-hook 'markdown-mode-hook 'flyspell-mode)

(defvar markdown-indent-on-enter)
(defvar ac-override-local-map)

(defun ser-markdown-mode-hooks ()
  "Hooks for Markdown mode."
  (setq indent-tabs-mode nil tab-width 2)
  (setq allout-auto-activation t)
  (setq markdown-indent-on-enter t)
  (setq ac-auto-start 1 ac-dwim t ac-quick-help-delay 0.1)
  (setq ac-override-local-map nil ac-ignore-case t)
  (add-hook 'local-write-file-hooks (lambda()
                                      (indent-according-to-mode)
                                      (whitespace-cleanup)
                                      (delete-trailing-whitespace) nil)))
(add-hook 'markdown-mode-hook 'ser-markdown-mode-hooks)

;; mdccs!
(provide 'mdccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; mdccs ends here
