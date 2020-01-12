;;; package --- Summary:
;;; Customise `Web' mode
;; Package-Requires:
;;; Commentary:
;;; Code:
;;
;; Reference [](http://web-mode.org/)
;; Download it as follows
;; $ wget https://raw.githubusercontent.com/fxbois/web-mode/master/web-mode.el

(require 'package)

(add-to-list 'package-pinned-packages '(web-mode . "melpa") t)

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'web-mode)
  (package-install 'web-mode))

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex?\\'" . web-mode))

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
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (add-hook 'before-save-hook (lambda ()
                                (indent-according-to-mode)
                                (indent-region (point-min)
                                               (point-max) nil)
                                (whitespace-cleanup)) t t))

(add-hook 'web-mode-hook #'seriott-web-mode-hooks)

(provide 'webfccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; webfccs ends here
