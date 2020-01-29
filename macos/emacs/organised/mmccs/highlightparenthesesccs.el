;;; package --- Summary:
;;; Customise `Highlight-parentheses' mode
;; Package-Requires:
;;; Commentary:
;;
;; Reference:
;; [](https://github.com/tsdh/highlight-parentheses.el)
;;
;;; Code:

(show-paren-mode 1)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'highlight-parentheses)
  (package-install 'highlight-parentheses))

;;(add-hook 'prog-mode-hook 'highlight-parentheses-mode)

(provide 'highlightparenthesesccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; highlightparenthesesccs ends here
