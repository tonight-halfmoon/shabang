;;; package --- Summary:
;;; Customise `Highlight-parentheses' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

;;(show-paren-mode 1)
;; (global-set-key "%" 'match-paren)

;; Reference:
;; [](https://github.com/tsdh/highlight-parentheses.el)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'highlight-parentheses)
  (package-install 'highlight-parentheses))

(add-hook 'prog-mode-hook 'highlight-parentheses-mode)

;; highlightparenthesesccs!
(provide 'highlightparenthesesccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; highlightparenthesesccs ends here
