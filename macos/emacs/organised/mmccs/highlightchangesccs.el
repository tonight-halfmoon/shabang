;;; package --- Summary:
;;; Customise `Highlight-changes' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(add-hook 'text-mode-hook 'highlight-changes-mode)
(add-hook 'prog-mode-hook 'highlight-changes-mode)

(with-eval-after-load 'highlight-changes-mode
  ;;
  (set-face-attribute 'highlight-changes nil
                      :foreground "yellow")
  ;;
  (set-face-attribute 'highlight-changes-delete nil
                      :foreground "yellow"
                      :underline nil))

(defun highlight-changes-remove-after-save-hook()
  "Remove recent change after save."
  (add-hook 'after-save-hook (lambda()
                               (highlight-changes-remove-highlight (point-min)
                                                                   (point-max))) t t))

;; (add-hook 'groovy-mode-hook #'highlight-changes-remove-after-save-hook)

(provide 'highlightchangesccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; highlightchangesccs ends here
