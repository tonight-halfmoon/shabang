;;; package --- Summary:
;;; Customise `Highlight-changes' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(add-hook 'text-mode-hook 'highlight-changes-mode)
(add-hook 'prog-mode-hook 'highlight-changes-mode)

;; Function to remove highlight-changes after-save
;; Usage: Add it to a target major-mode's hook
(defun highlight-changes-remove-after-save-hook()
  "Remove recent change after save."
  (add-hook 'after-save-hook (lambda()
                               (highlight-changes-remove-highlight (point-min)
                                                                   (point-max)))))
;; Usage example:
(add-hook 'groovy-mode-hook 'highlight-changes-remove-after-save-hook)
;; Note: on emacs-lisp-mode you need to explicitly check current major mode is emacs-lisp
;; Check emacslispccs.el

;; highlightchangesccs!
(provide 'highlightchangesccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; highlightchangesccs ends here
