;;; package --- Summary:
;;; Customise `Change-Log' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

;; In this mode I do not want to indenting
;; the whole file upon save.
;; Instead, this customisation is configured to indent
;; the first 73 point in emacs buffer
;; as auto-format upon save
;; If you want to auto-indent for the complete file on-save, then
;; change to (indent-region (point-min) (point-max) 0)

(defun glaz-change-log-mode-hooks ()
  "Hooks for changel-log mode."
  (setq indent-line-function 'indent-relative)
  (add-hook 'before-save-hook (lambda()
                                (indent-region 1 73 0)
                                (whitespace-cleanup)) t t))

(add-hook 'change-log-mode-hook #'glaz-change-log-mode-hooks)

(provide 'changelogccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; changelogccs ends here
