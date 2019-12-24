;;; package --- Summary:
;;; Customise `Spell' check
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless (package-installed-p 'flyspell)
  (package-install 'flyspell))
(autoload 'flyspell-mode "flyspell" "on-the-fly check spelling." t)
(add-hook 'LaTex-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(declare-function ispell-init-process (ispell-init-process &optional args))
(defun message-off-advice (oldfun &rest args)
  "Quiet down messages in adviced OLDFUN ARGS."
  (let ((message-off (make-symbol "message-off")))
    (unwind-protect (progn (advice-add #'message
                                       :around #'ignore (list 'name message-off))
                           (apply oldfun args))
      (advice-remove #'message message-off))))

(advice-add #'ispell-init-process
            :around #'message-off-advice)

;; flyspellccs!
(provide 'flyspellccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; flyspellccs ends here
