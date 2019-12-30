;;; package --- Summary:
;;; configure `Whitespace' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless (package-installed-p 'whitespace-cleanup-mode)
  (package-install 'whitespace-cleanup-mode))
(require 'whitespace-cleanup-mode)
(require 'whitespace)

(cond ((string-equal system-type "berkeley-unix")
       (progn (custom-set-variables
               ;; FreeBSD
               ;; Check .login_conf
               '(whitespace-display-mappings (quote ((space-mark 32 []
                                                                 [46]
                                                                 [0]
                                                                 [32]
                                                                 [42]
                                                                 [46]))))))))

(custom-set-faces '(whitespace-big-indent ((t nil)))
                  '(whitespace-space ((t
                                       (:bold t
                                              :foreground "green"))))
                  '(whitespace-trailing ((t
                                          (:foreground "green"
                                                       :weight bold)))))


;; whitespaceccs!
(provide 'whitespaceccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; whitespaceccs ends here
