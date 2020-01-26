;;; package --- Summary:
;;; configure `Whitespace' mode
;; Package-Requires:
;;; Commentary:
;;
;; check and customise variable `whitespace-style'
;;
;;; Code:

(require 'package)

(add-to-list 'package-pinned-packages '(whitespace-cleanup-mode . "melpa") t)

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'whitespace-cleanup-mode)
  (package-install 'whitespace-cleanup-mode))

(require 'whitespace)

(when (string-equal system-type "berkeley-unix")
  ;; FreeBSD
  ;; Check `.login_conf'
  (setq whitespace-display-mappings '((space-mark 32 []
                                                  [46]
                                                  [0]
                                                  [32]
                                                  [42]
                                                  [46]))))

(set-face-attribute 'whitespace-big-indent nil)

(set-face-attribute 'whitespace-space nil
                    :foreground "#00cd00"
                    :weight 'bold)

(set-face-attribute 'whitespace-trailing nil
                    :foreground "#00cd00"
                    :weight 'bold)

(provide 'whitespaceccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; whitespaceccs ends here
