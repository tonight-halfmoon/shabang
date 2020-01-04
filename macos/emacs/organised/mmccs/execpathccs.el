;;; package --- Summary:
;;; Configure `exec-path´
;; Package-Requires:
;;; Commentary:
;; Alchemist server will start on "dev" mode
;;; Code:

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(require 'exec-path-from-shell)

(cond ((string-equal system-type "darwin")
       (progn (unless (package-installed-p 'exec-path-from-shell)
                (package-install 'exec-path-from-shell)
                (exec-path-from-shell-initialize)
                (setq exec-path-from-shell-check-startup-files nil)))))

(provide 'execpathccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; execpathccs ends here
