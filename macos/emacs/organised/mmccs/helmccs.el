;;; package --- Summary:
;;; Use `Helm' mode
;; Package-Requires:
;;; Commentary:
;;
;; Reference:
;; [](https://github.com/emacs-helm/helm/wiki#install)
;;
;;; Code:

(add-to-list 'package-pinned-packages '(helm . "melpa") t)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'helm)
  (package-install 'helm))

(require 'helm-config)

(helm-mode 1)

(provide 'helmccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; helmccs ends here
