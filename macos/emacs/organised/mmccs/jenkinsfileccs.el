;;; package --- Summary:
;;; Customise `jenkinsfile' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(setq auto-mode-alist (append '(("\\jenkinsfileccs.el\\'" . emacs-lisp-mode)) auto-mode-alist))

;; ==============================
;; `Jenkinsfile' mode (`scriptive')
;; ==============================
;; wget https://raw.githubusercontent.com/john2x/jenkinsfile-mode/master/jenkinsfile-mode.el

(eval-when-compile 'jenkinsfile-mode
                   ;;
                   (add-to-list 'load-path (expand-file-name "~/.emacs.d/jenkinsfile-mode"))
                   (require 'jenkinsfile-mode))

(add-to-list 'load-path "~/.emacs.d/jenkinsfile-mode")

(require 'jenkinsfile-mode)

(provide 'jenkinsfileccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; jenkinsfileccs ends here
