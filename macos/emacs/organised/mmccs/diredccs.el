;;; package --- Summary:
;; `diredful'  - customise dired mode
;; Package-Requires:
;;; Commentary:
;;
;; Reference:
;; [](https://www.emacswiki.org/emacs/Diredful)
;;
;; Post-setup configurations
;; Run diredful-add.
;; generated configuration file path
;; ~/.emacs.d/diredful-conf.el
;;
;;; Code:

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'diredful)
  (package-install 'diredful))

(diredful-mode 1)

(provide 'diredccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; diredccs ends here
