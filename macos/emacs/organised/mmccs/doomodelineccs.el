;;; package --- Summary:
;;; Use `doom-modeline' mode
;; Package-Requires:
;;; Commentary:
;;
;; Reference:
;; [](https://github.com/seagle0128/doom-modeline.git)
;;
;;; Code:

(add-to-list 'package-pinned-packages '(doom-modeline . "melpa") t)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'doom-modeline)
  (package-install 'doom-modeline))

(require 'doom-modeline)

(doom-modeline-mode 1)

(provide 'doomodelineccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; doomodelineccs ends here
