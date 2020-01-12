;;; package --- Summary:
;;; Use `Swiper'
;; Package-Requires:
;;; Commentary:
;;
;; Reference:
;; [](https://github.com/tsdh/swiper)
;;
;;; Code:

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'swiper)
  (package-install 'swiper))

(require 'swiper)

(ivy-mode 1)

(setq ivy-use-virtual-buffers t)

(global-set-key "\C-s" 'swiper)

(provide 'swiperccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; swiperccs ends here
