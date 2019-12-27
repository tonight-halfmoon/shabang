;;; package --- Summary:
;;; Customise `Projectile' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless package-archive-contents (package-refresh-contents))
;; Reference [](https://github.com/bbatsov/projectile)
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c C-c") 'projectile-command-map)


;; projectileccs!
(provide 'projectileccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; projectileccs ends here