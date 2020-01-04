;;; package --- Summary:
;;; Customise `Projectile' mode
;; Package-Requires:
;;; Commentary:
;;
;; Reference:
;; [Projectile](https://www.projectile.mx/en/latest/)
;; [](https://github.com/bbatsov/projectile)
;;
;;; Code:

(unless package-archive-contents (package-refresh-contents))

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
