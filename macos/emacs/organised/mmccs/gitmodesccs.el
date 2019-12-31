;;; package --- Summary:
;;; Use `git' modes
;; Package-Requires:
;;; Commentary:
;; Reference:
;; [git-modes](https://github.com/magit/git-modes/)
;;; Code:

(unless (package-installed-p 'gitconfig-mode)
  (package-install 'gitconfig-mode))

;; gitmodesccs!
(provide 'gitmodesccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; gitmodesccs ends here
