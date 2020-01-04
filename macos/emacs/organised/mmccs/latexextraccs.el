;;; package --- Summary:
;;; Use `latex-extra'
;; Package-Requires:
;;; Commentary:
;; Reference:
;; [](https://github.com/Malabarba/latex-extra)
;; In short, you nee the following two commands:
;; C-c C-q (to format your tex file)
;; and,
;; C-c C-a
;; and your pdf will pop-up in front of you!
;; For more detail, have a look at the reference above.
;;; Code:

(require 'package)

(add-to-list 'package-pinned-packages '(auctex . "gnu") t)

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'auctex)
  (package-install 'auctex))

(unless (package-installed-p 'latex-extra)
  (package-install 'latex-extra))

(add-hook 'LaTeX-mode-hook 'latex-extra-mode)

;; latexextraccs!
(provide 'latexextraccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; latexextraccs ends here
