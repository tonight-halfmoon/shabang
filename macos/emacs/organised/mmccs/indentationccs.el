;;; package --- Summary:
;;; Customise `Indentation'
;; Package-Requires:
;;; Commentary:
;;; Code:

;; Major-mode-specific configurations
;; highlight-indentation
;; Reference:
;; [](https://github.com/antonj/Highlight-Indentation-for-Emacs)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'highlight-indentation)
  (package-install 'highlight-indentation))

(add-hook 'text-mode-hook 'highlight-indentation-mode)
(add-hook 'text-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'prog-mode-hook 'highlight-indentation-mode)
(add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)

;; indentationccs!
(provide 'indentationccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; indentationccs ends here
