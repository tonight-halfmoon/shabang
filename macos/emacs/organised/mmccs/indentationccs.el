;;; package --- Summary:
;;; Customise `Indentation'
;; Package-Requires:
;;; Commentary:
;;
;; Add two modes
;;
;; (1) 'highlight-indentation'
;; (2) 'aggressive-indent'
;;
;; References:
;; [](https://github.com/antonj/Highlight-Indentation-for-Emacs)
;; [](https://github.com/Malabarba/aggressive-indent-mode)
;;
;;; Code:

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'highlight-indentation)
  (package-install 'highlight-indentation))

;;(add-hook 'text-mode-hook 'highlight-indentation-mode)
(add-hook 'text-mode-hook 'highlight-indentation-current-column-mode)
;;(add-hook 'prog-mode-hook 'highlight-indentation-mode)
(add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)


(with-eval-after-load 'highlight-indentation
  ;;
  (set-face-background 'highlight-indentation-face "brightgreen")
  (set-face-background 'highlight-indentation-current-column-face "brightgreen"))

;; -----------------------------------
;; Customise `aggressive-indent' mode
;; ===================================

(unless (package-installed-p 'aggressive-indent)
  (package-install 'aggressive-indent))

(require 'aggressive-indent)

(add-hook 'prog-mode-hook #'aggressive-indent-mode)

(provide 'indentationccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; indentationccs ends here
