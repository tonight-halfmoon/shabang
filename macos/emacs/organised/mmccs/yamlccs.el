;;; package --- Summary:
;;; Customise `yaml' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

(require 'yaml-mode)
(require 'company)
(require 'whitespace-cleanup-mode)
(require 'whitespace)

(add-hook 'yaml-mode-hook 'company-mode t)
;; YAML mode is a text-derived mode, check reference
;; [](https://github.com/yoshiki/yaml-mode)
;; But anyway, I want flyspell to work on prog mode
;;(remove-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'yaml-mode-hook 'flyspell-prog-mode)

(defvar ac-override-local-map)

(defun mor-yaml-mode-hooks()
  "Hooks for yaml-mode."
  (setq indent-tabs-mode nil)
  (setq yaml-indent-offset 2)
  (setq company-idle-delay 0.1 company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1 company-tooltip-flip-when-above t)
  (define-key yaml-mode-map "\C-m" 'newline-and-indent)
  (add-hook 'before-save-hook (lambda()
                                (whitespace-cleanup)) t t))
(add-hook 'yaml-mode-hook #'mor-yaml-mode-hooks)

;; yamlccs!
(provide 'yamlccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; yamlccs ends here
