;;; package --- Summary:
;;; Customise `JavaScript' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'js-auto-format-mode)
  (package-install 'js-auto-format-mode))

(require 'js-auto-format-mode)

;; Dependency: Either `eslint' or `prettier'
;; Option (1)
;; npm install --global eslint
;; add ~/.eslintrc with contents, e.g., { "extends": "eslint:recommended" }
;; Option (2)
;; npm install -g prettier
;; check configuration above wiht `cusomt-set-variables'
;; Dependency: ;; npm install --global eslint ;; add ~/.eslintrc with contents, e.g., { "extends": "eslint:recommended" }
;;             | prettier ;; npm install --global prettier
;; Reference [](https://github.com/ybiquitous/js-auto-format-mode)
;; Configure formatting engine (either eslint or prettier), as follows
;; Configure with `prettier` in custom-set-variables as follows:
;; (custom-set-variables
;;  '(js-auto-format-command "prettier")
;;  '(js-auto-format-command-args "--write --single-quote --no-semi"))
;; For clarity, it is configured here as follows:

;; Run upon Save
(defun seriott-js-save-hooks ()
  "Save hooks for js mode."
  (setq js-auto-format-command "prettier")
  (setq js-auto-format-command-args "--write --single-quote --no-semi")
  (setq-default js-indent-level 2)
  (add-hook 'after-init-hook (lambda()
                               (js-auto-format-mode)) t t)
  (add-hook 'before-save-hook (lambda()
                                (js-auto-format-execute)
                                (whitespace-cleanup)) t t))

(add-hook 'js-mode-hook #'seriott-js-save-hooks)

;; Dependency on cl-lib
;; (add-to-list 'load-path "~/.emacs.d/cl-lib")
;; (require 'cl-lib)

;; javascriptccs!
(provide 'javascriptccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; javascriptccs ends here
