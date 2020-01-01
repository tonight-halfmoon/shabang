;;; package --- Summary:
;;; Customise `JavaScript' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

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

(setq js-auto-format-command "prettier")
(setq js-auto-format-command-args "--write --single-quote --no-semi")

(setq-default js-indent-level 2)
;; The following line is conflicting with the save hook
;; (add-hook 'js-mood-hook #'js-auto-format-mode)

(defun ser-js-af-hooks ()
  "Hooks for js mode with js auto-format."
  (when (eq major-mode 'js-mode)
    (js-auto-format-mode)))
(add-hook 'after-init-hook (lambda()
                             (ser-js-af-hooks)) t t)
;; Run upon Save
(defun seriott-js-mode-save-hook ()
  "Save hooks for js mode."
  (when (eq major-mode 'js-mode)
    (whitespace-cleanup)
    (delete-trailing-whitespace)
    (js-auto-format-execute)))
(add-hook 'before-save-hook #'seriott-js-mode-save-hook t t)

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
