;;; package --- Summary:
;;; Customise `Web' mode
;; Package-Requires:
;;; Commentary:
;;; Code:

;; Reference [](http://web-mode.org/)
;; Download it as follows
;; $ wget https://raw.githubusercontent.com/fxbois/web-mode/master/web-mode.el
(add-to-list 'load-path "~/.emacs.d/web-mode")
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex?\\'" . web-mode))

;; Configure engine(s) when file extension is too general or unknown
(setq web-mode-engines-alist '(("php"    . "\\.phtml\\'")
                               ("blade"  . "\\.blade\\.")))

;; Configure
(add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/some/react/path/.*\\.js[x]?\\'" . web-mode))

;; associate a file path with content type
(setq web-mode-content-types-alist '(("json" . "/some/path/.*\\.api\\'")
                                     ("xml"  . "/other/path/.*\\.api\\'")
                                     ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))

;; (setq-default web-mode-indent-style 2)
(defun seriott-web-mode-hooks ()
  "Hooks for Web mode."
  (when (eq major-mode 'web-mode)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-script-padding 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-block-padding 4)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-current-column-highlight t)
    (add-hook 'local-write-file-hooks (lambda ()
                                        (whitespace-cleanup)
                                        (indent-according-to-mode)
                                        (indent-region (point-min)
                                                       (point-max) nil)
                                        (delete-trailing-whitespace) nil))))
(add-hook 'web-mode-hook 'seriott-web-mode-hooks)

;; ====================================================================
;; `web-beautify' formatter  when web mode
;; ====================================================================
;; (add-to-list 'load-path "~/.emacs.d/web-format")
;; (require 'web-beautify) ;; Dependency: $ npm -g install js-beautify;; add file `~/.jsbeautifyrc` with contents {"indent-size": 2, "end_with_newline": true}
;; Reference: [](https://github.com/yasuyk/web-beautify)

;; (defun seriott-web-beautify-save-hooks ()
;; "Save hooks with web-beautify for web mode."
;;  ;; (!) Becarefull! Add one and only one for the same major mode.
;; (web-beautify-html-buffer))
;; (add-hook 'web-mode-hook (lambda()
;;                            (add-hook 'before-save-hook 'seriott-web-beautify-save-hooks t t)))

;; Dependency: $ npm install -g js-beautify
;; Passing formatting configurations:
;; `Option-1'
;; add file `~/.jsbeautifyrc` with contents {"indent-size": 2, "end_with_newline": true}
;; `Option-2'
;; via environment variable
;; Other options, check: [](https://github.com/beautify-web/js-beautify#options)
;; Reference: [](https://github.com/yasuyk/web-beautify)
(defun halloween-web-beautify-save-hook ()
  "Hooks with web-beautify."
  ;; (!) Be careful!! Add one and only one for the same major mode!
  (setenv "jsbeautify_indent_size" "2")
  (setenv "jsbeautify_end_with_newline" "true")
  (setq indent-tabs-mode nil)
  (add-hook 'before-save-hook (lambda()
                                (web-beautify-js-buffer)
                                (whitespace-cleanup))))
(add-hook 'json-mode-hook 'halloween-web-beautify-save-hook)

;; webfccs!
(provide 'webfccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; webfccs ends here
