;;; package --- Summary:
;;; Customise `json' mode
;; Package-Requires:
;;
;; `npm install -g js-beautify`
;;; Commentary:
;;
;; Utilises `web-beautify'
;;
;; Reference: [](https://github.com/yasuyk/web-beautify)
;;
;; You may use `web-beautify' for other mojor modes
;; Check `webfccs'
;; If you switch json mode hook to utilise `web-mode', then
;; turn off `web-beautify' before-hand
;;; Code:

(require 'package)

(add-to-list 'package-pinned-packages '(web-beautify . "melpa") t)

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'web-beautify)
  (package-install 'web-beautify))

;; ========================
;; `web-beautify' formatter
;; ========================
;;
;; `Install' the `dependency':
;; $ npm -g install js-beautify
;;
;; `Provide' necessary `Configurations':
;;
;; `Option-1'
;; add file `~/.jsbeautifyrc` with contents as follows:
;; {"indent-size": 2, "end_with_newline": true}
;;
;; `Option-2'
;; via environment variable. See the hook follows:
;;
;; `Other-Options':
;; Check: [](https://github.com/beautify-web/js-beautify#options)

(require 'web-beautify)

(defun halloween-web-beautify-save-hook ()
  "Hooks with web-beautify."
  (setenv "jsbeautify_indent_size" "2")
  (setenv "jsbeautify_end_with_newline" "true")
  (setq indent-tabs-mode nil)
  (add-hook 'before-save-hook (lambda()
                                (web-beautify-js-buffer)
                                (whitespace-cleanup)) t t))

(add-hook 'json-mode-hook #'halloween-web-beautify-save-hook)

(provide 'jsonccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; jsonccs ends here
