;;; package --- Summary:
;;; Commentary:
;;; Code:

(unless (package-installed-p 'async)
  (package-install 'async))

(async-bytecomp-package-mode 1)

(require 'async-bytecomp)

(setq async-bytecomp-allowed-packages '(all))

(byte-recompile-directory "~/.emacs.d/mmccs/" 0)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/mmccs"))

(load "changelogccs"
      (require 'changelogccs))
(load "clipboardccs"
      (require 'clipboardccs))
(load "codingccs"
      (require 'codingccs))
(load "colorthemeccs"
      (require 'colorthemeccs))
(load "confunixccs"
      (require 'confunixccs))
(load "cursorccs"
      (require 'cursorccs))
(load "diredccs"
      (require 'diredccs))
(load "dockerfileccs"
      (require 'dockerfileccs))
(load "doomodelineccs"
      (require 'doomodelineccs))
(load "emacslispccs"
      (require 'emacslispccs))
(load "erlccs"
      (require 'erlccs))
(load "eshellccs"
      (require 'eshellccs))
(load "exccs"
      (require 'exccs))
(load "execpathccs"
      (require 'execpathccs))
(load "flycheckccs"
      (require 'flycheckccs))
(load "flyspellccs"
      (require 'flyspellccs))
(load "gitmodesccs"
      (require 'gitmodesccs))
(load "groovyccs"
      (require 'groovyccs))
(load "helmccs"
      (require 'helmccs))
(load "highlightchangesccs"
      (require 'highlightchangesccs))
(load "highlightparenthesesccs"
      (require 'highlightparenthesesccs))
(load "indentationccs"
      (require 'indentationccs))
(load "javascriptccs"
      (require 'javascriptccs))
(load "jenkinsfileccs"
      (require 'jenkinsfileccs))
(load "jsonccs"
      (require 'jsonccs))
(load "keybindingsccs"
      (require 'keybindingsccs))
(load "latexextraccs"
      (require 'latexextraccs))
(load "mdccs"
      (require 'mdccs))
(load "nlinumccs"
      (require 'nlinumccs))
(load "projectileccs"
      (require 'projectileccs))
(load "pyccs"
      (require 'pyccs))
(load "shccs"
      (require 'shccs))
(load "swiperccs"
      (require 'swiperccs))
(load "thememinimumccs"
      (require 'thememinimumccs))
(load "webfccs"
      (require 'webfccs))
(load "whitespaceccs"
      (require 'whitespaceccs))
(load "yamlccs"
      (require 'yamlccs))

(provide 'features)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; features ends here
