;;; package --- Summary:
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/mmccs")

(load "whitespaceccs"
      (require 'whitespaceccs))

(load "clipboardccs"
      (require 'clipboardccs))

(load "colorthemeccs"
      (require 'colorthemeccs))

(load "projectileccs"
      (require 'projectileccs))

(load "keybindingsccs"
      (require 'keybindingsccs))

(load "flycheckccs"
      (require 'flycheckccs))

(load "flyspellccs"
      (require 'flyspellccs))

(load "highlightchangesccs"
      (require 'highlightchangesccs))

(load "indentationccs"
      (require 'indentationccs))

(load "highlightparenthesesccs"
      (require 'highlightparenthesesccs))

(load "cursorccs"
      (require 'cursorccs))

(load "aggressiveindentccs"
      (require 'aggressiveindentccs))

(load "swiperccs"
      (require 'swiperccs))

(load "smartmodelineccs"
      (require 'smartmodelineccs))

;; ------------------------------------
;; `Major-Mode-Specific-Configurations'
;; ------------------------------------

(load "emacslispccs"
      (require 'emacslispccs))

(load "exccs"
      (require 'exccs))

(load "erlccs"
      (require 'erlccs))

(load "shccs"
      (require 'shccs))

(load "mdccs"
      (require 'mdccs))

(load "confunixccs"
      (require 'confunixccs))

(load "groovyccs"
      (require 'groovyccs))

(load "eshellccs"
      (require 'eshellccs))

(load "dockerfileccs"
      (require 'dockerfileccs))

(load "jenkinsfileccs"
      (require 'jenkinsfileccs))

(load "yamlccs"
      (require 'yamlccs))

(load "changelogccs"
      (require 'changelogccs))

(load "javascriptccs"
      (require 'javascriptccs))

(load "webfccs"
      (require 'webfccs))

(load "latexextraccs"
      (require 'latexextraccs))

(load "gitmodesccs"
      (require 'gitmodesccs))

(provide 'features)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; features ends here
