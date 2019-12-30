;;; package --- Summary:
;;; Customise `Clip-board' Copy - Paste- from- / to MAC OS
;; Package-Requires:
;;; Commentary:
;;; Code:

;; Reference:
;; [](https:gist.github.com/the-kenny/267162)

(unless (package-installed-p 'diredful)
  (package-install 'diredful))

(require 'diredful)

(defun copy-from-osx ()
  "Copy from osx."
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  "Copy `TEXT` to osx as `PUSH`."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))


;; check OS type
(cond ((string-equal system-type "windows-nt") ; Microsoft Windows
       (progn (message "Microsoft Windows")))
      ((string-equal system-type "gnu/linux") ; linux
       (progn (message "Linux")))
      ((string-equal system-type "berkeley-unix")
       (progn (message "FreeBSD")))
      ((string-equal system-type "darwin") ; Mac OS X
       (progn (message "Mac OS X")
              ;; ====================================
              ;; solve `ls does not support --dired;'
              ;; ====================================
              (setq dired-use-ls-dired nil)

              ;; ======
              ;; Integrate pbcopy
              ;; =====
              (setq interprogram-cut-function 'paste-to-osx)
              (setq interprogram-paste-function 'copy-from-osx))))

;; clipboardccs!
(provide 'clipboardccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; clipboardccs ends here
