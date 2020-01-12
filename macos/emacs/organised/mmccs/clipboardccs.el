;;; package --- Summary:
;;; Customise `Clip-board' Copy - Paste- from- / to MAC OS
;; Package-Requires:
;;; Commentary:
;; Reference:
;; [](https:gist.github.com/the-kenny/267162)
;;; Code:

(defun copy-from-osx ()
  "Copy from osx."
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  "Copy `TEXT` to osx as `PUSH`."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))


(cond ((string-equal system-type "berkeley-unix")
       (progn (message "FreeBSD")))
      ((string-equal system-type "gnu/linux") ; linux
       (progn (message "Linux")))
      ((string-equal system-type "darwin") ; Mac OS X
       (progn (message "Mac OS X")
              ;; ====================================
              ;; solve `ls does not support --dired;'
              ;; ====================================
              (defvar dired-use-ls-dired)
              (setq dired-use-ls-dired nil)

              ;; ======
              ;; Integrate pbcopy
              ;; =====
              (setq interprogram-cut-function 'paste-to-osx)
              (setq interprogram-paste-function 'copy-from-osx))))

(provide 'clipboardccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; clipboardccs ends here
