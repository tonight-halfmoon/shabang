;;; package --- Summary:
;;; Customise `Key-bindings'
;; Package-Requires:
;;; Commentary:
;;; Code:

(global-set-key (kbd "M-9") 'kill-whole-line)
(global-set-key (kbd "C-x p")
		(lambda()
		  (interactive)
		  (other-window -1)))

;; (when window-system ...)
;; Macos bind Meta with key 'cmd'
;; (setq-default mac-option-modifier 'alt)
;; (setq-default mac-command-modifier 'meta)
;; (setq select-enable-clipboard t)

;; keybindingsccs!
(provide 'keybindingsccs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; keybindingsccs ends here
