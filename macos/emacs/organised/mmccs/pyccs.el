;;; package --- Summary:
;; Customise `Python' Mode
;; Package-Requires:
;; 1. Pylint
;; 2. MyPy
;;
;; On FreeBSD
;; `cd /usr/ports/devel/pylint && make install clean && cd`
;; `cd /usr/ports/devel/py-mypy && make install clean && cd`
;;
;; On MacOS
;; `brew cleanup && brew update && brew upgrade && brew cleanup && brew doctor`
;; `brew install pylint`
;; `brew install mypy`
;;; Commentary:
;;; Code:

(require 'package)

(add-to-list 'package-pinned-packages '(python-mode . "melpa") t)
(add-to-list 'package-pinned-packages '(flycheck-pycheckers . "melpa") t)

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'python-mode)
  (package-install 'python-mode))

(unless (package-installed-p 'flycheck-pycheckers)
  (package-install 'flycheck-pycheckers))

(require 'python)

(defun ser-python-mode-hooks ()
  "Hooks for python mode."
  (setq python-indent-offset 2)
  (setq python-indent-guess-indent-offset 2)
  (add-hook 'before-save-hook (lambda()
                                (indent-according-to-mode)
                                (whitespace-cleanup)) t t))

(add-hook 'python-mode-hook #'ser-python-mode-hooks)

(with-eval-after-load 'flycheck (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

;; `Elpy'
;; TODO:
;; Reference [](https://elpy.readthedocs.io/en/latest/introduction.html\#installation)
;; (elpy-enable)

(provide 'pyccs)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; indent-tabs-mode: nil
;; End:
;;; pyccs ends here
