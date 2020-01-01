;;; package --- Summary:
;; Customise `coding'
;;; Commentary:
;;; Code:

(setq file-coding-system-alist (quote (("\\.json\\'" . utf-8)
                                       ("\\.conf\\'" . utf-8)
                                       ("\\.tzst\\'" no-conversion . no-conversion)
                                       ("\\.zst\\'" no-conversion . no-conversion)
                                       ("\\.dz\\'" no-conversion . no-conversion)
                                       ("\\.txz\\'" no-conversion . no-conversion)
                                       ("\\.xz\\'" no-conversion . no-conversion)
                                       ("\\.lzma\\'" no-conversion . no-conversion)
                                       ("\\.lz\\'" no-conversion . no-conversion)
                                       ("\\.g?z\\'" no-conversion . no-conversion)
                                       ("\\.\\(?:tgz\\|svgz\\|sifz\\)\\'" no-conversion .
                                        no-conversion)
                                       ("\\.tbz2?\\'" no-conversion . no-conversion)
                                       ("\\.bz2\\'" no-conversion . no-conversion)
                                       ("\\.Z\\'" no-conversion . no-conversion)
                                       ("\\.elc\\'" . utf-8-emacs)
                                       ("\\.el\\'" . prefer-utf-8)
                                       ("\\.utf\\(-8\\)?\\'" . utf-8)
                                       ("\\.xml\\'" . xml-find-file-coding-system)
                                       ("\\(\\`\\|/\\)loaddefs.el\\'" raw-text . raw-text-unix)
                                       ("\\.tar\\'" no-conversion . no-conversion)
                                       ("\\.po[tx]?\\'\\|\\.po\\." . po-find-file-coding-system)
                                       ("\\.\\(tex\\|ltx\\|dtx\\|drv\\)\\'" .
                                        latexenc-find-file-coding-system)
                                       ("" undecided))))

(provide 'codingccs)

;; Local Variables:
;; coding: utf-8
;; buffer-file-coding-system: utf-8
;; indent-tabs-mode: nil
;; End:
;;; codingccs ends here
