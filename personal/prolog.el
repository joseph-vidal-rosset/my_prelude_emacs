;; prolog-mode loader
;; Thierry.Martinez@inria.fr
;; under GPLv3

(add-to-list
 'load-path
 (or
  (and load-in-progress (file-name-directory load-file-name))
  (file-name-directory (buffer-file-name))))

(autoload 'prolog-mode "prolog-mode" "Major mode for editing Prolog code." t)

(defun swi-mode ()
  "Specific prolog mode with SWI Prolog by default"
  (interactive)
  (prolog-mode 'swi))

(defun gprolog-mode ()
  "Specific prolog with GNU Prolog by default"
  (interactive)
  (prolog-mode 'gprolog))

(defun jekejeke-mode ()
  "Specific prolog mode with Jekejeke Prolog by default"
  (interactive)
  (prolog-mode 'jekejeke))


(setq auto-mode-alist (append '(
                                ("\\.jb$" . jekejeke-mode)
                                ("\\.gpl$" . gprolog-mode)
                                ("\\.pl$" . swi-mode)
                                )
                              auto-mode-alist))
(provide 'prolog)
