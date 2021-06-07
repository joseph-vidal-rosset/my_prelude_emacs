;;; latex.el --- latex-setup-for-emacs               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Joseph Vidal-Rosset

;; Author: Joseph Vidal-Rosset <joseph.vidal.rosset@gmail.com>
;; Keywords: latex , bitex


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;CDLaTeX à ne *pas* parametrer plus automatiquement que ça :
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
;;;;;; LaTeX  Auctex et Reftex
				;	(setenv "PATH" (concat "/usr/local/texlive/2018/bin/x86_64-linux/:" (getenv "PATH")))
				;	(setenv "PATH" (concat "/usr/texbin:/usr/local/bin/:" (getenv "PATH")))
					;(setq exec-path (append '("/usr/texbin" "/usr/local/bin/:") exec-path))
					;(load "auctex.el" nil t t)
					;(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
					; (setq-default TeX-master nil) Attention : si on décommente, on n'a plus le menu dans org  !
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;; pour avoir la compilation en pdf automatiquement
(setq TeX-PDF-mode t)
;; on choisit evince pour voir les pdf

(setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
(setq TeX-view-program-selection '((output-pdf "Evince")))

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(setq TeX-source-correlate-start-server t)

(setq TeX-output-view-style
      (quote
       ("^dvi$" "." "evince -f %o"
        ("^pdf$" "." "evince -f %o")
        ("^html?$" "." "firefox %o"))))

(setq TeX-view-program-selection
      '((output-dvi "DVI Viewer")
        (output-pdf "PDF Viewer")
        (output-html "firefox")))
(setq TeX-view-program-list
      '(("DVI Viewer" "evince %o")
        ("PDF Viewer" "evince %o")
        ("Google Chrome" "firefox %o")))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(require 'reftex)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
;; pour avoir la compilation en pdf automatiquement
(setq TeX-PDF-mode t)

;;; RefTeX
(autoload 'reftex-mode               "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex            "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-add-to-label-alist "reftex" "RefTeX Minor Mode" nil)
;;;
;;; To turn RefTeX Minor Mode on and off in a particular buffer, use
;;; `M-x reftex-mode'.
;;;
;;; To turn on RefTeX Minor Mode for all LaTeX files,
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

;;; Replace AUCTeX functions
(setq reftex-plug-into-AUCTeX t)

;; set up reftex. BIBINPUTS is set in bash profile _Merci Scott!
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-plug-into-AUCTeX t)

					;(autoload 'refer-find-entry "refer" nil t)
;; default bibliography.bib
					;(setq reftex-default-bibliography
					;     (quote
					; ("$HOME/Dropbox/Bibliographies/jvr-biblatex-references")))       
					;(setq refer-bib-directory "$HOME/Dropbox/Bibliographies/jvr-biblatex-references.bib")
;;(setq reftex-default-bibliography '("~/Dropbox/Negation/VRIN/jvr-biblatex-references.bib"))

(setq-default TeX-master t)
(setq reftex-default-bibliography
      (quote
       ("/home/joseph/Dropbox/Orgzly/reforg.bib")))

;(add-to-list 'exec-path "/usr/bin/bibtex2html")

;;choix entre bibtex et biber : IMPORTANT !
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber") t))

(defun TeX-run-Biber (name command file)
  "Create a process for NAME using COMMAND to format FILE with Biber."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function 'TeX-Biber-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-Biber-sentinel (process name)
  "Cleanup TeX output buffer after running Biber."
  (goto-char (point-max))
  (cond
   ;; Check whether Biber reports any warnings or errors.
   ((re-search-backward (concat
                         "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
                         "\\(warnings?\\|error messages?\\))") nil t)
    ;; Tell the user their number so that she sees whether the
    ;; situation is getting better or worse.
    (message (concat "Biber finished with %s %s. "
                     "Type `%s' to display output.")
             (match-string 1) (match-string 2)
             (substitute-command-keys
              "\\\\[TeX-recenter-output-buffer]")))
   (t
    (message (concat "Biber finished successfully. "
                     "Run LaTeX again to get citations right."))))
  (setq TeX-command-next TeX-command-default))
;;
(setq TeX-view-program-selection
      '((output-dvi "DVI Viewer")
        (output-pdf "PDF Viewer")
        (output-html "Google Chrome")))
(setq TeX-view-program-list
      '(("DVI Viewer" "evince %o")
        ("PDF Viewer" "evince %o")
        ("Google Chrome" "firefox %o")))
;;CDLaTeX à ne *pas* parametrer plus automatiquement que ça :
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
;; compter les mots
(defun my-latex-setup ()
  (defun latex-word-count ()
    (interactive)
    (let* ((this-file (buffer-file-name))
           (word-count
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process "texcount" nil t nil "-brief" this-file)))))
      (string-match "\n$" word-count)
      (message (replace-match "" nil nil word-count))))
  (define-key LaTeX-mode-map "\C-cw" 'latex-word-count))
(add-hook 'LaTeX-mode-hook 'my-latex-setup t)

(provide 'latex)
;;; latex.el ends here
