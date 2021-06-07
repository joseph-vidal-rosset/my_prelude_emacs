;;; pdf.el --- pdf files in emacs                    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Joseph Vidal-Rosset

;; Author: Joseph Vidal-Rosset <joseph.vidal.rosset@gmail.com>
;; Keywords: convenience

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



;;; Code:

(require 'pdf-tools)

;; (require 'use-package-el-get)
;; (use-package-el-get-setup)

(use-package pdf-tools
 ;;:pin manual ;; manually update
 :config
 ;; initialise
(pdf-tools-install)
 ;; open pdfs scaled to fit page
(setq-default pdf-view-display-size 'fit-page)
 ;; automatically annotate highlights
(setq pdf-annot-activate-created-annotations t)
 ;; use normal isearch
(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
 ;; turn off cua so copy works
(add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
 ;; more fine-grained zooming
(setq pdf-view-resize-factor 1.1)
 ;; keyboard shortcuts
(define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
(define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
(define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete))

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
TeX-source-correlate-start-server t)

(add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
	  #'TeX-revert-document-buffer)


(require 'pdf-tools-extension)
(eval-after-load 'org '(require 'org-pdfview))

(add-to-list 'org-file-apps 
             '("\\.pdf\\'" . (lambda (file link)
                               (org-pdfview-open link))))

(setq pdf-cache-image-limit 15)
(setq pdf-cache-prefetch-delay 2)
(setq image-cache-eviction-delay 15)

(require 'vlf-setup)

(provide 'pdf)
;;; pdf.el ends here
