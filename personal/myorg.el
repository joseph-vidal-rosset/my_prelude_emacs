;;; org.el ---                    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Joseph Vidal-Rosset

;; Author: Joseph Vidal-Rosset <joseph.vidal.rosset@gmail.com>
;; Keywords:

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

;;;
(require 'org)
(require 'org-capture)
(setq org-use-speed-commands t)
;; resize images
(setq org-image-actual-width nil)
;;LaTeX export for Org-Mode
(require 'ox-latex)
(setq org-src-fontify-natively t)
(setq org-highlight-latex-and-related '(latex))
(setq org-highlight-latex-and-related '(latex script entities))
(setq org-latex-listings 'minted)
(require 'ob-latex)
(require 'org-auctex-keys)
(require 'org-mime)
(setq org-mime-library 'mml)
;; from Kitchin
(require 'org-install)
;; fontify code in code blocks
(require 'ob-latex)
(setq org-src-fontify-natively t)
(require 'ox-html)
(require 'ox-ascii)
(setq org-src-fontify-natively t)
(require 'org-protocol)
(require 'ox-beamer)
;(beacon-mode 1)
(require 'org-context)
(org-context-activate)
(setq org-ref-show-broken-links nil)
(require 'outorg)
(require 'ox-word)
(require 'org-mouse)
; (with-eval-after-load 'ox
					; (require ' ox-pandoc))
 ;
;
;
  ;; Add minted to the defaults packages to include when exporting.
  (add-to-list 'org-latex-packages-alist '("" "minted"))
;; Tell the latex export to use the minted package for source
;; code coloration.
					;(setq org-latex-listings 'minted)

;; Add minted to the defaults packages to include when exporting.
					;	(add-to-list 'org-latex-packages-alist '("" "minted"))
;; Tell the latex export to use the minted package for source
;; code coloration.
					;	(setq org-latex-listings 'minted)
;; Let the exporter use the -shell-escape option to let latex
;; execute external programs.
;; This obviously and can be dangerous to activate!
					; (setq org-latex-pdf-process
;;     '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")



(provide 'org)
;;; org.el ends here
