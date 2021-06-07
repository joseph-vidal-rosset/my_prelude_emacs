;;; shortcuts.el --- key binding section             -*- lexical-binding: t; -*-

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

;;; Commentary:

;;

;;; Code:
(global-set-key [f1] 'package-list-packages)
(global-set-key [f2] 'gnus)
(global-set-key [f3] 'org-latex-preview)
(global-set-key [f4] 'org-mime-htmlize)
(global-set-key [f5] 'kill-current-buffer)
(global-set-key [f6] 'message-mode)
(global-set-key [f7] 'org-mode)
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'org-capture)
(global-set-key [f10] 'org-schedule)
(global-set-key [f11] 'org-deadline)
(global-set-key [f12] 'org-export-head)

(global-set-key (kbd "<f11>") nil) ;; Remove the old keybinding
;; (global-set-key (kbd "<f11> c") 'org-capture)
					;(global-set-key (kbd "<f11> r") 'org-refile)
(global-set-key (kbd "<f11> a") 'org-archive-subtree)
					;(global-set-key (kbd "<f11> e") 'org-export-head)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; C-c C-s  org-schedule
;; C-c C-d  org-deadline
;; C-c C-w  org-refile at a point
;; C-c C-t  org : done

;; snippets
(setq yas-snippet-dirs
      '( "~/.emacs.d/snippets/"           ;; foo-mode and bar-mode snippet collection
	 ;; "~/.emacs.d/snippets/orgmode"
        ))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.


(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet"
	     "~/.emacs.d/snippets/mysnippets")
(require 'yasnippet)
(yas-global-mode 1)



(provide 'shortcuts)
;;; shortcuts.el ends here
