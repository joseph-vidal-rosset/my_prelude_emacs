;;; dashboard.el --- dashboard for emacs             -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'dashboard)
(dashboard-setup-startup-hook)
;; Or if you use use-package


(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Set the title
(setq dashboard-banner-logo-title "Emacs shows you the way!")
;; Set the banner
(setq dashboard-startup-banner  "~/.emacs.d/stanemacs-finger.png")

;(setq dashboard-startup-banner 'official)

;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.png" which displays whatever image you would prefer

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts t)
(add-to-list 'recentf-exclude "\\.recentf\\'")
(setq dashboard-items '((recents  . 4)
                        (bookmarks . 5)
                        ;(projects . 3)
                        (agenda . 3)
                        ;; (registers . 3)
                        ))
;(add-to-list 'recentf-exclude "\\.recentf\\'")
;(add-to-list 'recentf-exclude "~/News")
;(add-to-list 'recentf-exclude "\\.\\'")
;(add-to-list 'recentf-exclude "synctex.gz")
;(add-to-list 'recentf-exclude ".pdf")  ;; no .pdf files in recent list
;(add-to-list 'recentf-exclude ".html")
;(add-to-list 'recentf-exclude ".log")  ;; no .log files in recent list
;(add-to-list 'recentf-exclude ".sty")  ;; no .log files in recent list
;(add-to-list 'recentf-exclude ".aux")  ;; no .log files in recent list
;(add-to-list 'recentf-exclude ".png")  ;; no .log files in recent list
;(add-to-list 'recentf-exclude ".eps")  ;; no .log files in recent list
;(add-to-list 'recentf-exclude ".rel")  ;; no .log files in recent list
;(add-to-list 'recentf-exclude ".nav")  ;; no .log files in recent list
;(add-to-list 'recentf-exclude ".bbl")  ;; no .log files in recent list
;(add-to-list 'recentf-exclude ".blg")  ;; no .log files in recent list
;(add-to-list 'recentf-exclude ".snm")  ;; no .log files in recent list
;(add-to-list 'recentf-exclude ".out")  ;; no .log files in recent list
;(add-to-list 'recentf-exclude ".dvi")  ;; no .log files in recent list
;(add-to-list 'recentf-exclude "RMAIL")  ;; no .log files in recent list
;;To add icons to the widget headings and their items:

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

;; To show info about the packages loaded and the init time:

(setq dashboard-set-init-info t)

;; Also, the message can be customized like this:

;(setq dashboard-init-info "Here we go!")

;; To display today’s agenda items on the dashboard, add agenda to dashboard-items:

;; (add-to-list 'dashboard-items '(agenda) t)

;; To show agenda for the upcoming seven days set the variable show-week-agenda-p to t.

(setq show-week-agenda-p t)

;(setq dashboard-org-agenda-categories '("Tasks" "Links"))

;; A randomly selected footnote will be displayed. To disable it:

;; (setq dashboard-set-footer nil)


(provide 'dashboard)
;;; dashboard.el ends here
