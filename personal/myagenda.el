;;; org-agendal.el --- agenda with org-mode          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Joseph Vidal-Rosset

;; Author: Joseph Vidal-Rosset <joseph.vidal.rosset@gmail.com>
;; Keywords: calendar

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

;; in this file my org-agenda setup

;;; Code:

(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-set-date-style 'european)))

(setq calendar-week-start-day 1
      calendar-day-name-array ["Lundi" "Mardi" "Mercredi"
                               "Jeudi" "Vendredi" "Samedi" "Dimanche"]
      calendar-month-name-array ["Janvier" "Février" "Mars" "Avril" "Mai"
                                 "Juin" "Juillet" "Août" "Septembre"
                                 "Octobre" "Novembre" "Décembre"])

;; keybindings


;;file to save todo items
(setq org-agenda-files (quote ("~/Dropbox/Orgzly/todo.org" "~/Dropbox/Orgzly/notes.org" "~/Dropbox/Orgzly/links.org" "~/Dropbox/Orgzly/drafts.org" )))

(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file "~/Dropbox/Orgzly/todo.org")))

(set-register ?o (cons 'file "~/Dropbox/Orgzly/todo.org"))

(global-set-key (kbd "C-c n")
                (lambda () (interactive) (find-file "~/Dropbox/Orgzly/notes.org")))

(global-set-key (kbd "C-c d")
                (lambda () (interactive) (find-file "~/Dropbox/Orgzly/drafts.org")))

(set-register ?n (cons 'file "~/Dropbox/Orgzly/notes.org"))

(ido-mode)
(setq org-completion-use-ido t)

;;set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;;open agenda in current window
(setq inhibit-splash-screen t)
(setq org-agenda-window-setup (quote current-window))

(setq org-agenda-custom-commands
      '(("t" "Agenda and Email-links tasks"
         ((agenda "")
          (tags-todo "email")
          (tags "link")))
        ))

; (global-set-key (kbd "<f11> c") 'org-capture)
;;capture todo items using C-c c t
;(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(
	("t" "todo" entry (file+headline "~/Dropbox/Orgzly/todo.org" "Tasks")
         "* TODO [#A] %?\n [[~/Dropbox/Orgzly/links.org::%(org-insert-time-stamp (org-read-date nil t \"%:date\"))]] \n* %(org-insert-time-stamp (org-read-date nil t \"%:date\")) %a "
	 )
	("n"      ; key
	 "note"   ;name
	 entry    ;type
	 (file+headline "~/Dropbox/Orgzly/notes.org" "Notes")  ;target
         "* NOTE du %(org-insert-time-stamp (org-read-date nil t \"%:date\")) \n Voir %a " ; template
	 )
	("H" "HOWTO: C-c C-s : schedule ; C-c C-d : deadline ; C-c C-w : org-refile at a point; last: C-c C-t : DONE - F11-a: org-archive"
	  entry (file+headline "" "") ""
	 )
	)
      )
;; Merci à  Bob Newell , avec ce code, le lien de l'email est modifié correctement dès l'usage de la fonction refile C-c C-w
 (defun jr/fix-the-link (&rest args)
 (interactive)
 (save-excursion
  (find-file "~/Dropbox/Orgzly/todo.org")
   (goto-char (point-min))
   (while (search-forward "gnus:INBOX#" nil t)
   (replace-match "gnus:Archive#"))
   (save-buffer)))
(advice-add 'org-capture-finalize :after #'jr/fix-the-link)


;; si cela échoue, cette fonction peut prendre le relai.

(defun hs/replace ()
   (interactive)
   (goto-char 1)
   (let ((search-invisible t)) (replace-string "gnus:INBOX#" "gnus:Archive#")))
(define-key global-map (kbd "C-c r") 'hs/replace)
(add-hook 'org-capture-prepare-finalize-hook 'hs/replace)

;; refiling
;;refile  https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
;;refile https://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

;(setq org-refile-targets (quote (("todo.org" :maxlevel . 6)
 ;                               ("links.org" :level . 2)
					;                               )))
(ido-mode)
(setq org-completion-use-ido t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode agenda options                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(org-agenda nil "a")
;;open agenda in current window
;(setq org-agenda-window-setup (quote current-window))
;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))
;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;don't give awarning colour to tasks with impending deadlines
;;if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;;don't show tasks that are scheduled or have deadlines in the
;;normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))
;;sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
  (quote
   ((agenda deadline-up priority-down)
    (todo priority-down category-keep)
    (tags priority-down category-keep)
    (search category-keep))))

(defun my/org-agenda-goto-heading-in-indirect-buffer (&optional switch-to)
    "Go to the current agenda headline in an indirect buffer. If SWITCH-TO is non-nil, close the org-agenda window."
    (interactive)
    (if switch-to
        (org-agenda-switch-to)
      (org-agenda-goto))
    (org-tree-to-indirect-buffer)

    ;; Put the non-indirect buffer at the bottom of the prev-buffers
    ;; list so it won't be selected when the indirect buffer is killed
    (set-window-prev-buffers nil (append (cdr (window-prev-buffers))
                                         (car (window-prev-buffers)))))

  (defun my/org-agenda-switch-to-heading-in-indirect-buffer ()
    (interactive)
    (my/org-agenda-goto-heading-in-indirect-buffer t))

(defun my/org-rename-tree-to-indirect-buffer (&rest args)
  "Rename the new buffer to the current org heading after using org-tree-to-indirect-buffer."
  (with-current-buffer (car (buffer-list (car (frame-list))))
    (save-excursion
      (let* ((heading (nth 4 (org-heading-components)))
             (name (if (string-match org-bracket-link-regexp heading)
                       ;; Heading is an org link; use link name
                       (match-string 3 heading)
                     ;; Not a link; use whole heading
                     heading)))
        (rename-buffer name) t))))
(advice-add 'org-tree-to-indirect-buffer :after 'my/org-rename-tree-to-indirect-buffer)

(provide 'org-agenda)
;;; org-agendal.el ends here
