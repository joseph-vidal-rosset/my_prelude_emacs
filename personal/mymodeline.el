;; Time-stamp: <2019-01-14 10:32:08 kmodi>

;; Customize the mode-line

;; Contents:
;;
;;  Date/Time
;;  Line and column numbers
;;  Buffer percentage
;;  Minibuffer-line
;;  Smart Mode-Line
;;    Rich Minority
;;  Organize the order of minor mode lighters


;;; Date/Time
					;
(defvar modi/show-date-time-in-mode-line t
  "If non-nil, show the date-time in the mode-line.
If nil, show the same in the minibuffer.")

;; Date, time, load average, mail in mode-line
(if modi/show-date-time-in-mode-line
    (setq display-time-format "Nancy, le %A %d %B %Y, %H:%M." )
  (setq display-time-format ""))

;; Do NOT show average system load time
(setq display-time-default-load-average nil)

(display-time-mode 1)

(setq global-mode-string (remove 'display-time-string global-mode-string))
(setq mode-line-end-spaces
      (list (propertize " " 'display '(space :align-to (- right 65)))
            'display-time-string))

;;; Line and column numbers
(line-number-mode 1)
(column-number-mode 1)


(provide 'setup-mode-line)

;; Variables used in display-time-format
;; http://docs.splunk.com/Documentation/Splunk/5.0.2/SearchReference/Commontimeformatvariables
;;
;; | %y | year in numbers (2-digit)                   |
;; | %Y | year in numbers (4-digit)                   |
;; | %m | month in number (eg: 12)                    |
;; | %B | full month name (eg: December)              |
;; | %b | short month name (eg: Dec)                  |
;; | %d | day in numbers, with leading zeros (eg: 08) |
;; | %e | day in numbers, no leading zeros (eg: 8)    |
;; | %A | full weekday name (eg: Sunday)              |
;; | %a | short weekday name (eg: Sun)                |
;; | %H | hours in 24-clock, with leading zeros       |
;; | %k | hours in 24-clock, no leading zeros         |
;; | %l | hours in 12-clock, with leading zeros       |
;; | %p | am/pm                                       |
;; | %T | time in 24-hour notation (%H:%M:%S)         |
