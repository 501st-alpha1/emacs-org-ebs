;; Evidence-Based Scheduling for Emacs Org Mode
;; Copyright (C) 2017 Scott Weldon
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;; Evidence-Based Scheduling:
;; 1. Estimate time for tasks (as granular as possible).
;;    Here using Org-Mode `Effort` property.
;; 2. Track actual time completed.
;;    Here using Org-Mode timeclocks.
;; 3. Calculate velocity (estimated time / actual time).
;;    To save calculation time, this is stored to Org-Mode `Velocity` property.
;; 4. Predict odds based on velocity.
;;    For a given estimate, divide by random velocities and aggregate results
;;    to get percent chance task will be completed in a given timeframe.
;; 5. Profit.
;;
;; More info:
;; https://www.joelonsoftware.com/2007/10/26/evidence-based-scheduling/

(defvar org-ebs-files '())

;; https://stackoverflow.com/a/24357106/2747593
(defun org-ebs-append-to-list(list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(defun org-ebs-get-all-velocities()
  (unless org-ebs-files
    (user-error "Error: No org-ebs-files defined."))
  (let ((all-velocities '()))
    (dolist (file org-ebs-files all-velocities)
      (org-ebs-append-to-list 'all-velocities
                              (org-ebs-get-all-velocities-in-file file)))))

;; FIXME: Broken when all headings are not expanded.
;; TODO: Track date, weight values by most recent.
(defun org-ebs-get-all-velocities-in-file(file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((velocities '()))
      (while (search-forward ":Velocity:" nil t)
        (let ((current-headline (save-excursion (org-back-to-heading))))
          (push (string-to-number
                 (org-entry-get current-headline "Velocity")) velocities)))
      velocities)))

(defun org-ebs-get-full-brackets(estimate)
  "For given ESTIMATE, calculate odds of completion in given timeframe.  Return list of numbers representing odds work will be completed at or before each hour.  E.g., returned value of:

(23 45 80 100)

means that there is a 23% chance of completion between 0 and 1 hours, 45 percent chance under 2 hours, 80 percent chance under 3 hours, and 100 percent chance under 4 hours."
  (interactive "nEnter time estimate (minutes): ")
  (let* ((velocities (org-ebs-get-all-velocities))
         (RANDOM-TIMES 200)
         (pct-per-time (/ 100.0 RANDOM-TIMES))
         (brackets '())
         (full-brackets '())
         (sum 0)
         (max-key 0)
         (estimates (org-ebs-get-random-adjusted-estimates estimate velocities
                                                           RANDOM-TIMES)))
    (dolist (element estimates)
      (let ((bracket (floor (/ element 60))))
        (add-to-list 'brackets `(,bracket . 0) t 'org-ebs-key-used-p)
        (let ((old-val (cdr (assoc bracket brackets))))
          (setcdr (assoc bracket brackets) (+ old-val pct-per-time)))
        (when (< max-key bracket)
          (setq max-key bracket))))
    (dotimes (i (+ max-key 1))
      (let ((element (assoc i brackets)))
        (setq sum (+ sum (if element (cdr element) 0)))
        (push sum full-brackets)))
    (setq full-brackets (reverse full-brackets))))

(defun org-ebs-get-hours-for-odds(estimate percent)
  (interactive
   "nEnter time estimate (minutes): \nnEnter desired percentage (0-100): ")
  (let* ((full-brackets (org-ebs-get-full-brackets estimate))
         (match
          (catch 'break
            (dotimes (i (length full-brackets))
              (let* ((element (nth i full-brackets)))
                (when (> element percent)
                  (throw 'break i)))))))
    (message "Work has %s%% odds of completion in under %s hours."
             percent (+ match 1))))

(defun org-ebs-get-random-adjusted-estimates(estimate velocities
                                                      &optional random-times)
  "Divide ESTIMATE by random elements from VELOCITIES, and return results in sorted list.  Divide RANDOM-TIMES times, defaulting to 100."
  (let ((random-times (or random-times 100))
        (len (length velocities))
        (estimates '()))
    (dotimes (i RANDOM-TIMES)
      (push (round (/ estimate (nth (random len) velocities))) estimates))
    (setq estimates (sort estimates '<))))

;; https://stackoverflow.com/a/25100962/2747593
(defun org-ebs-key-used-p(elt1 elt2)
  "Helper function for add-to-list: returns non-nil if key is
already in use in an association list."
  (eq (car elt1) (car elt2)))

(defun org-ebs-set-velocity()
  "Compare the estimated Effort for the current task to the time clocked, calculate the Velocity (effort / actual), and save that value to `Velocity` property."
  (let* ((current-headline (or (and (org-at-heading-p)
                                    (point))
                               (save-excursion (org-back-to-heading))))
         (effort-prop (org-entry-get current-headline "Effort"))
         (effort (org-duration-string-to-minutes
                  (if effort-prop effort-prop 0)))
         (actual (org-clock-sum-current-item))
         (velocity (/ effort actual)))
    (cond ((= actual 0)
           (message "No time clocked, skipping velocity calculation."))
          ((= effort 0)
           (message "No time estimate, skipping velocity calculation."))
          (t
           (org-entry-put current-headline "Velocity"
                          (number-to-string velocity))))))

(defun org-ebs-set-velocity-when-done()
  "Calculate and set velocity using `org-ebs-set-velocity` for the current task if the state has just changed to DONE or equivalent."
  (when (catch 'break
          (dolist (element org-done-keywords)
            (when (string= (nth 2 (org-heading-components)) element)
              (throw 'break t))))
    (org-ebs-set-velocity)))

(add-hook 'org-after-todo-state-change-hook 'my-org-calc-velocity-when-done)

(provide 'org-ebs)
