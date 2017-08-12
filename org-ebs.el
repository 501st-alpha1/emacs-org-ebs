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
