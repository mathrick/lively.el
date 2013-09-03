;;; lively.el --- Interactively updating text

;;; Copyright 2009 Luke Gorrie <luke@bup.co.nz>

;; Author: Luke Gorrie <luke@bup.co.nz>
;; Modified by: Darius Bacon <darius@wry.me>, Maciej Katafiasz <mathrick@gmail.com>
;; Version: 0.3

;;; Go to the end of any of the following lines and run `M-x lively'
;;;   Current time:      (current-time-string)
;;;   Last command:      last-command
;;;   Open buffers:      (length (buffer-list))
;;;   Unix processes:    (lively-shell-command "ps -a | wc -l")
;;;
;;; then the code will be replaced by its formatted result -- and
;;; periodically updated. You can create little dashboards.
;;; Use `M-x lively-stop' to restore order.
;;;
;;; Based on the Squeak hack by Scott Wallace.

(require 'cl)

(defvar lively-overlays nil "List of active overlays representing lively text.")
(defvar lively-frozen-overlays nil "List of lively overlays currently not updating.")
(defvar lively-recent-overlays nil "List of lively overlays last known to contain point.")
(make-variable-buffer-local 'lively-recent-overlays)

(defvar lively-timer    nil "Idle timer for updating lively text.")
(defvar lively-interval 0.25 "Idle time before lively text update in seconds.")

;;;###autoload
(defun lively ()
 "Make the expression before point lively."
 (interactive)
 (lively-region (save-excursion (backward-sexp) (point)) (point)))

;;;###autoload
(defun lively-region (start end)
 "Make the region lively."
  (interactive "r")
  (when (null lively-timer)
    (lively-init-timer))
  (add-to-list 'pre-command-hook #'lively-pre-command-hook)
  (add-to-list 'post-command-hook #'lively-post-command-hook)
  (push (make-overlay start end) lively-overlays)
  (overlay-put (first lively-overlays) 'buffer (current-buffer)))

(defun lively-update ()
  "Update the display of all visible lively text."
 (interactive)
 (dolist (o lively-overlays)
   (when (get-buffer-window (overlay-buffer o))
      (condition-case err
          (lively-update-overlay o)
        (error (message "Error in lively expression: %S" err)
               (lively-delete-overlay o))))))

(defun lively-delete-overlay (o)
  (delete-overlay o)
  (setq lively-overlays (remove o lively-overlays)))

(defun lively-update-overlay (o)
 "Update the text of O if it is both lively and visible."
  (with-current-buffer (overlay-buffer o)
    (let ((expr (buffer-substring (overlay-start o) (overlay-end o))))
      (overlay-put o 'display (format "%s" (eval (read expr)))))))

(defun lively-freeze-overlay (o)
  "Temporarily suspend a lively overlay and remove its text from
  display, allowing it to be edited.

See also `lively-thaw-overlay'."
  (when (find o lively-overlays)
    (setf lively-overlays (remove o lively-overlays))
    (add-to-list 'lively-frozen-overlays o)
    (overlay-put o 'display nil)))

(defun lively-thaw-overlay (o)
  "Unfreeze a previously frozen overlay.

See also `lively-freeze-overlay'."
  (when (find o lively-frozen-overlays)
    (setf lively-frozen-overlays (remove o lively-frozen-overlays))
    (add-to-list 'lively-overlays o)))

(defun lively-pre-command-hook ()
  "Hook to hide lively overlays right before a command, allowing
  undisturbed movement."
  (dolist (o lively-overlays)
    (overlay-put o 'display nil)))

(defun lively-post-command-hook ()
  "Hook to restore lively overlays after a command."
  (let ((overlays-here (overlays-at (point)))
        (recent lively-recent-overlays))
   (dolist (o overlays-here)
     (lively-freeze-overlay o)
     (add-to-list 'lively-recent-overlays o))
   (dolist (o recent)
     (unless (find o overlays-here)
       (lively-thaw-overlay o)
       (setq lively-recent-overlays (remove o lively-recent-overlays))))
   (dolist (o lively-overlays)
     (lively-update-overlay o))))

(defun lively-init-timer ()
  "Setup background timer to update lively text."
  (setq lively-timer (run-with-timer 0 lively-interval 'lively-update)))

(defun lively-stop ()
 "Remove all lively regions in Emacs."
 (interactive)
 (when lively-timer (cancel-timer lively-timer))
 (setq lively-timer nil)
 (mapc 'delete-overlay (concatenate 'list
                                    lively-overlays
                                    lively-frozen-overlays))
 (setq lively-overlays nil))

;;; Nice to have:

(defun lively-shell-command (command)
 "Execute COMMAND and return the output, sans trailing newline."
 (let ((result (shell-command-to-string command)))
   (substring result 0 (1- (length result)))))

(provide 'lively)
;;; lively.el ends here
