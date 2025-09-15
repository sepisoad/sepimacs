;;; sepi-pulse.el --- Pulse copied or pasted text -*- lexical-binding: t -*-

;;; Commentary:
;; Provides functions to pulse (highlight briefly) text that is copied or pasted.

;;; Code:
(defun sepi-pulse-region (beg end)
  "Pulse the region between BEG and END if pulse function is available."
  (when (and (fboundp 'pulse-momentary-highlight-region) beg end)
    (pulse-momentary-highlight-region beg end)))

(defun sepi-pulse-after-copy (beg end &optional _)
  "Pulse the region between BEG and END after copying."
  (interactive "r")
  (sepi-pulse-region beg end))

(defun sepi-pulse-after-paste (&rest _)
  "Pulse the pasted region after insertion."
  (interactive)
  (when (and (mark) (not (equal (point) (mark))))
    (sepi-pulse-region (min (point) (mark)) (max (point) (mark)))))

(defun sepi-enable-pulse-copy-paste ()
  "Enable pulsing for copy and paste operations."
  (interactive)
  (advice-add 'kill-ring-save :after #'sepi-pulse-after-copy)
  (advice-add 'yank :after #'sepi-pulse-after-paste))

(defun sepi-disable-pulse-copy-paste ()
  "Disable pulsing for copy and paste operations."
  (interactive)
  (advice-remove 'kill-ring-save #'sepi-pulse-after-copy)
  (advice-remove 'yank #'sepi-pulse-after-paste))

(provide 'sepi-pulse)
;;; sepi-pulse.el ends here
