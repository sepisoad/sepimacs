;;; sepi-recenter.el --- Recenter and pulse functionality -*- lexical-binding: t -*-

;;; Commentary:
;; Provides a function to recenter the window and pulse the current line.

;;; Code:
(defun sepi-recenter-and-pulse ()
  "Recenter window and pulse current line."
  (interactive)
  (recenter)
  (when (fboundp 'pulse-momentary-highlight-one-line)
    (pulse-momentary-highlight-one-line (point))))

(provide 'sepi-recenter)
;;; sepi-recenter.el ends here
