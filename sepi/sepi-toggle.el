;;; sepi-toggle.el --- Toggle utilities -*- lexical-binding: t -*-

;;; Commentary:
;; Provides functions to toggle line numbers, fringe, and line wrap.

;;; Code:
(defun sepi-toggle-line-numbers-and-fringe ()
  "Toggle line numbers and fringe display."
  (interactive)
  (if display-line-numbers-mode
      (progn
        (global-display-line-numbers-mode -1)
        (set-fringe-mode 0))
    (progn
      (global-display-line-numbers-mode 1)
      (set-fringe-mode 8))))

(defun sepi-toggle-line-wrap ()
  "Toggle line wrapping."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (redraw-display))

(provide 'sepi-toggle)
;;; sepi-toggle.el ends here
