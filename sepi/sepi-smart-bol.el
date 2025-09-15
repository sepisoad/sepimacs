;;; sepi-smart-bol.el --- Toggle between bol and indentation -*- lexical-binding: t; -*-

;; Author: Sepi
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: convenience
;; URL: https://example.com/sepi-smart-bol

;;; Commentary:
;; Toggle between beginning-of-line and back-to-indentation on repeated presses.

;;; Code:

(defvar sepi-smart-bol--toggle-state nil
  "Internal state to toggle between beginning of line and indentation.")

;;;###autoload
(defun sepi-smart-bol ()
  "Alternate between beginning of line and first non-whitespace character."
  (interactive)
  (if sepi-smart-bol--toggle-state
      (beginning-of-line)
    (back-to-indentation))
  (setq sepi-smart-bol--toggle-state (not sepi-smart-bol--toggle-state)))

(defun sepi-smart-bol--reset-state ()
  "Reset toggle state unless last command was `sepi-smart-bol`."
  (unless (eq this-command #'sepi-smart-bol)
    (setq sepi-smart-bol--toggle-state nil)))

;;;###autoload
(define-minor-mode sepi-smart-bol-mode
  "Minor mode to enable smart beginning-of-line behavior."
  :global t
  (if sepi-smart-bol-mode
      (add-hook 'post-command-hook #'sepi-smart-bol--reset-state)
    (remove-hook 'post-command-hook #'sepi-smart-bol--reset-state)))

(provide 'sepi-smart-bol)

;;; sepi-smart-bol.el ends here
