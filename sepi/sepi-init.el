;;; sepi-init.el --- Init file utilities -*- lexical-binding: t -*-

;;; Commentary:
;; Provides function to quickly open init.el.

;;; Code:
(defun sepi-open-init-file ()
  "Open init.el configuration file."
  (interactive)
  (find-file user-init-file))

(provide 'sepi-init)
;;; sepi-init.el ends here
