;;; sepi-c-comment-box.el --- Insert centered C-style comment box  -*- lexical-binding: t; -*-

;; Author: Sepi
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, comments
;; URL: https://example.invalid

;;; Commentary:
;; M-x sepi-c-comment-box
;; - If region is active, uses that text; otherwise prompts.
;; - Centers the text between /* ... */ with = rulers.

;;; Code:

(defgroup sepi-c-comment-box nil
  "Centered C-style comment boxes."
  :group 'convenience)

(defcustom sepi-c-comment-box-width 53
  "Inner width between `/* ' and ` */'."
  :type 'integer :group 'sepi-c-comment-box)

(defcustom sepi-c-comment-box-filler ?=
  "Character used for the top/bottom ruler."
  :type 'character :group 'sepi-c-comment-box)

(defun sepi-c-comment-box--sanitize (s)
  (let* ((s (replace-regexp-in-string "\n+" " " (or s ""))))
    (string-trim s)))

;;;###autoload
(defun sepi-c-comment-box (title)
  "Insert a centered C-style comment box using TITLE."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Comment box text: "))))
  (let* ((title (sepi-c-comment-box--sanitize title))
         (inner (max sepi-c-comment-box-width (length title)))
         (left  (/ (- inner (length title)) 2))
         (right (- inner (length title) left))
         (line1 (concat "/* " (make-string inner sepi-c-comment-box-filler) " */\n"))
         (line2 (concat "/* " (make-string left ?\s) title (make-string right ?\s) " */\n"))
         (box (concat line1 line2 line1)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert box)))

(provide 'sepi-c-comment-box)
;;; sepi-c-comment-box.el ends here
