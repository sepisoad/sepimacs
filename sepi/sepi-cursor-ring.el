;;; sepi-cursor-ring.el --- Simplified cursor location ring navigation -*- lexical-binding: t -*-

;;; Commentary:
;; A unified cursor ring for navigation, internally tracking typing and jumps.

;;; Code:
(require 'ring)
(require 'pulse)
(require 'xref)

;; Internal rings
(defvar-local sepi-jump-ring (make-ring 3)
  "Internal ring for locations jumped to (max 3).")
(defvar-local sepi-pre-jump-ring (make-ring 3)
  "Internal ring for locations before jumps (max 3).")
(defvar-local sepi-typing-ring (make-ring 1)
  "Internal ring for typing location (max 1).")

;; Unified navigation ring
(defvar-local sepi-unified-ring (make-ring 5)
  "Unified ring for user navigation (max 5).")
(defvar-local sepi-unified-ring-index 0
  "Current index in the unified ring.")

(defconst sepi-line-window 4
  "Lines above/below to consider for deduplication.")

(defun sepi--line-at-point ()
  "Return the current line number at point."
  (line-number-at-pos (point)))

(defun sepi--in-window-p (line1 line2)
  "Check if LINE1 is within 4 lines of LINE2."
  (<= (abs (- line1 line2)) sepi-line-window))

(defun sepi--add-to-ring (ring pos ring-name)
  "Add POS to RING, deduplicating within 4-line window and by line."
  (let* ((marker (copy-marker pos))
         (line (line-number-at-pos pos))
         (ring-list (ring-elements ring)))
    ;; Remove any existing entry on the same line or within 4 lines
    (dolist (m ring-list)
      (when (and m (marker-position m)
                 (or (eq (line-number-at-pos (marker-position m)) line)
                     (sepi--in-window-p (line-number-at-pos (marker-position m)) line)))
        (ring-remove ring (ring-member ring m))))
    ;; Add to ring (replace for typing, limit others)
    (if (eq ring sepi-typing-ring)
        (progn
          (when (= (ring-length ring) 1) (ring-remove ring 0))
          (ring-insert ring marker))
      (when (< (ring-length ring) (ring-size ring))
        (ring-insert ring marker)))
    (message "Added to %s ring at line %d (size: %d)" ring-name line (ring-length ring))
    ;; Update unified ring
    (sepi--update-unified-ring)))

(defun sepi--update-unified-ring ()
  "Update the unified ring with entries from all internal rings."
  (let ((all-positions (append (ring-elements sepi-typing-ring)
                               (ring-elements sepi-pre-jump-ring)
                               (ring-elements sepi-jump-ring))))
    (setq sepi-unified-ring (make-ring 5))
    (dolist (marker all-positions)
      (when (and marker (marker-position marker)
                 (< (ring-length sepi-unified-ring) (ring-size sepi-unified-ring)))
        (let* ((pos (marker-position marker))
               (line (line-number-at-pos pos))
               (exists (seq-some (lambda (m)
                                   (and m (marker-position m)
                                        (or (eq (line-number-at-pos (marker-position m)) line)
                                            (sepi--in-window-p (line-number-at-pos (marker-position m)) line))))
                                 (ring-elements sepi-unified-ring))))
          (unless exists
            (ring-insert sepi-unified-ring (copy-marker pos))))))
    (setq sepi-unified-ring-index (min sepi-unified-ring-index (max 0 (1- (ring-length sepi-unified-ring)))))))

(defun sepi-add-to-typing-ring ()
  "Add current position to typing ring if typing occurs."
  (interactive)
  (when (eq this-command 'self-insert-command)
    (sepi--add-to-ring sepi-typing-ring (point) "typing")))

(defun sepi--track-before-jump-advice (&rest _args)
  "Save position to pre-jump ring before jumping."
  (sepi--add-to-ring sepi-pre-jump-ring (point) "pre-jump"))

(defun sepi--track-after-jump (&rest _args)
  "Save position to jump ring after a jump."
  (sepi--add-to-ring sepi-jump-ring (point) "jump"))

(defun sepi-navigate-unified-ring (direction)
  "Navigate the unified ring forward (1) or backward (-1)."
  (interactive "P")
  (sepi--update-unified-ring) ; Ensure ring is up-to-date
  (if (ring-empty-p sepi-unified-ring)
      (message "Cursor ring is empty")
    (let* ((len (ring-length sepi-unified-ring))
           (new-index (mod (+ sepi-unified-ring-index (if (numberp direction) direction 1)) len)))
      (setq sepi-unified-ring-index new-index)
      (let ((marker (ring-ref sepi-unified-ring new-index)))
        (if (and marker (marker-position marker))
            (progn
              (goto-char (marker-position marker))
              (recenter nil)
              (pulse-momentary-highlight-one-line (point))
              (message "Moved to position %d/%d on line %d"
                       (1+ new-index) len (sepi--line-at-point)))
          (message "Invalid marker at index %d" new-index))))))

(defun sepi-delete-current-position ()
  "Delete the current position from the unified ring and its source."
  (interactive)
  (sepi--update-unified-ring)
  (if (ring-empty-p sepi-unified-ring)
      (message "Cursor ring is empty, nothing to delete")
    (let* ((current-marker (ring-ref sepi-unified-ring sepi-unified-ring-index))
           (current-pos (marker-position current-marker))
           (current-line (line-number-at-pos current-pos)))
      ;; Remove from unified ring
      (ring-remove sepi-unified-ring sepi-unified-ring-index)
      ;; Adjust index if needed
      (setq sepi-unified-ring-index (min sepi-unified-ring-index (max 0 (1- (ring-length sepi-unified-ring)))))
      ;; Remove from source rings if it matches
      (dolist (ring (list sepi-typing-ring sepi-pre-jump-ring sepi-jump-ring))
        (let ((ring-list (ring-elements ring)))
          (dolist (m ring-list)
            (when (and m (marker-position m)
                       (eq (line-number-at-pos (marker-position m)) current-line))
              (ring-remove ring (ring-member ring m))))))
      (sepi--update-unified-ring) ; Rebuild unified ring
      (message "Deleted position on line %d (remaining: %d)" current-line (ring-length sepi-unified-ring)))))

(defun sepi-reset-all-positions ()
  "Reset all positions in all rings."
  (interactive)
  (setq sepi-jump-ring (make-ring 3))
  (setq sepi-pre-jump-ring (make-ring 3))
  (setq sepi-typing-ring (make-ring 1))
  (setq sepi-unified-ring (make-ring 5))
  (setq sepi-unified-ring-index 0)
  (message "All cursor ring positions reset"))

;; Keybindings
(global-set-key (kbd "s-]") (lambda () (interactive) (sepi-navigate-unified-ring 1)))
(global-set-key (kbd "s-[") (lambda () (interactive) (sepi-navigate-unified-ring -1)))
(global-set-key (kbd "s-<backspace>") #'sepi-delete-current-position)
(global-set-key (kbd "C-s-<backspace>") #'sepi-reset-all-positions)

;; Setup hooks
(advice-add 'xref-find-definitions :before #'sepi--track-before-jump-advice)
(advice-add 'xref-find-definitions :after #'sepi--track-after-jump)
(add-hook 'post-command-hook #'sepi-add-to-typing-ring)

(provide 'sepi-cursor-ring)
;;; sepi-cursor-ring.el ends here
