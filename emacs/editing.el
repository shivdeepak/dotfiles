;; show line numbers on the left
(global-display-line-numbers-mode 1)

;; show line length indicator
(global-display-fill-column-indicator-mode 1)

(defun set-line-limit-indicator ()
  "Set line limit indicator in current buffer"
   (setq fill-column 79)
   )

(set-line-limit-indicator)
(add-hook 'find-file-hook 'set-line-limit-indicator)

;; show point/cursor position on mode line
(setq column-number-mode t)

;; on save hooks for better collaboration
(defun my-cleanup-buffer-on-save ()
  "Remove trailing whitespace and ensure a newline at the end of file"
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'before-save-hook 'ensure-final-newline))

(defun ensure-final-newline ()
  "Ensure the file ends with a single newline."
  (when (not (eq (char-before) ?\n))
    (save-excursion
      (goto-char (point-max))
      (insert "\n"))))

;; Activate the cleanup function globally or per mode
(add-hook 'before-save-hook 'my-cleanup-buffer-on-save)
