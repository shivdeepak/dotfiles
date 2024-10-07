(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)

  (use-package magit
    :after magit
    :config
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  (diff-hl-flydiff-mode)
)
