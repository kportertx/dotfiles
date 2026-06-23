;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :bind
  ("M-<up>" . diff-hl-previous-hunk)
  ("M-<down>" . diff-hl-next-hunk))

(use-package git-timemachine
  :ensure t
  :bind
  ("C-c T" . git-timemachine))

(use-package magit :ensure t)

(provide 'init-git)
;;; init-git.el ends here
