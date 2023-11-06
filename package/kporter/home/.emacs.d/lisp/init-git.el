;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package git-gutter+
  ;; View, stage and revert Git changes straight from the buffer.
  :ensure t
  :custom
  (git-gutter+-disabled-modes '(org-mode))
  :bind
  :config
  ;; Move between local changes
  (global-set-key (kbd "M-<up>") 'git-gutter+-previous-hunk)
  (global-set-key (kbd "M-<down>") 'git-gutter+-next-hunk)
  :hook
  (prog-mode . git-gutter+-mode))

(use-package magit :ensure t)

(provide 'init-git.el)
;;; init-git.el ends here
