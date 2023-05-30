;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package no-littering :demand t)

(use-package desktop
  :demand t
  :bind
  ("C-M-s-k" . desktop-clear)
  :config
  (setq desktop-restore-frames t)
  (setq desktop-restore-in-current-display t)
  (setq desktop-restore-forces-onscreen nil)
  (desktop-save-mode 1))

(use-package autorevert ; revert buffers when files on disk change
  :demand t
  :custom
  (auto-revert-verbose nil)
  :hook
  (after-change-major-mode-hook . auto-revert-mode))

(use-package savehist
  :demand t
  :custom
  (savehist-autosave-interval 60)
  (history-delete-duplicates t)
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring
                                   file-name-history
                                   read-expression-history
                                   command-history
                                   extended-command-history
                                   window-config-alist
                                   magit-read-rev-history))
  :config
  (put 'kill-ring 'history-length 300)
  (savehist-mode))

(use-package recentf
  :defer t
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 100)
  (recentf-auto-cleanup 'never)
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :config
  (recentf-mode)
  :bind
  ("C-x C-r" . recentf)
  :hook
  (dired-mode-hook . recentf-add-dired-directory))

(provide 'init-persistence.el)
;;; init-persistence.el ends here
