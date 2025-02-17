;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package autorevert
  ;; revert buffers when files on disk change
  :ensure nil
  :demand t
  :custom
  (auto-revert-verbose nil)
  :hook
  (after-change-major-mode-hook . auto-revert-mode))

(use-package no-littering
  ;; Help keeping ~/.config/emacs clean.
  :ensure t
  :demand t)

(use-package savehist
  ;; Many editors (e.g. Vim) have the feature of saving minibuffer
  ;; history to an external file after exit.  This package provides the
  ;; same feature in Emacs.  When set up, it saves recorded minibuffer
  ;; histories to a file (`~/.emacs.d/history' by default).  Additional
  ;; variables may be specified by customizing
  ;; `savehist-additional-variables'.
  :ensure nil
  :demand t
  :custom
  (savehist-autosave-interval 60)
  (history-delete-duplicates t)
  (savehist-additional-variables
    '(kill-ring
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

(provide 'init-persistence.el)
;;; init-persistence.el ends here
