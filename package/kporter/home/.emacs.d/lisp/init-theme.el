;;; Package --- Summary
;;; Commentary:
;;; Code:

;; NOTE - Try https://github.com/catppuccin/emacs
(use-package color-theme-sanityinc-tomorrow
  :init
  (setq custom-safe-themes t) ; warning will not go away
  (load-theme 'sanityinc-tomorrow-eighties))

(provide 'init-theme.el)
;;; init-theme.el ends here
