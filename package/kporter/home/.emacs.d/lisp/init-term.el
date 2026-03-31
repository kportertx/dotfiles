;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eat
  ;; Eat (Emulate A Terminal) is a pure-Elisp terminal emulator.
  ;; Replaces vterm without requiring native compilation of libvterm.
  :ensure t
  :bind
  ("C-c t" . eat))

(use-package eshell
  :after eat
  :custom
  (tramp-default-method "ssh")
  (eshell-banner-message "")
  (eshell-buffer-maximum-lines 20000)
  (eshell-buffer-shorthand t)
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-error-if-no-glob t)
  (eshell-highlight-prompt t)
  (eshell-hist-ignoredups t)
  (eshell-history-size 10000)
  (eshell-list-files-after-cd nil)
  (eshell-prefer-lisp-functions nil)
  (eshell-save-history-on-exit t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output nil)
  :hook
  (eshell-load-hook . eat-eshell-mode)
  (eshell-load-hook . eat-eshell-visual-command-mode)
  :init
  (add-hook
    'eshell-mode-hook
    (lambda ()
      (eshell/alias "e" "find-file $1")
      (eshell/alias "ff" "find-file $1")
      (eshell/alias "emacs" "find-file $1")
      (eshell/alias "ee" "find-file-other-window $1")
      (eshell/alias "d" "dired $1"))))

(use-package eshell-syntax-highlighting
  :after eshell
  :ensure t
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-git-prompt
  :after eshell
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'powerline))

(provide 'init-term)
;;; init-term.el ends here
