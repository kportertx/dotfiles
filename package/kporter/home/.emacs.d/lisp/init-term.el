;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package eat
  ;; Eat's name self-explanatory, it stands for "Emulate A Terminal".
  ;; Eat is a terminal emulator.  It can run most (if not all)
  ;; full-screen terminal programs, including Emacs.
  :ensure t
  :straight
  (eat :type git
    :host codeberg
    :repo "akib/emacs-eat"
    :files ("*.el" ("term" "term/*.el") "*.texi"
             "*.ti" ("terminfo/e" "terminfo/e/*")
             ("terminfo/65" "terminfo/65/*")
             ("integration" "integration/*")
             (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package eshell
  ;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
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
  ;; Highlights commands as the user types to validate commands and syntax.
  :after eshell
  :ensure t
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-git-prompt
  ;; This package provides some themes of Emacs Shell (Eshell) prompt.
  :after eshell
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package multi-vterm
  ;; Managing multiple vterm buffers in Emacs
  :ensure t
  :bind
  ("C-c t" . multi-vterm))

(use-package vterm
  ;; Emacs-libvterm (vterm) is fully-fledged terminal emulator based on an
  ;; external library (libvterm) loaded as a dynamic module.
  :ensure t
  :demand t
  :ensure-system-package
  (libtool . libtool-bin)
  :custom
  (vterm-max-scrollback 100000)
  (vterm-clear-scrollback-when-clearing t))

(provide 'init-term.el)
;;; init-term.el ends here
