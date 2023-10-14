;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :demand t
  ;; These five color themes are designed for use with Emacs' built-in
  ;; theme support in Emacs 24. However, they also work with older Emacs
  ;; versions, in which case color-theme.el is required.
  :init
  (setq custom-safe-themes t) ; warning will not go away
  (load-theme 'sanityinc-tomorrow-eighties))

(use-package hl-line
  :ensure nil
  :init
  (global-hl-line-mode) )

(use-package doom-modeline
  ;; This package offers a fancy and fast mode-line inspired by minimalism design.
  :ensure t
  :demand t
  :init
  ;; If non-nil, cause imenu to see `doom-modeline' declarations.
  ;; This is done by adjusting `lisp-imenu-generic-expression' to
  ;; include support for finding `doom-modeline-def-*' forms.
  ;; Must be set before loading doom-modeline.
  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-height 25)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  ;; Whether display the buffer encoding.
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-project-detection 'projectile)
  :config
  (set-face-attribute 'doom-modeline-evil-insert-state nil :foreground "orange")
  :hook (after-init . doom-modeline-mode))

(use-package diminish :ensure t :demand t) ;; if you use :diminish
(use-package delight :ensure t :demand t)  ;; Use delighting for modes

(use-package all-the-icons
  ;; M-x all-the-icons-install-fonts
  :ensure t
  :demand t
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
	(all-the-icons-install-fonts t))
  :if
  (display-graphic-p))

(use-package nerd-icons
  :ensure t
  ;; M-x nerd-icons-install-fonts
  :config
  (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
    (nerd-icons-install-fonts t))
  :if
  (display-graphic-p))

(provide 'init-theme.el)
;;; init-theme.el ends here
