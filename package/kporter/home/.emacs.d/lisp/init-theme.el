;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

(use-package doom-modeline
  ;; This package offers a fancy and fast mode-line inspired by minimalism design.
  :ensure t
  :demand t
  :custom
  ;; If non-nil, cause imenu to see `doom-modeline' declarations.
  ;; This is done by adjusting `lisp-imenu-generic-expression' to
  ;; include support for finding `doom-modeline-def-*' forms.
  ;; Must be set before loading doom-modeline.
  (doom-modeline-support-imenu t)
  (doom-modeline-height 25)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  ;; Whether display the buffer encoding.
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-project-detection 'auto)
  (doom-modeline-indent-info t)
  :config
  (set-face-attribute 'doom-modeline-evil-insert-state nil :foreground "orange")
  :hook
  (after-init . doom-modeline-mode))

(use-package modus-themes
  ;; The Modus themes conform with the highest standard for
  ;; color-contrast accessibility between background and foreground
  ;; values (WCAG AAA).  Built-in since Emacs 28.
  :ensure nil
  :bind
  ("C-c SPC t t" . modus-themes-toggle)
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-mixed-fonts t)
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  :init
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package nerd-icons
  ;; M-x nerd-icons-install-fonts
  :ensure t
  :init
  (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
    (nerd-icons-install-fonts t))
  :if
  (display-graphic-p))

(provide 'init-theme)
;;; init-theme.el ends here
