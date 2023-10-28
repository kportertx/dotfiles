;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package all-the-icons
  ;; This package is a utility for using and formatting various Icon
  ;; fonts within Emacs.
  ;; M-x all-the-icons-install-fonts
  :ensure t
  :demand t
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t))
  :if
  (display-graphic-p))

(use-package all-the-icons-completion
  ;;  Add icons to completion candidates.
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

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

(use-package hl-line
  ;; Provides a local minor mode (toggled by M-x hl-line-mode) and
  ;; a global minor mode (toggled by M-x global-hl-line-mode) to
  ;; highlight, on a suitable terminal, the line on which point is.
  :ensure nil
  :commands hl-line-mode
  :hook
  (prog-mode . hl-line-mode))

(use-package modus-themes
  ;; The Modus themes conform with the highest standard for
  ;; color-contrast accessibility between background and foreground
  ;; values (WCAG AAA).
  :ensure t
  :straight (modus-themes :type git :host gitlab :repo "protesilaos/modus-themes")
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

(provide 'init-theme.el)
;;; init-theme.el ends here
