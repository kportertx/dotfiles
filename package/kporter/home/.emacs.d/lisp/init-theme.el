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
  ;; Syntax highlighting follows https://tonsky.me/blog/syntax-highlighting/
  ;; - Only highlight: strings, constants, comments, definitions, punctuation
  ;; - Unhighlight: keywords, types, builtins, variable/function references
  ;; - Max 4-5 colors, no bold/italic, reserve red for errors
  :ensure nil
  :bind
  ("C-c SPC t t" . modus-themes-toggle)
  :custom
  (modus-themes-italic-constructs nil)
  (modus-themes-bold-constructs nil)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-mixed-fonts t)
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  (modus-themes-common-palette-overrides
   '(;; Strings: green
     (string green-warmer)
     ;; Constants/numbers: purple
     (constant magenta-cooler)
     ;; Comments: bright yellow (treat as important content)
     (comment yellow)
     (docstring yellow-faint)
     ;; Function definitions: blue
     (fnname blue)
     ;; Variable declarations: cyan
     (variable cyan)
     ;; Unhighlight keywords, types, builtins, preprocessor
     (keyword fg-main)
     (type fg-main)
     (builtin fg-main)
     (preprocessor fg-main)
     ;; Dim punctuation/delimiters
     (delimiter fg-dim)))
  :init
  (load-theme 'modus-vivendi-tinted :no-confirm)
  :config
  ;; Unhighlight function calls and variable references (Emacs 29+ faces)
  (with-eval-after-load 'font-lock
    (set-face-attribute 'font-lock-function-call-face nil
                        :foreground 'unspecified :inherit 'default)
    (set-face-attribute 'font-lock-variable-use-face nil
                        :foreground 'unspecified :inherit 'default)
    (set-face-attribute 'font-lock-bracket-face nil
                        :foreground 'unspecified :inherit 'shadow)
    (set-face-attribute 'font-lock-operator-face nil
                        :foreground 'unspecified :inherit 'shadow)
    (set-face-attribute 'font-lock-punctuation-face nil
                        :foreground 'unspecified :inherit 'shadow)))

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
