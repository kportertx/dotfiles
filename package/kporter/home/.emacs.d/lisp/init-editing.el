;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package aggressive-indent
  ;; `aggressive-indent-mode' is a minor mode that keeps your code always
  ;; indented.  It reindents after every change, making it more reliable
  ;; than `electric-indent-mode'.
  :ensure t
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package crux
  :ensure t
  :bind
  ("C-a" . crux-move-beginning-of-line)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
  ("C-S-<return>" . crux-smart-open-line-above)
  ("S-<return>" . crux-smart-open-line)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c R" . crux-rename-file-and-buffer))

(use-package easy-kill
  :ensure t
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp] . easy-mark))

(use-package avy
  ;; With Avy, you can move point to any position in Emacs – even in a
  ;; different window – using very few keystrokes.
  :ensure t
  :bind
  ("C-:" . avy-goto-char))

(use-package display-fill-column-indicator
  :ensure nil
  :hook
  (prog-mode . display-fill-column-indicator-mode))

;; (use-package dumb-jump
;;   ;; Dumb Jump is an Emacs "jump to definition" package with support for 50+
;;   ;; programming languages that favors "just working" over speed or accuracy.
;;   ;; This means minimal -- and ideally zero -- configuration with absolutely no
;;   ;; stored indexes (TAGS) or persistent background processes.
;;   :ensure t
;;   :hook
;;   (xref-backend-functions . dumb-jump-xref-activate))

(use-package expand-region
  ;; Expand region increases the selected region by semantic units. Just keep
  ;; pressing the key until it selects what you want.
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package hl-todo
  ;; Highlight TODO and similar keywords in comments and strings.
  :ensure t
  :custom
  (hl-todo-keyword-faces
    '(("TODO" . "magenta")
       ("FIXME" . "magenta")
       ("WIP" . "lime green")
       ("WORK" . "lime green")
       ("NEXT" . "lime green")
       ("NOTE" . "purple")
       ("WAIT" . "orange")
       ("HACK" . "orange")
       ("TEMP" . "orange")
       ("XXX+" . "orange")
       ("DONE" . "gray")))
  :hook
  (prog-mode . hl-todo-mode))

(use-package hungry-delete
  ;; cc-mode implements hungry deletion for its programming modes.  This
  ;; package borrows its implementation in a minor mode, so that hungry
  ;; deletion can be used in all modes.
  :ensure t
  :bind
  ("C-c SPC t h" . hungry-delete-mode))

(use-package iedit
  ;; This package includes Emacs minor modes (iedit-mode and iedit-rectangle-mode)
  ;; based on a API library (iedit-lib) and allows you to alter one occurrence of
  ;; some text in a buffer (possibly narrowed) or region, and simultaneously have
  ;; other occurrences changed in the same way, with visual feedback as you type.
  :ensure t
  :config
  (set-face-background 'iedit-occurrence "blue")
  :bind
  ("C-;" . iedit-mode)) ; select all occurnces at-point for edit

(use-package pcre2el
  :ensure t
  :defer t)

(use-package subword
  ;; Treating terms in CamelCase symbols as separate words makes editing.
  :ensure nil
  :demand t
  :init (global-subword-mode 1))

(use-package sudo-edit
  ;; This package allows to open files as another user, by default "root":
  :ensure t
  :init
  (sudo-edit-indicator-mode)
  :bind
  ("C-c C-r" . sudo-edit))

(use-package visual-regexp-steroids
  ;; visual-regexp-steroids is an extension to visual-regexp which enables the
  ;; use of modern regexp engines (no more escaped group parentheses, and other
  ;; goodies!). In addition to that, you can optionally use the better regexp
  ;; syntax to power isearch-forward-regexp and isearch-backward-regexp.
  :ensure t
  :custom
  (vr/engine 'pcre2el "Use PCRE regular expressions")
  :bind
  ("C-c r" . vr/replace)
  ("C-c q" . vr/query-replace)
  ("C-r"   . vr/isearch-backward)
  ("C-S-s" . vr/isearch-forward)
  ("C-M-s" . isearch-forward)
  ("C-M-r" . isearch-backward))

(use-package vundo
  ;; Vundo (visual undo) displays the undo history as a tree and lets you
  ;; move in the tree to go back to previous buffer states. To use vundo,
  ;; type M-x vundo RET in the buffer you want to undo. An undo tree buffer
  ;; should pop up.
  :ensure t
  :bind
  ("C-c u" . vundo))

(provide 'init-editing)
;;; init-editing.el ends here
