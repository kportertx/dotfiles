;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package avy
  :ensure t
  :bind
  ("C-:" . avy-goto-char))

(use-package display-fill-column-indicator
  :ensure t
  :init
  (setq-default fill-column  80)
  :hook
  (prog-mode . display-fill-column-indicator-mode))

(use-package vundo
  ;; Vundo (visual undo) displays the undo history as a tree and lets you
  ;; move in the tree to go back to previous buffer states. To use vundo,
  ;; type M-x vundo RET in the buffer you want to undo. An undo tree buffer
  ;; should pop up.
  :ensure t
  :bind
  ("C-u" . vundo))

(use-package flycheck
  ;; Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs,
  ;; intended as replacement for the older Flymake extension which is part of GNU
  ;; Emacs.
  :ensure t
  :demand t
  :init
  (global-flycheck-mode))

(use-package flyspell
  ;; Flyspell is a minor Emacs mode performing on-the-fly spelling
  ;; checking.
  :ensure t
  :defer t
  :diminish)

(use-package sudo-edit
  ;; This package allows to open files as another user, by default "root":
  :ensure t
  :init
  (sudo-edit-indicator-mode)
  :bind
  ("C-c C-r" . sudo-edit))

(use-package expand-region
  ;; Expand region increases the selected region by semantic units. Just keep
  ;; pressing the key until it selects what you want.
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package pcre2el
  ;; `pcre2el' or `rxt' (RegeXp Translator or RegeXp Tools) is a utility
  ;; for working with regular expressions in Emacs, based on a
  :ensure t
  :demand t)

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

(use-package subword
  ;; Treating terms in CamelCase symbols as separate words makes editing.
  :ensure t
  :demand t
  :init (global-subword-mode 1))

(use-package hungry-delete
  ;; cc-mode implements hungry deletion for its programming modes.  This
  ;; package borrows its implementation in a minor mode, so that hungry
  ;; deletion can be used in all modes.
  :ensure t
  :bind
  ("C-c SPC t h" . hungry-delete-mode))

(use-package aggressive-indent
  ;; `aggressive-indent-mode' is a minor mode that keeps your code always
  ;; indented.  It reindents after every change, making it more reliable
  ;; than `electric-indent-mode'.
  :ensure t
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package hl-todo
  ;; Highlight TODO and similar keywords in comments and strings.
  :ensure t
  :custom
  (hl-todo-keyword-faces
   '(("TODO" . "magenta")
     ("FIXME" . "magenta")
     ("\\?\\?\\?+" . "magenta")
     ("WIP" . "lime green")
     ("WORK" . "lime green")
     ("NEXT" . "lime green")
     ("NOTE" . "purple")
     ("WAIT" . "orange")
     ("KLUDGE" . "orange")
     ("HACK" . "orange")
     ("TEMP" . "orange")
     ("XXX+" . "orange")
     ("DONE" . "gray")))
  :bind
  (:map hl-todo-mode-map
        ("M-s h i" . hl-todo-insert)
        ("M-s h C-p" . hl-todo-previous)
        ("M-s h C-n" . hl-todo-next)
        ("M-s h o" . hl-todo-occur))
  :hook
  (prog-mode . hl-todo-mode))

;; (use-package format-all
;;   ;; Lets you auto-format source code in many languages using the same
;;   ;; command for all languages, instead of learning a different Emacs
;;   ;; package and formatting command for each language.
;;   :ensure t
;;   :hook
;;   (prog-mode . format-all-mode))

;;(use-package clean-aindent-mode :ensure t :hook (prog-mode . clean-aindent-mode))
;;(use-package dtrt-indent :ensure t :hook (prog-mode . dtrt-indent-mode))
(use-package ws-butler :ensure t :hook (prog-mode . ws-butler-mode))

(provide 'init-editing.el)
;;; init-editing.el ends here
