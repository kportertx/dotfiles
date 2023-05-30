;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package vundo
  :ensure t
  :bind
  ("C-u" . vundo))

(use-package flycheck
  :ensure t
  :demand t
  :init
  (global-flycheck-mode))

(use-package flycheck-prospector
  :ensure t
  :after flycheck
  :init (flycheck-prospector-setup))

(use-package sudo-edit
  :ensure t
  :init
  (sudo-edit-indicator-mode)
  :bind
  ("C-c C-r" . sudo-edit))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package pcre2el :ensure t :demand t)

(use-package visual-regexp-steroids
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
  :ensure t
  :config
  (set-face-background 'iedit-occurrence "blue")
  :bind
  ("C-;" . iedit-mode)) ; select all occurnces at-point for edit

;; Treating terms in CamelCase symbols as separate words makes editing.
(use-package subword
  :ensure t
  :demand t
  :config (global-subword-mode 1))

(use-package flyspell
  :ensure t
  :defer t
  :diminish)

(use-package hungry-delete
  :ensure t
  :init
  (global-hungry-delete-mode))

(use-package aggressive-indent
  :ensure t
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package hl-todo
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

(use-package format-all
  :ensure t
  :hook
  (prog-mode . format-all-mode))

;;;;; Useful to switch between window / file layouts.
;; C-x r w ;; save a layout
;; C-x r j ;; load a layout

(provide 'init-editing.el)
;;; init-editing.el ends here
