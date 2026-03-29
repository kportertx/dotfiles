;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package eglot
  :ensure t
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0)
  (eglot-send-changes-idle-time 0.5)
  :config
  (setq eglot-stay-out-of '())
  :init
  (add-hook 'prog-mode-hook
    (lambda ()
      (unless (derived-mode-p
                'emacs-lisp-mode
                'hcl-mode
                'makefile-mode
                'clojure-ts-mode)
        (eglot-ensure))))
  :bind
  (:map eglot-mode-map
    ("C-c l a" . eglot-code-actions)
    ("C-c l r" . eglot-rename)
    ("C-c l f" . eglot-format)
    ("C-c l d" . eldoc)
    ("C-c l h" . eldoc-doc-buffer)
    ("C-c l o" . eglot-code-action-organize-imports)))

(provide 'init-lsp)
;;; init-lsp.el ends here
