;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package lsp-mode
  ;; Manual setup https://github.com/nodesource/distributions
  :ensure t
  :custom
  (lsp-auto-configure t)
  (lsp-before-save-edits t)
  (lsp-completion-enable t)
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-render-all nil)
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)         ; nil didn't fix 'editorconfig'.
  (lsp-enable-on-type-formatting nil)  ; nil didn't fix 'editorconfig'.
  (lsp-enable-links t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-xref t)
  (lsp-file-watch-threshold 100)
  (lsp-idle-delay 0.1)
  (lsp-keymap-prefix "C-c l")
  (lsp-lens-enable t)
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-semantic-tokens-enable t)
  (lsp-signature-auto-activate t)
  (lsp-signature-doc-lines 10)
  (lsp-signature-render-documentation t)
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (unless (derived-mode-p
                       'emacs-lisp-mode
                       'hcl-mode
                       'makefile-mode)
                (lsp-deferred))))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands
  lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil))

(provide 'init-lsp.el)
;;; init-lsp.el ends here
