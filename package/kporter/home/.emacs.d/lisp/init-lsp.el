;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-auto-configure t
	lsp-before-save-edits t
	lsp-eldoc-enable-hover t
	lsp-eldoc-render-all nil
	lsp-completion-enable t
	lsp-file-watch-threshold 10
	lsp-enable-imenu t
	lsp-enable-indentation t
	lsp-enable-links t
	lsp-enable-xref t
	lsp-semantic-tokens-enable t
	lsp-signature-auto-activate t
	lsp-signature-render-documentation t
	lsp-signature-doc-lines 10
	lsp-idle-delay 0.5
	lsp-enable-symbol-highlighting t)
  :init
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (python-mode . lsp)
  (c-mode . lsp)
  (c++-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands
  lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(provide 'init-lsp.el)
;;; init-lsp.el ends here
