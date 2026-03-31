;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Pin grammars missing from treesit-auto or needing ABI-14 compat.
  (dolist (source '((c "https://github.com/tree-sitter/tree-sitter-c" "v0.21.4")
                    (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2")
                    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.4")
                    (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua" "v0.1.0")))
    (setf (alist-get (car source) treesit-language-source-alist) (cdr source)))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Aerospike C code uses tabs for indentation.
;; Must reset indent-bars after setting indent-tabs-mode since
;; indent-bars already activated on prog-mode-hook with the wrong value.
(dolist (hook '(c-ts-mode-hook c-mode-hook c++-ts-mode-hook c++-mode-hook))
  (add-hook hook (lambda ()
                   (setq-local indent-tabs-mode t)
                   (when (bound-and-true-p indent-bars-mode)
                     (indent-bars-reset)))))

(use-package cargo
  :ensure t
  :defer t
  :hook (rust-ts-mode . cargo-minor-mode))
(use-package clojure-ts-mode :ensure t :demand t)
(use-package docker :ensure t :bind ("C-c d" . docker))
(use-package docker-compose-mode :ensure t :defer t :commands docker-compose-mode)
(use-package dockerfile-mode :ensure t :defer t :commands dockerfile-mode)
(use-package eglot-java
  :ensure t
  :defer t
  :hook
  (java-mode . eglot-java-mode))
(use-package elisp-mode
  :ensure nil
  :config
  (use-package elisp-lint :ensure t :demand t))
(use-package go-mode :ensure t :defer t :commands go-mode)
(use-package hcl-mode :ensure t :defer t :commands hcl-mode)
(use-package java-mode :ensure nil :defer t :commands java-mode)
(use-package logview :ensure t :defer t :commands logview-mode)
(use-package lua-mode :ensure t :defer t :commands lua-mode)
(use-package markdown-mode
  :ensure t
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . variable-pitch-mode))
;; rust-ts-mode is built-in; eglot handles rust-analyzer
(use-package rust-ts-mode
  :ensure nil
  :defer t
  :hook
  (rust-ts-mode . (lambda () (setq-local rust-format-on-save t))))
(use-package web-mode :ensure t :defer t :commands web-mode)
(use-package yaml-mode :ensure t :defer t :commands yaml-mode)

(provide 'init-prog)
;;; init-prog.el ends here
