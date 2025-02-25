;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package clojure-ts-mode :ensure t :demand t)
(use-package elisp-mode
  :ensure nil
  :config
  (use-package elisp-lint :ensure t :demand t))
(use-package go-mode :ensure t :defer t :commands go-mode)

(use-package lsp-java
  :ensure t
  :defer t
  :hook
  (java-mode-hook . lsp))
(use-package java-mode :ensure nil :defer t :commands java-mode)
(use-package js2-mode :ensure t :defer t :commands js2-mode)
(use-package lua-mode :ensure t :defer t :commands lua-mode)

(use-package python-mode :ensure t :defer t :commands python-mode)
(use-package lsp-pyright
  :ensure t
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))

(use-package web-mode :ensure t :defer t :commands web-mode)
(use-package rust-mode :ensure t :defer t :commands rust-mode)
(use-package rustic
  :ensure t
  :defer t
  :custom
  (rustic-format-on-save t)
  :hook
  (rustic-mode-hook . rk/rustic-mode-hook))

;; (use-package tla-mode
;;   :straight (tla-mode :type git :host github :repo "ratish-punnoose/tla-mode")
;;   :ensure t
;;   :defer t
;;   :mode "\.tla$")

;; Docker
(use-package docker :ensure t :bind ("C-c d" . docker))
(use-package dockerfile-mode :ensure t :defer t :commands dockerfile-mode)
(use-package docker-compose-mode :ensure t :defer t :commands docker-compose-mode)
(use-package hcl-mode :ensure t :defer t :commands hcl-mode)

;; Other
(use-package markdown-mode
  :ensure t
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . variable-pitch-mode))
(use-package yaml-mode :ensure t :defer t :commands yaml-mode)
(use-package logview :ensure t :defer t :commands logview-mode)

(provide 'init-prog.el)
;;; init-prog.el ends here

