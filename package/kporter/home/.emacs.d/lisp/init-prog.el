;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package c++-mode
  :ensure nil
  :defer t
  :init
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  :commands c++-mode)

(use-package c-mode
  :ensure nil
  :config
  :init
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  :commands c-mode)

(use-package dockerfile-mode :ensure t :defer t :commands dockerfile-mode)
(use-package elisp-lint :ensure t :demand t)
(use-package go-mode :ensure t :defer t :commands go-mode)
(use-package java-mode :ensure nil :defer t :commands java-mode)
(use-package js2-mode :ensure t :defer t :commands js2-mode)
(use-package lua-mode :ensure t :defer t :commands lua-mode)
(use-package python-mode
  :ensure t
  :defer t
  :init
  (setq indent-tabs-mode nil)
  (setq python-indent-offset 4)
  :commands python-mode)
(use-package web-mode :ensure t :defer t :commands web-mode)
(use-package rust-mode :ensure t :defer t :commands rust-mode)
(use-package rustic
  :ensure t
  :defer t
  :config
  (setq rustic-format-on-save t)
  :hook
  (rustic-mode-hook . rk/rustic-mode-hook))

(use-package markdown-mode
  :ensure t
  :hook
  (markdown-mode . visual-line-mode)
  (maqrkdown-mode . variable-pitch-mode))
(use-package yaml-mode :ensure t :defer t :commands yaml-mode)

(provide 'init-prog.el)
;;; init-prog.el ends here
