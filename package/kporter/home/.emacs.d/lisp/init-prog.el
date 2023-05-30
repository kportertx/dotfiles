;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package elisp-lint :ensure t :demand t)

(use-package markdown-mode
  :ensure t
  :hook
  (markdown-mode . visual-line-mode)
  (maqrkdown-mode . variable-pitch-mode))

(use-package c++-mode :ensure nil :defer t :commands c++-mode)
(use-package c-mode :ensure nil :config :commands c-mode)
(use-package dockerfile-mode :ensure t :defer t :commands dockerfile-mode)
(use-package go-mode :ensure t :defer t :commands go-mode)
(use-package java-mode :ensure nil :defer t :commands java-mode)
(use-package lua-mode :ensure t :defer t :commands lua-mode)
(use-package python-mode :ensure t :defer t :commands python-mode)
(use-package yaml-mode :ensure t :defer t :commands yaml-mode)

(use-package clean-aindent-mode :ensure t :hook (prog-mode . clean-aindent-mode))
(use-package dtrt-indent :ensure t :hook (prog-mode . dtrt-indent-mode))
;;(use-package ws-butler :esnure t :hook (prog-mode . ws-butler-mode))


(provide 'init-prog.el)
;;; init-prog.el ends here
