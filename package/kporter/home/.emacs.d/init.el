;;; Package --- Summary
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packaging)

(require 'init-defaults)
(require 'init-theme)

(require 'init-ai)
(require 'init-editing)
(require 'init-formatting)
(require 'init-git)
(require 'init-lsp)
(require 'init-persistence)
(require 'init-prog)
(require 'init-term)
(require 'init-ui)
(require 'init-org)

;; Garbage Collection ;;
(setq gc-cons-threshold 100000000)

(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

(provide 'init)
;;; init.el ends here
