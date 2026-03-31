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

;; Garbage Collection ;;

(use-package gcmh
  :defer t
  :config
  (gcmh-mode))

(provide 'init)
;;; init.el ends here
