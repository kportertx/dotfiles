;;; Package --- Summary
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defvar flycheck-emacs-lisp-load-path 'inherit)

(require 'init-packaging.el)
(require 'init-defaults.el)
(require 'init-theme.el)
(require 'init-persistence.el)
(require 'init-ui.el)
(require 'init-editing.el)
(require 'init-lsp.el)
(require 'init-prog.el)


;; Garbage Collection ;;

(use-package gcmh
  :defer t
  :config
  (gcmh-mode))

(provide 'init.el)
;;; init.el ends here
