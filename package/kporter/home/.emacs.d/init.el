;;; Package --- Summary
;;; Commentary:
;;; Code:

(load-file "~/.emacsrc.el")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defvar flycheck-emacs-lisp-load-path 'inherit)

(require 'init-packaging.el)

(require 'init-defaults.el)
(require 'init-theme.el)

(require 'init-ai.el)
(require 'init-editing.el)
(require 'init-git.el)
(require 'init-lsp.el)
(require 'init-persistence.el)
(require 'init-prog.el)
(require 'init-term.el)
(require 'init-ui.el)

;; Garbage Collection ;;

(use-package gcmh
  :defer t
  :config
  (gcmh-mode))

(provide 'init.el)
;;; init.el ends here
