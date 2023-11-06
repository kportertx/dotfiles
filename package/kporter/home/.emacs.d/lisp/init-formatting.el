;;; Package --- Summary
;;; Commentary:
;;; Code:

;; (use-package format-all
;;   ;; Lets you auto-format source code in many languages using the same
;;   ;; command for all languages, instead of learning a different Emacs
;;   ;; package and formatting command for each language.
;;   :ensure t
;;   :hook
;;   (prog-mode . format-all-mode))

(use-package editorconfig
  :ensure t
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :init
  (editorconfig-mode))

(use-package ws-butler :ensure t :demand t :commands ws-butler-mode)

(use-package whitespace
  :ensure nil
  :init
  (setq whitespace-indentation 'whitespace-trailing)
  :custom
  (whitespace-style '(face trailing indentation space-after-tab))
  (whitespace-indentation-regexp '("^\t*\\( +\\)[^\n]" . "^ *\\(\t+\\)[^\n]"))
  :commands whitespace-mode
  :hook
  (prog-mode . whitespace-mode))

(provide 'init-formatting.el)
;;; init-formatting.el ends here
