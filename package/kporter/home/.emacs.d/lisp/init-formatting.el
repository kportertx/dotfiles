;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package editorconfig
  :ensure nil
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :init
  (editorconfig-mode))

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

(use-package ws-butler :ensure t :demand t :commands ws-butler-mode)

(provide 'init-formatting)
;;; init-formatting.el ends here
