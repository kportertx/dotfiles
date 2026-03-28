;;; Package --- Summary
;;; Commentary:
;;; Code:

;; (use-package c3po
;;   ;; C3PO.el is an Emacs package for interacting with the ChatGPT API.
;;   :straight
;;   (
;;     :host github
;;     :repo "d1egoaz/c3po.el"))

;; (use-package gptel
;;   ;; A simple ChatGPT client for Emacs.
;;   :ensure t)

;; :ensure-system-package is parsed before inner quasiquote can run; wrap in a macro so
;; use-package sees plain (SYMBOL . STRING) pairs.  See use-package-normalize/:ensure-system-package.
(defmacro init-ai--agent-shell ()
  `(use-package agent-shell
     :ensure t
     :ensure-system-package
     ;; Native installer: Linux/macOS/WSL.  Homebrew (--cask) on macOS only.
     ((claude . ,(if (eq system-type 'darwin)
                     "brew install --cask claude-code"
                   "curl -fsSL https://claude.ai/install.sh | bash"))
      (claude-agent-acp . "npm install -g @zed-industries/claude-agent-acp"))
     :config
     (setq agent-shell-anthropic-authentication
           (agent-shell-anthropic-make-authentication :login t))))

(init-ai--agent-shell)

(provide 'init-ai)
;;; init-ai.el ends here
