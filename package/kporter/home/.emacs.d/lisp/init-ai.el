;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
     :bind
     ("C-c g" . agent-shell-toggle)
     :config
     (setq agent-shell-anthropic-authentication
           (agent-shell-anthropic-make-authentication :login t))
     (setq agent-shell-preferred-agent-config 'claude-code)
     (setq agent-shell-thought-process-expand-by-default t)
     (setq agent-shell-dot-subdir-function
           (lambda (subdir)
             (let* ((cwd (agent-shell-cwd))
                    (base (file-name-nondirectory (directory-file-name cwd)))
                    (dir-name (concat base "_" (md5 cwd)))
                    (session-dir (file-name-concat
                                  (expand-file-name "~/.agent-shell")
                                  dir-name))
                    (project-file (expand-file-name ".project-path" session-dir)))
               (unless (file-exists-p project-file)
                 (make-directory session-dir t)
                 (with-temp-file project-file
                   (insert cwd)))
               (expand-file-name subdir session-dir))))))

(init-ai--agent-shell)

(use-package agent-shell-attention
  :after agent-shell
  :vc (:url "https://github.com/ultronozm/agent-shell-attention.el")
  :demand t
  :custom
  (agent-shell-attention-render-function #'agent-shell-attention-render-active)
  :config
  (agent-shell-attention-mode))

(use-package meta-agent-shell
  :after agent-shell
  :vc (:url "https://github.com/ElleNajt/meta-agent-shell")
  :custom
  (meta-agent-shell-start-function #'agent-shell)
  (meta-agent-shell-heartbeat-interval 900)
  :bind
  ("C-c a m" . meta-agent-shell-start)
  ("C-c a d" . meta-agent-shell-jump-to-dispatcher)
  ("C-c a h" . meta-agent-shell-heartbeat-start)
  ("C-c a H" . meta-agent-shell-heartbeat-stop)
  ("C-c a !" . meta-agent-shell-big-red-button))

(use-package mcp-server
  :vc (:url "https://github.com/rhblind/emacs-mcp-server"
       :rev :newest)
  :ensure-system-package (socat . "sudo apt-get install -y socat")
  :custom
  (mcp-server-emacs-tools-enabled 'all)
  :config
  (add-hook 'emacs-startup-hook #'mcp-server-start-unix))

(use-package knockknock
  :vc (:url "https://github.com/konrad1977/knockknock")
  :ensure t)

(use-package agent-shell-knockknock
  :after (agent-shell knockknock)
  :vc (:url "https://github.com/xenodium/agent-shell-knockknock")
  :custom
  (agent-shell-knockknock-duration 5)
  :hook
  (agent-shell-mode . agent-shell-knockknock-mode))

(provide 'init-ai)
;;; init-ai.el ends here
