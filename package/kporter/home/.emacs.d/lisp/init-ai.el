;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Requires: claude (claude.ai/install.sh), claude-agent-acp (npm -g)
(use-package agent-shell
  :ensure t
  :bind
  ("C-c g" . agent-shell-toggle)
  (:map agent-shell-mode-map
   ("RET" . newline)
   ("C-<return>" . shell-maker-submit))
  :config
  (setq agent-shell-header-style 'text)
  (set-face-attribute 'header-line nil :height 0.85)
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  (setq agent-shell-preferred-agent-config 'claude-code)
  (setq agent-shell-thought-process-expand-by-default t)
  ;; Pass Emacs MCP server config to Claude when launched from agent-shell.
  (setq agent-shell-anthropic-claude-acp-command
        '("claude-agent-acp"
          "--mcp-config" "~/.emacs.d/emacs-mcp-claude.json"))
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
            (expand-file-name subdir session-dir)))))

(use-package agent-shell-attention
  :after agent-shell
  :vc (:url "https://github.com/ultronozm/agent-shell-attention.el")
  :demand t
  :custom
  (agent-shell-attention-render-function #'agent-shell-attention-render-active)
  :config
  (agent-shell-attention-mode))

;; Requires: apt install socat
(use-package mcp-server
  :vc (:url "https://github.com/rhblind/emacs-mcp-server"
       :rev :newest)
  :custom
  (mcp-server-emacs-tools-enabled 'all)
  ;; Prompt in minibuffer for unapproved dangerous ops instead of blocking.
  (mcp-server-security-prompt-for-permissions t)
  ;; Allow read-only buffer/file access needed for eglot LSP lookups.
  ;; Destructive ops (shell-command, delete-file, kill-emacs, etc.) stay gated.
  (mcp-server-security-allowed-dangerous-functions
   '(with-current-buffer
     find-file-noselect
     set-buffer
     save-current-buffer
     insert-file-contents
     getenv
     view-file))
  ;; LSP requests via eglot can be slow; allow up to 60s.
  (mcp-server-security-max-execution-time 60)
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

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el"
       :rev :newest)
  :bind
  ("C-c c" . claude-code-ide-send-prompt)
  :custom
  (claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-use-ide-diff t)
  :config
  (setenv "COLOR" "truecolor")
  (defun init-ai--claude-ide-swap-return ()
    "Make RET insert newline and C-RET submit in claude-code-ide eat buffers."
    (when (string-match-p "\\*Claude Code" (buffer-name))
      (local-set-key (kbd "RET") #'claude-code-ide-insert-newline)
      (local-set-key (kbd "C-<return>") #'claude-code-ide--terminal-send-return)))
  (add-hook 'eat-mode-hook #'init-ai--claude-ide-swap-return))

(use-package eca
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs"
       :rev :newest)
  :bind
  ("C-c e" . eca))

;; Transcripts live in ~/.agent-shell/<name_hash>/transcripts/ due to the
;; custom agent-shell-dot-subdir-function above.
(use-package agent-recall
  :ensure t
  :hook (agent-shell-mode . agent-recall-track-sessions)
  :custom
  (agent-recall-search-paths '("~/.agent-shell"))
  (agent-recall-search-function 'consult-ripgrep)
  (agent-recall-browse-sort 'modified-desc))

(provide 'init-ai)
;;; init-ai.el ends here
