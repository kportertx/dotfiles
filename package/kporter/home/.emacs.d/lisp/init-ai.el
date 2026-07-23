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

;; Treemacs section listing live agent-shell buffers, using treemacs's
;; extension API (buffer lists are its own documented example case).
;; ponytail: re-queried only on collapse/expand (TAB), not pushed live when
;; a shell opens/closes elsewhere; toggle the section to refresh.
(with-eval-after-load 'treemacs
  (require 'treemacs-extensions)

  (defun agent-shell--treemacs-buffers ()
    "Live `agent-shell-mode' buffers, for the treemacs Agent Shells section."
    (sort (seq-filter (lambda (b) (with-current-buffer b (derived-mode-p 'agent-shell-mode)))
                       (buffer-list))
          :key #'buffer-name :lessp #'string<))

  (defun agent-shell--treemacs-visit (&optional _arg)
    (interactive "P")
    (-when-let (node (treemacs-node-at-point))
      (-when-let (buf (treemacs-button-get node :agent-shell-buffer))
        (if (buffer-live-p buf)
            (pop-to-buffer buf)
          (treemacs-pulse-on-failure "That agent-shell buffer is gone.")))))

  (defun agent-shell--treemacs-visit-in-last-window (&optional _arg)
    "Open the clicked agent-shell buffer in whichever window last had focus.
Mirrors `treemacs-visit-node-in-most-recently-used-window', which only
dispatches for file/dir/tag nodes and doesn't know about this custom type."
    (interactive "P")
    (-when-let (node (treemacs-node-at-point))
      (-when-let (buf (treemacs-button-get node :agent-shell-buffer))
        (if (not (buffer-live-p buf))
            (treemacs-pulse-on-failure "That agent-shell buffer is gone.")
          (run-hook-with-args
           'treemacs-after-visit-functions
           (let ((win (get-mru-window (selected-frame) nil :not-selected)))
             (if win
                 (progn (select-window win) (switch-to-buffer buf))
               (pop-to-buffer buf))))))))

  (treemacs-define-leaf-node agent-shell-buffer
    (treemacs-as-icon "  " 'face 'font-lock-keyword-face)
    :ret-action #'agent-shell--treemacs-visit
    :mouse1-action #'agent-shell--treemacs-visit-in-last-window
    :visit-action #'agent-shell--treemacs-visit)

  (defun agent-shell--treemacs-status (buf)
    "Return `busy', `pending', `permission', or `idle' for agent-shell BUF.
Reuses `agent-shell-attention''s own tracking rather than re-deriving it."
    (let ((entry (gethash buf agent-shell-attention--pending)))
      (cond
       ((and entry (agent-shell-attention--pending-entry-permission-p entry)) 'permission)
       (entry 'pending)
       ((agent-shell-attention--buffer-busy-p buf) 'busy)
       (t 'idle))))

  (defun agent-shell--treemacs-status-icon (buf)
    (pcase (agent-shell--treemacs-status buf)
      ('busy       (treemacs-as-icon "⚙️ "))
      ('pending    (treemacs-as-icon "💬 "))
      ('permission (treemacs-as-icon "🔒 "))
      (_           (treemacs-as-icon "  "))))

  (defun agent-shell--treemacs-base-name (buf)
    "Buffer name with the uniquify `<N>' disambiguator stripped."
    (replace-regexp-in-string "<[0-9]+>\\'" "" (buffer-name buf)))

  (defun agent-shell--treemacs-grouped-items ()
    "Group `(agent-shell--treemacs-buffers)' by common name prefix.
Returns a list where each element is either a lone buffer, or
\(:group BASE-NAME . BUFFERS) for names sharing a base with >1 member."
    (let ((groups (make-hash-table :test #'equal))
          (order nil))
      (dolist (buf (agent-shell--treemacs-buffers))
        (let ((base (agent-shell--treemacs-base-name buf)))
          (unless (gethash base groups) (push base order))
          (push buf (gethash base groups))))
      (mapcar (lambda (base)
                (let ((members (nreverse (gethash base groups))))
                  (if (cdr members)
                      (list :group base members)
                    (car members))))
              (nreverse order))))

  (treemacs-define-expandable-node agent-shell-group
    :icon-open (treemacs-as-icon "- " 'face 'font-lock-keyword-face)
    :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-keyword-face)
    :query-function (treemacs-button-get node :agent-shell-group-buffers)
    :render-action
    (treemacs-render-node
     :icon (agent-shell--treemacs-status-icon item)
     ;; The group's own label already shows the shared prefix; strip it
     ;; here so children just show their `<N>' differentiator. The one
     ;; member with no suffix (name == base) gets a placeholder instead
     ;; of the full name, which just repeats the group's own label.
     :label-form (let* ((full (buffer-name item))
                         (suffix (substring full (length (agent-shell--treemacs-base-name item)))))
                   (if (string-empty-p suffix) "•" suffix))
     :state treemacs-agent-shell-buffer-state
     :key-form (buffer-name item)
     :more-properties (:agent-shell-buffer item)))

  ;; treemacs-define-expandable-node has no :mouse1-action (that's only on
  ;; the leaf-node macro), so double-click on the group folder itself was
  ;; never wired -- matching how real directory nodes handle it.
  (treemacs-define-doubleclick-action treemacs-agent-shell-group-open-state #'treemacs-toggle-node)
  (treemacs-define-doubleclick-action treemacs-agent-shell-group-closed-state #'treemacs-toggle-node)

  (treemacs-define-expandable-node agent-shells
    :icon-open (treemacs-as-icon "- " 'face 'font-lock-keyword-face)
    :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-keyword-face)
    :query-function (agent-shell--treemacs-grouped-items)
    :render-action
    (if (and (consp item) (eq (car item) :group))
        (treemacs-render-node
         :icon treemacs-icon-agent-shell-group-closed
         :label-form (cadr item)
         :state treemacs-agent-shell-group-closed-state
         :key-form (cadr item)
         :more-properties (:agent-shell-group-buffers (caddr item)))
      (treemacs-render-node
       :icon (agent-shell--treemacs-status-icon item)
       :label-form (buffer-name item)
       :state treemacs-agent-shell-buffer-state
       :key-form (buffer-name item)
       :more-properties (:agent-shell-buffer item)))
    :top-level-marker t
    :root-label "Agent Shells"
    :root-face 'font-lock-keyword-face
    :root-key-form "Agent Shells")

  (treemacs-define-top-level-extension
   :extension #'treemacs-AGENT-SHELLS-extension
   :position 'top)

  ;; agent-shell-attention has no public "status changed" hook, and its own
  ;; state-mutating functions are numerous/private enough that hooking all
  ;; of them individually is fragile. Poll instead: cheap (a handful of
  ;; hash lookups) and only touches the buffer when the section is open.
  ;; ponytail: full collapse+expand redraw rather than patching icons in
  ;; place, since icon strings vary in length (emoji codepoint counts
  ;; differ), so a fixed-offset in-place patch would be wrong.
  ;;
  ;; Must go through the interactive `treemacs-collapse-agent-shells' /
  ;; `treemacs-expand-agent-shells' (which re-locate the node fresh via
  ;; `treemacs-node-at-point' each time) -- calling the low-level
  ;; `treemacs--do-collapse-agent-shells' / `treemacs--do-expand-agent-shells'
  ;; directly with a manually re-fetched dom marker duplicated the section
  ;; on every call, even from a freshly-rebuilt buffer.
  (defun agent-shell--treemacs-refresh-section ()
    (-when-let (win (treemacs-get-local-window))
      (with-selected-window win
        ;; Look up "Agent Shells" by dom key, not (point-min) -- it's no
        ;; longer guaranteed to be the first thing in the buffer now that
        ;; Open Buffers also registers at :position 'top (add-to-list
        ;; prepends, so whichever section registered later renders first).
        (-when-let (dom-node (treemacs-find-in-dom (list :custom "Agent Shells")))
          (-when-let (pos (treemacs-dom-node->position dom-node))
            (when (eq (treemacs-button-get pos :state) treemacs-agent-shells-open-state)
              (save-excursion
                (goto-char pos)
                (treemacs-collapse-agent-shells)
                (treemacs-expand-agent-shells)))))
        ;; hl-line-mode's own post-command-hook entry only re-anchors the
        ;; *selected* window's overlay, and treemacs is never selected
        ;; when you're focused on a split elsewhere -- so unlike
        ;; `treemacs-goto-file-node'/`treemacs-goto-extension-node' (which
        ;; call this themselves), the plain collapse+expand above leaves
        ;; hl-line's overlay stretched across whatever got deleted/
        ;; reinserted. Re-anchor it before deciding what else to do.
        (hl-line-highlight)
        ;; The collapse+expand above invalidates whatever point/overlay
        ;; was previously tracking, so re-sync to the MRU window's buffer
        ;; rather than trusting wherever point landed post-refresh (which
        ;; is wherever treemacs-expand-agent-shells happens to leave it --
        ;; e.g. the group's first child -- not necessarily what's focused).
        (let ((mru-buf (window-buffer (or (get-mru-window (selected-frame) nil :not-selected) win))))
          (if (with-current-buffer mru-buf (derived-mode-p 'agent-shell-mode))
              (agent-shell--treemacs-goto-buffer mru-buf)
            (agent-shell--treemacs-clear-if-agent-shell-overlay))))))

  (defvar agent-shell--treemacs-refresh-timer nil)
  (when agent-shell--treemacs-refresh-timer
    (cancel-timer agent-shell--treemacs-refresh-timer))
  (setq agent-shell--treemacs-refresh-timer
        (run-with-timer 3 3 #'agent-shell--treemacs-refresh-section))

  ;; Follow-mode's own file-follow only watches `buffer-file-name'/dired, so
  ;; it silently ignores agent-shell buffers. This mirrors its debounce
  ;; (`treemacs--follow-after-buffer-list-update') using treemacs's own
  ;; `treemacs-goto-extension-node' primitive for custom-extension follow,
  ;; then reuses the same label/marquee overlay file-follow already updates.
  (defvar agent-shell--treemacs-follow-timer nil)

  (defun agent-shell--treemacs-goto-buffer (buf)
    "Move point/overlay in the (already-selected) treemacs window to BUF,
expanding/refreshing the section/group as needed."
    (when (treemacs--custom-top-level-in-dom-p (list :custom "Agent Shells"))
      (let* ((name (buffer-name buf))
             (base (agent-shell--treemacs-base-name buf))
             (grouped (> (length (seq-filter (lambda (b) (equal (agent-shell--treemacs-base-name b) base))
                                              (agent-shell--treemacs-buffers)))
                         1))
             (path (if grouped
                       (list :custom "Agent Shells" base name)
                     (list :custom "Agent Shells" name)))
             (root-immediate-child (if grouped (list :custom "Agent Shells" base) path)))
        ;; goto-extension-node's auto-expand fallback assumes directory-node
        ;; semantics and doesn't know how to open our custom section/group
        ;; nodes, so expand/refresh them explicitly first.
        (treemacs--ensure-custom-node-visible
         (list :custom "Agent Shells") root-immediate-child
         treemacs-agent-shells-open-state treemacs-agent-shells-closed-state
         #'treemacs-expand-agent-shells #'treemacs-collapse-agent-shells)
        (when grouped
          (treemacs--ensure-custom-node-visible
           (list :custom "Agent Shells" base) path
           treemacs-agent-shell-group-open-state treemacs-agent-shell-group-closed-state
           #'treemacs-expand-agent-shell-group #'treemacs-collapse-agent-shell-group))
        (treemacs-goto-extension-node path)
        (treemacs--update-selected-label-overlay))))

  (defun agent-shell--treemacs-clear-if-agent-shell-overlay ()
    "Clear the agent-shell section's tracked overlay.
Unlike files, an agent-shell isn't \"current\" once you're no longer
looking at it -- files/open-buffers keep their own independent slot
and are untouched by this."
    (treemacs--clear-tracked-overlay 'agent-shell))

  (defun agent-shell--treemacs-follow-now ()
    (setq agent-shell--treemacs-follow-timer nil)
    ;; Capture the mode/buffer BEFORE selecting the treemacs window --
    ;; with-selected-window changes current-buffer to treemacs itself.
    (let ((is-agent-shell (derived-mode-p 'agent-shell-mode))
          (buf (current-buffer)))
      (-when-let (win (treemacs-get-local-window))
        (with-selected-window win
          (if is-agent-shell
              (agent-shell--treemacs-goto-buffer buf)
            (agent-shell--treemacs-clear-if-agent-shell-overlay))))))

  (defun agent-shell--treemacs-follow ()
    ;; run-with-timer (not run-with-idle-timer): an idle timer only fires
    ;; after Emacs is continuously idle for the delay, which never happens
    ;; if you're actively typing into the agent-shell you just switched to.
    ;; A plain zero-delay timer still defers off the current call stack
    ;; (buffer-list-update-hook isn't a safe place to select windows from
    ;; synchronously) without waiting on activity to stop.
    (unless agent-shell--treemacs-follow-timer
      (setq agent-shell--treemacs-follow-timer
            (run-with-timer 0 nil #'agent-shell--treemacs-follow-now))))

  (add-hook 'buffer-list-update-hook #'agent-shell--treemacs-follow))

(provide 'init-ai)
;;; init-ai.el ends here
