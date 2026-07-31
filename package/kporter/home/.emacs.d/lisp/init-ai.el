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
            (expand-file-name subdir session-dir))))
  ;; M-left (better-jumper-jump-backward) uses per-window jump lists. Switching
  ;; INTO an agent-shell window records the jump in the source window's list,
  ;; leaving the agent-shell window's list empty. Fix: after any navigation
  ;; that lands in an agent-shell window, record the origin in THIS window's
  ;; list so M-left can navigate back.
  (defun my-record-jump-into-agent-shell (orig &rest args)
    (let ((src-buf (current-buffer))
          (src-pos (point)))
      (apply orig args)
      (when (derived-mode-p 'agent-shell-mode)
        (with-current-buffer src-buf
          (better-jumper-set-jump src-pos)))))
  (advice-add 'agent-shell-toggle :around #'my-record-jump-into-agent-shell)
  (advice-add 'switch-to-buffer   :around #'my-record-jump-into-agent-shell)
  (advice-add 'pop-to-buffer      :around #'my-record-jump-into-agent-shell))

;; agent-shell output regions are marked agent-shell-markdown-frozen which
;; blocks jit-lock, so font-lock keywords never fire there. Apply link-face
;; overlays instead, with mouse-1 click handlers, scanning the full buffer
;; after each output chunk (debounced via idle timer).
(defvar aerospike--link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'aerospike--link-mouse-follow)
    (define-key map [mouse-2] #'aerospike--link-mouse-follow)
    map)
  "Keymap applied to aerospike link overlays.")

(defun aerospike--link-mouse-follow (event)
  (interactive "e")
  (let* ((pos (posn-point (event-start event)))
         (ov  (cl-find-if (lambda (o) (overlay-get o 'aerospike-link-action))
                          (overlays-at pos))))
    (when ov (funcall (overlay-get ov 'aerospike-link-action)))))

(defvar-local aerospike--link-overlay-timer nil)

(defun aerospike--link-overlays-apply (buf)
  "Apply clickable link overlays for Jira tickets and relative paths throughout BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq aerospike--link-overlay-timer nil)
      ;; Jira tickets
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward aerospike-jira-ticket-re nil t)
          (let* ((mbeg   (match-beginning 0))
                 (mend   (match-end 0))
                 (ticket (match-string 0)))
            (unless (cl-some (lambda (o) (overlay-get o 'aerospike-link))
                             (overlays-in mbeg mend))
              (let ((action (let ((u (concat "https://aerospike.atlassian.net/browse/" ticket)))
                              (lambda () (browse-url u))))
                    (ov (make-overlay mbeg mend)))
                (overlay-put ov 'face 'link)
                (overlay-put ov 'mouse-face 'highlight)
                (overlay-put ov 'help-echo (concat "mouse-1: open " ticket " in browser"))
                (overlay-put ov 'keymap aerospike--link-keymap)
                (overlay-put ov 'aerospike-link t)
                (overlay-put ov 'aerospike-link-action action)
                (overlay-put ov 'priority 10))))))
      ;; Relative file paths
      (let ((cwd (if (derived-mode-p 'agent-shell-mode) (agent-shell-cwd) default-directory)))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward aerospike-relative-path-re nil t)
            (let* ((mbeg (match-beginning 1))
                   (mend (match-end 1))
                   (rel  (match-string 1))
                   (full (expand-file-name rel cwd)))
              (when (and (file-exists-p full)
                         (not (cl-some (lambda (o) (overlay-get o 'aerospike-link))
                                       (overlays-in mbeg mend))))
                (let ((action (let ((f full)) (lambda () (find-file f))))
                      (ov (make-overlay mbeg mend)))
                  (overlay-put ov 'face 'link)
                  (overlay-put ov 'mouse-face 'highlight)
                  (overlay-put ov 'help-echo (concat "mouse-1: open " full))
                  (overlay-put ov 'keymap aerospike--link-keymap)
                  (overlay-put ov 'aerospike-link t)
                  (overlay-put ov 'aerospike-link-action action)
                  (overlay-put ov 'priority 10))))))))))

(defun aerospike--link-overlays-schedule ()
  (when aerospike--link-overlay-timer
    (cancel-timer aerospike--link-overlay-timer))
  (setq aerospike--link-overlay-timer
        (run-with-idle-timer 0.5 nil #'aerospike--link-overlays-apply (current-buffer))))

(add-hook 'agent-shell-mode-hook
          (lambda ()
            (add-hook 'comint-output-filter-functions
                      (lambda (_) (aerospike--link-overlays-schedule))
                      nil t)))
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
      ('busy       (treemacs-as-icon (concat (aref agent-shell--spinner-frames agent-shell--spinner-index) " ") 'face '(:height 1.5)))
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
  ;; --- Self-heal deadlock fix (respect deliberate collapse) ---
  ;; A collapsed top-level section leaves `treemacs-dom', so the periodic
  ;; refresh -- which locates the section via the dom -- could never reopen
  ;; it once collapsed. It now reopens a collapsed section on the next tick,
  ;; UNLESS the user collapsed it themselves. Track that intent by advising
  ;; the interactive expand/collapse commands (user TAB/RET routes through
  ;; them), suppressing the flag around our own programmatic calls.
  (defvar agent-shell--treemacs-user-collapsed nil
    "Non-nil once the user deliberately collapses the Agent Shells section.
Suppresses the periodic refresh's auto-reopen until they expand it again.")
  (defvar agent-shell--treemacs-suppress-intent nil
    "Bound non-nil around programmatic expand/collapse so the intent-tracking
advice ignores those internal calls.")

  (defun agent-shell--treemacs-note-user-collapse (&rest _)
    (unless agent-shell--treemacs-suppress-intent
      (setq agent-shell--treemacs-user-collapsed t)))
  (defun agent-shell--treemacs-note-user-expand (&rest _)
    (unless agent-shell--treemacs-suppress-intent
      (setq agent-shell--treemacs-user-collapsed nil)))
  (advice-add 'treemacs-collapse-agent-shells :before
              #'agent-shell--treemacs-note-user-collapse)
  (advice-add 'treemacs-expand-agent-shells :before
              #'agent-shell--treemacs-note-user-expand)

  (defun agent-shell--treemacs-section-pos ()
    "Buffer position of the top-level \"Agent Shells\" header button, or nil.
Found by scan (not the dom) so it resolves even while the section is
collapsed and therefore absent from `treemacs-dom'."
    (save-excursion
      (goto-char (point-min))
      ;; count-current t on the FIRST call (catch a button at point-min);
      ;; plain next-button on each step so pos always advances strictly --
      ;; next-button with count-current would return the same position and
      ;; spin forever.
      (let ((pos (next-button (point-min) t))
            found)
        (while (and pos (not found))
          (if (equal (treemacs-button-get pos :key) "Agent Shells")
              (setq found pos)
            (setq pos (next-button pos))))
        found)))

  (defun agent-shell--treemacs-refresh-section ()
    (-when-let (win (treemacs-get-local-window))
      (let* ((orig-win (selected-window))
             (in-treemacs (eq orig-win win))
             (focused-buf (window-buffer orig-win)))
      ;; The collapse+expand below deletes and reinserts the whole section,
      ;; which stretches hl-line across every shell and resets point. That's
      ;; fine when the user is elsewhere, but disruptive when they're inside
      ;; the tree selecting a shell -- so skip the entire refresh then. The
      ;; spinner just pauses until they leave the tree.
      (unless in-treemacs
      (with-selected-window win
        ;; The collapse+expand rewrites the section at the top of the buffer
        ;; every spinner tick (0.3s while a shell is busy). That shifts text
        ;; above the user's scrolled window-start, invalidating it, so redisplay
        ;; recomputes and walks the window up to keep its point visible. Anchor
        ;; window-start with a marker (survives the edits) and drop point onto
        ;; the top visible line at the end, so redisplay has nothing to chase.
        (let ((ws (copy-marker (window-start))))
        ;; Look up "Agent Shells" by dom key, not (point-min) -- it's no
        ;; longer guaranteed to be the first thing in the buffer now that
        ;; Open Buffers also registers at :position 'top (add-to-list
        ;; prepends, so whichever section registered later renders first).
        ;; But that key is only in the dom while the section is EXPANDED --
        ;; a collapsed top-level section drops out of the dom entirely, so a
        ;; dom-only lookup could never reopen it (the self-heal deadlock:
        ;; whatever transiently collapses the section leaves it stuck). So
        ;; handle both states below.
        (let* ((dom-node (treemacs-find-in-dom (list :custom "Agent Shells")))
               (pos (and dom-node (treemacs-dom-node->position dom-node))))
          (cond
           ;; Open and in the dom: redraw in place to refresh icons/grouping.
           ((and pos (eq (treemacs-button-get pos :state)
                         treemacs-agent-shells-open-state))
            (let ((agent-shell--treemacs-suppress-intent t))
              (save-excursion
                (goto-char pos)
                (treemacs-collapse-agent-shells)
                (treemacs-expand-agent-shells))))
           ;; Collapsed/absent, NOT collapsed on purpose, and there are
           ;; shells to show: reopen it. The header button is still in the
           ;; buffer even while collapsed (just not in the dom), so locate
           ;; it by scan. This is what breaks the deadlock.
           ((and (not agent-shell--treemacs-user-collapsed)
                 (agent-shell--treemacs-buffers))
            (-when-let (hpos (agent-shell--treemacs-section-pos))
              (when (eq (treemacs-button-get hpos :state)
                        treemacs-agent-shells-closed-state)
                (let ((agent-shell--treemacs-suppress-intent t))
                  (save-excursion
                    (goto-char hpos)
                    (treemacs-expand-agent-shells))))))))
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
        (if (with-current-buffer focused-buf (derived-mode-p 'agent-shell-mode))
            (agent-shell--treemacs-goto-buffer focused-buf)
          (agent-shell--treemacs-clear-if-agent-shell-overlay))
        (set-window-start win ws)
        (goto-char ws)
        (set-marker ws nil)))))))

  (defvar agent-shell--spinner-frames ["✱" "✲" "✳" "✺" "✹" "✸" "✷"])
  (setq agent-shell--spinner-frames ["✱" "✲" "✳" "✺" "✹" "✸" "✷"])
  (defvar agent-shell--spinner-index 0)
  (defvar agent-shell--treemacs-spinner-timer nil)

  (defvar agent-shell--treemacs-refresh-timer nil)
  (when agent-shell--treemacs-refresh-timer
    (cancel-timer agent-shell--treemacs-refresh-timer))
  (setq agent-shell--treemacs-refresh-timer
        (run-with-timer 3 3 #'agent-shell--treemacs-refresh-section))

  (defun agent-shell--treemacs-tick-spinner ()
    (setq agent-shell--spinner-index
          (% (1+ agent-shell--spinner-index) (length agent-shell--spinner-frames)))
    (when (seq-some (lambda (b) (eq (agent-shell--treemacs-status b) 'busy))
                    (agent-shell--treemacs-buffers))
      (agent-shell--treemacs-refresh-section)))

  (when agent-shell--treemacs-spinner-timer
    (cancel-timer agent-shell--treemacs-spinner-timer))
  (setq agent-shell--treemacs-spinner-timer
        (run-with-timer 0.3 0.3 #'agent-shell--treemacs-tick-spinner))

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
             (root-immediate-child (if grouped (list :custom "Agent Shells" base) path))
             ;; This runs only when treemacs is NOT the selected window (a
             ;; background follow driven by agent-shell output). Moving point
             ;; to the node and letting treemacs recenter would yank the user's
             ;; scroll to the top every time the shell emits output. The node's
             ;; highlight is a standalone overlay, so after updating it we drop
             ;; point back onto the top visible line and restore window-start --
             ;; point must land inside the viewport or redisplay re-scrolls to
             ;; reveal it, defeating the restore.
             (twin (selected-window))
             (ws (copy-marker (window-start twin))))
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
        (treemacs--update-selected-label-overlay)
        (set-window-start twin ws)
        (goto-char ws)
        (set-marker ws nil))))

  (defun agent-shell--treemacs-clear-if-agent-shell-overlay ()
    "Clear the agent-shell section's tracked overlay.
Unlike files, an agent-shell isn't \"current\" once you're no longer
looking at it -- files/open-buffers keep their own independent slot
and are untouched by this."
    (treemacs--clear-tracked-overlay 'agent-shell))

  (defun agent-shell--treemacs-follow-now ()
    (setq agent-shell--treemacs-follow-timer nil)
    ;; Use the selected window's buffer, not current-buffer. buffer-list-update-hook
    ;; fires for background process output too (e.g. agent-shell writing output), and
    ;; by the time this 0-delay timer fires current-buffer may be an agent-shell buffer
    ;; that Emacs set internally -- not what the user is looking at. selected-window
    ;; always reflects the user's actual focus, unaffected by process I/O.
    (let* ((orig-win (selected-window))
           (focused-buf (window-buffer orig-win))
           (is-agent-shell (with-current-buffer focused-buf (derived-mode-p 'agent-shell-mode))))
      (-when-let (win (treemacs-get-local-window))
        ;; Don't touch the tree while the user is navigating it directly.
        (unless (eq orig-win win)
          (with-selected-window win
            (if is-agent-shell
                (agent-shell--treemacs-goto-buffer focused-buf)
              (agent-shell--treemacs-clear-if-agent-shell-overlay)))))))

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
