;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package aggressive-indent
  ;; `aggressive-indent-mode' is a minor mode that keeps your code always
  ;; indented.  It reindents after every change, making it more reliable
  ;; than `electric-indent-mode'.
  :ensure t
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package crux
  :ensure t
  :bind
  ("C-a" . crux-move-beginning-of-line)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
  ("C-S-<return>" . crux-smart-open-line-above)
  ("S-<return>" . crux-smart-open-line)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c R" . crux-rename-file-and-buffer))

(use-package easy-kill
  :ensure t
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp] . easy-mark))

(use-package avy
  ;; With Avy, you can move point to any position in Emacs – even in a
  ;; different window – using very few keystrokes.
  :ensure t
  :bind
  ("C-:" . avy-goto-char))

(use-package display-fill-column-indicator
  :ensure nil
  :hook
  (prog-mode . display-fill-column-indicator-mode))

;; (use-package dumb-jump
;;   ;; Dumb Jump is an Emacs "jump to definition" package with support for 50+
;;   ;; programming languages that favors "just working" over speed or accuracy.
;;   ;; This means minimal -- and ideally zero -- configuration with absolutely no
;;   ;; stored indexes (TAGS) or persistent background processes.
;;   :ensure t
;;   :hook
;;   (xref-backend-functions . dumb-jump-xref-activate))

(use-package expand-region
  ;; Expand region increases the selected region by semantic units. Just keep
  ;; pressing the key until it selects what you want.
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package hl-todo
  ;; Highlight TODO and similar keywords in comments and strings.
  :ensure t
  :custom
  (hl-todo-keyword-faces
    '(("TODO" . "magenta")
       ("FIXME" . "magenta")
       ("WIP" . "lime green")
       ("WORK" . "lime green")
       ("NEXT" . "lime green")
       ("NOTE" . "purple")
       ("WAIT" . "orange")
       ("HACK" . "orange")
       ("TEMP" . "orange")
       ("XXX+" . "orange")
       ("DONE" . "gray")))
  :hook
  (prog-mode . hl-todo-mode))

(use-package hungry-delete
  ;; cc-mode implements hungry deletion for its programming modes.  This
  ;; package borrows its implementation in a minor mode, so that hungry
  ;; deletion can be used in all modes.
  :ensure t
  :bind
  ("C-c SPC t h" . hungry-delete-mode))

(use-package iedit
  ;; This package includes Emacs minor modes (iedit-mode and iedit-rectangle-mode)
  ;; based on a API library (iedit-lib) and allows you to alter one occurrence of
  ;; some text in a buffer (possibly narrowed) or region, and simultaneously have
  ;; other occurrences changed in the same way, with visual feedback as you type.
  :ensure t
  :config
  (set-face-background 'iedit-occurrence "blue")
  :bind
  ("C-;" . iedit-mode)) ; select all occurnces at-point for edit

(use-package pcre2el
  :ensure t
  :defer t)

(use-package subword
  ;; Treating terms in CamelCase symbols as separate words makes editing.
  :ensure nil
  :demand t
  :init (global-subword-mode 1))

(use-package sudo-edit
  ;; This package allows to open files as another user, by default "root":
  :ensure t
  :init
  (sudo-edit-indicator-mode)
  :bind
  ("C-c C-r" . sudo-edit))

(use-package visual-regexp-steroids
  ;; visual-regexp-steroids is an extension to visual-regexp which enables the
  ;; use of modern regexp engines (no more escaped group parentheses, and other
  ;; goodies!). In addition to that, you can optionally use the better regexp
  ;; syntax to power isearch-forward-regexp and isearch-backward-regexp.
  :ensure t
  :custom
  (vr/engine 'pcre2el "Use PCRE regular expressions")
  :bind
  ("C-c r" . vr/replace)
  ("C-c q" . vr/query-replace)
  ("C-r"   . vr/isearch-backward)
  ("C-S-s" . vr/isearch-forward)
  ("C-M-s" . isearch-forward)
  ("C-M-r" . isearch-backward))

(use-package vundo
  ;; Vundo (visual undo) displays the undo history as a tree and lets you
  ;; move in the tree to go back to previous buffer states. To use vundo,
  ;; type M-x vundo RET in the buffer you want to undo. An undo tree buffer
  ;; should pop up.
  :ensure t
  :bind
  ("C-c u" . vundo))

(use-package hyperbole
  :ensure t
  :demand t
  :init
  (hyperbole-mode 1)
  :config
  ;; Bare Jira ticket tags like SERVER-971 or AER-6936 open in the browser.
  ;; Full https://aerospike.atlassian.net/browse/... URLs are already handled
  ;; by hyperbole's built-in URL ibtype.
  (defconst aerospike-jira-ticket-re
    "\\b\\(AER\\|DOCS\\|SERVER\\|QE\\|SECOPS\\|TOOLS\\|CLIENT\\)-[0-9]+"
    "Regex matching Aerospike Jira ticket tags.")
  (defib aerospike-jira-ticket ()
    "Open Aerospike Jira ticket at point (e.g. SERVER-971, AER-6936)."
    (unless (derived-mode-p 'treemacs-mode)
      (when (thing-at-point-looking-at aerospike-jira-ticket-re 50)
        (ibut:label-set (match-string 0) (match-beginning 0) (match-end 0))
        (hact 'browse-url
              (concat "https://aerospike.atlassian.net/browse/" (match-string 0))))))
  ;; Relative file paths like as/include/fabric/partition.h (optionally :LINE)
  ;; in agent-shell or any buffer, resolved against agent-shell-cwd or
  ;; default-directory. Only activates when the resolved file actually exists.
  (defconst aerospike-relative-path-re
    ;; Leading [^/...] prevents matching substrings of absolute paths
    ;; (e.g. `home/...' inside `/home/...').  \b after the extension
    ;; prevents `.h' matching inside `.html'.  Longer extensions before
    ;; shorter in the alternation so `html?' wins over `hh?'.
    (concat "\\(?:^\\|[^/[:alnum:]_.-]\\)"
            "\\([[:alnum:]_.][[:alnum:]_.-]*/[[:alnum:]_./-]+"
            "\\.\\(?:html?\\|txt\\|log\\|md\\|cpp\\|cc\\|hh?\\|c\\)\\)"
            "\\b"
            "\\(?::\\([0-9]+\\)\\)?")
    "Regex for relative file paths with recognized extensions, optionally :LINE.
Group 1 = path, group 2 = line number.")
  (defib aerospike-relative-path ()
    "Open relative file path at point resolved from the buffer's working directory."
    (unless (derived-mode-p 'treemacs-mode)
      (when (thing-at-point-looking-at aerospike-relative-path-re 300)
      (let* ((rel  (match-string 1))
             (line (when (match-string 2) (string-to-number (match-string 2))))
             (base (if (derived-mode-p 'agent-shell-mode)
                       (agent-shell-cwd)
                     default-directory))
             (full (expand-file-name rel base)))
        (when (file-exists-p full)
          (ibut:label-set rel (match-beginning 1) (match-end 1))
          (if line
              (hact 'link-to-file-line full line)
            (hact 'link-to-file full)))))))
  ;; Left click follows an implicit button.  Anywhere else it is plain
  ;; `mouse-set-point'.  Mode-local mouse-1 bindings (treemacs, magit...)
  ;; still win over this global one.
  (defun my-hyperbole-click (event)
    "Set point at EVENT, then follow the Hyperbole implicit button there."
    (interactive "e")
    (mouse-set-point event)
    (when (and (not (region-active-p)) (ibut:at-p))
      (action-key)))
  (global-set-key [mouse-1] #'my-hyperbole-click)
  ;; Visual link highlighting for Jira tickets and relative file paths.
  (defun aerospike-jira-ticket--fontify ()
    (unless (derived-mode-p 'treemacs-mode 'special-mode)
    ;; Remove any stale relative-path keywords (old regex used a-zA-Z0-9 char
    ;; class; new one uses [:alnum:]).  font-lock-remove-keywords equality
    ;; comparison is fragile, so filter by unique substring instead.
    (setq-local font-lock-keywords
                (cl-remove-if
                 (lambda (kw)
                   (and (consp kw) (stringp (car kw))
                        (string-match-p "a-zA-Z0-9.*/" (car kw))))
                 font-lock-keywords))
    (font-lock-add-keywords
     nil
     `((,aerospike-jira-ticket-re 0 'link t)
       (,aerospike-relative-path-re 1 'link t))
     'append)))
  (add-hook 'font-lock-mode-hook #'aerospike-jira-ticket--fontify))

(defun my-open-in-file-browser ()
  "Open the current file's directory in the system file browser."
  (interactive)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               default-directory)))
    (call-process "xdg-open" nil 0 nil dir)))

(defun my-open-buffer-in-browser ()
  "Open the current buffer in the default browser."
  (interactive)
  (browse-url-of-buffer))

(provide 'init-editing)
;;; init-editing.el ends here
