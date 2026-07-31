;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Fallback xref backend: when current buffer has no eglot server (e.g.
;; agent-shell, markdown), proxy through any active C eglot session using
;; workspace/symbol -- which is identifier-based and needs no cursor position,
;; unlike textDocument/definition which eglot falls back to at point.
(defun my/eglot-any-c-buffer ()
  "Return an eglot-managed C buffer (MRU order), or nil."
  (seq-find (lambda (buf)
              (with-current-buffer buf
                (and (eglot-current-server)
                     (derived-mode-p 'c-mode 'c-ts-mode 'c++-mode 'c++-ts-mode))))
            (buffer-list)))

(defun my/eglot-fallback-xref-backend ()
  (when (and (not (eglot-current-server)) (my/eglot-any-c-buffer))
    'my-eglot-any))

(cl-defmethod xref-backend-definitions ((_backend (eql my-eglot-any)) identifier)
  (when-let ((buf (my/eglot-any-c-buffer)))
    (with-current-buffer buf
      (xref-backend-apropos 'eglot identifier))))

(cl-defmethod xref-backend-references ((_backend (eql my-eglot-any)) identifier)
  (when-let ((buf (my/eglot-any-c-buffer)))
    (with-current-buffer buf
      (xref-backend-references 'eglot identifier))))

(cl-defmethod xref-backend-apropos ((_backend (eql my-eglot-any)) pattern)
  (when-let ((buf (my/eglot-any-c-buffer)))
    (with-current-buffer buf
      (xref-backend-apropos 'eglot pattern))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql my-eglot-any)))
  (thing-at-point 'symbol t))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql my-eglot-any)))
  (when-let ((buf (my/eglot-any-c-buffer)))
    (with-current-buffer buf
      (xref-backend-identifier-completion-table 'eglot))))

(add-hook 'xref-backend-functions #'my/eglot-fallback-xref-backend t)

;; etags--xref-backend always returns 'etags regardless of whether a TAGS
;; file exists, causing a hard error when xref falls back to it. Advise it
;; to abstain (return nil) when no TAGS file is reachable, so eglot's "no
;; definition found" message appears instead of a TAGS file error.
(with-eval-after-load 'etags
  (advice-add 'etags--xref-backend :around
              (lambda (orig)
                (when (or tags-file-name
                          (and (buffer-file-name)
                               (locate-dominating-file (buffer-file-name) "TAGS")))
                  (funcall orig)))))

(use-package eglot
  :ensure t
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0)
  (eglot-send-changes-idle-time 0.5)
  :config
  (setq eglot-stay-out-of '())
  (add-to-list 'eglot-server-programs
               '((html-mode html-ts-mode mhtml-mode) . ("vscode-html-language-server" "--stdio")))
  :init
  (defun my/eglot-ensure-maybe ()
    (unless (or (buffer-base-buffer)
                (derived-mode-p
                  'emacs-lisp-mode
                  'hcl-mode
                  'makefile-mode
                  'clojure-ts-mode
                  'html-mode
                  'html-ts-mode
                  'mhtml-mode))
      (eglot-ensure)))
  (add-hook 'prog-mode-hook #'my/eglot-ensure-maybe)
  :bind
  (:map eglot-mode-map
    ("C-c l a" . eglot-code-actions)
    ("C-c l r" . eglot-rename)
    ("C-c l f" . eglot-format)
    ("C-c l d" . eldoc)
    ("C-c l h" . eldoc-doc-buffer)
    ("C-c l o" . eglot-code-action-organize-imports)
    ("C-c l i" . eglot-inlay-hints-mode)
    ("C-c l c" . eglot-show-call-hierarchy)
    ("C-c l t" . eglot-show-type-hierarchy)))

(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind
  (:map eglot-mode-map
    ("C-c l s" . consult-eglot)))

(provide 'init-lsp)
;;; init-lsp.el ends here
