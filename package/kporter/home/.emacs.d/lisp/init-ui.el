;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package all-the-icons :ensure t :demand t)

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package smooth-scrolling
  :ensure t
  :init
  (smooth-scrolling-mode))

(use-package buffer-move
  :ensure t
  :bind
  ("C-M-W" . buf-move-up)
  ("C-M-S" . buf-move-down)
  ("C-M-A" . buf-move-left)
  ("C-M-D" . buf-move-right))

(use-package display-fill-column-indicator
  :ensure t
  :init
  (setq-default fill-column  80)
  :hook
  (prog-mode . display-fill-column-indicator-mode))

(use-package uniquify
  :ensure nil ;; Package doesn't actually exists - will slow emacs startup.
  :defer t
  :custom
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*")
  ;; (uniquify-buffer-name-style 'reverse)
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-strip-common-suffix t))

(use-package imenu-anywhere
  :ensure t
  :defer t
  :bind ("C-c C-SPC" . imenu-anywhere))

(use-package rainbow-mode
  :ensure t
  :diminish
  :init
  (rainbow-mode))

(use-package doom-modeline
  :ensure t
  :demand t
  :init
  ;; If non-nil, cause imenu to see `doom-modeline' declarations.
  ;; This is done by adjusting `lisp-imenu-generic-expression' to
  ;; include support for finding `doom-modeline-def-*' forms.
  ;; Must be set before loading doom-modeline.
  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-height 25)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  ;; Whether display the buffer encoding.
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-project-detection 'projectile)
  :config
  (set-face-attribute 'doom-modeline-evil-insert-state nil :foreground "orange")
  :hook (after-init . doom-modeline-mode))

(use-package winum
  :ensure t
  :init
  (winum-mode)
  :custom
  (winum-auto-setup-mode-line nil)
  :bind
  ("M-0" . winum-select-window-0-or-10)
  ("M-1" . winum-select-window-1)
  ("M-2" . winum-select-window-2)
  ("M-3" . winum-select-window-3)
  ("M-4" . winum-select-window-4)
  ("M-5" . winum-select-window-5)
  ("M-6" . winum-select-window-6)
  ("M-7" . winum-select-window-7)
  ("M-8" . winum-select-window-8))

(use-package winner
  :defer t
  :preface
  (defun winner-wrong-window ()
    "Open the last opened buffer in the other window."
    (interactive)
    (let* ((current (window-list))
           (previous (save-window-excursion (winner-undo) (window-list)))
           (window (seq-some (lambda (w) (not (memq w previous))) current))
           (buffer (window-buffer window)))
      (winner-undo)
      (switch-to-buffer-other-window buffer)))

  :config
  (winner-mode)
  :bind
  ("C-c [" . winner-undo)
  ("s-[" . winner-undo)
  ("C-c ]" . winner-redo)
  ("s-]" . winner-redo)
  ("C-c z" . winner-wrong-window))

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '(("~/projects/" . 3)))
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("C-c p" . projectile-command-map)))

(use-package dirvish ; dired replacement
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-hide-details nil)
  (setq dirvish-attributes
	'(all-the-icons collapse file-size file-time subtree-state vc-states))
  (setq dired-listing-switches
        "-l --all --human-readable --group-directories-first --no-group")
  :bind
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(use-package vterm :ensure t :defer t)

(use-package multi-vterm
  :ensure t
  :bind
  ("C-c t" . multi-vterm))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-log-arguments '("-n256" "--graph" "--decorate" "--color")
	magit-diff-refine-hunk t))

(use-package git-gutter+
  :ensure t
  :config
  (setq git-gutter+-disabled-modes '(org-mode))
  ;; Move between local changes
  (global-set-key (kbd "M-<up>") 'git-gutter+-previous-hunk)
  (global-set-key (kbd "M-<down>") 'git-gutter+-next-hunk)
  :hook
  (prog-mode . git-gutter+-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (custom-set-faces
   '(rainbow-delimiters-unmatched-face
     ((t (:background "red" :foreground "white")))))
  :hook
  ((prog-mode cider-repl-mode) . rainbow-delimiters-mode))

(use-package dumb-jump
  :ensure t
  :hook
  (xref-backend-functions . dumb-jump-xref-activate))

(use-package eldoc ; shows argument list of function call you are writing
  :ensure t
  :diminish
  :hook
  (prog-mode       . turn-on-eldoc-mode)
  (cider-repl-mode . turn-on-eldoc-mode))

(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-key)
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key))

(use-package marginalia
  :ensure t
  :defer t
  :init
  (marginalia-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; (setq vertico-scroll-margin 0)     ; Different scroll margin
  ;; (setq vertico-count 20)            ; Show more candidates
  ;; (setq vertico-resize t)            ; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-cycle t)             ; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  )

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ; Enable auto completion
  ;; (corfu-separator ?\s)          ; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  )

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind
  (;; C-c bindings in `mode-specific-map'
   ("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("C-x g" . consult-goto-line)             ;; goto-line - eclipse emacs
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings in `search-map'
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; The narrowing key.
  (setq consult-narrow-key "<") ;; "C-+"
  )

(use-package consult-dir
  :ensure t
  :after (consult)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq embark-verbose-indicator-display-action '(display-buffer-below-selected))
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :after (consult embark)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-ui.el)
;;; init-ui.el ends here
