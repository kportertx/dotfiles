;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package better-jumper
  ;; Better-jumper is configurable jump list implementation for Emacs that can be used
  ;; to easily jump back to previous locations. That provides optional integration with
  ;; evil.
  :ensure t
  :init
  (better-jumper-mode 1)
  :custom
  ;;(better-jumper-context 'window)
  ;;(better-jumper-new-window-behavior 'copy)
  ;;(better-jumper-max-length 100)
  (better-jumper-add-jump-behavior 'replace)
  :preface
  (defun my-jump-advice (oldfun &rest args)
    (let ((old-pos (point)))
      (apply oldfun args)
      (when (> (abs (- (line-number-at-pos old-pos)
                      (line-number-at-pos (point))))
              1)
        (better-jumper-set-jump old-pos))))
  :config
  ;; jump scenarios
  ;; use M-x view-lossage
  (advice-add 'vertico-exit :around #'my-jump-advice)         ; may handle jumps from M-x commands
  (advice-add 'exit-minibuffer :around #'my-jump-advice)      ; handles goto-line
  (advice-add 'mouse-set-point :around #'my-jump-advice)
  (advice-add 'xref-find-def :around #'my-jump-advice)        ; FIXME - working?
  (advice-add 'xref-find-references :around #'my-jump-advice) ; FIXME - working?
  :bind
  ("M-<left>" . 'better-jumper-jump-backward)
  ("M-<right>" . 'better-jumper-jump-forward))

(use-package centaur-tabs
  :ensure t
  :init
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-set-icon t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-modified-marker t)
  :bind
  ("C-c SPC p" . centaur-tabs-backward)
  ("C-c SPC n" . centaur-tabs-forward))

(use-package consult
  ;; Consult implements a set of `consult-<thing>' commands, which aim to
  ;; improve the way you use Emacs.  The commands are founded on
  ;; `completing-read', which selects from a list of candidate strings.
  ;; Consult provides an enhanced buffer switcher `consult-buffer' and
  ;; search and navigation commands like `consult-imenu' and
  ;; `consult-line'.  Searching through multiple files is supported by the
  ;; asynchronous `consult-grep' command.  Many Consult commands support
  ;; previewing candidates.  If a candidate is selected in the completion
  ;; view, the buffer shows the candidate immediately.
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
  :custom
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (register-preview-delay 0.5 register-preview-function #'consult-register-format)
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref xref-show-definitions-function #'consult-xref)
  ;; The narrowing key.
  (consult-narrow-key "<") ;; "C-+"
  :init
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
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
  )

(use-package consult-dir
  ;; Consult-dir implements commands to easily switch between "active"
  ;; directories. The directory candidates are collected from user bookmarks,
  ;; projectile project roots (if available), project.el project roots and recentf
  ;; file locations. The `default-directory' variable not changed in the process.
  :ensure t
  :after (consult)
  :bind
  (("C-x C-d" . consult-dir)
    :map vertico-map
    ("C-x C-d" . consult-dir)
    ("C-x C-j" . consult-dir-jump-file)))

(use-package corfu
  ;; Corfu enhances in-buffer completion with a small completion popup.
  ;; The current candidates are shown in a popup below or above the
  ;; point.  The candidates can be selected by moving up and down.
  ;; Corfu is the minimalistic in-buffer completion counterpart of the
  ;; Vertico minibuffer UI.
  :ensure t
  :init
  (global-corfu-mode)
  ;; Optional customizations
  :custom
  (corfu-auto t)                 ; Enable auto completion
  (corfu-cycle t)                ; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'valid)       ; Preselect the prompt
  ;; (corfu-separator ?\s)          ; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ; Disable current candidate preview
  ;; (corfu-on-exact-match nil)     ; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :bind
  (:map corfu-map
    ("TAB" . corfu-next)
    ([tab] . corfu-next)
    ("S-TAB" . corfu-previous)
    ([backtab] . corfu-previous)))

(use-package dirvish ; dired replacement
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-hide-details nil)
  (dirvish-attributes
    '(all-the-icons collapse file-size file-time subtree-state vc-states))
  (dired-listing-switches
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

(use-package eldoc
  ;; shows argument list of function call you are writing
  :ensure t
  :diminish
  :hook
  (prog-mode       . turn-on-eldoc-mode)
  (cider-repl-mode . turn-on-eldoc-mode))

(use-package embark
  ;; This package provides a sort of right-click contextual menu for
  ;; Emacs, accessed through the `embark-act' command (which you should
  ;; bind to a convenient key), offering you relevant actions to use on
  ;; a target determined by the context:
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
    ("M-." . embark-dwim)
    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :custom
  (embark-verbose-indicator-display-action '(display-buffer-below-selected))
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  :init
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
  ;; This package provides integration between Embark and Consult.  The package
  ;; will be loaded automatically by Embark.
  :ensure t ; only need to install it, embark loads it after consult if found
  :after (consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package helpful
  ;; Helpful is a replacement for *help* buffers that provides much more
  ;; contextual information.
  :ensure t
  :commands (helpful-callable helpful-variable helpful-key)
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key))

(use-package imenu-anywhere
  ;; `imenu-anywhere` provides navigation for imenu tags across all buffers that
  ;; satisfy grouping criteria. Available criteria include - all buffers with the
  ;; same major mode, same project buffers and user defined list of friendly mode
  ;; buffers.
  :ensure t
  :defer t
  :bind ("C-c C-SPC" . imenu-anywhere))

(use-package marginalia
  ;; Enrich existing commands with completion annotations
  :ensure t
  :defer t
  :init
  (marginalia-mode))

(use-package num3-mode
  :ensure t
  :init
  (global-num3-mode))

(use-package orderless
  ;; This package provides an `orderless' completion style that divides
  ;; the pattern into components (space-separated by default), and
  ;; matches candidates that match all of the components in any order.
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic partial-completion emacs22))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package indent-bars
  ;; indent-bars highlights indentation with configurable font-lock
  ;; based vertical bars, using stipples.  The color and appearance
  :ensure t
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.15))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.3)
  (indent-bars-pad-frac 0.3)
  (indent-bars-zigzag nil)
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)) ; blend=1: blend with BG only
  (indent-bars-highlight-current-depth '(:blend 0.5)) ; pump up the BG blend on current
  (indent-bars-display-on-blank-lines t)
  :hook
  (prog-mode . indent-bars-mode))

(use-package rainbow-delimiters
  ;; Rainbow-delimiters is a "rainbow parentheses"-like mode which highlights
  ;; parentheses, brackets, and braces according to their depth.
  :ensure t
  :config
  (custom-set-faces
    '(rainbow-delimiters-unmatched-face
       ((t (:background "red" :foreground "white")))))
  :hook
  ((prog-mode cider-repl-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  ;; This minor mode sets background color to strings that match color
  ;; names, e.g. #0000ff is displayed in white with a blue background.
  :ensure t
  :diminish
  :init
  (rainbow-mode))

(use-package recentf
  ;; This package maintains a list of recently opened files and makes it
  ;; easy to visit them.  The recent files list is automatically saved
  ;; across Emacs sessions.
  :defer t
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 1000)
  (recentf-auto-cleanup 'never)
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
          (file-directory-p dired-directory)
          (not (string= "/" dired-directory)))
      (let ((last-idx (1- (length dired-directory))))
        (recentf-add-file
          (if (= ?/ (aref dired-directory last-idx))
            (substring dired-directory 0 last-idx)
            dired-directory)))))
  :config
  (recentf-mode)
  :bind
  ("C-x C-r" . recentf)
  :hook
  (dired-mode-hook . recentf-add-dired-directory))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (treemacs-git-mode 'deferred)
  (treemacs-git-commit-diff-mode t)
  :bind
  (:map global-map
    ("M-0"       . treemacs-select-window)
    ("C-x t 1"   . treemacs-delete-other-windows)
    ("C-x t t"   . treemacs)
    ("C-x t d"   . treemacs-select-directory)
    ("C-x t B"   . treemacs-bookmark)
    ("C-x t C-t" . treemacs-find-file)
    ("C-x t M-t" . treemacs-find-tag)))

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

(use-package vertico
  ;; Vertico provides a performant and minimalistic vertical completion UI
  ;; based on the default completion system.  By reusing the built-in
  ;; facilities, Vertico achieves full compatibility with built-in Emacs
  ;; completion commands and completion tables.
  :ensure t
  :init
  (vertico-mode)
  :custom
  ;; (vertico-scroll-margin 0)     ; Different scroll margin
  ;; (vertico-count 20)            ; Show more candidates
  ;; (vertico-resize t)            ; Grow and shrink the Vertico minibuffer
  (vertico-cycle t)                ; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  )

(use-package winum
  ;; Window numbers for Emacs: Navigate your windows and frames using numbers.
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

;;;;; Useful to switch between window / file layouts.
;; C-x r w ;; save a layout
;; C-x r j ;; load a layout

(provide 'init-ui.el)
;;; init-ui.el ends here
