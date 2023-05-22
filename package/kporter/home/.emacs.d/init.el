;;; Package --- Summary
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;
;; Use Package ;;
;;;;;;;;;;;;;;;;;

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)

(use-package use-package
  :config
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t))

(setq use-package-compute-statistics t) ;; for profiling, M-x use-package-report

(use-package diminish :ensure t :after use-package) ;; if you use :diminish
(use-package bind-key :ensure t :after use-package) ;; if you use any :bind variant
(use-package delight :ensure t :after use-package)  ;; Use delighting for modes

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; Disable deprecation warnings about cl. The cl library has been deprecated,
;; but lots of packages still use it.
(setq byte-compile-warnings '(cl-functions))

;; If an Emacs package relies on the installation of a system package,
;; install that package.
(use-package use-package-ensure-system-package
  :demand t
  :custom
  (system-packages-package-manager 'apt))

;; Don’t pop up a buffer to warn me about deprecations and other minor issues.
(setq warning-minimum-level :emergency)

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

(setq custom-safe-themes t) ; warning will not go away
;; TODO - Try https://github.com/catppuccin/emacs
(use-package color-theme-sanityinc-tomorrow
  :init
  (load-theme 'sanityinc-tomorrow-eighties))


;;;;;;;;;;;;;;
;; Defaults ;;
;;;;;;;;;;;;;;

(use-package emacs
  :defer nil
  :init
  ;; Ensure we are always using UTF-8 encoding.
  (set-language-environment "UTF-8")
  (set-charset-priority 'unicode)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  (show-paren-mode t)                           ; Visually indicates pair of matching parentheses
  (delete-selection-mode t)                     ; Start writing straight after deletion
  (put 'narrow-to-region 'disabled nil)	        ; Allows narrowing bound to C-x n n (region) and C-x n w (widen)
  (global-hl-line-mode 0)			; Highlight the current line to make it more visible
  (pixel-scroll-precision-mode)	                ; Precision scrolling
  (column-number-mode)                          ; Display column (and line) number in mode line
  (global-display-line-numbers-mode)		; Display line numbers in every buffer
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  ;; (dolist  (mapping '((python-mode . python-ts-mode)
  ;; 		      (ruby-mode . ruby-ts-mode)
  ;; 		      (c-mode . c-ts-mode)
  ;; 		      (c++-mode . c++-ts-mode)
  ;; 		      (c-or-c++-mode . c-or-c++-ts-mode)
  ;; 		      (css-mode . css-ts-mode)
  ;; 		      (js-mode . js-ts-mode)
  ;; 		      (javascript-mode . js-ts-mode)
  ;; 		      (typescript-mode . tsx-ts-mode)
  ;; 		      (js-json-mode . json-ts-mode)
  ;; 		      (sh-mode . bash-ts-mode)))
  ;; (add-to-list 'major-mode-remap-alist mapping))
  :bind
  ("M-_" . undo-redo)
  :custom
  (setq-default cursor-type 'bar)           ; Line-style cursor similar to other text editors
  (setq-default frame-title-format '("%b")) ; Make window title the buffer name
  (setq-default fill-column 80)		    ; Set fill column to 80 rather than 70, in all cases.
  (initial-major-mode 'fundamental-mode)    ; No need to have an Elisp buffer when starting up
  (inhibit-startup-screen t)                ; Disable startup screen
  (confirm-kill-processes nil)              ; Stop confirming the killing of processes
  (use-short-answers t)                     ; y-or-n-p makes answering questions faster
  (read-process-output-max (* 1024 1024))   ; Increase the amount of data which Emacs reads from the process
  (create-lockfiles nil)                    ; lock files kill `npm start'
  (completion--cycle-threshold 3)           ; TAB cycle if there are only few candidates
  (tab-always-indent 'complete)             ; Enable indentation+completion using the TAB key.
  (visible-bell nil)
  (enable-recursive-minibuffers t)

  ;; Persist Emacs session data.
  ;; Store all backup and autosave files in their own directory since it is bad to
  ;; clutter project directories. This also backs up TRAMP files locally.
  (backup-directory-alist
   `(("." . ,(concat user-emacs-directory "backups"))))
  ;; Automatic backup file housekeeping.
  (kept-new-versions 10)
  (kept-old-versions 4)
  (delete-old-versions t)
  (backup-by-copying t)             ; Don't clobber symlinks.
  (backup-by-copying-when-linked t) ; Don't break multiple hardlinks.
  (version-control t)               ; Use version numbers for backup files.
  (vc-make-backup-files t)          ; Backup even if file is in vc.
  (auto-save-list-file-prefix "~/.emacs.d/autosave/")
  (auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))
  ;; Don't create `#file-name' lockfiles in $PWD. Lockfiles are useful but
  ;; they generate too much activity from tools watching for changes during
  ;; development.
  (create-lockfiles nil)
  ;; Increase undo limit to 5MB per buffer.
  (undo-limit 5242880)
  :hook (text-mode-hook . auto-fill-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages - Persistence ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package no-littering
  :demand t)

(use-package desktop
  :demand t
  :bind
  ("C-M-s-k" . desktop-clear)
  :config
  (setq desktop-restore-frames t)
  (setq desktop-restore-in-current-display t)
  (setq desktop-restore-forces-onscreen nil)
  (desktop-save-mode 1))

(use-package recentf
  :defer t
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 100)
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
  :hook
  (dired-mode-hook . recentf-add-dired-directory))

(use-package autorevert ; revert buffers when files on disk change
  :demand t
  :custom
  (auto-revert-verbose nil)
  :hook
  (after-change-major-mode-hook . auto-revert-mode))

(use-package savehist
  :demand t
  :custom
  (savehist-autosave-interval 60)
  (history-delete-duplicates t)
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring
                                   file-name-history
                                   read-expression-history
                                   command-history
                                   extended-command-history
                                   window-config-alist
                                   magit-read-rev-history))
  :config
  (put 'kill-ring 'history-length 300)
  (savehist-mode))

;;;;;;;;;;;;;;;;;;
;; Package - UI ;;
;;;;;;;;;;;;;;;;;;

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

(use-package imenu-anywhere
  :defer t
  :bind ("C-."))

(use-package smooth-scrolling
  :demand t
  :config (smooth-scrolling-mode))

(use-package which-key
  :defer t
  :diminish which-key-mode
  :init
  (which-key-mode)
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 2)
  (which-key-idle-secondary-delay 0.05))

(use-package uniquify
  :defer t
  :ensure nil
  :custom
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*")
  ;; (uniquify-buffer-name-style 'reverse)
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-strip-common-suffix t))

(use-package magit
  :defer t
  :config
  (setq magit-log-arguments '("-n256" "--graph" "--decorate" "--color")
	magit-diff-refine-hunk t))

(use-package git-gutter+
  :defer t
  :ensure t
  :config
  (setq git-gutter+-disabled-modes '(org-mode))
  ;; Move between local changes
  (global-set-key (kbd "M-<up>") 'git-gutter+-previous-hunk)
  (global-set-key (kbd "M-<down>") 'git-gutter+-next-hunk)
  :hook
  (prog-mode . git-gutter+-mode))

(use-package vterm
  :defer t)

(use-package multi-vterm
  :defer t
  :bind
  ("C-c t" . multi-vterm))

(use-package marginalia
  :defer t
  :init
  (marginalia-mode))

(use-package vertico
  :demand t
  :init
  (vertico-mode)
  ;; (setq vertico-scroll-margin 0)     ; Different scroll margin
  ;; (setq vertico-count 20)            ; Show more candidates
  ;; (setq vertico-resize t)            ; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-cycle t)             ; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  )

(use-package corfu
  :demand t
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
  :demand t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :defer t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
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

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root) - overrides project.el by default.
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  )

;; (use-package consult-eglot)

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
      '(embark-which-key-indicator
	embark-highlight-indicator
	embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

(use-package embark
  :defer t
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init

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

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :defer t
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package dirvish ; dired replacement
  :defer t
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-hide-details nil)
  (setq dirvish-attributes
	'(all-the-icons collapse file-size file-time subtree-state vc-state))
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

(use-package vundo
  :defer t
  :bind
  ("C-u" . vundo))

(use-package rainbow-mode
  :demand t
  :init
  (rainbow-mode))

(use-package buffer-move
  :defer t
  :bind
  ("C-M-W" . buf-move-up)
  ("C-M-S" . buf-move-down)
  ("C-M-A" . buf-move-left)
  ("C-M-D" . buf-move-right))

(use-package winum
  :demand t
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

(use-package all-the-icons
  :demand t)

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package doom-modeline
  :demand t
  :init
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-height 15)
  (setq doom-modeline-project-detection 'projectile)
  :config
  (doom-modeline-mode 1)
  (set-face-attribute 'doom-modeline-evil-insert-state nil :foreground "orange"))

(use-package display-fill-column-indicator
  :defer t
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :init
  (setq-default fill-column  80)
  ;; (setq display-fill-column-indicator-character "|")
  )

(use-package flycheck
  :demand t
  :init
  (global-flycheck-mode))

(use-package flycheck-prospector
  :after flycheck
  :init (flycheck-prospector-setup))

(use-package projectile
  :demand t
  :init
  (setq projectile-project-search-path '(("~/projects/" . 3)))
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("C-c p" . projectile-command-map)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package - Editiing ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sudo-edit
  :demand t
  :init
  (sudo-edit-indicator-mode)
  :bind
  ("C-c C-r" . sudo-edit))

(use-package expand-region
  :defer t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package pcre2el
  :demand t)

(use-package visual-regexp-steroids
  :defer t
  :custom
  (vr/engine 'pcre2el "Use PCRE regular expressions")
  :bind
  ("C-c r" . vr/replace)
  ("C-c q" . vr/query-replace)
  ("C-r"   . vr/isearch-backward)
  ("C-S-s" . vr/isearch-forward)
  ("C-M-s" . isearch-forward)
  ("C-M-r" . isearch-backward))

(use-package iedit
  :defer t
  :config
  (set-face-background 'iedit-occurrence "Magenta")
  :bind
  ("C-;" . iedit-mode))

(use-package eldoc ; shows argument list of function call you are writing
  :defer t
  :diminish
  :hook
  (prog-mode       . turn-on-eldoc-mode)
  (cider-repl-mode . turn-on-eldoc-mode))

(use-package flyspell
  :defer t
  :diminish)

(use-package rainbow-delimiters
  :defer t
  :config
  (custom-set-faces
   '(rainbow-delimiters-unmatched-face
     ((t (:background "red" :foreground "white")))))
  :hook
  ((prog-mode cider-repl-mode) . rainbow-delimiters-mode))

(use-package format-all
  :defer t
  :hook
  (prog-mode . format-all-mode))

(use-package hungry-delete
  :init
  (global-hungry-delete-mode))

(use-package dumb-jump
  :defer t
  :hook
  (xref-backend-functions . dumb-jump-xref-activate))

(use-package aggressive-indent
  :defer t
  :hook
  (prog-mode . aggressive-indent-mode)
  (python-mode . (lambda () (aggressive-indent-mode -1))))

(use-package hl-todo
  :defer t
  :custom
  (hl-todo-keyword-faces
   '(("TODO" . "magenta")
     ("FIXME" . "magenta")
     ("\\?\\?\\?+" . "magenta")
     ("WIP" . "lime green")
     ("WORK" . "lime green")
     ("NEXT" . "lime green")
     ("NOTE" . "purple")
     ("WAIT" . "orange")
     ("KLUDGE" . "orange")
     ("HACK" . "orange")
     ("TEMP" . "orange")
     ("XXX+" . "orange")
     ("DONE" . "gray")))

  :hook
  (prog-mode . hl-todo-mode)
  :bind
  (:map hl-todo-mode-map
        ("M-s h i" . hl-todo-insert)
        ("M-s h C-p" . hl-todo-previous)
        ("M-s h C-n" . hl-todo-next)
        ("M-s h o" . hl-todo-occur)))

;;;;; Useful to switch between window / file layouts.
;; C-x r w ;; save a layout
;; C-x r j ;; load a layout

;;;;;;;;;;;;;;;;;;;;;
;; Packages - Lang ;;
;;;;;;;;;;;;;;;;;;;;;

;; Treating terms in CamelCase symbols as separate words makes editing.
(use-package subword
  :demand t
  :config (global-subword-mode 1))

(use-package elisp-lint
  :demand t)

(use-package markdown-mode
  :defer t
  :hook
  (markdown-mode . visual-line-mode)
  (maqrkdown-mode . variable-pitch-mode))

(use-package lua-mode :demand t)
(use-package yaml-mode :demand t)
(use-package go-mode :demand t)
(use-package dockerfile-mode :demand t)

;;;;; Not compiled with tree-sitter
;; ; Open python files in tree-sitter mode.
;; (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
;; (use-package eglot
;;   :bind
;;   (:map eglot-mode-map
;;         ("C-c C-d" . eldoc)
;;         ("C-c C-e" . eglot-rename)
;;         ("C-c C-o" . python-sort-imports)
;;         ("C-c C-f" . eglot-format-buffer))
;;   :config
;;   (setq-default
;;    eglot-workspace-configuration
;;    '((:pylsp
;;       . (:configurationSources
;; 	 ["flake8"]
;; 	 :plugins (
;; 		   :pycodestyle (:enabled :json-false)
;; 		   :mccabe (:enabled :json-false)
;; 		   :pyflakes (:enabled :json-false)
;; 		   :flake8 (:enabled :json-false
;; 				     :maxLineLength 80)
;; 		   :ruff (:enabled t :lineLength 80)
;; 		   :pydocstyle (:enabled t :convention "numpy")
;; 		   :yapf (:enabled :json-false)
;; 		   :autopep8 (:enabled :json-false)
;; 		   :black (:enabled t :line_length 80
;; 				    :cache_config t))))))
;;   (with-eval-after-load "eglot"
;;     (add-to-list 'eglot-stay-out-of 'eldoc))
;;   :hook
;;   (python-mode . eglot-ensure)
;;   (python-mode . flyspell-prog-mode)
;;   (python-mode . superword-mode)
;;   (python-mode . hs-minor-mode)
;;   (python-mode . (lambda () (set-fill-column 80))))

(use-package lsp-mode
  :defer t
  :config
  (setq lsp-auto-configure t
	lsp-before-save-edits t
	lsp-eldoc-enable-hover t
	lsp-eldoc-render-all nil
	lsp-completion-enable t
	lsp-file-watch-threshold 100
	lsp-enable-imenu t
	lsp-enable-indentation t
	lsp-enable-links t
	lsp-enable-xref t
	lsp-semantic-tokens-enable t
	lsp-signature-auto-activate t
	lsp-signature-render-documentation t
	lsp-signature-doc-lines 10
	lsp-idle-delay 0.5
	lsp-enable-symbol-highlighting t)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; The helpful package provides more context in Help buffers.
(use-package helpful
  :defer t
  :commands (helpful-callable helpful-variable helpful-key)
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key))

;;;;;;;;;;;
;; Hooks ;;
;;;;;;;;;;;

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;
;; Garbage Collection ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gcmh
  :defer t
  :config
  (gcmh-mode))

(provide 'init)
;;; init.el ends here
