;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; HiDPI fringe bitmap scaling — must run before any package loads that calls
;; define-fringe-bitmap (diff-hl, flycheck, git-gutter, …).
;; Adapted from github.com/blahgeek/emacs-fringe-scale.
(defcustom my/fringe-scale-width 16
  "Target width for all fringe bitmaps."
  :type 'integer :group 'display)

(defun my/fringe-scale--width (bits-row orig-w new-w)
  (let ((res 0) (i 0))
    (while (< i new-w)
      (let* ((j (floor (* orig-w (/ (float i) new-w))))
             (bit (logand 1 (lsh bits-row (- j)))))
        (setq res (logior res (lsh bit i))))
      (setq i (1+ i)))
    res))

(defun my/fringe-scale--height (vec orig-h new-h)
  (let ((res (make-vector new-h nil)) (i 0))
    (while (< i new-h)
      (aset res i (elt vec (floor (* orig-h (/ (float i) new-h)))))
      (setq i (1+ i)))
    res))

(defun my/fringe-scale--advice (orig-func &rest args)
  (let* ((bitmap (nth 0 args))
         (bits   (nth 1 args))
         (height (or (nth 2 args) (length bits)))
         (width  (or (nth 3 args) 8))
         (align  (or (nth 4 args) 'center)))
    (when (< width my/fringe-scale-width)
      (let* ((nw my/fringe-scale-width)
             (nh (floor (* height (/ (float nw) width))))
             (scaled (my/fringe-scale--height
                      (mapcar (lambda (r) (my/fringe-scale--width r width nw)) bits)
                      height nh)))
        (setq bits scaled height nh width nw)))
    (funcall orig-func bitmap bits height width align)))

(advice-add 'define-fringe-bitmap :around #'my/fringe-scale--advice)

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
  (defun my-record-jump-before-buffer-switch (buffer-or-name &rest _)
    "Record current position in better-jumper before switching to a different buffer."
    (let ((target (ignore-errors (get-buffer buffer-or-name))))
      (when (and target (not (eq target (current-buffer))))
        (better-jumper-set-jump))))
  :config
  ;; jump scenarios
  ;; use M-x view-lossage
  (advice-add 'vertico-exit :around #'my-jump-advice)         ; may handle jumps from M-x commands
  (advice-add 'exit-minibuffer :around #'my-jump-advice)      ; handles goto-line
  (advice-add 'mouse-set-point :around #'my-jump-advice)
  (advice-add 'xref-find-def :around #'my-jump-advice)        ; FIXME - working?
  (advice-add 'xref-find-references :around #'my-jump-advice) ; FIXME - working?
  (advice-add 'switch-to-buffer :before #'my-record-jump-before-buffer-switch)
  (advice-add 'pop-to-buffer :before #'my-record-jump-before-buffer-switch)
  :bind
  ("M-<left>" . better-jumper-jump-backward)
  ("M-<right>" . better-jumper-jump-forward)
  )

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
    ;; ("C-x b" . consult-buffer)             ;; left at Emacs default switch-to-buffer
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
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
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
    consult-buffer :sort nil
    consult-theme :preview-key '(:debounce 0.2 any)
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-recent-file consult-xref
    consult-source-bookmark consult-source-file-register
    consult-source-recent-file consult-source-project-recent-file
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
  :config
  ;; Guard against orphaned dirvish buffers crashing the mode-line.
  ;; When a dirvish session is killed but its dired buffers linger,
  ;; (dirvish-curr) returns nil and the mode-line eval errors on every
  ;; redisplay cycle, causing significant UI lag.
  (advice-add 'dirvish-curr :filter-return
              (lambda (dv)
                (or dv
                    (when (derived-mode-p 'dired-mode)
                      (kill-buffer (current-buffer))
                      nil))))
  :custom
  (dirvish-hide-details nil)
  (dirvish-attributes
    '(nerd-icons collapse file-size file-time subtree-state vc-states))
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
                 (if (cdr targets) "\u2026" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-map" (cdr binding))))))))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

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
  (embark-indicators '(embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  :init
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; Add agent-shell actions for regions
  (define-key embark-region-map (kbd "g") #'agent-shell-send-region)
  (define-key embark-region-map (kbd "G") #'agent-shell-send-region-to)
  ;; Add eglot actions for identifiers
  (define-key embark-identifier-map (kbd "c") #'eglot-show-call-hierarchy)
  (define-key embark-identifier-map (kbd "T") #'eglot-show-type-hierarchy))

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
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key))

(use-package imenu-anywhere
  ;; `imenu-anywhere` provides navigation for imenu tags across all buffers that
  ;; satisfy grouping criteria. Available criteria include - all buffers with the
  ;; same major mode, same project buffers and user defined list of friendly mode
  ;; buffers.
  :ensure t
  :defer t
  :bind ("C-c C-SPC" . imenu-anywhere))

(use-package indent-bars
  ;; indent-bars highlights indentation with vertical bar characters.
  :ensure t
  :custom
  (indent-bars-prefer-character t)
  (indent-bars-treesit-support t)
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1))
  (indent-bars-highlight-current-depth '(:blend 0.5))
  (indent-bars-display-on-blank-lines t)
  :hook
  (prog-mode . indent-bars-mode))

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

(use-package project
  :custom
  (project-switch-commands
   '((consult-ripgrep "Grep" ?g)
     (project-find-file "Find file" ?f)
     (project-switch-to-buffer "Buffer" ?b)
     (project-dired "Dired" ?d)
     (magit-project-status "Magit" ?m)))
  :bind-keymap
  ("C-x p" . project-prefix-map)
  :bind
  (:map project-prefix-map
   ("g" . consult-ripgrep)))

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

(defface treemacs-selected-label-face
  '((t :inherit treemacs-file-face))
  "Face for the selected node's label only, sized independent of the rest of the shrunk tree.")

(defvar-local treemacs--selected-label-overlay nil)
(defvar-local treemacs--marquee-timer nil)
(defvar-local treemacs--marquee-offset 0)

(defun treemacs--marquee-stop ()
  (when treemacs--marquee-timer
    (cancel-timer treemacs--marquee-timer)
    (setq treemacs--marquee-timer nil)))

(defun treemacs--marquee-render-width (win btn)
  "Pixel width of whatever is currently rendered for BTN in WIN.
Live measurement, so it always matches reality regardless of which
face-remaps (`treemacs-text-scale', etc.) are active in this buffer."
  (car (window-text-pixel-size win (treemacs-button-start btn) (treemacs-button-end btn))))

(defun treemacs--marquee-fit (win btn overlay candidate avail-pixel)
  "Binary-search the longest prefix of CANDIDATE that fits AVAIL-PIXEL,
actually displaying each guess in OVERLAY and measuring the real result."
  (let ((lo 0) (hi (length candidate)))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi 1) 2)))
        (overlay-put overlay 'display (substring candidate 0 mid))
        (if (<= (treemacs--marquee-render-width win btn) avail-pixel)
            (setq lo mid)
          (setq hi (1- mid)))))
    (overlay-put overlay 'display (substring candidate 0 lo))))

(defun treemacs--marquee-tick (buf overlay btn avail-pixel full-text)
  "Slide FULL-TEXT through AVAIL-PIXEL worth of space in OVERLAY."
  (if (not (and (buffer-live-p buf) (overlay-buffer overlay)))
      (treemacs--marquee-stop)
    (with-current-buffer buf
      (-if-let (win (get-buffer-window buf t))
          (let* ((padded (concat full-text "   "))
                 (len (length padded))
                 (start (mod treemacs--marquee-offset len))
                 (rotated (concat (substring padded start) (substring padded 0 start))))
            (treemacs--marquee-fit win btn overlay rotated avail-pixel)
            (setq treemacs--marquee-offset (1+ treemacs--marquee-offset)))
        (treemacs--marquee-stop)))))

(defun treemacs--update-selected-label-overlay ()
  "Move the label-size overlay to the button on the current line only.
Leaves indentation guides and the icon at `treemacs-text-scale' size.
Marquees the label text if it would overflow the panel width."
  (treemacs--marquee-stop)
  (when treemacs--selected-label-overlay
    (delete-overlay treemacs--selected-label-overlay))
  (-when-let (btn (treemacs-current-button))
    (setq treemacs--selected-label-overlay
          (make-overlay (treemacs-button-start btn) (treemacs-button-end btn)))
    (overlay-put treemacs--selected-label-overlay 'face 'treemacs-selected-label-face)
    (-when-let (win (get-buffer-window (current-buffer) t))
      (let* ((full-text (buffer-substring-no-properties
                          (treemacs-button-start btn) (treemacs-button-end btn)))
             (prefix-pixel (car (window-text-pixel-size
                                 win (line-beginning-position) (treemacs-button-start btn))))
             (avail-pixel (- (window-body-width win t) prefix-pixel))
             (label-pixel (treemacs--marquee-render-width win btn)))
        (when (> label-pixel avail-pixel)
          (setq treemacs--marquee-offset 0)
          (setq treemacs--marquee-timer
                (run-with-timer 0 0.3 #'treemacs--marquee-tick
                                 (current-buffer) treemacs--selected-label-overlay
                                 btn avail-pixel full-text)))))))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :custom
  (treemacs-collapse-dirs 3) ; fewer nodes to render/git-decorate on deep monorepo trees
  (treemacs-width 18)
  (treemacs-text-scale -3)
  :config
  (treemacs-git-mode 'deferred)
  (treemacs-git-commit-diff-mode t)
  (treemacs-resize-icons 10)
  ;; ponytail: filewatch-mode auto-enables on load and inotify-watches every
  ;; expanded dir; costly on core/aerospike-size trees. Off here, use `g` to
  ;; refresh manually. Re-enable with (treemacs-filewatch-mode t) if external
  ;; file changes (branch switches, build output) need to auto-reflect.
  (treemacs-filewatch-mode -1)
  (treemacs-follow-mode t)
  ;; Selected node's label renders at the -1 scale used before the last
  ;; shrink (not full original size), since that's what "prior" meant here.
  (set-face-attribute 'treemacs-selected-label-face nil
                       :height (expt text-scale-mode-step (- -1 treemacs-text-scale)))
  ;; `treemacs--follow' (file-follow) runs off an idle timer, not the
  ;; command loop, so our buffer-local `post-command-hook' overlay updater
  ;; never fires for it. Refresh explicitly once it's done moving point.
  ;; `treemacs--follow' restores the original window/buffer before
  ;; returning, so the advice must re-select the treemacs window itself --
  ;; otherwise the overlay updater runs against the wrong buffer and no-ops.
  (advice-add 'treemacs--follow :after
              (lambda (&rest _)
                (-when-let (win (treemacs-get-local-window))
                  (with-selected-window win
                    (treemacs--update-selected-label-overlay)))))
  :hook
  (treemacs-mode . (lambda ()
                     (display-line-numbers-mode -1)
                     (add-hook 'post-command-hook #'treemacs--update-selected-label-overlay nil t)))
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

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

(use-package which-key
  :ensure nil
  :demand t
  :config
  (which-key-mode))

;; (use-package why-this
;;   :ensure t
;;   :straight (why-this :type git :host codeberg
;;               :repo "akib/emacs-why-this.git")
;;   :init
;;   (global-why-this-mode))

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

(winner-mode 1)

;;;;; Useful to switch between window / file layouts.
;; C-x r w ;; save a layout
;; C-x r j ;; load a layout

(use-package read-aloud
  :ensure t
  :bind
  ("C-c r" . read-aloud-this)
  :config
  ;; Piper voice model — installed options in ~/.local/share/piper/:
  ;;   ~/.local/share/piper/en_US-ryan-medium.onnx    ; previous default
  ;;   ~/.local/share/piper/en_US-lessac-medium.onnx
  ;;   ~/.local/share/piper/glados_piper_medium.onnx  ; current
  ;; GLaDOS downloaded from https://huggingface.co/DavesArmoury/GLaDOS_TTS
  ;; (glados_piper_medium.onnx + glados_piper_medium.onnx.json under /resolve/main/).
  ;; All are 22050 Hz mono, so the aplay flags below stay unchanged.
  (defvar kp/read-aloud-piper-model
    "~/.local/share/piper/glados_piper_medium.onnx"
    "Path to the Piper .onnx voice model used by the read-aloud engine.")

  (setq read-aloud-engines
        `("piper"
          (cmd "bash"
               args ("-c" ,(format "uv run piper --length_scale 0.67 -m %s --output-raw 2>/dev/null | aplay -r 22050 -f S16_LE -c 1 -t raw -q"
                                   kp/read-aloud-piper-model)))
          ,@read-aloud-engines))
  (setq read-aloud-engine "piper")

  ;; Don't pronounce Markdown syntax (asterisks, backticks, #, links, ...).
  ;; `read-aloud--string' is the single chokepoint that feeds the TTS engine,
  ;; so filtering its text argument strips syntax from every read-aloud command
  ;; at once. On-screen highlighting is unaffected (it is computed from buffer
  ;; positions, not from this string). Underscores become spaces so code-like
  ;; identifiers (e.g. safe_lst) are spoken as words ("safe lst") instead of
  ;; being crammed into one fast-mumbled token by the neural TTS.
  (defun kp/read-aloud-strip-markdown (str)
    "Return STR with common Markdown syntax removed, for cleaner speech."
    (if (not (stringp str))
        str
      (let ((s str))
        ;; images ![alt](url) -> alt ; links [text](url) and [text][ref] -> text
        (setq s (replace-regexp-in-string "!?\\[\\([^][]*\\)\\](\\(?:[^()]*\\))" "\\1" s))
        (setq s (replace-regexp-in-string "\\[\\([^][]*\\)\\]\\[[^][]*\\]" "\\1" s))
        (setq s (replace-regexp-in-string "\\[\\([^][]*\\)\\]" "\\1" s))
        ;; fenced code blocks: drop the ``` fence lines (incl. language tag)
        (setq s (replace-regexp-in-string "^[ \t]*`\\{3,\\}[^\n]*$" "" s))
        ;; inline/remaining code: drop the backticks, keep the content
        (setq s (replace-regexp-in-string "`+" "" s))
        ;; bold/italic asterisks and ~~strikethrough~~
        (setq s (replace-regexp-in-string "\\*+" "" s))
        (setq s (replace-regexp-in-string "~~" "" s))
        ;; line-leading heading #, blockquote >, and list bullets (- or +)
        (setq s (replace-regexp-in-string "^[ \t]*#+[ \t]*" "" s))
        (setq s (replace-regexp-in-string "^[ \t]*>+[ \t]*" "" s))
        (setq s (replace-regexp-in-string "^[ \t]*[-+][ \t]+" "" s))
        ;; horizontal rules (--- ___ ===) -> nothing
        (setq s (replace-regexp-in-string "^[ \t]*\\([-_=]\\)\\1\\1+[ \t]*$" "" s))
        ;; table pipes -> spaces
        (setq s (replace-regexp-in-string "|" " " s))
        ;; underscores -> spaces, so identifiers read as words (safe_lst -> "safe lst")
        (setq s (replace-regexp-in-string "_" " " s))
        ;; comparison/arrow operators -> spoken words (longest match first), so
        ;; "lut < safe_lst" reads "lut less than safe lst" rather than mumbling.
        (setq s (replace-regexp-in-string "<=" " less than or equal to " s))
        (setq s (replace-regexp-in-string ">=" " greater than or equal to " s))
        (setq s (replace-regexp-in-string "!=" " not equal to " s))
        (setq s (replace-regexp-in-string "==" " equals " s))
        (setq s (replace-regexp-in-string "->" " to " s))
        (setq s (replace-regexp-in-string "<" " less than " s))
        (setq s (replace-regexp-in-string ">" " greater than " s))
        (setq s (replace-regexp-in-string "=" " equals " s))
        ;; section sign -> word (docs use "§9" etc.)
        (setq s (replace-regexp-in-string "§" " section " s))
        ;; intra-word . : / (file.c:1234, paths, version numbers) -> spaces, so
        ;; they read as separate words rather than one mangled token. Looped to
        ;; catch chains like a.b.c (Emacs regexps have no lookahead). Sentence
        ;; punctuation is untouched: it is followed by a space, not an alnum.
        (let ((prev ""))
          (while (not (string= prev s))
            (setq prev s)
            (setq s (replace-regexp-in-string
                     "\\([[:alnum:]]\\)[.:/]\\([[:alnum:]]\\)" "\\1 \\2" s))))
        ;; brackets/parens TTS stumbles on -> spaces (sentence . , ; : ! ? kept)
        (setq s (replace-regexp-in-string "[][(){}]" " " s))
        ;; collapse runs of horizontal whitespace left by the substitutions
        ;; (keep newlines: the TTS engine uses them for pauses)
        (setq s (replace-regexp-in-string "[ \t]\\{2,\\}" " " s))
        s)))

  (defun kp/read-aloud--strip-markdown-args (args)
    "Advice: strip Markdown from the text argument of `read-aloud--string'."
    (cons (kp/read-aloud-strip-markdown (car args)) (cdr args)))

  (advice-add 'read-aloud--string :filter-args
              #'kp/read-aloud--strip-markdown-args))

;; Context menu (built-in, Emacs 28+) with Eglot and prog-mode entries.
(when (fboundp 'context-menu-mode)
  (context-menu-mode 1)

  (defun my/context-menu-eglot (menu _click)
    "Add Eglot items to MENU when an LSP server is active."
    (when (and (fboundp 'eglot-current-server) (eglot-current-server))
      (define-key-after menu [eglot-ctx-sep]     '(menu-item "--"))
      (define-key-after menu [eglot-ctx-actions] '(menu-item "Code Actions"      eglot-code-actions))
      (define-key-after menu [eglot-ctx-rename]  '(menu-item "Rename Symbol"     eglot-rename))
      (define-key-after menu [eglot-ctx-format]  '(menu-item "Format (LSP)"      eglot-format-buffer))
      (define-key-after menu [eglot-ctx-imports] '(menu-item "Organize Imports"  eglot-code-action-organize-imports))
      (define-key-after menu [eglot-ctx-calls]   '(menu-item "Call Hierarchy"    eglot-show-call-hierarchy)))
    menu)

  (defun my/context-menu-prog (menu _click)
    "Add Apheleia format entry in prog-mode buffers."
    (when (derived-mode-p 'prog-mode)
      (define-key-after menu [prog-ctx-sep]    '(menu-item "--"))
      (define-key-after menu [prog-ctx-format] '(menu-item "Format Buffer" apheleia-format-buffer)))
    menu)

  (add-to-list 'context-menu-functions #'my/context-menu-eglot)
  (add-to-list 'context-menu-functions #'my/context-menu-prog))

(provide 'init-ui)
;;; init-ui.el ends here
