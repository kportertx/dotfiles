;;; Package --- Summary
;;; Commentary:
;;; Code:

(use-package emacs
  :ensure nil
  :config
  (global-unset-key [(control wheel-up)])       ; Prevent accidental brush with trackpad
  (global-unset-key [(control wheel-down)])     ; Prevent accidental brush with trackpad
  :bind
  ("RET" . newline-and-indent)
  ("DEL" . backward-delete-char)
  ("M-_" . undo-redo)
  ("C-/" . comment-or-uncomment-region)
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("C-x C-b" . 'ibuffer)
  ("C-s" . 'isearch-forward-regexp)
  ("C-r" . 'isearch-backward-regexp)
  ("C-M-s" . 'isearch-forward)
  ("C-M-r" . 'isearch-backward)
  :init
  (setq-default fill-column 80)                 ; Set fill column to 80 rather than 70, in all cases.
  (setq-default indent-tabs-mode nil)
  (setq-default require-final-newline t)
  (setq-default show-trailing-whitespace nil)   ; Enabling shows up in terminals - leaving it disabled.
  (setq-default tab-width 4)

  ;; Ensure we are always using UTF-8 encoding.
  (set-language-environment "UTF-8")
  (set-charset-priority 'unicode)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  (column-number-mode)                          ; Display column (and line) number in mode line
  (delete-selection-mode t)                     ; Start writing straight after deletion
  (global-display-line-numbers-mode)            ; Display line numbers in every buffer
  ;;(pixel-scroll-precision-mode)               ; (very slow) Precision scrolling
  (put 'narrow-to-region 'disabled nil)         ; Allows narrowing bound to C-x n n (region) and C-x n w (widen)
  (save-place-mode 1)                           ; https://www.emacswiki.org/emacs/SavePlace
  (savehist-mode 1)
  (show-paren-mode t)                           ; Visually indicates pair of matching parentheses

  :custom
  (custom-file (locate-user-emacs-file "custom-vars.el"))

  (completion--cycle-threshold 3)           ; TAB cycle if there are only few candidates
  (confirm-kill-processes nil)              ; Stop confirming the killing of processes
  (create-lockfiles nil)                    ; lock files kill `npm start'
  (cursor-type 'bar)                        ; Line-style cursor similar to other text editors
  (display-line-numbers-width 3)
  (enable-recursive-minibuffers t)
  (frame-inhibit-implied-resize t)
  (frame-title-format '("%b"))              ; Make window title the buffer name
  (inhibit-compacting-font-caches t)        ; Improve performance while scrolling.
  (inhibit-startup-screen t)                ; Disable startup screen
  (initial-major-mode 'fundamental-mode)    ; No need to have an Elisp buffer when starting up
  (kill-do-not-save-duplicates t)
  (load-prefer-newer t)
  (read-process-output-max (* 1024 1024))   ; Increase the amount of data which Emacs reads from the process
  (save-interprogram-paste-before-kill t)
  (scroll-conservatively 101)               ; Improve performance while scrolling.
  (sentence-end-double-space nil)
  (tab-always-indent 'complete)             ; Enable indentation+completion using the TAB key.
  (tabify-regexp "^\t* [ \t]+")             ; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the middle of a line.
  (use-short-answers t)                     ; y-or-n-p makes answering questions faster
  (visible-bell nil)

  ;; Persist Emacs session data.
  ;; Store all backup and autosave files in their own directory since it is bad to
  ;; clutter project directories. This also backs up TRAMP files locally.
  (backup-directory-alist
   `(("." . ,(concat user-emacs-directory "backups"))))

  (auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))
  (auto-save-list-file-prefix "~/.emacs.d/autosave/")
  (backup-by-copying t)                     ; Don't clobber symlinks.
  (backup-by-copying-when-linked t)         ; Don't break multiple hardlinks.
  (create-lockfiles nil)                    ; Don't create '#file-name' flockfiles in $PWD.
  (delete-old-versions t)
  (kept-new-versions 10)                    ; Automatic backup file housekeeping.
  (kept-old-versions 4)
  (undo-limit 5242880)                      ; Increase undo limit to 5MB per buffer.
  (vc-make-backup-files t)                  ; Backup even if file is in vc.
  (version-control t)                       ; Use version numbers for backup files.

  :hook
  (before-save-hook . diff-delete-trailing-whitespace)
  (text-mode-hook . auto-fill-mode))

(provide 'init-defaults.el)
;;; init-defaults.el ends here
