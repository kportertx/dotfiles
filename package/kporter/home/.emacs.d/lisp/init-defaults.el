;;; Package --- Summary
;;; Commentary:
;;; Code:

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
  (pixel-scroll-precision-mode)	                ; Precision scrolling
  (column-number-mode)                          ; Display column (and line) number in mode line
  (global-display-line-numbers-mode)            ; Display line numbers in every buffer
  (setq sentence-end-double-space nil)
  (setq-default show-trailing-whitespace t)
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  (setq-default require-final-newline t)
  :bind
  ("M-_" . undo-redo)
  ("RET" . newline-and-indent)
  ("C-/" . comment-or-uncomment-region)
  :custom
  (setq-default cursor-type 'bar)           ; Line-style cursor similar to other text editors
  (setq-default frame-title-format '("%b")) ; Make window title the buffer name
  (setq-default fill-column 80)             ; Set fill column to 80 rather than 70, in all cases.
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
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode t)
  ;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
  ;; middle of a line.
  (tabify-regexp "^\t* [ \t]+")
  (kill-do-not-save-duplicates t)
  (setq-default display-line-numbers-width 3)
  

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
  :hook
  (before-save-hook . diff-delete-trailing-whitespace)q
  (text-mode-hook . auto-fill-mode))

(provide 'init-defaults.el)
;;; init-defaults.el ends here
