;;; Package --- Summary
;;; Commentary:
;;; Code:

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

;; Setup straight package manager.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'use-package)
(use-package use-package
  :ensure nil
  :init
  ;; Disable deprecation warnings about cl. The cl library has been deprecated,
  ;; but lots of packages still use it.
  (setq byte-compile-warnings '(cl-functions))
  ;; Don’t pop up a buffer to warn me about deprecations and other minor issues.
  ;; (setq warning-minimum-level :emergency)
  :config
  (setq use-package-compute-statistics t) ;; for profiling, M-x use-package-report
  )

(use-package use-package-ensure-system-package
  ;; If an Emacs package relies on the installation of a system package,
  ;; install that package.
  :ensure t
  :demand t
  :custom
  (system-packages-package-manager 'apt))

(use-package bind-key :ensure nil :demand t) ;; if you use any :bind variant

(use-package auto-package-update
  ;; This package provides functionality for automatically updating your Emacs
  ;; packages periodically. It is specially useful for people that work in
  ;; multiple machines and tend to forget to manually update packages from time to
  ;; time.
  :ensure t
  :demand t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(provide 'init-packaging.el)
;;; init-packaging.el ends here
