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

(require 'use-package)
(use-package use-package
  :ensure t
  :init
  ;; Disable deprecation warnings about cl. The cl library has been deprecated,
  ;; but lots of packages still use it.
  (setq byte-compile-warnings '(cl-functions))
  ;; Don’t pop up a buffer to warn me about deprecations and other minor issues.
  ;; (setq warning-minimum-level :emergency)
  :config
  (setq use-package-always-ensure t)
  (setq use-package-compute-statistics t) ;; for profiling, M-x use-package-report
  )

;; If an Emacs package relies on the installation of a system package,
;; install that package.
(use-package use-package-ensure-system-package
  :demand t
  :custom
  (system-packages-package-manager 'apt))

(use-package diminish :demand t) ;; if you use :diminish
(use-package bind-key :demand t) ;; if you use any :bind variant
(use-package delight :demand t)  ;; Use delighting for modes

(use-package auto-package-update
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
