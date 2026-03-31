;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setopt package-archives
  '(("melpa" . "https://melpa.org/packages/")
     ("elpa" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(when (not package-archive-contents)
  (package-refresh-contents))

(require 'use-package)
(use-package use-package
  :ensure nil
  :custom
  (byte-compile-warnings '(cl-functions))
  (use-package-compute-statistics t))

(use-package auto-package-update
  :ensure t
  :demand t)

(use-package bind-key :ensure nil :demand t)
(use-package diminish :ensure t :demand t)

(use-package use-package-ensure-system-package
  :ensure t
  :demand t
  :custom
  (system-packages-package-manager 'apt))

(provide 'init-packaging)
;;; init-packaging.el ends here
