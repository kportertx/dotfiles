;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setopt package-archives
  '(("melpa" . "https://melpa.org/packages/")
     ("elpa" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; `package-initialize' restores `package-archive-contents' from the on-disk
;; cache even when that cache is stale, which makes the variable-nil check
;; below skip refresh and `:ensure t' fail for any package added since the
;; cache was written. Refresh when any archive's on-disk cache is missing or
;; older than a week.
(let ((max-age (* 7 24 60 60)))
  (when (or (not package-archive-contents)
            (cl-some
             (lambda (archive)
               (let ((f (expand-file-name
                         (format "archives/%s/archive-contents" (car archive))
                         package-user-dir)))
                 (or (not (file-exists-p f))
                     (> (- (float-time)
                           (float-time
                            (file-attribute-modification-time
                             (file-attributes f))))
                        max-age))))
             package-archives))
    (package-refresh-contents)))

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

(provide 'init-packaging)
;;; init-packaging.el ends here
