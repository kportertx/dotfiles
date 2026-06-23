;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure nil
  :custom
  (org-hide-emphasis-markers t)
  :hook
  (org-mode . org-indent-mode))

(use-package org-appear
  :ensure t
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  :hook
  (org-mode . org-appear-mode))

(use-package org-modern
  :ensure t
  :hook
  (org-mode . org-modern-mode))

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 100)
  :bind
  ("C-c SPC t o" . olivetti-mode))

(use-package toc-org
  :ensure t
  :hook
  (org-mode . toc-org-mode))

(use-package denote
  :ensure t
  :custom
  (denote-directory (expand-file-name "~/notes/"))
  (denote-known-keywords '("project" "meeting" "reference" "idea"))
  (denote-file-type 'org)
  :bind
  ("C-c n n" . denote)
  ("C-c n o" . denote-open-or-create)
  ("C-c n i" . denote-link-or-create)
  ("C-c n b" . denote-backlinks)
  ("C-c n f" . denote-find-link)
  ("C-c n F" . denote-find-backlink)
  ("C-c n r" . denote-rename-file))

(provide 'init-org)
;;; init-org.el ends here
