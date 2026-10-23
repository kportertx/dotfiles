;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package git-auto-commit-mode
  :ensure t
  :defer t)

(defun my-org-link-roam-titles-at-point ()
  "Link text in the heading or list item line at point that matches an
org-roam node title. Case-insensitive whole words, longest title first, first
unlinked match each. Skips daily notes, the current file's own nodes, nodes
the line already links to, and text already in a link."
  (save-excursion
    (let (beg end)
      (cond
       ((org-at-heading-p)
        (org-back-to-heading t)
        (when (looking-at org-complex-heading-regexp)
          (setq beg (match-beginning 4) end (match-end 4))))
       ((org-at-item-p)
        (beginning-of-line)
        (when (looking-at (org-item-re))
          (goto-char (match-end 0))
          (when (looking-at "\\[.\\][ \t]*") (goto-char (match-end 0)))
          (setq beg (point) end (line-end-position)))))
      (when (and beg (fboundp 'org-roam-db-query))
        (my-org-create-jira-notes beg end)
        (my-org-link-roam-titles beg end)))))

(defconst my-org-jira-key-re "\\b\\(SERVER\\|AER\\|QE\\|DOCS\\|SUP\\|OPS\\)-[0-9]+\\b"
  "Jira keys that get their own note under jira/.")

(defun my-org-create-jira-notes (beg end)
  "Create jira/<key>.org for each Jira key between BEG and END that has no
node yet, so `my-org-link-roam-titles' can link it.  Keys match in any
case (server-123); the note title is upper case (SERVER-123).
Keys already inside a link are skipped."
  (save-excursion
    (goto-char beg)
    (let ((case-fold-search t))
      (while (re-search-forward my-org-jira-key-re end t)
        (let ((key (upcase (match-string-no-properties 0))))
          (unless (or (save-match-data (org-in-regexp org-link-any-re))
                      (org-roam-db-query [:select id :from nodes
                                          :where (like title $s1)]
                                         key))
            (let ((file (expand-file-name (format "jira/%s.org" (downcase key))
                                          org-roam-directory)))
              (unless (file-exists-p file)
                (with-temp-file file
                  (insert (format (concat ":PROPERTIES:\n:ID:       %s\n:END:\n"
                                          "#+title: %s\n"
                                          "- Jira: [[https://aerospike.atlassian.net/browse/%s][%s]]\n")
                                  (org-id-new) key key key)))
                (org-roam-db-update-file file)))))))))

(defun my-org-link-roam-titles (beg end)
  "Link org-roam node titles found between BEG and END."
  (save-excursion
    (let* ((end (copy-marker end))
           (daily (expand-file-name org-roam-dailies-directory org-roam-directory))
           (self (and buffer-file-name (expand-file-name buffer-file-name)))
           (case-fold-search t)
           (nodes (sort (seq-remove
                         (lambda (n) (or (string-prefix-p daily (nth 2 n))
                                         (equal (nth 2 n) self)))
                         (org-roam-db-query [:select [title id file] :from nodes]))
                        (lambda (a b) (> (length (car a)) (length (car b)))))))
      (dolist (n nodes)
        (goto-char beg)
        (let ((re (concat "\\b" (regexp-quote (car n)) "\\b"))
              (done (save-excursion (search-forward (concat "id:" (nth 1 n)) end t))))
          (while (and (not done) (re-search-forward re end t))
            (unless (save-match-data (org-in-regexp org-link-any-re))
              (replace-match (org-link-make-string (concat "id:" (nth 1 n))
                                                   (match-string 0))
                             t t)
              (setq done t)))))
      (set-marker end nil))))

(defun my-org-return-dwim ()
  "Like `org-meta-return', but outdent an empty heading or list item.
An empty level-1 heading is cleared. An empty top-level item becomes a
heading at the enclosing heading's level, or is cleared if there is none."
  (interactive)
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (cond
     ((and (org-at-heading-p) (string-match-p "\\`\\*+[ \t]*\\'" line))
      (if (> (org-current-level) 1)
          (org-do-promote)
        (delete-region (line-beginning-position) (line-end-position))))
     ((and (org-at-item-p)
           (string-match-p
            "\\`[ \t]*\\([-+*]\\|[0-9]+[.)]\\)[ \t]*\\(\\[.\\][ \t]*\\)?\\'" line))
      (let ((struct (org-list-struct)))
        (if (org-list-get-parent (line-beginning-position) struct
                                 (org-list-parents-alist struct))
            (org-outdent-item)
          (let ((level (org-current-level)))
            (delete-region (line-beginning-position) (line-end-position))
            (when level (insert (make-string level ?*) " "))))))
     (t
      (when (or (org-at-heading-p) (org-at-item-p))
        ;; Advancing marker keeps point after a link inserted right before it.
        (let ((pt (copy-marker (point) t)))
          (my-org-link-roam-titles-at-point)
          (goto-char pt)
          (set-marker pt nil)))
      (org-meta-return)))))

(defun my-org-demote-dwim ()
  "Demote the list item or heading at point, else `org-cycle' (e.g. blocks)."
  (interactive)
  (cond ((org-at-item-p) (org-indent-item-tree))
        ((org-at-heading-p) (org-do-demote))
        (t (org-cycle))))

(defun my-org-promote-dwim ()
  "Promote the list item or heading at point, else `org-shifttab'."
  (interactive)
  (cond ((org-at-item-p) (org-outdent-item-tree))
        ((org-at-heading-p) (org-do-promote))
        (t (org-shifttab))))

(defun my-org-star-dwim (n)
  "Self-insert `*', with two exceptions at the start of a line's text.
On a heading title, add a level.  On a list item, turn it into a heading at
the enclosing heading's level."
  (interactive "p")
  (cond
   ((/= n 1) (self-insert-command n))
   ((looking-back "^\\*+ " (line-beginning-position))
    (save-excursion (beginning-of-line) (insert "*")))
   ((and (looking-back "^[ \t]*[-+] " (line-beginning-position))
         (org-current-level)
         (not (org-in-src-block-p)))
    (let ((stars (make-string (org-current-level) ?*)))
      (delete-region (line-beginning-position) (point))
      (insert stars " ")))
   (t (self-insert-command n))))

(defun my-org-dash-dwim (n)
  "Self-insert `-', except at the start of a heading title: make it a list item."
  (interactive "p")
  (if (and (= n 1) (looking-back "^\\*+ " (line-beginning-position)))
      (progn (delete-region (line-beginning-position) (point))
             (insert "- "))
    (self-insert-command n)))

(defun my-org-backspace-dwim (n)
  "Backspace, except at the start of a level 2+ heading title: drop a level."
  (interactive "p")
  (if (and (= n 1) (looking-back "^\\*\\*+ " (line-beginning-position)))
      (save-excursion (beginning-of-line) (delete-char 1))
    (org-delete-backward-char n)))

(defvar my-word-freq-stopwords
  (let ((h (make-hash-table :test 'equal)))
    (dolist (w (split-string
                "a about above after again against all also am an and any are aren't as at
be because been before being below between both but by can can't cannot could couldn't did
didn't do does doesn't doing don't down during each few for from further had hadn't has
hasn't have haven't having he her here hers herself him himself his how i if in into is
isn't it it's its itself just let's like may me might more most much must mustn't my
myself no nor not now of off on once only or other ought our ours ourselves out over own
per same shan't she should shouldn't so some such than that that's the their theirs them
themselves then there there's these they they're this those through to too under until up
upon us very via was wasn't we we're were weren't what what's when where which while who
whom why will with won't would wouldn't yet you you're your yours yourself yourselves
get got gets one two three need needs needed using use used will want wants make makes
todo done cancelled wait waiting next see per etc ie eg vs"
                "[ \n]+" t))
      (puthash w t h))
    h)
  "Words `my-org-word-frequency' ignores.")

(dolist (w (split-string
            "above across actually afterwards ahead alone along already although always
among anyone anything anyway anywhere apart around aside away awhile basically
because becomes becoming begin began begun behind believe believed beside besides
better best beyond bit bring brings brought call called calling calls came certain
certainly change changed changes changing check checked checking clear clearly
close closed come comes coming completely consider continue continued continues
could couple course currently decide decided different done early easily easy either
else end enjoy enough entire especially even eventually ever every everyone
everything everywhere exactly example examples explain explained fact far
favorite feel feels felt few final finally find finding finds fine first follow
followed following found full further general generally gets getting give given
gives giving goes going gone good got great half hand happen happened happens hard
harder having held help helped helping hence hold however idea ideas important
include included includes including indeed instead interested interesting
introduction keep keeping keeps kept kind knew know knowing known knows large last
late later least leave leaves leaving left less less lets level likely line little
lives long longer look looked looking looks lot lots made main make makes making
many maybe mean means meant mention mentioned might mind minor moment month months
morning move moved moves moving much must near nearly necessary need needed needs
never next nice night nobody nothing noticed number often okay old once ones onto
open opened order original others otherwise overall own part particular parts past
people perhaps place places plenty point possible possibly probably problem problems
provide put puts putting question questions quick quickly quite rather read reading
reads ready real really reason reasons receive received remember remove removed
rest right run running runs said same saw say saying says second see seem seemed
seems seen send sent set sets setting several short should show showed showing
shown shows side similar simple simply since small smaller something sometimes
somewhere soon sort sorry special specific start started starting starts still stop
stopped such sure take taken takes taking talk talked talking tell tells thank
thanks thing things think thinking thinks thought three throughout thus time times
today together told tomorrow took top toward towards tried tries try trying turn
turned turns twice understand understood unless until unusual upon used usually
various walk want wanted wants way ways week weeks well went whatever whenever
whether whichever whole whose within without word words work worked working works
world worse worst would write writing written wrong year years yes yesterday
arrived arrive arrives discuss discussed discussing discussion able already
adding add added adds update updated updates updating"
            "[ \n]+" t))
  (puthash w t my-word-freq-stopwords))

(dolist (w (split-string
            "new ask asks asked asking another existing additional back focus focused
focusing join joined joining share shared sharing learn learned learning familiar
smart huge missing complete completed ongoing based interest interests interested
multiple prior current total arrival arrivals found finding fixed fixes anything
you've you'd you'll we've we'd we'll they've they'd they'll i'm i've i'll i'd he's
she's who's let's that'll there'll it'll couldn't wouldn't shouldn't didn't doesn't
isn't aren't wasn't weren't won't can't don't haven't hasn't hadn't
next previous following later earlier recent recently
also though although yet just only very really
sin onbaording"
            "[ \n]+" t))
  (puthash w t my-word-freq-stopwords))

(defun my-word-freq-template-dirs ()
  "Alist of (DIRECTORY . TEMPLATE-NAME) from `org-roam-capture-templates'."
  (delq nil
        (mapcar (lambda (tpl)
                  (let* ((target (plist-get (nthcdr 4 tpl) :target))
                         (path (and (eq (car-safe target) 'file+head) (nth 1 target))))
                    (when (and path (string-match "\\`\\([^/$]+\\)/" path))
                      (cons (match-string 1 path) (nth 1 tpl)))))
                org-roam-capture-templates)))

(defun my-word-freq-tags ()
  "Return (BY-TITLE . BY-ID) hashes of template names for roam nodes.
BY-TITLE maps a lower-case title to a list of template names, BY-ID maps a
node id to its template name.  Only nodes in a template folder are included."
  (let ((dirs (my-word-freq-template-dirs))
        (root (file-name-as-directory (expand-file-name org-roam-directory)))
        (by-title (make-hash-table :test 'equal))
        (by-id (make-hash-table :test 'equal)))
    (pcase-dolist (`(,title ,file ,id)
                   (org-roam-db-query [:select [title file id] :from nodes]))
      (let* ((rel (file-relative-name file root))
             (tag (cdr (seq-find (lambda (d) (string-prefix-p (concat (car d) "/") rel))
                                 dirs))))
        (when tag
          (cl-pushnew tag (gethash (downcase title) by-title) :test #'equal)
          (puthash id tag by-id))))
    (cons by-title by-id)))

(defun my-word-freq-follow ()
  "Show every mention of the word or link text on this row."
  (interactive)
  (let ((word (tabulated-list-get-id))
        (default-directory (file-name-as-directory (expand-file-name org-roam-directory))))
    (unless word (user-error "No word on this line"))
    (compilation-start
     (format "grep -rnHiwF --include='*.org' --exclude-dir=Old --exclude-dir=data --exclude-dir=jira --exclude-dir=.git -e %s ."
             (shell-quote-argument word))
     'grep-mode
     (lambda (_) (format "*Word: %s*" word)))))

(defun my-word-freq-follow-click (event)
  "Like `my-word-freq-follow' for the row clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (my-word-freq-follow))

(define-derived-mode my-word-freq-mode tabulated-list-mode "Words"
  "Word frequency table.  Click a row, or press RET, to list where the word
is mentioned."
  (define-key my-word-freq-mode-map (kbd "RET") #'my-word-freq-follow)
  (define-key my-word-freq-mode-map [mouse-1] #'my-word-freq-follow-click)
  (define-key my-word-freq-mode-map [mouse-2] #'my-word-freq-follow-click))

(defun my-org-word-frequency (&optional top)
  "Show word frequency across the roam notes, minus common words.
Text inside a link counts as one entry (the whole link text, so
[[id:..][Grant Haywood]] is \"Grant Haywood\"), not as separate words.  An
entry is tagged with a template name (person, project, ...) when it is a link
to a note in a template folder, or a word or phrase matching such a note's
title.  RET on a row lists where it is mentioned.  TOP limits the rows
(default 500; C-u for all).  Click a column header to re-sort."
  (interactive (list (cond ((consp current-prefix-arg) most-positive-fixnum)
                           ((numberp current-prefix-arg) current-prefix-arg)
                           (t 500))))
  (let* ((tags (my-word-freq-tags))
         (by-title (car tags))
         (by-id (cdr tags))
         (counts (make-hash-table :test 'equal))
         (shown (make-hash-table :test 'equal))
         (link-tag (make-hash-table :test 'equal))
         (link-re "\\[\\[\\([^]]*\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]")
         rows)
    (cl-flet ((bump (key text)
                (puthash key (1+ (gethash key counts 0)) counts)
                (unless (gethash key shown) (puthash key text shown))))
      ;; jira/ notes are generated stubs, and ticket keys (SERVER-123) would
      ;; drown out real words, so neither is counted.
      (dolist (file (seq-remove (lambda (f) (string-match-p "/jira/" f))
                                (org-roam-list-files)))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*:[A-Za-z_]+:.*$\\|^#\\+.*$" nil t)
            (replace-match " "))
          ;; Links: the whole description is one entry.
          (goto-char (point-min))
          (while (re-search-forward link-re nil t)
            ;; Capture everything from the match first: `split-string' and
            ;; `string-match-p' below clobber the match data, and a later
            ;; `replace-match' would then rewrite the wrong text and send
            ;; point backwards (an endless loop).
            (let* ((mb (match-beginning 0))
                   (me (match-end 0))
                   (target (match-string 1))
                   (desc (match-string 2))
                   (text (and desc (string-join (split-string desc) " "))))
              (when (and text (> (length text) 0)
                         (not (let ((case-fold-search t))
                                (string-match-p (concat "\\`" my-org-jira-key-re "\\'") text)))
                         ;; A lone common word as link text ("Me") is noise
                         ;; unless it links to a template note.
                         (not (and (gethash (downcase text) my-word-freq-stopwords)
                                   (not (and (string-prefix-p "id:" target)
                                             (gethash (substring target 3) by-id)))
                                   (not (gethash (downcase text) by-title)))))
                (let ((key (downcase text)))
                  (bump key text)
                  (let ((tag (if (string-prefix-p "id:" target)
                                 (gethash (substring target 3) by-id)
                               (car (gethash key by-title)))))
                    (when tag (cl-pushnew tag (gethash key link-tag) :test #'equal)))))
              (delete-region mb me)
              (goto-char mb)
              (insert " ")))
          (goto-char (point-min))
          (while (re-search-forward "https?://[^ \t\n]+" nil t) (replace-match " "))
          (goto-char (point-min))
          (let ((case-fold-search t))
            (while (re-search-forward my-org-jira-key-re nil t) (replace-match " ")))
          (goto-char (point-min))
          (while (re-search-forward "\\b[[:alpha:]][[:alpha:]'’-]*[[:alpha:]]\\b" nil t)
            (let ((w (downcase (match-string 0))))
              (unless (or (< (length w) 3) (gethash w my-word-freq-stopwords))
                (bump w w)))))))
    (maphash (lambda (key n)
               (let ((text (gethash key shown)))
                 (push (list text
                             (vector (number-to-string n)
                                     (propertize text 'mouse-face 'highlight
                                                 'help-echo "Click to grep the notes for this")
                                     (string-join (or (gethash key link-tag)
                                                      (gethash key by-title))
                                                  ", ")))
                       rows)))
             counts)
    (setq rows (seq-take (sort rows (lambda (a b)
                                      (> (string-to-number (aref (cadr a) 0))
                                         (string-to-number (aref (cadr b) 0)))))
                         top))
    (with-current-buffer (get-buffer-create "*Word Frequency*")
      (my-word-freq-mode)
      (setq tabulated-list-format
            [("Count" 8 (lambda (a b) (> (string-to-number (aref (cadr a) 0))
                                         (string-to-number (aref (cadr b) 0))))
              :right-align t)
             ("Word" 32 t) ("Template" 20 t)]
            tabulated-list-entries rows
            tabulated-list-sort-key nil)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(global-set-key (kbd "C-c n w") #'my-org-word-frequency)

(use-package org
  :ensure nil
  :custom
  (org-fold-core-style 'overlays)
  ;; Shift/Ctrl-Shift arrows select text everywhere, like other modes.
  ;; TODO: C-c C-t, priority: C-c ,, bullets: C-c -.
  (org-support-shift-select 'always)
  ;; Collapse #+begin_ blocks on open; TAB on the header line expands one.
  (org-hide-block-startup t)
  (org-hide-emphasis-markers t)
  ;; Show x_{1} as a subscript.  Only the braced form counts, so snake_case
  ;; words stay plain.
  (org-pretty-entities t)
  (org-use-sub-superscripts '{})
  (org-agenda-inhibit-startup t)
  ;; Agenda takes over only the current window; q puts the layout back.
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  ;; Follow file and id: links in the current window (C-u C-c C-o for other).
  (org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)
                          (wl . wl-other-frame)))
  (org-agenda-hide-tags-regexp "\\`\\(do_now\\|schedule\\|delegate\\|later\\)\\'")
  (org-agenda-dim-blocked-tasks nil)
  (org-priority-highest ?A)
  (org-priority-lowest ?D)
  (org-priority-default ?C)
  (org-directory "~/Documents/roam/")
  (org-default-notes-file "~/Documents/roam/notes.org")
  ;; Directories aren't searched recursively, so list each capture-template
  ;; folder (Old/ and data/ deliberately excluded).  New files in them are
  ;; picked up automatically.
  (org-agenda-files
   (mapcar (lambda (d) (concat "~/Documents/roam/" d))
           '("" "daily/" "people/" "projects/" "meetings/" "customers/"
             "teams/" "jira/" "releases/" "personal/" "topics/")))
  (org-agenda-sorting-strategy
   '((agenda category-down time-up priority-down)
     (todo priority-down category-down)
     (tags category-down priority-down)
     (search category-down)))
  (org-todo-keywords '((sequence "TODO(t)" "QUESTION(q)" "|" "DONE(d)" "CANCELLED(c@)")))
  (org-tag-alist '(("do_now" . ?1) ("schedule" . ?2) ("delegate" . ?3) ("later" . ?4)))
  (org-agenda-custom-commands
   '(("q" "Eisenhower quadrants"
      ((todo "TODO"
             ((org-agenda-overriding-header "")
              (org-super-agenda-groups
               '((:name "Do now" :tag "do_now")
                 (:name "Schedule" :tag "schedule")
                 (:name "Delegate" :tag "delegate")
                 (:name "Later" :tag "later")
                 (:name "Unknown" :anything t)))))))))
  :hook
  (org-mode . org-indent-mode)
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  (:map org-mode-map
        ("RET" . my-org-return-dwim)
        ("M-RET" . org-return)
        ("<C-return>" . org-return)
        ("TAB" . my-org-demote-dwim)
        ("<backtab>" . my-org-promote-dwim)
        ("*" . my-org-star-dwim)
        ("-" . my-org-dash-dwim)
        ("DEL" . my-org-backspace-dwim)
        ("C-<tab>" . org-cycle)
        ("C-S-<tab>" . org-shifttab)
        ;; Jump history like code buffers; TAB/S-TAB cover promote/demote.
        ("M-<left>" . better-jumper-jump-backward)
        ("M-<right>" . better-jumper-jump-forward))
  :config
  ;; Record a jump when following a link within the same file (cross-file
  ;; links are already recorded by the switch-to-buffer advice).
  (advice-add 'org-open-at-point :around #'my-jump-advice)
  ;; hyperbole-mode's global keymap binds M-RET to hkey-either, and a minor
  ;; mode's keymap always outranks a major mode's own map -- so the org-mode-map
  ;; binding above is unreachable without this. minor-mode-overriding-map-alist
  ;; is checked before minor-mode-map-alist, letting org-mode override just
  ;; this one binding from hyperbole-mode without touching it anywhere else.
  (defvar my-org-hyperbole-override-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-RET") #'org-return)
      map))
  (add-hook 'org-mode-hook
            (lambda ()
              (make-local-variable 'minor-mode-overriding-map-alist)
              (setf (alist-get 'hyperbole-mode minor-mode-overriding-map-alist)
                    my-org-hyperbole-override-map)))
  ;; org-get-agenda-file-buffer visits every agenda file via plain
  ;; find-file-noselect, which fires org-mode-hook, find-file-hook, and
  ;; after-change-major-mode-hook -- so building the agenda re-runs
  ;; org-indent-mode/org-appear-mode/org-modern-mode/toc-org-mode, four
  ;; doom-modeline recalculations, recentf tracking, and a dozen global minor
  ;; modes (font-lock, display-line-numbers, auto-revert w/ its own
  ;; file-watcher, treesit-auto, corfu...) on every file -- hundreds of them --
  ;; freezing the single-threaded daemon for every client while it does. None
  ;; of that matters for a buffer only being scanned for TODO entries, so
  ;; suppress all three hooks for this one call. But org-agenda-goto/-switch-to
  ;; fetch the buffer to actually visit through this same function, so a
  ;; buffer created here stays hook-less forever -- no org-indent-mode,
  ;; org-modern-mode, org-appear-mode, toc-org-mode -- looks broken once you
  ;; jump into it from the agenda. Mark such buffers and catch them up via
  ;; org-agenda-after-show-hook, which runs with point in the buffer you just
  ;; opened.
  (defvar-local my-org-agenda-quiet-buffer nil)

  (defun my-org-get-agenda-file-buffer-quiet (orig file)
    (let ((already-open (get-file-buffer file)))
      (let (org-mode-hook find-file-hook after-change-major-mode-hook)
        (let ((buf (funcall orig file)))
          (unless already-open
            (with-current-buffer buf
              (setq my-org-agenda-quiet-buffer t)))
          buf))))
  (advice-add 'org-get-agenda-file-buffer :around #'my-org-get-agenda-file-buffer-quiet)

  (defun my-org-agenda-activate-buffer-hooks (&rest _)
    (when my-org-agenda-quiet-buffer
      (setq my-org-agenda-quiet-buffer nil)
      (run-mode-hooks 'org-mode-hook)
      (run-hooks 'find-file-hook)))
  (add-hook 'org-agenda-after-show-hook #'my-org-agenda-activate-buffer-hooks)

  ;; Switching to such a buffer by any other route (consult-buffer, C-x b,
  ;; a link) never runs org-agenda-after-show-hook, so also catch it up the
  ;; moment its window becomes the selected one.
  (defun my-org-agenda-activate-selected-buffer (&rest _)
    (with-current-buffer (window-buffer (selected-window))
      (my-org-agenda-activate-buffer-hooks)))
  (add-hook 'window-buffer-change-functions #'my-org-agenda-activate-selected-buffer)
  (add-hook 'window-selection-change-functions #'my-org-agenda-activate-selected-buffer))

;; Loaded here, not inside the form below: when org-agenda is already loaded the
;; body runs at once, and the macro must exist before the form is expanded.
(require 'transient)
(with-eval-after-load 'org-agenda
  (transient-define-prefix my-org-agenda-transient ()
    "Transient menu for org-agenda actions."
    [["Status"
      ("t" "Todo state" org-agenda-todo)
      ("," "Priority"   org-agenda-priority)]
     ["Schedule"
      ("s" "Schedule"   org-agenda-schedule)
      ("d" "Deadline"   org-agenda-deadline)]
     ["Organize"
      (":" "Tags"       org-agenda-set-tags)
      ("r" "Refile"      org-agenda-refile)
      ("k" "Archive"     org-agenda-archive)]
     ["Other"
      ("q" "Quit"        transient-quit-one)]])
  (define-key org-agenda-mode-map (kbd "@") #'my-org-agenda-transient))

(use-package org-super-agenda
  :ensure t
  :after org
  :config
  (org-super-agenda-mode))

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
  :custom
  (org-modern-priority
   '((?A . "High")
     (?B . "Medium")
     (?C . "Low")
     (?D . "Trivial")))
  (org-modern-priority-faces
   '((?A . error)
     (?B . warning)
     (?C . success)
     (?D . shadow)))
  ;; Same glyphs as the defaults, 30% larger so the box reads at a glance.
  (org-modern-checkbox
   '((?X . #("☑" 0 1 (face (:height 1.3))))
     (?- . #("□–" 0 2 (composition ((2)) face (:height 1.3))))
     (?\s . #("□" 0 1 (face (:height 1.3))))))
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :config
  ;; org-modern--priority only applies a face when it falls back to the plain
  ;; `t' bracket-hiding behavior -- supplying custom label text via the
  ;; `org-modern-priority' alist (above) skips face application entirely, so
  ;; labels render as unstyled text. Re-apply the same face lookup on top of
  ;; the custom label.
  (defun my-org-modern--priority-with-face (orig)
    (let* ((beg (match-beginning 1))
           (end (match-end 1))
           (prio (char-before (1- end))))
      (if-let* ((rep (and (consp org-modern-priority)
                          (cdr (assq prio org-modern-priority)))))
          (progn
            (put-text-property beg end 'display rep)
            (put-text-property
             beg end 'face
             (if-let* ((face (or (cdr (assq prio org-modern-priority-faces))
                                 (cdr (assq t org-modern-priority-faces)))))
                 `(,face org-modern-label)
               'org-modern-priority)))
        (funcall orig))))
  (advice-add 'org-modern--priority :around #'my-org-modern--priority-with-face))

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

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/roam/"))
  (org-roam-completion-everywhere t)
  (org-roam-file-exclude-regexp '("data/" "\\`Old/"))
  ;; One template per folder, no timestamp prefix (links are by ID).
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "person" plain "%?"
      :target (file+head "people/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("j" "project" plain "%?"
      :target (file+head "projects/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("m" "meeting" plain "%?"
      :target (file+head "meetings/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("c" "customer" plain "%?"
      :target (file+head "customers/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("t" "team" plain "%?"
      :target (file+head "teams/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("i" "jira issue" plain "%?"
      ;; Quoted list: the head must be one literal string, not (concat ...).
      :target (file+head "jira/${slug}.org" "#+title: ${title}\n- Jira: [[https://aerospike.atlassian.net/browse/${title}][${title}]]\n")
      :unnarrowed t)
     ("r" "release" plain "%?"
      :target (file+head "releases/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("P" "personal" plain "%?"
      :target (file+head "personal/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("T" "topic" plain "%?"
      :target (file+head "topics/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind
  ("C-c n f" . org-roam-node-find)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n c" . org-roam-capture)
  ("C-c n g" . org-roam-graph)
  ("C-c n j" . org-roam-dailies-goto-today)
  ("C-c n d" . org-roam-dailies-goto-date)
  :config
  ;; Whitelisted so ~/Documents/roam/.dir-locals.el's git-auto-commit-mode
  ;; setup doesn't re-prompt as a risky local variable on every new file.
  (add-to-list 'safe-local-variable-values '(eval . (git-auto-commit-mode 1)))
  (add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t))
  (add-to-list 'safe-local-variable-values '(gac-debounce-interval . 300))

  (org-roam-db-autosync-mode)
  ;; Hyphenated filenames (mirza-karacic.org) to match hand-named notes.
  (defun my-org-roam-hyphen-slug (slug) (string-replace "_" "-" slug))
  (advice-add 'org-roam-node-slug :filter-return #'my-org-roam-hyphen-slug)
  ;; Stock "complete everywhere" only replaces the last word, so picking
  ;; "2026 Engineering Leadership Summit" after typing "2026 Engineering"
  ;; gave "2026 2026 Engineering ...".  Extend the region back over earlier
  ;; words (same line, up to 6) while the text still prefixes some title.
  (defun my-org-roam-completion-start (titles)
    (let* ((end (point))
           (start (car (bounds-of-thing-at-point 'word)))
           (best start)
           (bol (line-beginning-position)))
      (save-excursion
        (goto-char start)
        (dotimes (_ 5)
          (skip-chars-backward " \t" bol)
          (when (and (> (point) bol) (looking-back "\\w" (1- (point))))
            (backward-word)
            (let ((prefix (buffer-substring-no-properties (point) end)))
              (when (and (not (save-match-data (org-in-regexp org-link-any-re)))
                         (seq-some (lambda (title)
                                     (string-prefix-p prefix title t))
                                   titles))
                (setq best (point)))))))
      best))
  (defun my-org-roam-complete-everywhere ()
    (when (and org-roam-completion-everywhere
               (thing-at-point 'word)
               (not (org-in-src-block-p))
               (not (save-match-data (org-in-regexp org-link-any-re))))
      (let ((titles (org-roam--get-titles)))
        (list (my-org-roam-completion-start titles) (cdr (bounds-of-thing-at-point 'word))
              titles
              :exit-function
              (lambda (str _status)
                (delete-char (- (length str)))
                (insert "[[id:"
                        (org-roam-node-id (org-roam-node-from-title-or-alias
                                           (substring-no-properties str)))
                        "][" str "]]"))
              :exclusive 'no))))
  (advice-add 'org-roam-complete-everywhere :override #'my-org-roam-complete-everywhere)
  ;; Autosync only sees saves from Emacs; after 5 idle minutes, also index
  ;; notes added or changed outside it (git pull, scripts).  Unchanged files
  ;; are skipped by hash, so this is cheap.
  (defun my-org-roam-idle-sync ()
    (let ((inhibit-message t))
      (with-demoted-errors "org-roam idle sync: %S"
        (org-roam-db-sync))))
  (run-with-idle-timer 300 t #'my-org-roam-idle-sync)

  ;; Commit and push the whole notes repo (new and deleted files included, which
  ;; per-buffer git-auto-commit-mode never sees) every 10 minutes.
  (defvar my-org-roam-autocommit-process nil)
  (defun my-org-roam-autocommit ()
    (unless (process-live-p my-org-roam-autocommit-process)
      (setq my-org-roam-autocommit-process
            (make-process :name "roam-autocommit"
                          :buffer (get-buffer-create " *roam-autocommit*")
                          :command (list (expand-file-name "~/.local/bin/roam-autocommit"))
                          :noquery t))))
  (defvar my-org-roam-autocommit-timer
    (run-with-timer 600 600 #'my-org-roam-autocommit))

  (defun my-org-roam-backlink-visit (bl)
    (let ((node (org-roam-backlink-source-node bl)))
      (find-file (org-roam-node-file node))
      (goto-char (org-roam-backlink-point bl))))

  (defun my-org-roam-backlink-context (bl)
    (with-temp-buffer
      (insert-file-contents (org-roam-node-file (org-roam-backlink-source-node bl)))
      (goto-char (org-roam-backlink-point bl))
      (let ((line (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
        (setq line (replace-regexp-in-string "^\\*+\\s-*" "" line))
        (setq line (replace-regexp-in-string "\\[\\[[^]]*\\]\\[\\([^]]*\\)\\]\\]" "\\1" line))
        (replace-regexp-in-string "\\[\\[\\([^]]*\\)\\]\\]" "\\1" line))))

  (defun my-org-roam-backlinks--text (node)
    (let ((backlinks
           (sort (org-roam-backlinks-get node :unique t)
                 (lambda (a b)
                   (string> (org-roam-node-file (org-roam-backlink-source-node a))
                            (org-roam-node-file (org-roam-backlink-source-node b)))))))
      (when backlinks
        ;; Overlay text can't be scrolled into, so cap it and hand the full
        ;; list to org-roam's own (scrollable) backlinks buffer.
        (let ((shown 15))
          (concat
           (propertize "\n\n―――――――――――――――――――――――\nBacklinks:\n" 'face 'shadow)
           (mapconcat
            (lambda (bl)
              (concat "• " (buttonize (org-roam-node-title (org-roam-backlink-source-node bl))
                                       #'my-org-roam-backlink-visit bl)
                      (propertize (format "  —  %s" (my-org-roam-backlink-context bl)) 'face 'shadow)
                      "\n"))
            (seq-take backlinks shown))
           (when (> (length backlinks) shown)
             (concat (buttonize (format "… %d more, show all"
                                        (- (length backlinks) shown))
                                (lambda (_) (org-roam-buffer-display-dedicated node)))
                     "\n")))))))

  (defun my-org-roam-backlinks-refresh ()
    (when (org-roam-file-p)
      (save-restriction
        (widen)
        ;; Remove by property, not a buffer-local handle: re-running org-mode
        ;; (e.g. a revert) resets locals but leaves the old overlay behind.
        (remove-overlays (point-min) (point-max) 'my-org-roam-backlinks t)
        (let* ((node (org-roam-node-at-point))
               (text (and node (my-org-roam-backlinks--text node))))
          (when text
            (let ((ov (make-overlay (point-max) (point-max) nil t t)))
              (overlay-put ov 'my-org-roam-backlinks t)
              (overlay-put ov 'after-string text)))))))

  (add-hook 'org-mode-hook #'my-org-roam-backlinks-refresh)
  (add-hook 'after-save-hook #'my-org-roam-backlinks-refresh))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  :bind
  ("C-c n u" . org-roam-ui-mode))

(provide 'init-org)
;;; init-org.el ends here
