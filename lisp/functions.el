(require 'bookmark)

(defun recker/macos-p ()
  "Return t if running on a mac."
  (string= system-type "darwin"))

(defun recker/not-scratch-p ()
  "Return NIL if the current buffer is the *scratch* buffer."
  (not (equal (buffer-name (current-buffer)) "*scratch*")))

(defun recker/scratch-message ()
  "Return a scratch message from fortune-blog."
  (with-temp-buffer
    (insert (shell-command-to-string "fortune-blog"))
    (let ((comment-start ";; "))
      (comment-region (point-min) (point-max)))
    (fill-individual-paragraphs (point-min) (point-max))
    (concat "\n" (buffer-string) "\n")))

(defun recker/purge-buffers ()
  "Delete all buffers, except for *scratch*."
  (interactive)
  (mapc #'(lambda (b) (unless (string= (buffer-name b) "*scratch*") (kill-buffer b))) (buffer-list)))

(defun recker/ido-bookmark-jump (bookmark)
  "*Switch to bookmark BOOKMARK interactively using `ido'."
  (interactive (list (ido-completing-read "Bookmark: " (sort (bookmark-all-names) #'string<) nil t)))
  (bookmark-jump bookmark))

(defun recker/unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single logical line."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defun recker/handle-term-exit (&optional process-name msg)
  (message "%s | %s" process-name msg)
  (kill-buffer (current-buffer)))

(defun recker/ansi-term ()
  "Launch ansi-term with current shell."
  (interactive)
  (let ((shell (or (getenv "SHELL") "/bin/bash")))
    (ansi-term shell)))

(defun recker/install-lsp-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(defun recker/add-p-tags-to-buffer ()
  "Automatically wrap all paragraphs in buffer in <p></p> tags."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(\\`\\|\n\n+\\)\\([^< $\n]\\)" nil t)
      (replace-match "\\1<p>\\2" t))
    (goto-char (point-min))
    (while (re-search-forward "\\([^>}\n]\\)\\(\n\n+\\|\n\\'\\)" nil t)
      (replace-match "\\1</p>\\2" t))))

(defun recker/c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun recker/c-mode-hook ()
  (c-add-style
   "linux-tabs-only"
   '("linux" (c-offsets-alist
	      (arglist-cont-nonempty
	       c-lineup-gcc-asm-reg
	       recker/c-lineup-arglist-tabs-only))))
  (setq indent-tabs-mode t)
  (setq show-trailing-whitespace t)
  (c-set-style "linux-tabs-only"))

(defun recker/today ()
  "Open today's journal entry."
  (interactive)
  (let* ((target
	  (format-time-string "~/src/blog/entries/%Y-%m-%d.html"))
	 (frontmatter
	  (format-time-string "<!-- meta:title -->\n<!-- meta:banner %Y-%m-%d.jpg -->\n\n")))
    (if (file-exists-p target)
	(find-file target)
      (progn (find-file target)
	     (insert frontmatter)))))

(defun recker/fix-yas-in-org ()
  (setq-local yas-buffer-local-condition
	      '(not (org-in-src-block-p t))))

(defun recker/org-rerender-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(defun recker/opsat-find ()
  (interactive)
  (let* ((docs-path (if (recker/macos-p) "~/Documents/" "~/docs/"))
	 (files
	  (mapcar #'file-name-nondirectory (directory-files-recursively docs-path "\\.org$" nil nil t))))
    (find-file (concat docs-path (ido-completing-read "File: " files nil nil)))))
