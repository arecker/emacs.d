;; A place for miscellaneous functions to simplify configs.

(defun recker/log (msg &rest args)
  "Log a MSG (and ARGS) to the mini buffer."
  (let ((msg (format "recker/config: %s" msg)))
    (apply #'message msg args)))

(defun recker/load-config (filestem)
  "Load a config by FILENAME from the configs directory of emacs.d."
  (let ((filepath (concat user-emacs-directory "configs/" filestem ".el")))
    (unless (file-exists-p filepath)
      (error "File %s does not exist!" filepath))
    (load-file filepath)
    (recker/log "loaded %s config (%s)" filestem filepath)))

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

(global-set-key (kbd "C-c t") 'recker/today)

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

(defun recker/unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single logical line."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))
