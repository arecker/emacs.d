;; A place for miscellaneous functions to simplify configs.
(require 'cl-lib)

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

(defun recker/load-configs (names)
  "Load the configs in the order specified in NAMES.  Throw a warning if any files were left out."
  (let ((all-configs
         (mapcar #'(lambda (f) (file-name-sans-extension f))
                 (cl-remove-if #'(lambda (f) (or (string-prefix-p "." f)
                                              (not (string-suffix-p ".el" f))))
                            (directory-files (concat user-emacs-directory "configs"))))))
    (let ((orphaned-configs (cl-remove-if #'(lambda (c) (or (member c names) (string= c "functions"))) all-configs)))
      (when orphaned-configs
        (warn "Orphaned files in .emacs.d/configs: %s" orphaned-configs))))
  (dolist (config names)
    (recker/load-config config)))
