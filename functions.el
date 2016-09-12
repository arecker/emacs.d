(defun recker/purge-buffers ()
  "Deletes all buffers except for *scratch*"
  (interactive)
  (let ((kill-if-not-scratch
	 (lambda (b)
	   (unless (string= (buffer-name b) "*scratch*")
	     (kill-buffer b)))))
    (mapc kill-if-not-scratch (buffer-list))))

(defun recker/unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
	logical line.  This is useful, e.g., for use with
	`visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; TODO: modify this so it's not so dependent on helm
(defun recker/bookmark-open (link)
  "Opens a browser bookmark"
  (interactive
   (helm-comp-read "Select Bookmark: " recker/bookmark-list))
  (browse-url link))

(load-file "~/org/bookmarks.el")
(global-set-key (kbd "C-c b") 'recker/bookmark-open)

(defun recker/load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(recker/load-directory "~/.emacs.d/lisp/")
