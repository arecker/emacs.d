(require 'bookmark)
(require 'cl-lib)

(setq bookmark-save-flag 1)

(defun recker/list-bookmarks ()
  "List all bookmarks in alphabetical order, and filter out the junk entries I don't care about."
  (let ((junk-entries '("org-capture-last-stored")))
    (sort (cl-remove-if #'(lambda (b) (member b junk-entries))
                        (append (bookmark-all-names) ; actual saved bookmarks
                                ;; then all the dynamic ones
                                (recker/list-entries-as-bookmarks "Dropbox/org/")
                                (recker/list-files-as-bookmarks ".emacs.d/configs/" ".el")
                                (recker/list-entries-as-bookmarks "src/")
                                (recker/list-entries-as-bookmarks "src/work/")))
          #'string<)))

(defun recker/list-entries-as-bookmarks (parent)
  "List all the entries in the PARENT directory as if they were bookmarks."
  (mapcar #'(lambda (n) (concat parent n))
          (cl-remove-if #'(lambda (f) (string-prefix-p "." f))
                        (directory-files (expand-file-name (concat "~/" parent))))))

(defun recker/list-files-as-bookmarks (parent pattern)
  "List all the files matching pattern as if they were bookmarks."
  (mapcar #'(lambda (s) (string-remove-prefix (expand-file-name "~/") s))
          (directory-files-recursively (expand-file-name (concat "~/" parent)) pattern nil)))

(defun recker/ido-bookmark-jump (bookmark)
  "Switch to bookmark BOOKMARK interactively using `ido'."
  (interactive (list (ido-completing-read "Bookmark: " (recker/list-bookmarks) nil t)))
  (if (member bookmark (bookmark-all-names))
      (bookmark-jump bookmark)
    ;; If it's not in the actual bookmark file, just treat the key
    ;; like a relative path (ex. src/work/azdrain => ~/src/work/azdrain)
    (find-file (expand-file-name (concat "~/" bookmark)))))

(global-set-key (kbd "C-x r b") 'recker/ido-bookmark-jump)
