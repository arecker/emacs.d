(require 'bookmark)
(require 'cl-lib)

(setq bookmark-save-flag 1)

(defun recker/list-bookmarks ()
  "List all bookmarks in alphabetical order, and filter out the junk entries I don't care about."
  (let ((junk-entries '("org-capture-last-stored")))
    (sort (cl-remove-if #'(lambda (b) (member b junk-entries))
                        (append (bookmark-all-names) (recker/list-work-repos)))
          #'string<)))

(defun recker/list-work-repos ()
  "List all the repos cloned to ~/src/work/ as bookmarks."
  (mapcar #'(lambda (n) (concat "src/work/" n))
          (cl-remove-if #'(lambda (f) (member f '("." "..")))
                        (directory-files (expand-file-name "~/src/work")))))

(defun recker/ido-bookmark-jump (bookmark)
  "*Switch to bookmark BOOKMARK interactively using `ido'."
  (interactive (list (ido-completing-read "Bookmark: " (recker/list-bookmarks) nil t)))
  (if (member bookmark (bookmark-all-names))
      (bookmark-jump bookmark)
    ;; If it's not in the actual bookmark file, just treat the key
    ;; like a relative path (ex. src/work/azdrain => ~/src/work/azdrain)
    (find-file (expand-file-name (concat "~/" bookmark)))))

(global-set-key (kbd "C-x r b") 'recker/ido-bookmark-jump)
