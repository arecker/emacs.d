(defun recker/pass-directory ()
  (or (bound-and-true-p recker/active-pass-directory)
      (expand-file-name "~/.password-store")))

(defun recker/pass--file-to-entry (path)
  (file-name-sans-extension
   (file-relative-name path (recker/pass-directory))))

(defun recker/pass-list-entries ()
  (mapcar
   #'recker/pass--file-to-entry
   (directory-files-recursively (recker/pass-directory) ".gpg")))

(defun recker/pass-to-string (path)
  (cl-first
   (split-string
    (shell-command-to-string
     (format "PASSWORD_STORE_DIR=\"%s\" pass \"%s\" | head -1" (recker/pass-directory) path))
    "\n")))

(defun recker/pass-to-clip (path)
  (interactive (list (completing-read "Password: " (recker/pass-list-entries) nil t)))
  (shell-command
   (format "PASSWORD_STORE_DIR=\"%s\" pass -c \"%s\"" (recker/pass-directory) path)))

(defun recker/pass-to-clip-work ()
  (interactive)
  (let ((recker/active-pass-directory (expand-file-name "~/.password-store-work")))
    (funcall-interactively #'recker/pass-to-clip (completing-read "Password: " (recker/pass-list-entries) nil t))))

(global-set-key (kbd "C-x p") 'recker/pass-to-clip)
(global-set-key (kbd "C-x w") 'recker/pass-to-clip-work)
