(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

(defun recker/package-init ()
  "Initialize the package manager and install use-package."
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(defun recker/load-config ()
  "Tangle configuration and load it"
  (let ((config (concat (file-name-as-directory user-emacs-directory) "README.org")))
    (if (file-exists-p config)
	(org-babel-load-file config)
      (warn (concat config " not found - not loading")))))

(defun recker/touch-local-file ()
  "Ensure local.el is available" ()
  (let ((local (concat (file-name-as-directory user-emacs-directory) "local.el")))
    (unless (file-exists-p local)
      (with-temp-buffer
	(insert ";; Local Customizations\n")
	(write-file local)))
    (setq custom-file local)))

(recker/touch-local-file)
(recker/package-init)
(recker/load-config)
