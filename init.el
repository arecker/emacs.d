(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")))

(defun recker/package-init ()
  "Initialize the package manager and install use-package"
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(recker/package-init)
(org-babel-load-file "~/.emacs.d/README.org")
