(defun recker/package-init()
  (setq package-archives
	'(("melpa" . "https://melpa.org/packages/")
	  ("org" . "http://orgmode.org/elpa/")))

  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(recker/package-init)

(org-babel-load-file "~/.emacs.d/README.org")
