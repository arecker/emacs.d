(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(when (not (cl-remove-if-not 
	    (lambda (p) (equal 'org (car p)))
	    package-alist))
  (message "No org-mode package found; installing now...")
  (package-install 'org))

(org-babel-load-file "~/.emacs.d/config.org")
