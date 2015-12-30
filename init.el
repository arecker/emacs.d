(package-initialize)

(if (eq system-type 'darwin)            ;installed with homebrew
    (require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
  (require 'cask "~/.cask/cask.el"))
(cask-initialize)

(when (not (cl-remove-if-not 
	    (lambda (p) (equal 'org (car p)))
	    package-alist))
  (message "No org-mode package found; installing now...")
  (package-install 'org))

(org-babel-load-file "~/.emacs.d/README.org")

(if (file-exists-p "~/.emacs.d/healthgrades.org")
    (org-babel-load-file "~/.emacs.d/healthgrades.org"))
