(package-initialize)
(if (eq system-type 'darwin)
    (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
  (require 'cask "~/.cask/cask.el"))
(cask-initialize)
(org-babel-load-file "~/.emacs.d/README.org")
