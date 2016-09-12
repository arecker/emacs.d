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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (expand-region comment-dwim-2 git-gutter magit use-package org-plus-contrib))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
