(require 'package)

(setq custom-file (concat user-emacs-directory "custom.el"))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

(defun recker/init ()
  "Initialize the package manager and load the config."

    ;; set & load custom file first
    (if (file-exists-p custom-file) (load custom-file)
      (warn "couldn't find custom file %s" custom-file))

    ;; initialize the package manager
    (package-initialize)

    ;; make sure use-package is installed
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))

    ;; load the config
    (org-babel-load-file (concat user-emacs-directory "/README.org")))

(recker/init)
