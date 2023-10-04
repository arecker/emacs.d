(require 'package)

(defun recker/configure-packages ()
  "Set up the package manager."

  ;; set and load custom file first (this is gitignored)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (if (file-exists-p custom-file) (load custom-file)
    (warn "couldn't find custom file %s" custom-file))

  ;; set the repo URLs
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))

  ;; initialize the package manager
  (package-initialize)

  ;; install use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(recker/configure-packages)
