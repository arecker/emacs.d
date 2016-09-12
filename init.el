(setq user-full-name "Alex Recker"
      user-mail-address "alex@reckerfamily.com")

(defun recker/package-init ()
  (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("org" . "http://orgmode.org/elpa/")))

  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(recker/package-init)

(defun recker/load-init ()
  (let ((startup-files
         '(
           "startup.el"
           "interface.el"
           "git.el"
           "modes.el"
           "org.el"
	   "gnus.el"
           "functions.el"
           ))
        (load-it
         (lambda (f)
           (load-file
            (concat "~/.emacs.d/" f)))))
    (mapc load-it startup-files))
  (message "Configuration Loaded"))

(recker/load-init)
