;; set user info
(setq user-full-name "Alex Recker")
(setq user-mail-address "alex@reckerfamily.com")

;; load custom functions
(let ((functions-file (concat user-emacs-directory "configs/functions.el")))
  (load-file functions-file)
  (recker/log "loaded custom functions (%s)" functions-file))

;; load the configs from configs/*.el
(recker/load-config "packages")
(recker/load-config "appearance")
(recker/load-config "scratch")
(recker/load-config "shell")
(recker/load-config "gnus")

;; load the old config
(org-babel-load-file (concat user-emacs-directory "/README.org"))
