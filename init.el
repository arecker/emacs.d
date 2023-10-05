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
(recker/load-config "movement")
(recker/load-config "files")
(recker/load-config "git")
(recker/load-config "whitespace")
(recker/load-config "langs")
(recker/load-config "web")
(recker/load-config "go")
(recker/load-config "python")
(recker/load-config "bookmark")
(recker/load-config "org")
(recker/load-config "snippets")
(recker/load-config "gnus")

;; start the emacs server
(server-start)
