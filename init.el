;; set user info
(setq user-full-name "Alex Recker")
(setq user-mail-address "alex@reckerfamily.com")

;; load custom functions
(let ((functions-file (concat user-emacs-directory "configs/functions.el")))
  (load-file functions-file)
  (recker/log "loaded custom functions (%s)" functions-file))

;; load the configs from configs/*.el
(recker/load-configs '("packages"
                       "appearance"
                       "scratch"
                       "shell"
                       "movement"
                       "files"
                       "git"
                       "whitespace"
                       "langs"
                       "web"
                       "go"
                       "python"
                       "bookmark"
                       "org"
                       "snippets"
                       "gnus"))

;; start the emacs server
(server-start)
