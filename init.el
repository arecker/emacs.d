(defun recker/load-config ()
  "Tangle configuration and load it."
  (let ((config (concat (file-name-as-directory user-emacs-directory) "README.org")))
    (if (file-exists-p config)
        (org-babel-load-file config)
      (warn (concat config " not found - not loading")))))

;; We're going back to ONE GIANT README
;; all gas no brakes baby
(recker/load-config)

;; set user info
(setq user-full-name "Alex Recker")
(setq user-mail-address "alex@reckerfamily.com")

;; load custom functions
(let ((functions-file (concat user-emacs-directory "configs/functions.el")))
  (load-file functions-file)
  (recker/log "loaded custom functions (%s)" functions-file))

;; load the configs from configs/*.el
(recker/load-configs '("shell"
                       "langs"
                       "go"
                       "python"
                       "org"))

;; start the emacs server
(server-start)

(global-visual-line-mode)
