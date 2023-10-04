;; A place for miscellaneous functions to simplify configs.

(defun recker/log (msg &rest args)
  "Log a MSG (and ARGS) to the mini buffer."
  (let ((msg (format "recker/config: %s" msg)))
    (apply #'message msg args)))

(defun recker/load-config (filestem)
  "Load a config by FILENAME from the configs directory of emacs.d."
  (let ((filepath (concat user-emacs-directory "configs/" filestem ".el")))
    (unless (file-exists-p filepath)
      (error "File %s does not exist!" filepath))
    (load-file filepath)
    (recker/log "loaded %s config (%s)" filestem filepath)))
