(defun recker/load-config ()
  "Tangle configuration and load it."
  (let ((config (concat (file-name-as-directory user-emacs-directory) "README.org")))
    (if (file-exists-p config)
        (org-babel-load-file config)
      (warn (concat config " not found - not loading")))))

;; We're going back to ONE GIANT README
;; all gas no brakes baby
(recker/load-config)
