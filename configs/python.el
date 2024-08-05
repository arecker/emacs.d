(defun recker/python-mode-hook ()
  ;; disable fly* bullshit
  (flymake-mode -1)
  (flycheck-mode -1))

(add-hook #'python-mode-hook #'recker/python-mode-hook)
(add-hook 'python-mode-hook 'eglot-ensure)

;; Install one of the numerous MID python language servers to the
;; virtual environment, then activate it with (pyvenv-activate).
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(python-mode . ,(eglot-alternatives
                                 '(("pylsp")
                                   ("pyls" "--stdio")
                                   ("pyright" "--stdio")
                                   ("jedi-language-server"))))))

(defun recker/python-workon ()
  (interactive)
  "Activate a python environment.  If it's not in the WORKON_HOME list, create a symlink to a venv."
  (let* ((workon-home (or (getenv "$WORKON_HOME") (expand-file-name "~/.virtualenvs")))
         (existing-venvs (directory-files workon-home nil directory-files-no-dot-files-regexp))
         (chosen-venv (completing-read "Python Environment: " existing-venvs nil 'confirm))
         (symlink-dest (concat (file-name-as-directory workon-home) chosen-venv)))
    (unless (member chosen-venv existing-venvs)
      (let ((symlink-src (expand-file-name (read-directory-name "Path to venv: "))))
        (make-symbolic-link symlink-src symlink-dest)))
    (pyvenv-workon chosen-venv)
    (message "activated python environment \"%s\"" chosen-venv)))
