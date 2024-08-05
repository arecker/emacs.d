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
