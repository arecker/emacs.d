(defun recker/python-mode-hook ()
  ;; disable fly* bullshit
  (flymake-mode -1)
  (flycheck-mode -1))

(add-hook #'python-mode-hook #'recker/python-mode-hook)

;; Turn this on to use elpy.
;; (use-package elpy
;;   :ensure t
;;   :init (elpy-enable)
;;   :config
;;   (setq elpy-rpc-virtualenv-path 'current)
;;   (add-hook 'elpy-mode-hook (lambda ()
;;                               (add-hook 'before-save-hook
;;                                         'elpy-format-code nil t))))

;; comment this out to skip eglot
(add-hook 'python-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '((python-mode) "jedi-language-server"))
