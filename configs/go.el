;; GOLANG
(use-package go-mode :ensure t)

;; run this for eglot to work:
;; go install golang.org/x/tools/gopls@latest
(add-hook 'go-mode-hook 'eglot-ensure)

(defun recker/go-mode-hook ()
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook 'recker/go-mode-hook)
