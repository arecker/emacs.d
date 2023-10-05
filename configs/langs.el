;; Programming language support
(use-package eglot
  :ensure t
  :config
  (setq eglot-autoshutdown 't)
  (setq eglot-autoreconnect nil)
  (setq eglot-confirm-server-initiated-edits nil))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flyspell
  :config (setq ispell-program-name (executable-find "ispell"))
  :init (add-hook 'text-mode-hook #'(lambda () (flyspell-mode 1))))

(use-package writegood-mode
  :ensure t
  :bind (("C-c w" . writegood-mode)))

(use-package d-mode
  :ensure t
  :mode "\\.d\\'")

(use-package dockerfile-mode
  :ensure t
  :mode ("\\Dockerfile\\'"))

(use-package groovy-mode
  :ensure t
  :mode ("\\Jenkinsfile\\'" "\\.groovy\\'"))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'"
  :config
  (setq dhall-format-at-save t
        dhall-format-arguments (\` ("--ascii"))
        dhall-use-header-line nil))

(use-package jsonnet-mode
  :ensure t
  :mode ("\\.jsonnet\\'" "\\.libsonnet\\'"))

(use-package bats-mode
  :ensure t
  :mode ("\\.bats\\'"))

;; C (from linux kernel standards)
(defun recker/c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun recker/c-mode-hook ()
  (c-add-style
   "linux-tabs-only"
   '("linux" (c-offsets-alist
              (arglist-cont-nonempty
               c-lineup-gcc-asm-reg
               recker/c-lineup-arglist-tabs-only))))
  (setq indent-tabs-mode t)
  (setq show-trailing-whitespace t)
  (c-set-style "linux-tabs-only"))

(add-hook 'c-mode-hook #'recker/c-mode-hook)

(use-package slime
  :ensure t
  :config (setq inferior-lisp-program (executable-find "sbcl")))

(use-package slime-company
  :ensure t
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

(use-package markdown-mode
  :ensure t
  :init (add-hook #'markdown-mode-hook 'eglot-ensure))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" "\\.p8\\'"))

(use-package nftables-mode :ensure t)

(use-package nginx-mode :ensure t)

(use-package php-mode
  :ensure t)

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'"))

(use-package rst
  :ensure t
  :mode (("\\.rst$" . rst-mode)))

;; ruby
(setq ruby-deep-indent-paren nil)

;; run this for eglot to work:
;; gem install solargraph
(add-hook 'ruby-mode-hook 'eglot-ensure)

(use-package terraform-mode
  :ensure t)

(use-package company-terraform
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))
