(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package ido-vertical-mode
  :ensure t
  :config (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :init (ido-vertical-mode))

(use-package idomenu
  :ensure t
  :bind ("C-c i" . idomenu))

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind (("M-x" . 'smex)
	 ("M-X" . 'smex-major-mode-commands)))

(use-package rich-minority
  :ensure t
  :init (rich-minority-mode 1)
  :config (setq rm-blacklist ""))

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package editorconfig
  :ensure t
  :defer t
  :config (editorconfig-mode 1))

(use-package swiper
  :ensure t
  :bind ("C-c s" . swiper))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ido)
  :init (projectile-mode t))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode))

(use-package elpy
  :ensure t
  :defer t
  :config (setq elpy-rpc-virtualenv-path 'current)
  :init (advice-add 'python-mode :before 'elpy-enable))

(use-package go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook #'recker/install-lsp-save-hooks)
  (add-hook 'go-mode-hook 'lsp-deferred))

(use-package lsp-mode
  :ensure t
  :hook ((go-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package jsonnet-mode
  :ensure t
  :bind (("M-." . jsonnet-jump))
  :mode ("\\.jsonnet\\'" "\\.libsonnet\\'"))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package rst
  :ensure t
  :defer t
  :mode (("\\.rst$" . rst-mode)))

(use-package nginx-mode :ensure t :defer t)

(use-package nftables-mode :ensure t :defer t)

(use-package terraform-mode
  :ensure t)

(use-package company-terraform
  :ensure t)

(use-package lua-mode
  :ensure t
  :defer t
  :mode ("\\.lua\\'" "\\.p8\\'"))

(use-package haskell-mode
  :ensure t
  :defer t
  :mode "\\.hs\\'")

(use-package groovy-mode
  :ensure t
  :defer t
  :mode ("\\Jenkinsfile\\'" "\\.groovy\\'"))

(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'"
  :defer t
  :config
  (setq dhall-format-at-save t
	dhall-format-arguments (\` ("--ascii"))
	dhall-use-header-line nil))

(use-package d-mode
  :ensure t
  :defer t
  :mode "\\.d\\'")

(use-package slime
  :ensure t
  :defer t
  :config (setq inferior-lisp-program (executable-find "sbcl")))

(use-package slime-company
  :ensure t
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
		slime-company-after-completion 'slime-company-just-one-space))

(use-package bats-mode
  :ensure t
  :defer t
  :mode ("\\.bats\\'"))

(use-package writegood-mode
  :ensure t
  :defer t
  :init
  (add-hook 'text-mode-hook 'writegood-mode)
  (add-hook 'org-mode-hook 'writegood-mode)
  (add-hook 'gfm-mode-hook 'writegood-mode)
  (add-hook 'rst-mode-hook 'writegood-mode))

(use-package flyspell
  :config
  (setq ispell-program-name (executable-find "ispell"))
  :init
  (add-hook 'text-mode-hook #'(lambda () (flyspell-mode 1))))

(use-package dictionary :ensure t)

(use-package plantuml-mode
  :ensure t
  :config (setq org-plantuml-jar-path "~/.plantuml/plantuml.jar")
  :init
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

(use-package htmlize :ensure t)
