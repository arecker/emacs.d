(use-package company
  :ensure t
  :config (global-company-mode))

(defun recker/text-mode-hook ()
  (auto-fill-mode 1)
  (flyspell-mode 1))
(add-hook 'text-mode-hook 'recker/text-mode-hook)

(global-set-key (kbd "C-c l") 'sort-lines)

(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode))

(use-package js2-mode
  :ensure t
  :config (add-hook 'js-mode-hook 'js2-minor-mode))

(use-package elpy
  :ensure t
  :config (elpy-enable)
  :init (setq elpy-rpc-timeout 10))

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

(defun recker/ansi-term ()
  (interactive)
  (ansi-term "/bin/bash"))

(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-x t") 'recker/ansi-term)

(defun recker/term-mode-hook ()
  (global-hl-line-mode 0))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

(use-package yaml-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode)))
