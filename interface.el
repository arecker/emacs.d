(setq backup-inhibited t
      auto-save-default 0
      indent-tabs-mode nil)

(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar 0)

(define-minor-mode minor-mode-blackout-mode
  "Hides minor modes from the mode line."
  t)
(catch 'done
  (mapc (lambda (x)
	  (when (and (consp x)
		     (equal (cadr x) '("" minor-mode-alist)))
	    (let ((original (copy-sequence x)))
	      (setcar x 'minor-mode-blackout-mode)
	      (setcdr x (list "" original)))
	    (throw 'done t)))
	mode-line-modes))

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package helm
  :ensure t
  :config
  (setq helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match t)
  (helm-mode 1)
  :bind (("C-x C-b" . helm-buffers-list)
	 ("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files)
	 ("C-c h o" . helm-occur)
	 ("C-c i" . helm-imenu)
	 ("C-x r b" . helm-filtered-bookmarks)
	 ("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 :map helm-map
	 ("<tab>" . helm-execute-persistent-action)))

(use-package helm-projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  :bind ("C-c f" . helm-projectile))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))
