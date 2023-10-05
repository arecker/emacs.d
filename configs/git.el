(setq vc-follow-symlinks 't)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))
