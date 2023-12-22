(setq vc-follow-symlinks 't)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; (use-package git-auto-commit-mode
;;   :ensure t
;;   :init (add-hook 'org-mode-hook 'git-auto-commit-mode)
;;   :config
;;   (setq gac-automatically-push-p 't)
;;   (setq gac-automatically-add-new-files-p 't))
