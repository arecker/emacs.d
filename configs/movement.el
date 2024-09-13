(setq delete-selection-mode 't)

;; this silly thing that always gets automatically inserted in my init.el
(put 'downcase-region 'disabled nil)

(setq confirm-kill-emacs #'yes-or-no-p)

;; handy functions
(global-set-key (kbd "C-c r") 'replace-string)

;; upcase a region without emacs bothering you
(put 'upcase-region 'disabled nil)

(use-package expand-region
  :ensure t
  :bind (("C-=" . 'er/expand-region)))

;; autocomplete
(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))

;; swiper search
(use-package swiper
  :ensure t
  :bind ("C-c s" . swiper))

;; projectile for projects
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ido)
  :init (projectile-mode t))
