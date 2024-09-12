(setq delete-selection-mode 't)
(global-auto-revert-mode t)

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

;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)
(use-package ido-vertical-mode          ;IDO + vertical mode
  :ensure t
  :config (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :init (ido-vertical-mode))
(use-package idomenu                    ;IDO + imenu
  :ensure t
  :bind ("C-c i" . idomenu))

;; smex (M-x)
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind (("M-x" . 'smex)
         ("M-X" . 'smex-major-mode-commands)))

;; buffers
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(defun recker/purge-buffers ()
  "Delete all buffers, except for *scratch*."
  (interactive)
  (mapc #'(lambda (b) (unless (string= (buffer-name b) "*scratch*") (kill-buffer b))) (buffer-list)))

(global-set-key (kbd "C-x P") 'recker/purge-buffers)

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
