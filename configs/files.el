;; Files and stuff

(setq make-backup-files nil)
(setq auto-save-default nil)

;; dired
(require 'dired-x)
(setq dired-use-ls-dired nil)
(setq dired-clean-confirm-killing-deleted-buffers nil)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(add-hook 'dired-mode-hook 'dired-omit-mode)
