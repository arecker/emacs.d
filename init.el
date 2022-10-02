(defun load-local-file (filename)
  (let ((full-path (concat user-emacs-directory "/lisp/" filename)))
    (load-file full-path)))

(load-local-file "functions.el")

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq user-full-name "Alex Recker"
      user-mail-address "alex@reckerfamily.com")

;; don't show the splash screen
(setq inhibit-startup-message 't)

;; don't kill *scratch* buffer
(add-hook 'kill-buffer-query-functions 'recker/not-scratch-p)

;; set startup message from fortune-blog
(setq initial-scratch-message (recker/scratch-message))

;; UI widgets
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; font
(set-frame-font (if (recker/macos-p) "Monaco 18" "Menlo 16") nil t)

;; IDO mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; disable auto save files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; delete highlighted text
(setq delete-selection-mode t)

;; automatically follow symlinks
(setq vc-follow-symlinks t)

;; don't pass --dired flag to ls
(setq dired-use-ls-dired nil)

;; don't prompt when deleting files behind buffers
(setq dired-clean-confirm-killing-deleted-buffers nil)

;; kill buffer when shell exits
(advice-add 'term-handle-exit :after 'recker/handle-term-exit)

(add-hook 'before-save-hook 'whitespace-cleanup)

(add-hook 'c-mode-hook #'recker/c-mode-hook)

(add-hook 'mhtml-mode-hook 'turn-off-auto-fill)

(eval-after-load "ispell"
  '(progn (defun ispell-get-coding-system () 'utf-8)))

(setq bookmark-save-flag 1)

(setq ruby-deep-indent-paren nil)

;; start emacs server
(server-start)

(load-local-file "org.el")
(load-local-file "packages.el")
(load-local-file "pass.el")
(load-local-file "keys.el")
