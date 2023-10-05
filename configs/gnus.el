(setq auth-sources (list (concat user-emacs-directory "authinfo.gpg")))

(setq gnus-select-method '(nnml ""))
(setq gnus-secondary-select-methods '())
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; powerful placebo settings
(setq gnus-asynchronous t)
(setq gnus-use-cache t)
(setq gnus-check-new-newsgroups nil
      gnus-check-bogus-newsgroups nil)
(setq gnus-show-threads nil
      gnus-use-cross-reference nil
      gnus-nov-is-evil nil)
(setq gnus-check-new-newsgroups nil
      gnus-use-adaptive-scoring nil)
(setq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s")

(setq gnus-use-dribble-file nil)
(setq gnus-message-archive-group nil)

(use-package bbdb
  :ensure t
  :config (setq bbdb-file (concat user-emacs-directory "bbdb.el"))
  :init
  (bbdb-mua-auto-update-init 'message)
  (setq bbdb-mua-auto-update-p 'query)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))

(add-hook 'message-mode-hook #'turn-off-auto-fill)

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "alex@reckerfamily.com"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port "imaps")
                      (nnimap-stream ssl)
                      (nnmail-expiry-target "nnimap+alex@reckerfamily.com:[Gmail]/All Mail")
                      (nnmail-expiry-wait immediate)))

(setq smtpmail-smtp-service 587
      smtpmail-smtp-user "alex@reckerfamily.com"
      smtpmail-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it)
