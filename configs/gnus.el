;; keep passwords in ~/.password-store/authinfo.gpg (and work)
(setq auth-sources
      (list
       (concat (expand-file-name "~/.password-store/") "authinfo.gpg")
       (concat (expand-file-name "~/.password-store-work/") "authinfo.gpg")))

;; hide startup files in .emacs.d/gnus/
;; little known fact, there are an infinite number of gnus directories
;; and they WILL make their way to your home directory whether you
;; want it or not
(let ((gnus-dir (concat user-emacs-directory "gnus/")))
  (setq gnus-startup-file (concat gnus-dir "newsrc"))
  (setq gnus-home-directory (concat gnus-dir "gnus")
        nnfolder-directory (concat gnus-dir "gnus/Mail/archive")
        message-directory (concat gnus-dir "gnus/Mail")
        nndraft-directory (concat gnus-dir "gnus/Drafts")
        gnus-cache-directory (concat gnus-dir "gnus/cache")))

;; If you experience dribble, talk to your doctor.
(setq gnus-use-dribble-file nil)

;; set primary method to empty so the program doesn't absolutely
;; EXPLODE when you open it
(setq gnus-select-method '(nnml ""))

;; set topic mode (the only readable mode) as the default
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Don't move archived messages anywhere
(setq gnus-message-archive-group nil)

;; powerful placebo settings for faster perceived speed
(setq gnus-asynchronous t)
(setq gnus-use-cache t)
(setq gnus-check-new-newsgroups nil
      gnus-check-bogus-newsgroups nil)
(setq gnus-show-threads nil
      gnus-use-cross-reference nil
      gnus-nov-is-evil nil)
(setq gnus-check-new-newsgroups nil
      gnus-use-adaptive-scoring nil)

;; Look at this fucking variable lol
(setq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s
")

;; Use this nerdy bullshit to save email addresseses for autocompletion
(use-package bbdb
  :ensure t
  :config (setq bbdb-file (concat user-emacs-directory "bbdb.el"))
  :init
  (bbdb-mua-auto-update-init 'message)
  (setq bbdb-mua-auto-update-p 'query)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))

;; auto filled messages look like shit on most normal mail clients, so
;; just turn it off to appease all the filthy casuals I email
(add-hook 'message-mode-hook #'turn-off-auto-fill)

;; Finally, we're setting up IMAP accounts.
(setq gnus-secondary-select-methods '())

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "alex@reckerfamily.com"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port "imaps")
                      (nnimap-stream ssl)
                      (nnmail-expiry-target "nnimap+alex@reckerfamily.com:[Gmail]/Trash")
                      (nnmail-expiry-wait immediate)))

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "arecker@zendesk.com"
                      (nnimap-user "arecker@zendesk.com")
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port "imaps")
                      (nnimap-stream ssl)
                      (nnmail-expiry-target "nnimap+arecker@zendesk.com:[Gmail]/Trash")
                      (nnmail-expiry-wait immediate)))

;; sending mail (does this by shelling out to msmpt, which will look
;; at the sender address to figure out which outgoing server to use
(setq smtpmail-smtp-service 587
      smtpmail-smtp-user "alex@reckerfamily.com"
      smtpmail-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")
(setq mail-host-address "smtp.gmail.com")
(setq message-sendmail-f-is-evil 't)
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
