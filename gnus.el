(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-select-method '(nnml "")
      message-directory "~/.mail"
      gnus-directory "~/.news"
      nnfolder-directory "~/.mail/archive"
      gnus-always-read-dribble-file 't)

(setq gnus-secondary-select-methods
      '((nnimap "personal"
		(nnimap-address "imap.gmail.com")
		(nnimap-server-port 993)
		(nnimap-stream ssl))
	(nntp "news.gmane.org")))

;; TODO: only personal sending/pgp is hooked up right now
(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      message-send-mail-function 'smtpmail-send-it
      mml2015-encrypt-to-self t
      mml2015-signers '("1BF49F4B")
      gnus-message-archive-group nil)

(add-hook 'message-setup-hook 'mml-secure-message-encrypt)
