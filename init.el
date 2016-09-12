(setq user-full-name "Alex Recker"
      user-mail-address "alex@reckerfamily.com")

(defun recker/package-init ()
  (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("org" . "http://orgmode.org/elpa/")))

  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(recker/package-init)

(defun recker/init-better-defaults ()
  (setq backup-inhibited t
	auto-save-default 0
	indent-tabs-mode nil)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (toggle-scroll-bar 0))

(recker/init-better-defaults)


(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(defun recker/init-scratch ()
  (setq inhibit-startup-message 't)
  (let ((wilfred-installed (executable-find "wilfred-say"))
	(fortune-installed (executable-find "fortune"))
	(comment-command-output (lambda (c)
				  (concat
				   (mapconcat
				    (lambda (x) (concat ";; " x))
				    (split-string (shell-command-to-string c) "\n" t) "\n")
				   "\n" "\n"))))
    (if wilfred-installed
	(setq initial-scratch-message
	      (funcall comment-command-output "wilfred-say"))
      (if fortune-installed
	  (setq initial-scratch-message
		(funcall comment-command-output "fortune"))))))

(recker/init-scratch)

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

(defun recker/startup-registers ()
  (set-register ?b '(file . "~/git/blog"))
  (set-register ?d '(file . "~/Desktop"))
  (set-register ?e '(file . "~/.emacs.d/README.org"))
  (set-register ?g '(file . "~/git"))
  (set-register ?i '(file . "~/.emacs.d/init.el"))
  (set-register ?o '(file . "~/org"))
  (set-register ?p '(file . "~/org/personal.org"))
  (set-register ?w '(file . "~/org/work.org")))
(recker/startup-registers)

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

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode 1))

(use-package company
  :ensure t
  :config (global-company-mode))

(defun recker/text-mode-hook ()
  (auto-fill-mode 1)
  (flyspell-mode 1))
(add-hook 'text-mode-hook 'recker/text-mode-hook)

(global-set-key (kbd "C-c l") 'sort-lines)

(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode))

(use-package js2-mode
  :ensure t
  :config (add-hook 'js-mode-hook 'js2-minor-mode))

(use-package elpy
  :ensure t
  :config (elpy-enable)
  :init (setq elpy-rpc-timeout 10))

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

(defun recker/ansi-term ()
  (interactive)
  (ansi-term "/bin/bash"))

(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-x t") 'recker/ansi-term)

(defun recker/term-mode-hook ()
  (global-hl-line-mode 0))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))

(use-package yaml-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode)))

(use-package "org"
  :ensure t
  :pin "org"
  :init
  (setq org-agenda-files '("~/org")
	org-capture-templates
	'(
	  ("i" "idea" entry (file+headline "~/org/personal.org" "Ideas")
	   "* %?"
	   :empty-lines 1)
	  ))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (ruby . t)
     (sh . t)
     (java . t)
     (js . t)
     (C . t)))
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c s" . org-store-link)))

(use-package "org-plus-contrib"
  :ensure t
  :pin "org")

(setq org-export-date-timestamp-format "%B %e, %Y")

(defun blog/--read-partial (name)
  (with-temp-buffer
    (insert-file-contents (concat "~/git/blog/includes/" name))
    (buffer-string)))

(setq blog/partial-disqus (blog/--read-partial "disqus.html")
      blog/partial-analytics (blog/--read-partial "ga.html")
      blog/partial-highlight (blog/--read-partial "highlight.html"))

(defun blog/post-postamble (options)
  (concat "<hr>"
	  blog/partial-disqus
	  blog/partial-analytics
	  blog/partial-highlight))

(defun blog/index-postamble (options)
  (concat blog/partial-analytics))

(defun blog/post-preamble (options)
  (let ((slug (file-name-base (buffer-file-name))))
    (concat "<nav>"
	    "<a href=\"./index.html\">Home</a>"
	    "<div style=\"float: right;\">"
	    "<a href=\"" "./pdfs/" slug ".pdf" "\">PDF</a>"
	    "&nbsp;&nbsp;"
	    "<a href=\"" "./txt/" slug ".txt" "\">TXT</a>"
	    "</div>"
	    "</nav>")
    )
  )

(setq org-publish-project-alist
      '(

	("blog-index"
	 :base-directory "~/git/blog"
	 :exclude ".*"
	 :html-doctype "html5"
	 :html-head-extra "<link rel='stylesheet' href='./css/lora.css' />\n<link rel='stylesheet' href='./css/site.css' />"
	 :html-postamble blog/index-postamble
	 :include ["index.org"]
	 :publishing-directory "/var/www/blog"
	 :publishing-function org-html-publish-to-html
	 :section-numbers nil
	 :with-tags nil
	 :with-toc nil
	 )

	("blog-posts"
	 :base-directory "~/git/blog"
	 :base-extension "org"
	 :exclude "index.org"
	 :html-doctype "html5"
	 :html-head-extra "<link rel='stylesheet' href='./css/lora.css' />\n<link rel='stylesheet' href='./css/site.css' />"
	 :html-preamble blog/post-preamble
	 :html-postamble blog/post-postamble
	 :htmlized-source nil
	 :publishing-directory "/var/www/blog"
	 :publishing-function org-html-publish-to-html
	 :recursive t
	 :section-numbers nil
	 :with-toc nil
	 )

	("blog-posts-pdf"
	 :base-directory "~/git/blog"
	 :base-extension "org"
	 :completion-function (lambda() (shell-command "rm ~/git/blog/*.pdf && rm ~/git/blog/*.tex"))
	 :exclude "index.org"
	 :publishing-directory "/var/www/blog/pdfs"
	 :publishing-function org-latex-publish-to-pdf
	 :recursive t
	 :section-numbers nil
	 :with-toc nil
	 )

	("blog-posts-txt"
	 :base-directory "~/git/blog"
	 :base-extension "org"
	 :exclude "index.org"
	 :publishing-directory "/var/www/blog/txt"
	 :publishing-function org-ascii-publish-to-ascii
	 :recursive t
	 :section-numbers nil
	 :with-toc nil
	 )

	("blog-static"
	 :base-directory "~/git/blog"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "/var/www/blog"
	 :publishing-function org-publish-attachment
	 :recursive t
	 )

	("blog-rss"
	 :base-directory "~/git/blog"
	 :base-extension "org"
	 :exclude ".*"
	 :exclude-tags ("noexport" "norss")
	 :include ["index.org"]
	 :html-link-home "http://alexrecker.com"
	 :html-link-use-abs-url t
	 :publishing-directory "/var/www/blog/feed/"
	 :publishing-function (org-rss-publish-to-rss)
	 )

	))

(defun blog/push ()
  (interactive)
  (shell-command "rsync -razp /var/www/blog/ ebonhawk:/var/www/blog"))

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

(defun recker/purge-buffers ()
  "Deletes all buffers except for *scratch*"
  (interactive)
  (let ((kill-if-not-scratch
	 (lambda (b)
	   (unless (string= (buffer-name b) "*scratch*")
	     (kill-buffer b)))))
    (mapc kill-if-not-scratch (buffer-list))))

(defun recker/unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
	logical line.  This is useful, e.g., for use with
	`visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; TODO: modify this so it's not so dependent on helm
(defun recker/bookmark-open (link)
  "Opens a browser bookmark"
  (interactive
   (helm-comp-read "Select Bookmark: " recker/bookmark-list))
  (browse-url link))

(load-file "~/org/bookmarks.el")
(global-set-key (kbd "C-c b") 'recker/bookmark-open)

(defun recker/load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(recker/load-directory "~/.emacs.d/lisp/")
