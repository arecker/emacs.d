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
