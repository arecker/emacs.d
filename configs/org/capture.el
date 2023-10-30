(defun recker/opsat-generate-todays-template ()
  (format-time-string "* TODO %A, %B %d %Y [/]\nDEADLINE: <%Y-%m-%d %a>\n%?"))

(defun recker/build-opsat-today-capture ()
  `("o" "Today"
    entry
    (file+olp+datetree ,(concat org-directory "/opsat.org"))
    (function recker/opsat-generate-todays-template)
    :tree-type month
    :immediate-finish t))

(defun recker/opsat-find-todays-capture ()
  (goto-char (org-find-exact-headline-in-buffer (format-time-string "%A, %B %d %Y") (current-buffer) t))
  (org-end-of-subtree))

(defun recker/build-opsat-task-capture ()
  `("t" "Task"
    checkitem
    (file+function ,(concat org-directory "/opsat.org") recker/opsat-find-todays-capture)
    nil))

(defun recker/find-blog-capture ()
  "Opens today's blog entry."
  (find-file (expand-file-name (format-time-string "~/src/blog/entries/%Y-%m-%d.html")))
  (goto-char (point-min))
  (insert (format-time-string "<!-- meta:title -->\n<!-- meta:banner %Y-%m-%d.jpg -->\n\n"))
  (goto-char (point-max)))

(defun recker/build-blog-capture ()
  `("b"
    "Blog"
    plain
    (function recker/find-blog-capture)
    ""
    :immediate-finish t
    :jump-to-captured t))

(defun recker/build-capture-templates ()
  `(,(recker/build-opsat-today-capture)
    ,(recker/build-opsat-task-capture)
    ("w" "Work" entry (file ,(concat org-directory "/work.org.gpg") "* TODO %?\n" :empty-lines 1))
    ,(recker/build-blog-capture)
    ("h" "Hack" entry (file ,(concat org-directory "/hack.org")) "* TODO %? [/]\n" :empty-lines 1)
    ("m" "Therapy" plain (file+olp+datetree ,(concat org-directory "/therapy.org.gpg")))
    ("n" "Notes (work)" entry (file ,(concat org-directory "/work.org.gpg")) "* %?\n" :empty-lines 1 :jump-to-captured t)))

(setq org-capture-templates (recker/build-capture-templates))
(global-set-key (kbd "C-c c") 'org-capture)
