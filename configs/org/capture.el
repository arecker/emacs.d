(defun recker/opsat-generate-todays-template ()
  (format-time-string "* TODO %A, %B %d %Y [/]\nDEADLINE: <%Y-%m-%d %a>\n%?"))

(defun recker/opsat-find-todays-capture ()
  (goto-char (org-find-exact-headline-in-buffer (format-time-string "%A, %B %d %Y") (current-buffer) t))
  (org-end-of-subtree))

(defun recker/blog-target ()
  "Opens today's blog entry."
  (find-file (expand-file-name (format-time-string "~/src/blog/entries/%Y-%m-%d.html")))
  (goto-char (point-min)))

(defun recker/blog-template ()
  "Return the metadata for today's blog post."
  (format-time-string "<!-- meta:title -->\n<!-- meta:banner %Y-%m-%d.jpg -->\n\n"))

(setq org-capture-templates
      '(("o" "Today" entry (file+olp+datetree "opsat.org") (function recker/opsat-generate-todays-template) :tree-type month :immediate-finish t)
        ("t" "Task" checkitem (file+function "opsat.org" recker/opsat-find-todays-capture))
        ("w" "Work" entry (file "work.org.gpg") "* TODO %?\n" :empty-lines 1)
        ("b" "Blog" plain (function recker/blog-target) (function recker/blog-template) :immediate-finish t :jump-to-captured t)
        ("h" "Hack" entry (file "hack.org") "* TODO %? [/]\n" :empty-lines 1)
        ("m" "Therapy" plain (file+olp+datetree "therapy.org.gpg"))
        ("n" "Notes (work)" entry (file "work.org.gpg") "* %?\n" :empty-lines 1 :jump-to-captured t)))

(global-set-key (kbd "C-c c") 'org-capture)
