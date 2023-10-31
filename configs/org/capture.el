(defun recker/opsat-template ()
  (format-time-string "* TODO %? [/]\nDEADLINE: <%Y-%m-%d %a>\n%?"))

(defun recker/blog-target ()
  "Opens today's blog entry."
  (find-file (expand-file-name (format-time-string "~/src/blog/entries/%Y-%m-%d.html")))
  (goto-char (point-min)))

(defun recker/blog-template ()
  "Return the metadata for today's blog post."
  (format-time-string "<!-- meta:title -->\n<!-- meta:banner %Y-%m-%d.jpg -->\n\n"))

(setq org-capture-templates
      '(("o" "Task" entry (file+olp+datetree "opsat.org") (function recker/opsat-template))
        ("b" "Blog" plain (function recker/blog-target) (function recker/blog-template) :immediate-finish t :jump-to-captured t)
        ("h" "Hack" entry (file "hack.org") "* TODO %? [/]\n" :empty-lines 1)
        ("m" "Therapy" plain (file+olp+datetree "therapy.org.gpg"))
        ("w" "Work" entry (file "work.org.gpg") "* TODO %?\n" :empty-lines 1 :jump-to-captured t)))

(global-set-key (kbd "C-c c") 'org-capture)
