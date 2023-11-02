(defun recker/blog-target ()
  "Opens today's blog entry."
  (find-file (expand-file-name (format-time-string "~/src/blog/entries/%Y-%m-%d.html")))
  (goto-char (point-min)))

(defun recker/blog-template ()
  "Return the metadata for today's blog post."
  (format-time-string "<!-- meta:title -->\n<!-- meta:banner %Y-%m-%d.jpg -->\n\n"))

(setq org-capture-templates
      '(("o" "Task" entry (file+olp+datetree "opsat.org") "* TODO %^{Task}\nDEADLINE: %t\n" :immediate-finish t)
        ("b" "Blog" plain (function recker/blog-target) (function recker/blog-template) :immediate-finish t :jump-to-captured t)
        ("h" "Hack" entry (file+olp+datetree "hack.org") "* TODO %^{Task}\n%?")
        ("m" "Therapy" plain (file+olp+datetree "therapy.org.gpg"))
        ("w" "Work" entry (file+olp+datetree "work.org.gpg") "* TODO %^{Task}\n%?")
        ("W" "Meeting" entry (file+olp+datetree "work.org.gpg") "* %^{Meeting title}\nSCHEDULED: %T\n%?" :jump-to-captured t)
        ("B" "Bible" plain (file+olp+datetree "bible.org") "" :jump-to-captured t)))

(global-set-key (kbd "C-c c") 'org-capture)
