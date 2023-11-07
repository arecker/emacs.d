(defun recker/blog-target ()
  "Opens today's blog entry."
  (find-file (expand-file-name (format-time-string "~/src/blog/entries/%Y-%m-%d.html")))
  (goto-char (point-min)))

(defun recker/blog-template ()
  "Return the metadata for today's blog post."
  (format-time-string "<!-- meta:title -->\n<!-- meta:banner %Y-%m-%d.jpg -->\n\n"))

(setq org-capture-templates
      '(("t" "Task - Today" entry (file+olp+datetree "opsat.org") "* TODO %^{Task}\nDEADLINE: %t\n" :immediate-finish t)
        ("h" "Task - Hacking" entry (file+olp+datetree "hack.org") "* TODO %^{Task}\n%?")
        ("j" "Journaling")
        ("jb" "Blog" plain (function recker/blog-target) (function recker/blog-template) :immediate-finish t :jump-to-captured t)
        ("jm" "Therapy" plain (file+olp+datetree "therapy.org.gpg"))
        ("jt" "Thoughts" plain (file+olp+datetree "thoughts.org.gpg"))
        ("w" "Work")
        ("wt" "Work - Task" entry (file+olp+datetree "work.org.gpg") "* TODO %^{Task}\n%?")
        ("wm" "Work - Meeting" entry (file+olp+datetree "work.org.gpg") "* %^{Meeting title}\nSCHEDULED: %T\n%?" :jump-to-captured t)
        ("c" "Church")
        ("cb" "Bible Study Notes" plain (file+olp+datetree "bible.org") "" :jump-to-captured t)
        ("cw" "Worship Set" entry (file+olp+datetree "worship.org") "* TODO Worship Set\nSCHEDULED: %^t\n" :jump-to-captured t)))

(global-set-key (kbd "C-c c") 'org-capture)
