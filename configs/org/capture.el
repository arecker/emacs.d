(defun recker/build-opsat-today-capture ()
  `("o" "Today"
    entry
    (file+olp+datetree ,(concat org-directory "/opsat.org"))
    ,(format-time-string "* TODO %A, %B %d %Y [/]\nDEADLINE: <%Y-%m-%d %a>\n%?")
    :tree-type month
    :immediate-finish t))

(defun recker/build-opsat-task-capture ()
  `("t" "Task"
    checkitem
    (file+headline ,(concat org-directory "/opsat.org") ,(format-time-string "%A, %B %d %Y"))
    nil))

(defun recker/build-blog-capture ()
  `("b"
   "Blog"
   plain
   (file ,(expand-file-name (format-time-string "~/src/blog/entries/%Y-%m-%d.html")))
   ,(format-time-string "<!-- meta:title -->\n<!-- meta:banner %Y-%m-%d.jpg -->\n\n%?")
   :immediate-finish t
   :jump-to-captured t))

(defun recker/build-capture-templates ()
  (let* ((org org-directory)
         (work (list 'file (concat org "/work.org.gpg")))
         (hack (list 'file (concat org "/hack.org"))))
    `(,(recker/build-opsat-today-capture)
      ,(recker/build-opsat-task-capture)
      ("w" "Work" entry ,work "* TODO %?\n" :empty-lines 1)
      ,(recker/build-blog-capture)
      ("h" "Hack" entry ,hack "* TODO %? [/]\n" :empty-lines 1)
      ("m" "Therapy" plain (file+olp+datetree ,(concat org-directory "/therapy.org.gpg")))
      ("n" "Notes (work)" entry ,work "* %?\n" :empty-lines 1 :jump-to-captured t))))

(setq org-capture-templates (recker/build-capture-templates))
(global-set-key (kbd "C-c c") 'org-capture)
