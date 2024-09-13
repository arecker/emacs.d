(require 'org-tempo)

;; (require 'org-attach-git)               ; automatically git commit attachments

(setq org-directory (expand-file-name "~/org"))

;; Modules
(setq org-modules '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))


;;;;;;;;;;;
;; BABEL ;;
;;;;;;;;;;;
(setq org-confirm-babel-evaluate nil)

(global-set-key (kbd "C-c C--") #'org-insert-structure-template)

(org-babel-do-load-languages 'org-babel-load-languages '((python . t)
                                                         (ruby . t)
                                                         (shell . t)))

(setq org-structure-template-alist '(("e" . "src emacs-lisp")
                                     ("p" . "src python")
                                     ("r" . "src ruby")
                                     ("b" . "src bash")
                                     ("d" . "src plantuml")
                                     ("x" . "example")))

(use-package plantuml-mode
  :ensure t
  :config (setq org-plantuml-jar-path "~/.plantuml/plantuml.jar")
  :init
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;;;;;;;;;;;;;
;; CAPTURE ;;
;;;;;;;;;;;;;
(defun recker/blog-target ()
  "Opens today's blog entry."
  (find-file (expand-file-name (format-time-string "~/src/blog/entries/%Y-%m-%d.html")))
  (goto-char (point-min)))

(defun recker/blog-template ()
  "Return the metadata for today's blog post."
  (format-time-string "<!-- meta:title -->\n<!-- meta:banner %Y-%m-%d.jpg -->\n\n"))

(setq org-capture-templates
      '(("t" "personal task" entry (file "personal.org") "* TODO %?\nSCHEDULED: %t")
        ("w" "work task" entry (file "work.org") "* TODO %?\nSCHEDULED: %t")
        ("j" "journal entry" plain (file+olp+datetree "journal.org.gpg") "%^{Grattitude}\n\n%?")
        ("b" "blog entry" plain (function recker/blog-target) (function recker/blog-template) :immediate-finish t :jump-to-captured t)
        ("B" "bible study" plain (file+olp+datetree "bible.org"))))

(global-set-key (kbd "C-c c") 'org-capture)

;;;;;;;;;;;;
;; AGENDA ;;
;;;;;;;;;;;;
(setq org-agenda-files `( ,org-directory ))
(setq org-agenda-file-regexp "\\`[^.].*\\.org\\\(\\.gpg\\\)?\\'")

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-start-with-follow-mode nil)

(setq org-agenda-skip-scheduled-if-done 't)
(setq org-agenda-skip-deadline-if-done 't)
(setq org-agenda-archives-mode nil)
(setq org-deadline-warning-days 3)
(setq org-agenda-span 2)

;; Speed settings
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance nil)
(setq org-agenda-ignore-properties '(effort appt stats category))

(defun recker/org-agenda-if-tag(tag)
  "Skip entries that are tagged TAG"
  (let* ((entry-tags (org-get-tags-at (point))))
    (if (not (member tag entry-tags))
        (progn (outline-next-heading) (point))
      nil)))

(setq org-agenda-custom-commands
      '(("p" "personal agenda"
         ((agenda)
          (tags-todo "personal"))
         ((org-agenda-skip-function '(recker/org-agenda-if-tag "personal"))
          (org-agenda-overriding-header "Personal Agenda")))
        ("w" "work agenda"
         ((agenda)
          (tags-todo "work"))
         ((org-agenda-skip-function '(recker/org-agenda-if-tag "work"))))))

(setq org-use-tag-inheritance 't)
(setq org-agenda-tag-filter-preset '())

;;;;;;;;;;;;;
;; PUBLISH ;;
;;;;;;;;;;;;;
(use-package ox-jira :ensure t)
(use-package htmlize :ensure t)

(setq org-publish-project-alist '())

;; cookbook experiment
(let ((config (expand-file-name "~/src/cookbook/config.el")))
  (if (file-exists-p config)
      (load-file config)))

;; hack to fix yasnippet in org
(defun recker/fix-yas-in-org ()
  (setq-local yas-buffer-local-condition
              '(not (org-in-src-block-p t))))

(add-hook 'org-mode-hook #'recker/fix-yas-in-org)

(add-hook 'org-mode-hook #'turn-off-auto-fill)

(global-visual-line-mode)

(org-indent-mode 0)

(setq org-adapt-indentation nil)

(setq org-cycle-separator-lines -1)

(setq org-goto-auto-isearch nil)

(setq org-clock-persist 'history)

(setq org-log-into-drawer 't)

(org-clock-persistence-insinuate)

(setq org-todo-keywords '((sequence "TODO" "DONE")))

(global-set-key (kbd "C-c l") #'org-store-link)

(setq org-startup-with-inline-images nil)
