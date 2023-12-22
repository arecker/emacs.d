(require 'org-tempo)

;; (require 'org-attach-git)               ; automatically git commit attachments

(setq org-directory (expand-file-name "~/Dropbox/org"))

;; Modules
(setq org-modules '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))


;;;;;;;;;;;
;; BABEL ;;
;;;;;;;;;;;
(setq org-confirm-babel-evaluate nil)

(global-set-key (kbd "C-c C--") #'org-insert-structure-template)

(org-babel-do-load-languages 'org-babel-load-languages '((python . t)
                                                         (ruby . t)))

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
      '(("b" "Blog" plain (function recker/blog-target) (function recker/blog-template) :immediate-finish t :jump-to-captured t)))

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
(setq org-deadline-warning-days 5)
(setq org-agenda-span 'week)

;; Speed settings
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance nil)
(setq org-agenda-ignore-properties '(effort appt stats category))

(setq org-agenda-custom-commands
      '())

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

(add-hook 'org-mode-hook #'turn-on-auto-fill)

(org-indent-mode 0)

(setq org-adapt-indentation nil)

(setq org-cycle-separator-lines -1)

(setq org-goto-auto-isearch nil)

(setq org-clock-persist 'history)

(setq org-log-into-drawer 't)

(org-clock-persistence-insinuate)

(setq org-todo-keywords '((sequence "TODO" "DONE")))

(global-set-key (kbd "C-c l") #'org-store-link)

(setq org-startup-with-inline-images t)
