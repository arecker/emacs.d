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
      '(("t" "Task" entry (file "misc.org") "* TODO %?\nSCHEDULED: %t")
        ("b" "Blog Entry" plain (function recker/blog-target) (function recker/blog-template) :immediate-finish t :jump-to-captured t)))

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
      '(("c" "chores" tags-todo "chores")
        ("h" "habits" tags-todo "habits")
        ("m" "misc" tags-todo "misc")))

(defvar my/org-habit-show-graphs-everywhere nil
  "If non-nil, show habit graphs in all types of agenda buffers.

Normally, habits display consistency graphs only in
\"agenda\"-type agenda buffers, not in other types of agenda
buffers.  Set this variable to any non-nil variable to show
consistency graphs in all Org mode agendas.")
(setq my/org-habit-show-graphs-everywhere t)

(defun my/org-agenda-mark-habits ()
  "Mark all habits in current agenda for graph display.

This function enforces `my/org-habit-show-graphs-everywhere' by
marking all habits in the current agenda as such.  When run just
before `org-agenda-finalize' (such as by advice; unfortunately,
`org-agenda-finalize-hook' is run too late), this has the effect
of displaying consistency graphs for these habits.

When `my/org-habit-show-graphs-everywhere' is nil, this function
has no effect."
  (when (and my/org-habit-show-graphs-everywhere
         (not (get-text-property (point) 'org-series)))
    (let ((cursor (point))
          item data)
      (while (setq cursor (next-single-property-change cursor 'org-marker))
        (setq item (get-text-property cursor 'org-marker))
        (when (and item (org-is-habit-p item))
          (with-current-buffer (marker-buffer item)
            (setq data (org-habit-parse-todo item)))
          (put-text-property cursor
                             (next-single-property-change cursor 'org-marker)
                             'org-habit-p data))))))

(advice-add #'org-agenda-finalize :before #'my/org-agenda-mark-habits)

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
