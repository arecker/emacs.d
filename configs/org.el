;;;;;;;;;;;
;; BABEL ;;
;;;;;;;;;;;

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
      '(("t" "todays tasks" entry (file "tasks.org") "* TODO %<%A, %B %d %Y> [/]\nSCHEDULED: %t")
        ("m" "miscellaneous task" entry (file "tasks.org") "* TODO %?\nSCHEDULED: %t")
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


(defun recker/org-agenda-switch-to-narrowed-subtree ()
  (interactive)
  (org-agenda-switch-to)
  (org-narrow-to-subtree))

(add-hook 'org-agenda-mode-hook
          (lambda ()
                  (local-set-key (kbd "RET") 'recker/org-agenda-switch-to-narrowed-subtree)))

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

(setq org-agenda-custom-commands '())
      ;; '(("p" "personal agenda"
      ;;    ((agenda)
      ;;     (tags-todo "personal"))
      ;;    ((org-agenda-skip-function '(recker/org-agenda-if-tag "personal"))
      ;;     (org-agenda-overriding-header "Personal Agenda")))
      ;;   ("w" "work agenda"
      ;;    ((agenda)
      ;;     (tags-todo "work"))
      ;;    ((org-agenda-skip-function '(recker/org-agenda-if-tag "work"))))))

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

;; (global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c l") #'sort-lines)

(setq org-startup-with-inline-images nil)
