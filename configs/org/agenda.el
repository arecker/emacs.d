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
      '(("w" "Work Projects"
         ((tags-todo "work")))
        ("h" "Hack Projects"
         ((tags-todo "hack")))
        ("C" "Worship Notes"
         ((tags-todo "worship")))))
