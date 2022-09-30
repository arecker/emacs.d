(require 'org-tempo)

(add-hook 'org-mode-hook #'recker/fix-yas-in-org)
(add-hook 'org-babel-after-execute-hook #'recker/org-rerender-images)

(setq org-confirm-babel-evaluate nil)
(setq org-goto-auto-isearch nil)
(setq org-adapt-indentation nil)
(setq org-cycle-separator-lines -1)
(setq org-capture-templates '())
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done  t
      org-agenda-archives-mode nil)
(setq org-publish-project-alist '())
(setq org-agenda-start-with-follow-mode t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((java . t)
   (perl . t)
   (python . t)
   (ruby . t)
   (shell . t)))
