(require 'org-tempo)

;; hack to fix yasnippet in org
(defun recker/fix-yas-in-org ()
  (setq-local yas-buffer-local-condition
              '(not (org-in-src-block-p t))))

(add-hook 'org-mode-hook #'recker/fix-yas-in-org)

(setq org-adapt-indentation nil)
(setq org-cycle-separator-lines -1)
(setq org-goto-auto-isearch nil)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(global-set-key (kbd "C-c C--") #'org-insert-structure-template)
(setq org-confirm-babel-evaluate nil)
(setq org-structure-template-alist '(("e" . "src emacs-lisp")
                                     ("p" . "src python")
                                     ("r" . "src ruby")
                                     ("b" . "src bash")
                                     ("d" . "src plantuml")
                                     ("x" . "example")))

;; diagrams with plantuml
;; the plantuml jar should be located here:
(setq org-plantuml-jar-path "~/.plantuml/plantuml.jar")

(use-package plantuml-mode
  :ensure t
  :init
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

(setq org-directory (expand-file-name "~/org"))
(directory-files org-directory)
(setq org-agenda-files `( ,org-directory ))

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-start-with-follow-mode t)

(setq org-agenda-skip-scheduled-if-done 't)
(setq org-agenda-skip-deadline-if-done 't)
(setq org-agenda-archives-mode nil)

;; custom views
(setq org-agenda-custom-commands '())

(setq recker/custom-agenda '("a" "agenda"
                             ((agenda "" ((org-deadline-warning-days 0)
                                          (org-agenda-span 2))))))

(add-to-list 'org-agenda-custom-commands recker/custom-agenda)

;; capture templates
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates '())

(let ((opsat (concat org-directory "/opsat.org")))
  (add-to-list 'org-capture-templates
               `("t" "Misc. TODO" entry (file ,opsat)
                 "* TODO %?\n" :empty-lines 1)))

;; prettify code exported to HTML
(use-package htmlize :ensure t)

;; org publish is interesting, I should look into that.
(setq org-publish-project-alist '())