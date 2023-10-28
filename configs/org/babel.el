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
