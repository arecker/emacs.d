(use-package "org"
  :ensure t
  :pin "org"
  :init
  (setq org-agenda-files '("~/org")
	org-capture-templates
	'(
	  ("i" "idea" entry (file+headline "~/org/personal.org" "Ideas")
	   "* %?"
	   :empty-lines 1)
	  ))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (ruby . t)
     (sh . t)
     (java . t)
     (js . t)
     (C . t)))
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c s" . org-store-link)))

(use-package "org-plus-contrib"
  :ensure t
  :pin "org")
