(use-package ox-jira :ensure t)
(use-package htmlize :ensure t)

(setq org-publish-project-alist '())

;; blog experiment
(let ((config (expand-file-name "~/src/blog-org/config.el")))
  (if (file-exists-p config)
      (load-file config)))
