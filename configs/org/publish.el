(use-package ox-jira :ensure t)
(use-package htmlize :ensure t)

(setq org-publish-project-alist '())

;; cookbook experiment
(let ((config (expand-file-name "~/src/cookbook/config.el")))
  (if (file-exists-p config)
      (load-file config)))
