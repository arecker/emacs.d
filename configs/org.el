(require 'org-tempo)

(require 'org-attach-git)               ; automatically git commit attachments

(setq org-directory (expand-file-name "~/org"))

(recker/load-config "org/babel")
(recker/load-config "org/capture")
(recker/load-config "org/agenda")
(recker/load-config "org/publish")

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

(org-clock-persistence-insinuate)

(setq org-todo-keywords '((sequence "TODO" "DONE")))

(global-set-key (kbd "C-c l") #'org-store-link)

(setq org-startup-with-inline-images t)
