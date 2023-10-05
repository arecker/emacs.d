;; clean up whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; don't insert tabs (unless the mode wants to)
(setq-default indent-tabs-mode nil)
