(setq delete-selection-mode 't)

;; handy functions
(global-set-key (kbd "C-c r") 'replace-string)

;; upcase a region without emacs bothering you
(put 'upcase-region 'disabled nil)

(use-package expand-region
  :ensure t
  :bind (("C-=" . 'er/expand-region)))
