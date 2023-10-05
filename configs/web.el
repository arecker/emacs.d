(use-package emmet-mode
  :ensure t
  :config
  (setq emmet-move-cursor-between-quotes t)
  :mode ("\\.html\\'" "\\.html.j2\\'"))

(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html.j2\\'" . html-mode))

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
