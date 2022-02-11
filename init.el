(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(defun recker/package-init ()
  "Initialize the package manager and install use-package."
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(defun recker/load-config ()
  "Tangle configuration and load it."
  (let ((config (concat (file-name-as-directory user-emacs-directory) "README.org")))
    (if (file-exists-p config)
        (org-babel-load-file config)
      (warn (concat config " not found - not loading")))))

(recker/package-init)
(recker/load-config)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-ui kubernetes lsp-mode yaml-mode writegood-mode web-mode use-package terraform-mode swiper smex slime-company rich-minority projectile prettier-js plantuml-mode nginx-mode mediawiki markdown-mode magit lua-mode jsonnet-mode idomenu ido-vertical-mode htmlize haskell-mode groovy-mode flycheck expand-region exec-path-from-shell emmet-mode elpy editorconfig dockerfile-mode dictionary deft d-mode company-go bbdb)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
