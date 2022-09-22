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
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(git-timemachine nftables-mode rust-mode zenburn-theme yaml-mode writegood-mode web-mode use-package twittering-mode swiper spacemacs-theme smex slime-company rich-minority projectile prettier-js plantuml-mode ox-jira nginx-mode monokai-theme mediawiki magit lua-mode lsp-ui lsp-python-ms lsp-pyright kubernetes jsonnet-mode idomenu ido-vertical-mode htmlize haskell-mode groovy-mode flycheck fireplace expand-region exec-path-from-shell emmet-mode elpy eglot editorconfig edit-indirect doom-themes dockerfile-mode dictionary dhall-mode deft dap-mode d-mode csv-mode company-terraform company-go bbdb bats-mode autumn-light-theme async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
