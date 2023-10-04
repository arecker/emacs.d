;; configure how emacs looks

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; I sometimes use this theme (or other themes)
;; (use-package modus-themes
;;   :ensure t
;;   :config (load-theme 'modus-operandi-tinted 1 nil))

;; Use monaco font if running on MacOS
(when (string= system-type "darwin")
  (set-frame-font "Monaco 18" nil t))

;; Use monospace font on linux
(when (string= system-type "gnu/linux")
  (set-frame-font "Monospace 13" nil t))

;; Use this package to hide the minor modes.  Some might like this,
;; but I think it just clutters up the modeline.
(unless (bound-and-true-p rich-minority-mode) ;it breaks if it runs twice?
  (use-package rich-minority
    :ensure t
    :init (rich-minority-mode 't)
    :config (setq rm-blacklist "")))
