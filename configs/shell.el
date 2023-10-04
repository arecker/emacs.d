;; sync the system PATH with what emacs sees
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; C-x t to open terminal
(defun recker/ansi-term ()
  "Launch ansi-term with current shell."
  (interactive)
  (let ((shell (or (getenv "SHELL") "/bin/bash")))
    (ansi-term shell)))

(global-set-key (kbd "C-x t") 'recker/ansi-term)

;; C-c e to open eshell
(global-set-key (kbd "C-c e") 'eshell)

;; Automatically close the buffer when the shell exits
(defun recker/handle-term-exit (&optional process-name msg)
  (message "%s | %s" process-name msg)
  (kill-buffer (current-buffer)))

(advice-add 'term-handle-exit :after 'recker/handle-term-exit)
