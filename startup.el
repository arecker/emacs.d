(defun recker/startup-scratch-buffer ()
  (setq inhibit-startup-message 't)
  (let ((wilfred-installed (executable-find "wilfred-say"))
	(fortune-installed (executable-find "fortune"))
	(comment-command-output (lambda (c)
				  (concat
				   (mapconcat
				    (lambda (x) (concat ";; " x))
				    (split-string (shell-command-to-string c) "\n" t) "\n")
				   "\n" "\n"))))
    (if wilfred-installed
	(setq initial-scratch-message
	      (funcall comment-command-output "wilfred-say"))
      (if fortune-installed
	  (setq initial-scratch-message
		(funcall comment-command-output "fortune"))))))

(recker/startup-scratch-buffer)

(defun recker/startup-registers ()
  (set-register ?b '(file . "~/git/blog"))
  (set-register ?d '(file . "~/Desktop"))
  (set-register ?e '(file . "~/.emacs.d/README.org"))
  (set-register ?g '(file . "~/git"))
  (set-register ?i '(file . "~/.emacs.d/init.el"))
  (set-register ?o '(file . "~/org"))
  (set-register ?p '(file . "~/org/personal.org"))
  (set-register ?w '(file . "~/org/work.org")))

(recker/startup-registers)

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))
