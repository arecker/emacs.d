;; don't show the splash screen
(setq inhibit-splash-screen 't)

;; never kill the scratch buffer
(defun recker/not-scratch-p ()
  "Return NIL if the current buffer is the *scratch* buffer."
  (not (equal (buffer-name (current-buffer)) "*scratch*")))

(add-hook 'kill-buffer-query-functions 'recker/not-scratch-p)

;; display the output of "fortune" as the scratch message
(setq recker/scratch-message-command "fortune")

(defun recker/scratch-message ()
  "Return a scratch message from fortune-blog."
  (with-temp-buffer
    (insert (shell-command-to-string recker/scratch-message-command))
    (let ((comment-start ";; "))
      (comment-region (point-min) (point-max)))
    (fill-individual-paragraphs (point-min) (point-max))
    (concat "\n" (buffer-string) "\n")))

(setq initial-scratch-message (recker/scratch-message))
