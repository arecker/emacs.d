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
  (concat "\n"
          (recker/scratch-lisp-comment (format-time-string "%A, %B %-d %Y"))
          "\n\n"
          (recker/random-scratch-ascii)
          "\n\n"
          (recker/scratch-lisp-comment
           (shell-command-to-string recker/scratch-message-command))))

(defun recker/random-scratch-ascii ()
  "Return a lisp random ascii image from emacs.d/ascii."
  (let* ((ascii-files (file-expand-wildcards (concat user-emacs-directory "ascii/*.txt")))
         (choice (expand-file-name (nth (random (length ascii-files)) ascii-files))))
    (recker/scratch-lisp-comment (with-temp-buffer
                                   (insert-file-contents-literally choice)
                                   (buffer-string)))))

(defun recker/scratch-lisp-comment (text)
  "Turn text into a lisp comment."
  (with-temp-buffer
    (insert text)
    (let ((comment-start ";; "))
      (comment-region (point-min) (point-max)))
    (concat "\n" (buffer-string) "\n")
    (buffer-string)))

(setq initial-scratch-message (recker/scratch-message))
