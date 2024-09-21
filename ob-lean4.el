(defvar ob-lean4-lean-bin "lean")
(defvar ob-lean4-repl-bin "repl")
(defvar ob-lean4--session-process nil)
(defvar ob-lean4--last-env-id nil)

(defun ob-lean4--at-end-of-input ()
  (and (> (- (point-max) (point-min)) 2)
       (string= (buffer-substring (- (point-max) 2) (point-max)) "\n\n")))

(defun ob-lean4--session-command (body)
  (let ((command `(("cmd" . ,body))))
    (when ob-lean4--last-env-id
      (add-to-list 'command `("env" . ,ob-lean4--last-env-id)))
    (concat (json-encode command) "\r\n\r\n")))

(defun ob-lean4--evaluate-with-session (body)
  (let* ((session-process (ob-lean4--session-process))
         (session-buffer (process-buffer session-process)))
    (with-current-buffer session-buffer
      (erase-buffer)
      (process-send-string session-process (ob-lean4--session-command body))
      (while (not (ob-lean4--at-end-of-input))
        (accept-process-output session-process 0.01))
      (beginning-of-buffer)
      (json-read-object))))

(defun ob-lean4--session-process ()
  (or ob-lean4--session-process (ob-lean4--initiate-session)))

(defun ob-lean4--initiate-session ()
  (let* ((default-directory (file-name-parent-directory ob-lean4-repl-bin))
         (lean-path (file-name-parent-directory ob-lean4-lean-bin))
         (process-environment (cons (format "PATH=%s" (concat (getenv "PATH") ":" lean-path))
                                    process-environment))
         (process (make-process
                   :name "lean4-repl"
                   :buffer "*Lean*"
                   :connection-type 'pipe
                   :command `(,ob-lean4-repl-bin))))
    (setq ob-lean4--session-process process)))

(defun ob-lean4--fold-repl-message-to-string (acc diagnostic)
  (concat acc (format "%s" (alist-get 'data diagnostic)) "\n"))

(defun ob-lean4--process-repl-response (response)
  (let* ((messages (alist-get 'messages response))
         (env-id (alist-get 'env response))
         (response-string
          (seq-reduce #'ob-lean4--fold-repl-message-to-string messages "")))
    (setq ob-lean4--last-env-id env-id)
    (org-babel-trim response-string)))

(defun ob-lean4--evaluate (body)
  (let ((tmp-file (org-babel-temp-file "lean-src-" ".lean")))
    (with-temp-file tmp-file (insert body))
    (org-babel-eval (format "%s %s" ob-lean4-lean-bin tmp-file) "")))

(defun org-babel-execute:lean4 (body params)
  (let* ((processed-params (org-babel-process-params params))
         (session-name (cdr (assq :session processed-params))))
    (print session-name)
    (if session-name
        (ob-lean4--process-repl-response
         (ob-lean4--evaluate-with-session body))
      (ob-lean4--evaluate body))))

(provide 'ob-lean4)
