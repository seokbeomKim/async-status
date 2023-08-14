;;; async-status-test.el --- Async-status example
(require 'async)
(require 'async-status)

;;; Code:

(defun async-status-test (id)
  "Run an example of async-status usage.

`ID' must be set."
  (let* ((async-uuid-val (async-status--req-uuid)) ; 1. Requst UUID
         (async-msg-id-val (format "test #%d" id))
         (local-directory user-emacs-directory)
         (my-load-path (string-join load-path ",")))

    ;; 2. Request a message id with uuid.
    (async-status--req-msg-id async-uuid-val async-msg-id-val)
    ;; 3. Add the message to the status bar. This will watch the file change events.
    (async-status--add-msg-to-bar async-uuid-val async-msg-id-val)
    ;; 4. Show status bar
    (async-status--show)

    ;; Change status frame indicator width
    (setq async-status-indicator-width (/ 462 (window-font-width)))

    (async-start
     `(lambda ()
        ;; Set load-path as parent's directory
        (mapcar (lambda (v)
                  (add-to-list 'load-path v))
                (split-string ,my-load-path ","))
        (setq user-emacs-directory ,local-directory)

        ;; 5. Make sure async-status directory is set properly
        (setq async-status--private-directory
              (expand-file-name "async-status" user-emacs-directory))

        (require 'async-status)
        (let ((count 100.0))
          (dotimes (i count)
            ;; 6. Update the message value. The value MUST BE float.
            (async-status--safely-set-msg-val ,async-uuid-val
                                              ,async-msg-id-val
                                              (/ i count))
            (sleep-for 0.10))))
     `(lambda (res)
        ;; 7. After the use, clear the message-id and uuid
        (async-status--remove-msg-from-bar ,async-uuid-val ,async-msg-id-val)
        (async-status--done-msg-id ,async-uuid-val ,async-msg-id-val)
        (async-status--done-uuid ,async-uuid-val)
        (async-status--hide)))))

(dotimes (i 5)
  (async-status-test i))

(provide 'async-status-test)

;;; async-status-test.el ends here
