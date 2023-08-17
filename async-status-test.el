;;; async-status-test.el --- Async-status example
(require 'async)
(require 'async-status)

;;; Code:

(defun async-status-test (id)
  "Run an example of async-status usage.

`ID' must be set."
  (let* ((name-to-req-id (format "test-%d" id))
         (alloc-id (async-status-req-id name-to-req-id)) ; 1. Request ID
         (my-load-path (string-join load-path ",")))

    ;; 2. Add the message to the status bar. This will watch the file change events.
    (async-status-add-item-to-bar alloc-id name-to-req-id)
    ;; 3. Show status bar
    (async-status-show)

    ;; Change status frame indicator width
    (setq async-status-indicator-width (/ 462 (window-font-width)))

    (async-start
     `(lambda ()
        ;; Set load-path as parent's directory
        (mapcar (lambda (v)
                  (add-to-list 'load-path v))
                (split-string ,my-load-path ","))

        (require 'async-status)
        (let ((count 100.0))
          (dotimes (i count)
            ;; 4. Update the message value. The value MUST BE float.
            (async-status-safely-set-msg-val ,alloc-id
                                              (/ i count))
            (sleep-for 0.10))))
     `(lambda (res)
        ;; 5. After the use, clean up the used item
        (async-status-remove-item-from-bar ,alloc-id)
        (async-status-clean-up ,alloc-id)
        (async-status-hide)))))

(dotimes (i 5)
  (async-status-test i))

(provide 'async-status-test)

;;; async-status-test.el ends here
