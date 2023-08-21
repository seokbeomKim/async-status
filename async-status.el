;;; async-status.el --- A package for indicator support  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jason Kim
;;
;; Author: Jason Kim <sukbeom.kim@gmail.com>
;; Maintainer: Jason Kim <sukbeom.kim@gmail.com>
;; Created: August 14, 2023
;; Modified: August 14, 2023
;; Version: 0.1.0
;; Keywords: tools, async
;; Homepage: https://github.com/seokbeomkim/async-status
;; Package-Requires: ((emacs "28.1") (svg-lib "0.2.7") (posframe "1.4.2"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a simple indicator to display the progress of running
;; asynchronously Emacs processes.

;; Setup:
;; (require 'async-status)

;;; Code:
(require 'posframe)
(require 'svg-lib)
(require 'filenotify)

(defconst async-status--file-prefix "async-status-"
  "A prefix string of temporary file name.")

(defgroup async-status nil
  "An indicator to display the status of Emacs processes."
  :link '(url-link "https://github.com/seokbeomKim/async-status")
  :group 'alert)

(defcustom async-status-indicator-width (/ 462 (window-font-width))
  "The width of indicator bar."
  :type 'integer
  :group 'async-status)

(defcustom async-status-progress-bar-width 150.0
  "The default x-width of the progress bar."
  :type 'float
  :group 'async-status)

(defcustom async-status-progress-bar-height 20.0
  "The default x-height of the progress bar."
  :type 'float
  :group 'async-status)

(defvar async-status--shown-items '()
  "Items to display in the status bar.

The items must be typed of `async-status--item'.")

(cl-defstruct async-status--item
  "A structure of async-status--item.

`MSG-ID': Message id returned by `async-status-req-id'.

`FS-WATCHER-ID': File watcher descriptor. This must be set by
calling `async-status-add-item-to-bar'.

`FILE-PATH': The target file to watch file events. Emacs process
running asynchronously must set the progress value <- [0, 1.0].

`PROGRESS': The progress value. You MUST NOT set this value. This
is used as a threshold to update status bar.

`LABEL': A string value to display."
  msg-id fs-watcher-id file-path progress label)

(defun async-status--get-absolute-path-by-id (id)
  "Return the absolute file path by `ID'."
  (expand-file-name id temporary-file-directory))

(defun async-status-req-id (name)
  "Request ID by `NAME'.

If the related file exists, then the function returns FAIL."
  (let* ((fname (format "%s-%s-" async-status--file-prefix name))
         (tmpfile (make-temp-file fname)))
    (with-temp-file tmpfile
        (insert "0"))
    (file-name-base tmpfile)))

(defun async-status-clean-up (id)
  "Clean up the temporary corresponding file to `ID'."
  (delete-file (async-status--get-absolute-path-by-id id)))

(defun async-status--get-msg-val (id)
  "Get the value of `ID'."
  (let* ((filepath (async-status--get-absolute-path-by-id id)))
    (with-temp-buffer
      (insert-file-contents filepath)
      (string-trim (buffer-string)))))

(defun async-status-safely-set-msg-val (id val &optional threshold)
  "Set `VAL' to `ID'.

This function supports `THRESHOLD' that prevents Emacs from being
held by ridiculous file updates."
  (if (not (floatp val))
      (error "The type of `VAL' is not float")
    (let* ((prev_val (string-to-number (async-status--get-msg-val id))))
      (if (< (+ prev_val (or threshold 0.01)) val)
          (async-status-set-msg-val id val)))))

(defun async-status-set-msg-val (id val)
  "Set `VAL' to `ID'.

Be careful that using this function can cause UI hangout. Try to
use `async-status-safely-set-msg-val' instead."
  (let* ((filepath (async-status--get-absolute-path-by-id id))
         (strval (prin1-to-string val)))
    (with-temp-buffer
      (insert strval)
      (write-region (point-min) (point-max) filepath nil))))

(defun async-status-show ()
  "Show a status bar and update the *async-status* buffer."
  (posframe-show "*async-status*"
                 :border-color (foreground-color-at-point)
                 :border-width 2
                 :left-fringe 10
                 :right-fringe 10
                 :min-width async-status-indicator-width
                 :max-width async-status-indicator-width
                 :min-height (length async-status--shown-items)
                 :max-height (length async-status--shown-items)
                 :poshandler 'posframe-poshandler-frame-top-center)
  (async-status--refresh-status-bar))

(defun async-status-hide (&optional force)
  "Hide the status bar.

By default, this function would not hide the status bar. If you
are willing to hide forcely, set `FORCE' to t."
  (if (or force
          (null async-status--shown-items))
      (posframe-hide "*async-status*")))

(defun async-status--print-truncated-string (str max-length)
  "Print STR truncated with '...' if its length exceeds MAX-LENGTH."
  (if (> (length str) max-length)
      (format "%s..." (substring str 0 (- max-length 3))) str))

(defun async-status--redraw-item (item)
  "Redraw an `ITEM' of async-status.

The item must be typed of `async-status--item'."
  (with-current-buffer "*async-status*"
    (insert (format "%-26s"
                    (async-status--print-truncated-string (async-status--item-label item) 21)))
    (let ((progress (async-status--item-progress item)))
      (if (not (numberp progress))
          (setq progress (string-to-number progress)))
      (insert-image (svg-lib-progress-bar
                     progress
                     svg-lib-style-default
                     :height (/ async-status-progress-bar-height
                                (window-font-height))
                     :width (/ async-status-progress-bar-width
                               (window-font-width))))
      (insert "\n"))))

(defun async-status--refresh-status-bar ()
  "Refresh the status bar."
  (with-current-buffer "*async-status*"
    (erase-buffer))
  (mapcar #'async-status--redraw-item async-status--shown-items))

(defun async-status--update-items (event)
  "Update *async-status* buffer.

This function is the callback function of file `EVENT'."
  (let* ((filepath (nth 2 event))
         (msg-id (file-name-base filepath))
         (item (async-status--find-item-by-msgid msg-id)))
    (setf (async-status--item-progress item) (async-status--get-msg-val msg-id)))
  (async-status--refresh-status-bar)
  (async-status-show))

(defun async-status--find-item-by-msgid (id)
  "Find item by `ID'."
  (catch 'rval
    (dolist (item async-status--shown-items)
      (when (async-status--item-p item)
        (if (string= id (async-status--item-msg-id item))
            (throw 'rval item))))))

(defun async-status--remove-item (item)
  "Remove `ITEM' from `async-status--shown-items'."
  (setq async-status--shown-items
        (cl-remove-if (lambda (v) (eq v item))
                      async-status--shown-items)))

(defun async-status-add-item-to-bar (id &optional label)
  "Push the message item to status bar.

`ID' represents message id.
`LABEL' to display on indicator."
  (let* ((msg-file (async-status--get-absolute-path-by-id id))
         (new-fwatch-id (file-notify-add-watch msg-file
                                               '(change)
                                               'async-status--update-items))
         (new-item (make-async-status--item :msg-id id
                                            :fs-watcher-id new-fwatch-id
                                            :file-path msg-file
                                            :progress 0
                                            :label (or label id))))
    (push new-item async-status--shown-items)))

(defun async-status-remove-item-from-bar (id)
  "Pop the message item from the status bar.

`ID' represents message id."
  (let* ((item (async-status--find-item-by-msgid id)))
    (when item
      (file-notify-rm-watch (async-status--item-fs-watcher-id item))
      (async-status--remove-item item)
      (async-status--refresh-status-bar))))

(provide 'async-status)
;;; async-status.el ends here
