;;; async-status.el --- A package for indicator support
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
;; Package-Requires: ((emacs "28.1") (posframe) (svg-lib "0.2.7"))

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

(defvar async-status--private-directory
  (expand-file-name "async-status" user-emacs-directory)
  "Default directory path for async-status package.

This must be set for `async-start', since the
`USER-EMACS-DIRECTORY' could be different according to the
runtime environment.")

(defvar async-status--shown-items '()
  "Items to display in the status bar.

The items must be typed of `async-status--item'.")

(cl-defstruct async-status--item
  "A structure of async-status--item.

`UUID': UUID returned by `async-status--req-uuid'.

`MSG_ID': Message id returned by `async-status--req-msg-id'.

`FS_WATCHER_ID': File watcher descriptor. This must be set by
calling `async-status--add-msg-to-bar'.

`FILE_PATH': The target file to watch file events. Emacs process
running asynchronously must set the progress value <- [0, 1.0].

`PROGRESS': The progress value. You MUST NOT set this value. This
is used as a threshold to update status bar."
  uuid msg_id fs_watcher_id file_path progress)

(defun async-status--init ()
  "Remove out all directories.
This function must be invoked at initial time."
  (ignore-errors (delete-directory async-status--private-directory t))
  (ignore-errors (mkdir async-status--private-directory)))

(defun async-status--get-random-uuid ()
  "Generate a random UUID."
  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 6))
          (random (expt 16 6))))

(defun async-status--req-uuid ()
  "Generate UUID and create temporary directory."
  (let* ((uuid (async-status--get-random-uuid))
         (dirpath (expand-file-name uuid async-status--private-directory)))
    (mkdir dirpath) uuid))

(defun async-status--done-uuid (uuid)
  "Remove the `UUID' directory."
  (let* ((dirpath (expand-file-name uuid async-status--private-directory)))
    (delete-directory dirpath t)))

(defun async-status--req-msg-id (uuid id)
  "Request `ID' with `UUID'.

If the related file exists, then the function returns FAIL."
  (let* ((dirpath (expand-file-name uuid async-status--private-directory))
         (filepath (expand-file-name id dirpath)))
    (condition-case err
        (progn
          (make-empty-file filepath)
          (with-temp-buffer
            (insert "0")
            (write-region (point-min) (point-max) filepath nil))
          t)
      (error nil))))

(defun async-status--done-msg-id (uuid id)
  "Clean up `ID' with `UUID'."
  (let* ((dirpath (expand-file-name uuid async-status--private-directory))
         (filepath (expand-file-name id dirpath)))
    (delete-file filepath)))

(defun async-status--get-msg-val (uuid id)
  "Get the value of `ID' of `UUID'."
  (let* ((dirpath (expand-file-name uuid async-status--private-directory))
         (filepath (expand-file-name id dirpath)))
    (with-temp-buffer
      (insert-file-contents filepath)
      (string-trim (buffer-string)))))

(defun async-status--safely-set-msg-val (uuid id val &optional threshold)
  "Set `VAL' to `ID' of `UUID'.

This function supports `THRESHOLD' that prevents Emacs from being
held by ridiculous file updates."
  (if (not (floatp val))
      (error "The type of `VAL' is not float")
    (let* ((prev_val (string-to-number (async-status--get-msg-val uuid id))))
      (if (< (+ prev_val (or threshold 0.01)) val)
          (async-status--set-msg-val uuid id val)))))

(defun async-status--set-msg-val (uuid id val)
  "Set `VAL' to `ID' of `UUID'.

Be careful that using this function can cause UI hangout. Try to
use `async-status--safely-set-msg-val' instead."
  (let* ((dirpath (expand-file-name uuid async-status--private-directory))
         (filepath (expand-file-name id dirpath))
         (strval (prin1-to-string val)))
    (with-temp-buffer
      (insert strval)
      (write-region (point-min) (point-max) filepath nil))))

(defun async-status--show ()
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

(defun async-status--hide (&optional force)
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
                    (async-status--print-truncated-string (async-status--item-msg_id item) 21)))
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
         (msg_id (file-name-base filepath))
         (uuid (file-name-base (directory-file-name (file-name-directory filepath))))
         (item (async-status--find-item-by-uuid-and-msgid uuid msg_id)))
    (setf (async-status--item-progress item) (async-status--get-msg-val uuid msg_id)))
  (async-status--refresh-status-bar)
  (async-status--show))

(defun async-status--find-item-by-uuid-and-msgid (uuid id)
  "Find item by `UUID' and msg `ID'."
  (catch 'rval
    (dolist (item async-status--shown-items)
      (when (async-status--item-p item)
        (if (and (string= uuid (async-status--item-uuid item))
                 (string= id (async-status--item-msg_id item)))
            (throw 'rval item))))))

(defun async-status--remove-item (item)
  "Remove `ITEM' from `async-status--shown-items'."
  (setq async-status--shown-items
        (cl-remove-if (lambda (v) (eq v item))
                      async-status--shown-items)))

(defun async-status--add-msg-to-bar (uuid id)
  "Push the message item to status bar.

`UUID' is corresponding uuid for the message. `ID' represents
message id."
  (let* ((uuid-dir (expand-file-name uuid async-status--private-directory))
         (msg-file (expand-file-name id uuid-dir))
         (new-fwatch-id (file-notify-add-watch msg-file
                                               '(change)
                                               'async-status--update-items))
         (new-item (make-async-status--item :uuid uuid
                                            :msg_id id
                                            :fs_watcher_id new-fwatch-id
                                            :file_path msg-file
                                            :progress 0)))
    (push new-item async-status--shown-items)))

(defun async-status--remove-msg-from-bar (uuid id)
  "Pop the message item from the status bar.

`UUID' is corresponding uuid for the mssage. `ID' represents
message id."
  (let* ((item (async-status--find-item-by-uuid-and-msgid uuid id)))
    (when item
      (file-notify-rm-watch (async-status--item-fs_watcher_id item))
      (async-status--remove-item item)
      (async-status--refresh-status-bar))))

(provide 'async-status)
;;; async-status.el ends here
