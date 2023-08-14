# async-status
A Emacs package to show the status of asynchronous progress.

![async-status-usage](https://github.com/seokbeomKim/async-status/blob/images/async-status.gif "async-status usage")

## Installation

```
(require 'async-status)
```

## What is this package?

This package supports status bar to show the progress of asynchronous process.
Even though Emacs does not support threads, some packages similar to 'async' are
invoking Emacs process for the same purpose, but we need to wait without knowing
the progress. By using this package, packages using 'async' can notify the users
to know the working progress.

## Usage

Please refer 'async-status-test.el'. By evaluating the file, you can see the
result.

### APIs for parent process

- `async-status--req-uuid` returns UUID. 
- `async-status--req-msg-id` creates the file to interact with the child
  process.
- `async-status--add-msg-to-bar` registers the corresponding item to the
  indicator.
- `async-status--remove-msg-from-bar` removes the corresponding item from the
  indicator.
- `async-status--show` displays the indicator.
- `async-status--hide` hides the indicator unless there are no registered items.
- `async-status--done-msg-id` and `async-status--done-uuid` clean the
  corresponding file nodes.

### APIs for child process

- `async-status--safely-set-msg-val` writes the value to corresponding file
  node.

## Feedback

If you have any feedback, suggestions for improvements or advice, please feel
free to get in touch. 
