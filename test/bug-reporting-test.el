;; -*- lexical-binding: t -*-

(require 'demangle-test-helper "test-helper")

(require 'shut-up)

(defun demangle-test-interactive-bug-report (use-github)
  (cl-letf (((symbol-function #'y-or-n-p) (lambda (_prompt) use-github)))
    (shut-up
      (call-interactively #'demangle-mode-submit-bug-report))))

(ert-deftest demangle-test-bug-report-github ()
  "simulate submitting a bug report using GitHub"
  (let* ((browsed-urls)
	 (browse-url-browser-function
	  (lambda (url &rest _args)
	    (push url browsed-urls))))
    (demangle-test-interactive-bug-report t)
    (should (equal '("https://github.com/liblit/demangle-mode/issues") browsed-urls))))

(ert-deftest demangle-test-bug-report-reporter ()
  "simulate submitting a bug report using reporter"
  (save-window-excursion
    (cl-letf (((symbol-function #'read-string) (lambda (&rest ignored) "canary")))
      (demangle-test-interactive-bug-report nil))
    (should (eq major-mode 'message-mode))
    (set-buffer-modified-p nil)
    (kill-buffer)))
