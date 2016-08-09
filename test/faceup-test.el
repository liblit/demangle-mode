(require 'demangle-mode)
(require 'ert)
(require 'faceup)

(defun fontify-and-drain-demangler (beg end &optional verbose)
  (font-lock-default-fontify-region beg end verbose)
  (while (not (tq-queue-empty demangler-queue))
    (accept-process-output (tq-process demangler-queue) 5)))

(let ((default-directory (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (raw-name (file-expand-wildcards "faceup/*.raw"))
    (dolist (show-as '(demangled mangled))
      (let* ((base-name (file-name-sans-extension raw-name))
	     (faceup-name (format "%s.%s" base-name show-as))
	     (test-name-string (format "demangle-faceup-test-%s-%s" base-name show-as))
	     (test-name-symbol (intern test-name-string)))
	(eval `(ert-deftest ,test-name-symbol ()
		 ,(format "compare “%s” with “%s”, showing %s symbols" raw-name faceup-name show-as)
		 (let ((faceup-properties '(face display help-echo))
		       (faceup-text (with-temp-buffer
				      (insert-file-contents ,(expand-file-name faceup-name))
				      (buffer-substring-no-properties (point-min) (point-max))))
		       (font-lock-fontify-region-function #'fontify-and-drain-demangler))
		   (with-temp-buffer
		     (insert-file-contents ,(expand-file-name raw-name))
		     (should (faceup-test-font-lock-buffer
			      (lambda ()
				(demangle-mode)
				(demangle-show-as (quote ,show-as)))
			      faceup-text))))))))))

;; Local variables:
;; flycheck-disabled-checkers: 'emacs-lisp-checkdoc
;; End:
