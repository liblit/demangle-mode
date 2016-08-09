(require 'demangle-mode)
(require 'ert)
(require 'faceup)

(defun fontify-and-drain-demangler (beg end &optional verbose)
  (font-lock-default-fontify-region beg end verbose)
  (while (not (tq-queue-empty demangler-queue))
    (accept-process-output (tq-process demangler-queue) 5)))

(dolist (raw-name (file-expand-wildcards "test/faceup/*.raw"))
  (dolist (show-as '(demangled mangled))
    (let* ((root-name (file-name-base raw-name))
	   (test-name-string (format "demangle-faceup-test-%s-%s" root-name show-as))
	   (test-name-symbol (intern test-name-string))
	   (base-name (file-name-sans-extension raw-name))
	   (faceup-name (format "%s.%s" base-name show-as)))
      (eval `(ert-deftest ,test-name-symbol ()
	       ,(format "compare “%s” with “%s”, showing %s symbols" raw-name faceup-name show-as)
	       (let ((faceup-properties '(face display help-echo))
		     (faceup-text (with-temp-buffer
				    (insert-file-contents ,faceup-name)
				    (buffer-substring-no-properties (point-min) (point-max))))
		     (font-lock-fontify-region-function #'fontify-and-drain-demangler))
		 (with-temp-buffer
		   (insert-file-contents ,raw-name)
		   (should (faceup-test-font-lock-buffer
			    (lambda ()
			      (demangle-mode)
			      (demangle-show-as (quote ,show-as)))
			    faceup-text)))))))))

;; Local variables:
;; flycheck-disabled-checkers: 'emacs-lisp-checkdoc
;; End:
