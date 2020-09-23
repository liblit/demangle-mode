;; -*- lexical-binding: t -*-

(require 'demangle-test-helper "test-helper")

(require 'ert)
(require 'faceup)

(defun fontify-and-drain-demangler (beg end &optional verbose)
  (font-lock-default-fontify-region beg end verbose)
  (maphash (lambda (_command queue)
	     (while (not (tq-queue-empty queue))
	       (accept-process-output (tq-process queue) 5)))
	   demangle--queues))

(defun demangle-test-buffer-vs-file (mode faceup-file-name)
  (let ((faceup-properties '(face display help-echo))
	(faceup-text (with-temp-buffer
		       (insert-file-contents faceup-file-name)
		       (buffer-substring-no-properties (point-min) (point-max))))
	(font-lock-fontify-region-function #'fontify-and-drain-demangler))
    (should (faceup-test-font-lock-buffer `(,mode) faceup-text))))

(defconst demangle-test-dir (file-name-directory (or load-file-name buffer-file-name)))

(let ((default-directory demangle-test-dir))
  (dolist (raw-file-name (file-expand-wildcards "faceup/*.raw"))
    (dolist (show-as '(demangled mangled))
      (let* ((base-name (file-name-sans-extension raw-file-name))
	     (faceup-file-name (format "%s.%s" base-name show-as))
	     (test-name-symbol (intern (format "demangle-test-%s-%s" base-name show-as))))
	(eval `(ert-deftest ,test-name-symbol ()
		 ,(format "compare “%s” with “%s”, showing %s symbols" raw-file-name faceup-file-name show-as)
		 (with-temp-buffer
		   (make-local-variable 'demangle-show-as)
		   (insert-file-contents ,(expand-file-name raw-file-name))
		   (demangle-test-buffer-vs-file
		    (lambda ()
		      (demangle-mode)
		      (demangle-show-as (quote ,show-as)))
		    ,(expand-file-name faceup-file-name)))))))))

(ert-deftest demangle-test-change-show-as ()
  "change symbol display style from demangled to mangled and back again"
  (let* ((default-directory demangle-test-dir)
	 (raw-file-name "faceup/c++-shortest-with-args.raw")
	 (base-name (file-name-sans-extension raw-file-name))
	 (demangled-file-name (format "%s.demangled" base-name))
	 (mangled-file-name (format "%s.mangled" base-name)))
    (with-temp-buffer
      (make-local-variable 'demangle-show-as)
      (insert-file-contents raw-file-name)
      (demangle-test-buffer-vs-file
       (lambda ()
	 (demangle-mode)
	 (demangle-show-as 'demangled))
       demangled-file-name)
      (demangle-test-buffer-vs-file (lambda () (demangle-show-as 'mangled)) mangled-file-name)
      (demangle-test-buffer-vs-file (lambda () (demangle-show-as 'demangled)) demangled-file-name))))

(ert-deftest demangle-test-change-show-as-interactively ()
  "simulate user interaction to change symbol display style to mangled"
  (let* ((default-directory demangle-test-dir)
	 (raw-file-name "faceup/c++-shortest-with-args.raw")
	 (base-name (file-name-sans-extension raw-file-name))
	 (mangled-file-name (format "%s.mangled" base-name))
	 (completing-read-function (lambda (&rest ignored) "mangled")))
    (with-temp-buffer
      (make-local-variable 'demangle-show-as)
      (insert-file-contents raw-file-name)
      (demangle-test-buffer-vs-file
       (lambda ()
	 (demangle-mode)
	 (call-interactively #'demangle-show-as))
       mangled-file-name))))

(ert-deftest demangle-test-default-demangled ()
  "default show-as style should be demangled, not mangled"
  (let* ((default-directory demangle-test-dir)
	 (raw-file-name "faceup/c++-shortest-with-args.raw")
	 (base-name (file-name-sans-extension raw-file-name))
	 (demangled-file-name (format "%s.demangled" base-name)))
    (with-temp-buffer
      (insert-file-contents raw-file-name)
      (should (eq demangle-show-as 'demangled))
      (demangle-test-buffer-vs-file #'demangle-mode demangled-file-name))))

(ert-deftest demangle-test-turn-mode-off ()
  "remove extra text properties when turning demangle-mode off"
  (let* ((default-directory demangle-test-dir)
	 (raw-file-name "faceup/c++-shortest-with-args.raw"))
    (with-temp-buffer
      (insert-file-contents raw-file-name)
      (demangle-test-buffer-vs-file
       (lambda ()
	 (demangle-mode)
	 (demangle-mode -1))
       raw-file-name))))

(ert-deftest demangle-test-demangler-restart ()
  "stop demangler subprocess, then restart it when needed"
  (let* ((default-directory demangle-test-dir)
	 (raw-file-name "faceup/c++-shortest-with-args.raw")
	 (base-name (file-name-sans-extension raw-file-name))
	 (demangled-file-name (format "%s.demangled" base-name)))
    (with-temp-buffer
      (insert-file-contents raw-file-name)
      (demangle-test-buffer-vs-file #'demangle-mode demangled-file-name)
      (should (= 1 (hash-table-count demangle--queues)))
      (maphash (lambda (_command queue)
		 (interrupt-process (tq-process queue))
		 (accept-process-output (tq-process queue) 5))
	       demangle--queues)
      (should (zerop (hash-table-count demangle--queues)))
      (demangle-test-buffer-vs-file
       (lambda ()
	 (demangle-mode -1))
       raw-file-name)
      (demangle-test-buffer-vs-file #'demangle-mode demangled-file-name)
      (should (= 1 (hash-table-count demangle--queues))))))
