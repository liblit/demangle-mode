;;; demangle-mode.el --- Automatically demangle C++ symbols found in buffers

;; Copyright (C) 2014 Ben Liblit

;; Author: Ben Liblit <liblit@acm.org>
;; Created: 12 Feb 2014
;; Package-Version: 0.3
;; Package-Requires: ((emacs "24.3"))
;; Keywords: c tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; This package provides `demangle-mode': a minor mode for
;; automatically demangling C++ symbols embedded in Emacs buffers.
;; With `demangle-mode' on, mangled symbols are automatically
;; recognized and demangled, then displayed according to various user
;; preferences.  For example, in this mode:

;; - `_ZN9wikipedia7article6formatE' displays as `wikipedia::article::format'
;; - `_ZNSaIcED2Ev' displays as `std::allocator<char>::~allocator()'
;; - `_ZTISt10ostrstream' displays as `typeinfo for std::ostrstream'
;; - `_GLOBAL__I_abc' displays as `global constructors keyed to abc'
;; - `_GLOBAL__D_xyz' displays as `global constructors keyed to xyz'


;;; Background and Motivation:

;; C++ name mangling is `a way of encoding additional information in
;; the name of a function, structure, class or another datatype in
;; order to pass more semantic information from the compilers to
;; linkers' <https://en.wikipedia.org/wiki/Name_mangling>.  For
;; example, a C++ function named `print' taking a single int parameter
;; might be mangled as `_Z5printi'.  A different `print' function
;; taking two chars might mangle to `_Z5printcc', thereby
;; distinguishing the two same-named overloads.

;; Most programmer-facing tools (e.g., compilers or linkers) that work
;; with C++ decode (`demangle') these encoded symbols back to their
;; human-readable forms when producing output (e.g., error messages).
;; However, sometimes we find ourselves working with `raw' views of
;; code or other data in which mangled symbols are still mangled, and
;; therefore hard to read.  For example, inspecting LLVM assembly
;; source produced by the Clang C++ compiler reveals many raw, mangled
;; symbols.  It can be useful to demangle these in-place to make such
;; files easier to read and understand.


;;; How to Use:

;; Use `M-x demangle-mode' to toggle demangling on or off in any
;; buffer.  You will probably want to turn on font-lock-mode as well,
;; since `demangle-mode' uses `font-lock-mode' to automate symbol
;; recognition and demangling.

;; If you always want demangling on in certain major modes, then add
;; `demangle-mode' to the appropriate major-mode hook, such as:

;;     (add-hook 'llvm-mode-hook 'demangle-mode)

;; If you always want demangling on in certain major modes, then add
;; `demangle-mode' to the appropriate major-mode hook, such as:

;;     (add-hook 'llvm-mode-hook 'demangle-mode)

;; File and directory variables allow turning on demangling more
;; selectively.  For example, `-*- eval: (demangle-mode 1) -*-'
;; anywhere on the first line of a file will turn on `demangle-mode'
;; for that file only.  To activate demangling for all LLVM assembly
;; files in a specific directory, save the following text as
;; `.dir-locals.el' in that same directory:

;;     ((llvm-mode
;;       (eval . (demangle-mode 1))))

;; Explore the `demangle-mode' customization group for configurable
;; options that affect this mode's behavior:

;;     M-x customize-group RET demangle-mode RET


;;; Compatibility Notes:

;; `demangle-mode' uses `font-lock-mode` to recognize and change the
;; display style of mangled symbols.  Typically, if you want
;; `demangle-mode' on in some buffer, then you should turn on
;; `font-lock-mode' as well.  It is possible to turn on
;; `demangle-mode' without `font-lock-mode', but in that case
;; demangling will only be done when explicitly requested (e.g., via
;; `M-x font-lock-fontify-buffer') rather than automatically.

;; `demangle-mode' sets the `help-echo' and `display' text properties
;; on mangled symbols.  This could potentially interfere with other
;; packages or user actions that set these properties.

;; `demangle-mode' recognizes symbols mangled using the popular
;; Itanium ABI mangling scheme as well as a small number of Linux/GCC
;; extensions to that scheme.  Other mangled forms could be added
;; easily if needed.

;; Demangling uses the `c++filt' command.  On Linux systems, this is
;; installed as part of binutils.  Anyone interested in demangling at
;; all would surely already have binutils installed.


;;; Known Bugs and Design Notes:

;; When showing the demangled version of a symbol using a boxed face,
;; the right edge of the box is missing.  This seems to be a bad
;; interaction between the `:box' face attribute and the `display'
;; text property.  This should be reported as an Emacs bug; I have not
;; yet done so.

;; The faces used for mangled and demangled symbols are identical to
;; each other, and picked somewhat arbitrarily.  I welcome suggestions
;; for nicer ways to mark such symbols or distinguish the mangled and
;; demangled variants.

;; Building atop font-lock-mode dramatically simplifies keeping
;; demangled symbols current as buffer contents change.  However, some
;; users may be surprised when they turn on `demangle-mode' without
;; `font-lock-mode' and nothing seems to happen.  I am not sure
;; whether this will play out as a real problem in practice, or how to
;; improve things it if it is indeed a recurring source of confusion.

;; Demangling in an asynchronous background process is important for
;; performance but significantly complicates the implementation.
;; Demangling directly within Emacs would be simpler and probably
;; faster.  However, I know of no pure Emacs Lisp implementation of
;; name demangling and do not wish to create one myself.  Demanglers
;; as C libraries do exist, but Emacs offers no in-process way to call
;; into such a library.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  prologue: dependencies and option grouping
;;

(require 'cl-lib)
(require 'easymenu)
(require 'tq)

(defgroup demangle-mode nil
  "Automatically demangle C++ symbols found in buffers."
  :group 'tools)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  display styles for mangled and demangled symbols
;;

(defcustom demangle-show-as 'demangled
  "How to show mangled and demangled symbols.

This sets the default style for newly-created buffers.  Use the
\"Demangle\" minor-mode menu or the function `demangle-show-as'
to interactively change this in a single buffer."
  :type '(choice
	  (const
	   :tag "Demangled"
	   :format "%t\n%h"
	   :doc "Show the demangled symbol (read only) on screen.\nThe original mangled symbol is shown as a help message or tooltip."
	   demangled)
	  (const
	   :tag "Mangled"
	   :format "%t\n%h"
	   :doc "Show the original mangled symbol on screen.\nThe demangled symbol is shown as a help message or tooltip."
	   mangled)))

(defface demangled '((((supports :box (:line-width 1 :color "grey" :style nil))) (:box (:line-width 1 :color "grey")))
		     (default (:underline (:color "grey" :style wave))))
  "Display face for demangled symbols.")

(defface mangled '((((supports :box (:line-width 1 :color "grey" :style nil))) (:box (:line-width 1 :color "grey")))
		   (default (:underline (:color "grey" :style wave))))
  "Display face for mangled symbols.")

(defun demangle-font-lock-refresh ()
  "Re-fontify the current buffer if option `font-lock-mode' is active.
This is generally done when turning on command `demangle-mode' or
using command `demangle-show-as' to change the demangled display
style."
  (when font-lock-mode
    (font-lock-fontify-buffer)))

(defun demangle-show-as (style)
  "Show demangled symbols in the given STYLE: either 'demangled or 'mangled.

This changes the style for the current buffer only.  Use the
option `demangle-show-as' to change the default style for all new
buffers."
  (interactive
   (list (intern (completing-read "Show demangled symbols as: "
				  '("demangled" "mangled")))))
  (make-local-variable 'demangle-show-as)
  (set-variable 'demangle-show-as style)
  (demangle-font-lock-refresh))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  management of the demangler subprocess and transaction queue
;;

(defvar demangler-queue nil
  "Transaction queue for background demangling of C++ symbols.")

(defun demangler-stop ()
  "Stop the demangler subprocess and transaction queue.
This is safe to call at any time; the demangler subprocess and
transaction queue restarts automatically when needed."
  (when demangler-queue
    (tq-close demangler-queue)
    (setq demangler-queue nil)))

(defun demangler-start ()
  "Start the demangler subprocess and transaction queue."
  (unless demangler-queue
    (let* ((process-connection-type nil)
	   (subprocess (start-process "demangler" nil "c++filt")))
      (set-process-query-on-exit-flag subprocess nil)
      (set-process-sentinel subprocess
			    (function (lambda (process message)
					(demangler-stop))))
      (setq demangler-queue (tq-create subprocess)))))

(defun demangler-answer-received (closure answer)
  "Process a response received from the demangler transaction queue.
CLOSURE is a list (mangled start end) consisting of the original
MANGLED symbol text and the START and END markers where this
mangled text appeared.  ANSWER is the raw response received from
the `demangler-queue'."
  (pcase closure
    (`(,mangled-original ,start ,end)
     (let ((demangled (substring answer 0 -1))
	   (buffer (marker-buffer start)))
       (with-current-buffer buffer
	 (let ((mangled-current (buffer-substring start end)))
	   (if (string= mangled-original mangled-current)
	       (with-silent-modifications
		 (cl-case demangle-show-as
		   ('demangled
		    (put-text-property start end 'display demangled)
		    (put-text-property start end 'help-echo mangled-current))
		   ('mangled
		    (put-text-property start end 'help-echo demangled))
		   (t (error "Unrecognized demangle display style `%s'" demangle-show-as))))
	     (warn "Mangled symbol changed from \"%s\" to \"%s\" while waiting for background demangler; leaving font-lock properties unchanged" mangled-original mangled-current))))))
    (_ (error "Malformed transaction queue closure `%s'" closure))))

(defun demangler-demangle ()
  "Begin demangling a mangled symbol.
Match data from the most recent regular expression search
determines the location and text of the mangled symbol.
Demangling proceeds in the background, though `demangler-queue'.
Once demangling is complete, `demangler-answer-received' updates
this matched region's display style accordingly."
  (demangler-start)
  (let* ((mangled (match-string 0))
	 (question (concat mangled "\n")))
    (cl-assert (pcase (match-data)
		 (`(,(pred markerp) ,(pred markerp)) t)))
    (tq-enqueue demangler-queue question "\n"
		(cons mangled (match-data)) #'demangler-answer-received)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  minor mode and mode-specific menu
;;

(defconst demangle-mode-map (make-sparse-keymap)
  "Extra key bindings for command `demangle-mode'.
This provides a small mode-specific menu with options for
changing the display style of demangled symbols (see option
`demangle-show-as').")

(defconst demangle-font-lock-keywords
  '(("\\_<\\(?:_Z\\|_GLOBAL__[DI]_\\)[_[:alnum:]]+\\_>"
     0
     (progn
       (demangler-demangle)
       (list 'face demangle-show-as))
     prepend))
  "Font-lock patterns matching mangled C++ symbols.
The standard patterns recognize two common families of mangled
symbols.  The first consists of identifiers starting with \"_Z\":
these have been mangled using the popular Itanium ABI mangling
scheme.  The second family consists of identifiers starting with
either \"_GLOBAL__I_\" or \"_GLOBAL__D_\": these are global
constructors or destructors (respectively), mangled using a
Linux/GCC scheme that extends beyond the Itanium ABI.")

(define-minor-mode demangle-mode
  "Toggle demangle mode.
Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode; any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, while `toggle' toggles the state.

When Demangle mode is enabled, mangled C++ symbols appearing
within the buffer are demangled, making their decoded C++ forms
visible."
  :lighter " Demangle"
  :keymap demangle-mode-map
  (if demangle-mode
      (progn
	(make-local-variable 'demangle-show-as)
	(make-local-variable 'font-lock-extra-managed-props)
	(setq font-lock-extra-managed-props
	      (cl-union font-lock-extra-managed-props
			'(display help-echo)))
	(font-lock-add-keywords nil demangle-font-lock-keywords))
    (font-lock-remove-keywords nil demangle-font-lock-keywords))
  (demangle-font-lock-refresh))

(easy-menu-define nil demangle-mode-map nil
  '("Demangle"
    ["Show Demangled Symbols"
     (demangle-show-as 'demangled)
     :style radio
     :selected (eq demangle-show-as 'demangled)]
    ["Show Mangled Symbols"
     (demangle-show-as 'mangled)
     :style radio
     :selected (eq demangle-show-as 'mangled)]
    "-"
    ;; standard menu items copied from `minor-mode-menu-from-indicator'
    ["Turn Off minor mode" (demangle-mode 0)]
    ["Help for minor mode" (describe-function 'demangle-mode)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;
;;  epilogue
;;

(provide 'demangle-mode)
;;; demangle-mode.el ends here
