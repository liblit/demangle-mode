# Demangle Mode

`demangle-mode` is an Emacs minor mode that automatically demangles
C++ and D symbols. For example, in this mode:

- the mangled C++ symbol `_ZNSaIcED2Ev` displays as <span
  title="_ZNSaIcED2Ev" style="border:  1px solid
  gray">`std::allocator<char>::~allocator()`</span>

- the mangled C++ symbol `_ZTISt10ostrstream` displays as <span
  title="_ZTISt10ostrstream" style="border: 1px solid gray">`typeinfo
  for std::ostrstream`</span>

- the mangled C++ symbol `_GLOBAL__I_abc` displays as <span
  title="_GLOBAL__I_abc" style="border: 1px solid gray">`global
  constructors keyed to abc`</span>

- the mangled D symbol `_D4test3fooAa` displays as <span
  title="_GLOBAL__I_abc" style="border: 1px solid
  gray">`test.foo`</span>

## How to Use

### Quick Start

Install `demangle-mode` from the fantastic
[<abbr title="Milkypostman’s Emacs Lisp Package Archive">MELPA</abbr>](https://melpa.org/#/getting-started)
repository:
[![MELPA](http://melpa.org/packages/demangle-mode-badge.svg)](http://melpa.org/#/demangle-mode),
[![MELPA Stable](http://stable.melpa.org/packages/demangle-mode-badge.svg)](http://stable.melpa.org/#/demangle-mode).
Or [save
`demangle-mode.el`](https://raw2.github.com/liblit/demangle-mode/master/demangle-mode.el)
somewhere in your Emacs
[`load-path`](http://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Search.html),
then use `M-x load-library RET demangle-mode RET` to load the package.

Now use `M-x demangle-mode` to toggle demangling on or off in any
buffer. Turn on `font-lock-mode` as well:  `demangle-mode` uses this
to stay in sync as buffer contents change.

### Advanced Usage

If you did not install from the
[<abbr title="Milkypostman’s Emacs Lisp Package Archive">MELPA</abbr>](https://melpa.org/#/getting-started)
repository, add `(autoload 'demangle-mode "demangle-mode" nil t)` to
your
[Emacs init file](http://www.gnu.org/software/emacs/manual/html_node/elisp/Init-File.html)
to load `demangle-mode` whenever you start Emacs.

If you always want demangling on in certain major modes, add
`demangle-mode` to the appropriate major-mode hook, such as:

```elisp
;; demangle symbols in LLVM bitcode
(add-hook 'llvm-mode-hook #'demangle-mode)

;; demangle symbols in linker error messages
(add-hook 'compilation-minor-mode-hook 'demangle-mode)
```

[File](https://www.gnu.org/software/emacs/manual/html_node/emacs/File-Variables.html)
and
[directory](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html)
variables allow selective activation. For example, `-*- eval:
(demangle-mode) -*-` anywhere on the first line of a file will turn on
`demangle-mode` for that file only. To activate demangling for all
LLVM assembly files in a specific directory, save the following text
as `.dir-locals.el` in that same directory:

```elisp
((llvm-mode
  (eval . (demangle-mode))))
```

### Customization

Explore the `demangle-mode` customization group for configurable
options that affect this mode’s behavior: `M-x customize-group RET
demangle-mode RET`. You can choose between two styles of showing
mangled/demangled symbols:

- show the demangled symbol (read-only) on screen, with the original
  mangled symbol available as a help message or tooltip; or

- show the mangled symbol on screen, with the demangled symbol
  available as a help message or tooltip.

Customization changes the initial style. A mode-specific menu allows
switching between these styles when `demangle-mode` is on.

Additionally, you can customize the display face (font, color,
underline, etc.) used for highlighting mangled and demangled
symbols. The default highlighting face uses a <span style="border: 1px
solid gray">thin gray box</span> or <span style="text-decoration:
underline; text-decoration-color: gray; text-decoration-style:
wavy">wavy gray underline</span>, depending on the output terminal’s
capabilities.

Finally, you can change the executable used to run the c++filt
command, either specifying a different command name,
e.g. `llvm-cxxfilt` or the absolute path to the executable.  You can
also add or remove the command line options passed to c++filt.

## Background and Motivation

[Name mangling](https://en.wikipedia.org/wiki/Name_mangling) is “a way
of encoding additional information in the name of a function,
structure, class or another datatype in order to pass more semantic
information from the compilers to linkers.” For example, a C++
function named `print` taking a single `int` parameter might mangle to
`_Z5printi`. A different `print` function taking two chars might
mangle to `_Z5printcc`. This lets linkers and other tools distinguish
the two functions.

Most programmer-facing C++ and D tools demangle symbols back to their
human-readable forms when producing output. Sometimes, though, we must
work with “raw” text containing mangled, hard-to-read symbols. For
example, [LLVM assembly source](http://llvm.org/docs/LangRef.html)
from the [Clang C++ compiler](http://clang.llvm.org/) contains many
raw, mangled symbols. It can be useful to demangle these in-place to
make such files easier to read and understand. `demangle-mode`
[scratches that
itch](http://www.catb.org/~esr/writings/cathedral-bazaar/cathedral-bazaar/ar01s02.html).

## Compatibility Notes

`demangle-mode` uses
[`font-lock-mode`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html)
to recognize and change the display style of mangled symbols. If you
want `demangle-mode` on in some buffer, you should usually turn on
`font-lock-mode` as well. If you use `demangle-mode` without
`font-lock-mode`, demangling happens only when explicitly requested
(e.g., via `M-x font-lock-fontify-buffer`).

`demangle-mode` sets the
[`help-echo`](http://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html)
and
[`display`](http://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html)
text properties on mangled symbols. This could interfere with other
packages or user actions that set these properties.

`demangle-mode` recognizes the popular
[Itanium ABI mangling scheme](http://mentorembedded.github.io/cxx-abi/abi.html#mangling)
plus a few Linux/GCC extensions. Adding other mangled forms would be
easy, if needed.

Mangled symbols may optionally begin with an extra leading underscore,
as some platforms prefix all symbols in this manner. Both `_Z5printi`
and `__Z5printi` demangle identically to `print(int)`, regardless of
the host platform’s prefixing conventions.

Demangling uses the
[`c++filt`](https://sourceware.org/binutils/docs-2.24/binutils/c_002b_002bfilt.html)
command. On GNU systems, this is part of
[`binutils`](http://www.gnu.org/software/binutils/). If you need
`demangle-mode` at all, you probably have `binutils` installed
already.

## Known Issues and Design Limitations

The standard
[search commands](http://www.gnu.org/software/emacs/manual/html_node/emacs/Incremental-Search.html)
are unaware of demangled symbol text. If `_ZNSaIcED2Ev` is being
displayed as <span title="_ZNSaIcED2Ev" style="border:  1px solid
gray">`std::allocator<char>::~allocator()`</span>,
[incremental search](http://www.gnu.org/software/emacs/manual/html_node/emacs/Incremental-Search.html)
and related commands will find this text as a match for `SaI` but not
for `::`.

When showing the demangled version of a symbol using a boxed face, the
right edge of the box is <span style="border: 1px solid gray;
border-right: none">missing</span> in Emacs 24.3 and earlier. This was
a [known bug](http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16870);
[the fix](http://bzr.savannah.gnu.org/lh/emacs/trunk/revision/116585)
appears in Emacs 24.4 and later.

The faces used for <span style="border: 1px solid gray">mangled</span>
and <span style="border: 1px solid gray">demangled</span> symbols are
identical to each other, and picked somewhat arbitrarily. I welcome
[suggestions](https://github.com/liblit/demangle-mode/issues) for
nicer ways to mark such symbols or distinguish the mangled and
demangled variants.

Building atop `font-lock-mode` simplifies keeping demangled symbols
current as buffer contents change. However, this may surprise a user
who turns on `demangle-mode` without `font-lock-mode`, then sees
nothing happen.

Running `c++filt` once per symbol is too slow. Instead, we demangle in
an asynchronous background process. This boosts performance but
complicates the implementation. Demangling directly within Emacs would
be clean and fast. Unfortunately, I know of no pure Emacs Lisp
implementation of name demangling and do not wish to create one
myself. Demanglers as C libraries do exist, but Emacs offers no
in-process way to call into such a library.

## Project Status at a Glance

Continuous Integration|General Info|Current Versions
---|---|---
[![Build Status](https://travis-ci.org/liblit/demangle-mode.svg?branch=master)](https://travis-ci.org/liblit/demangle-mode) [![Coverage Status](https://coveralls.io/repos/github/liblit/demangle-mode/badge.svg?branch=master)](https://coveralls.io/github/liblit/demangle-mode?branch=master)|[![License GPLv3](https://img.shields.io/badge/license-GPLv3-blue.svg)](https://github.com/liblit/demangle-mode/blob/master/LICENSE.md) [![Motivation](https://img.shields.io/badge/itch-scratched-blue.svg)](https://github.com/liblit/demangle-mode/#background-and-motivation)|[![MELPA](http://melpa.org/packages/demangle-mode-badge.svg)](http://melpa.org/#/demangle-mode) [![MELPA Stable](http://stable.melpa.org/packages/demangle-mode-badge.svg)](http://stable.melpa.org/#/demangle-mode)
