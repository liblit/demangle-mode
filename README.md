This package provides `demangle-mode`: a minor mode for automatically
demangling C++ symbols embedded in Emacs buffers. With `demangle-mode`
on, mangled symbols are automatically recognized and demangled, then
displayed according to various user preferences. For example, in this
mode:

- `_ZN9wikipedia7article6formatE` displays as <span
  title="_ZN9wikipedia7article6formatE" style="border: 1px solid
  gray">`wikipedia::article::format`</span>
- `_ZNSaIcED2Ev` displays as <span title="_ZNSaIcED2Ev" style="border:
  1px solid gray">`std::allocator<char>::~allocator()`</span>
- `_ZTISt10ostrstream` displays as <span title="_ZTISt10ostrstream"
  style="border: 1px solid gray">`typeinfo for std::ostrstream`</span>
- `_GLOBAL__I_abc` displays as <span title="_GLOBAL__I_abc"
  style="border: 1px solid gray">`global constructors keyed to
  abc`</span>
- `_GLOBAL__D_xyz` displays as <span title="_GLOBAL__D_xyz"
  style="border: 1px solid gray">`global constructors keyed to
  xyz`</span>

# Quick Start: How to Use

Use `M-x demangle-mode` to toggle demangling on or off in any
buffer. You will probably want to turn on font-lock-mode as well,
since `demangle-mode` uses `font-lock-mode` to automate symbol
recognition and demangling.

If you always want demangling on in certain major modes, then add
`demangle-mode` to the appropriate major-mode hook, such as:

```elisp
(add-hook 'llvm-mode-hook 'demangle-mode)
```

[File](https://www.gnu.org/software/emacs/manual/html_node/emacs/File-Variables.html)
and
[directory](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html)
variables allow turning on demangling more selectively. For example,
`-*- eval: (demangle-mode 1) -*-` anywhere on the first line of a file
will turn on `demangle-mode` for that file only. To activate
demangling for all LLVM assembly files in a specific directory, save
the following text as `.dir-locals.el` in that same directory:

```elisp
((llvm-mode
  (eval . (demangle-mode 1))))
```

Explore the `demangle-mode` customization group for configurable
options that affect this mode’s behavior: `M-x customize-group RET
demangle-mode RET`.

# Background and Motivation

[Name mangling](https://en.wikipedia.org/wiki/Name_mangling) is “a way
of encoding additional information in the name of a function,
structure, class or another datatype in order to pass more semantic
information from the compilers to linkers.” For example, a C++
function named `print` taking a single `int` parameter might be
mangled as `_Z5printi`. A different `print` function taking two chars
might mangle to `_Z5printcc`, thereby distinguishing the two
same-named overloads.

Most programmer-facing tools (e.g., compilers or linkers) that work
with C++ decode (“demangle”) these encoded symbols back to their
human-readable forms when producing output (e.g., error
messages). However, sometimes we find ourselves working with “raw”
views of code or other data in which mangled symbols are still
mangled, and therefore hard to read. For example, inspecting
[LLVM assembly source](http://llvm.org/docs/LangRef.html) produced by
the Clang C++ compiler reveals many raw, mangled symbols. It can be
useful to demangle these in-place to make such files easier to read
and understand. `demangle-mode`
[scratches that itch](http://www.catb.org/~esr/writings/cathedral-bazaar/cathedral-bazaar/ar01s02.html).

# Compatibility Notes

`demangle-mode` uses
[`font-lock-mode`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html)
to recognize and change the display style of mangled
symbols. Typically, if you want `demangle-mode` on in some buffer,
then you should turn on `font-lock-mode` as well. It is possible to
turn on `demangle-mode` without `font-lock-mode`, but in that case
demangling will only be done when explicitly requested (e.g., via `M-x
font-lock-fontify-buffer`) rather than automatically.

`demangle-mode` sets the
[`help-echo`](http://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html)
and
[`display`](http://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html)
text properties on mangled symbols. This could potentially interfere
with other packages or user actions that set these properties.

`demangle-mode` recognizes symbols mangled using the popular
[Itanium ABI mangling scheme](http://mentorembedded.github.io/cxx-abi/abi.html#mangling)
as well as a small number of Linux/GCC extensions to that
scheme. Other mangled forms could be added easily if needed.

Demangling uses the
[`c++filt`](https://sourceware.org/binutils/docs-2.24/binutils/c_002b_002bfilt.html)
command. On GNU systems, this is installed as part of
[`binutils`](http://www.gnu.org/software/binutils/). Anyone interested
in demangling at all would surely already have `binutils` installed.

# Known Issues and Design Limitations

When showing the demangled version of a symbol using a boxed face, the
right edge of the box is <span style="border: 1px solid gray;
border-right: none">missing</span>. This seems to be a bad interaction
between the
[`:box`](http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html)
face attribute and the
[`display`](http://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html)
text property. This should be
[reported as an Emacs bug](http://debbugs.gnu.org/Emacs.html); I have
not yet done so.

The faces used for <span style="border: 1px solid gray">mangled</span>
and <span style="border: 1px solid gray">demangled</span> symbols are
identical to each other, and picked somewhat arbitrarily. I welcome
[suggestions](https://github.com/liblit/demangle-mode/issues) for
nicer ways to mark such symbols or distinguish the mangled and
demangled variants.

Building atop `font-lock-mode` dramatically simplifies keeping
demangled symbols current as buffer contents change. However, some
users may be surprised when they turn on `demangle-mode` without
`font-lock-mode` and nothing seems to happen. I am not sure whether
this will play out as a real problem in practice, or how to improve
things it if it is indeed a recurring source of confusion.

Demangling in an asynchronous background process boosts performance
over running `c++filt` separately for each symbol. Unfortunately this
also significantly complicates the implementation. Demangling directly
within Emacs would be simpler and probably faster. However, I know of
no pure Emacs Lisp implementation of name demangling and do not wish
to create one myself. Demanglers as C libraries do exist, but Emacs
offers no in-process way to call into such a library.
