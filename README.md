# ob-lean4

org-babel support for Lean 4 with support for sessions

## Installation

Install the package with `package.el`:

```emacs-lisp
(package-vc-install "https://github.com/UniverseSquared/ob-lean4")
```

The variable `ob-lean4-lean-bin` should be set to the path of the `lean` executable.

For session support, the variable `ob-lean4-repl-bin` should be set to the path of the
[Lean REPL](https://github.com/leanprover-community/repl).
