# Dynalint

Lint your Clojure programs as they run.

```
[com.ambrosebs/dynalint "0.1.1"]
```

Dynalint is a simplistic linter that wraps operations in runtime checks.

It is designed to never change behaviour, except for throwing different
kinds of exceptions for bad input.

Preserving performance is not a goal of Dynalint, but it will always
preserve type hints.

Designed for Clojure 1.5.1.

## Caveat: Prone to blowing the stack

Dynalint's design is intrinsically prone to cyclic calling. If you get a StackOverflowException
where a Dynalint namespace is obviously to blame, please stop using Dynalint for your
project and file an issue.

## Usage

Dynalint should only be used at dev time.

If you want to use Dynalint at the REPL, call `dynalint.lint/lint`.

```clojure
(require '[dynalint.lint :as dyn])
(dyn/lint)
```

`lint` adds dynamic checks to vars and their inlinings.

If you want to check a var's inlining or a macro's expansion, relevant
forms must be compiled *after* running the linter.

## Contributions

Clojure CA required please.

## License

Copyright © 2014 Ambrose Bonnaire-Sergeant

Distributed under the Eclipse Public License, the same as Clojure.
