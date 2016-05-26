# ABANDONED

The original author has abandoned this approach.
Please fork if you want to continue this work, otherwise
please do not send new PR's.

# Dynalint

Lint your Clojure programs as they run.

Dynalint is a simplistic linter that wraps operations in runtime checks.
It is designed to never change behaviour, except for throwing different
kinds of exceptions for bad input.

Preserving performance is not a goal of Dynalint, but it will always
preserve type hints.

Designed for Clojure 1.5.1, but has been modified so it can be tried
with Clojure 1.6.0, too.


## Caveat: Prone to blowing the stack

Dynalint's design is intrinsically prone to cyclic calling. If you get a StackOverflowException
where a Dynalint namespace is obviously to blame, please stop using Dynalint for your
project and file an issue.


## Usage

Dynalint should only be used at dev time.  You can start it manually
in a REPL, or in a Leiningen project you may modify your `project.clj`
file to start it automatically for you.

No matter how you start Dynalint, it is enabled by calling its `lint`
function.  `lint` adds dynamic checks to vars and their inlinings.

If you want to check a var's inlining or a macro's expansion, relevant
forms must be compiled *after* calling `lint`.


### Starting Dynalint manually

Add this to your project's dependencies:

```clojure
[com.ambrosebs/dynalint "0.1.3"]
```

If you want to use Dynalint at the REPL, call `dynalint.lint/lint`.

```clojure
(require '[dynalint.lint :as dyn])
(dyn/lint)
```


### Starting Dynalint automatically with Leiningen

If you want Dynalint to be automatically enabled every time you run
`lein test` or `lein repl`, here is a minimal `project.clj` file
example that can achieve that:

```clojure
(defproject testdyna "0.1.0"
  ;; ... other stuff here ...
  :profiles {:dev {:dependencies [[com.ambrosebs/dynalint "0.1.3"]]
                   :injections [(require 'dynalint.lint)
                                (dynalint.lint/lint)]}}
  ;; ... other stuff here ...
  )
```

If you move the `:injections` keyword and its value to a `:test`
profile, Dynalint will be enabled near the beginning of `lein test`,
but not `lein repl`.


### Dynalint options

Change `(dynalint.lint/lint)` to `(dynalint.lint/lint :start-message
true)` if you want to enable a 1-line startup message to be printed to
`*out*` when `dynalint.lint/lint` is called, to verify that it is
being called in your project.

Other options that can be supplied to `lint`:

* `:log-file "filename"` - Dynalint will create the directory
  containing `filename`, delete the file, and then write to it the
  full stack trace of every lint warning or error generated.
* `:warning-interval 0.1` - Specify the minimum time in seconds that
  Dynalint will issue consecutive warnings.  Specifying `0` or `nil`
  will show all warnings.  The default is 1 second.  If this value is
  neither `0` nor `nil`, the warnings printed by Dynalint can change
  from one run to the next, due to variations in execution time.
* `:enable` - The value can be a single keyword `:all`, `:error`, or
  `:warn`, or a sequence of those keywords.  By default, both `:error`
  and `:warn` are enabled.
* `:disable` - The values are the same as for `:enable` above.

The following example enables the start message and logging to file
`dynalint-log.txt`.  It disables the minimum time interval between
warnings, thus showing all of them, no matter how close in time they
occur.

```clojure
(defproject testdyna "0.1.0"
  ;; ... other stuff here ...
  :profiles {:dev {:dependencies [[com.ambrosebs/dynalint "0.1.4-SNAPSHOT"]]
                   :injections [(require 'dynalint.lint)
                                (dynalint.lint/lint
                                  :start-message true
                                  :log-file "dynalint-log.txt"
                                  :warning-interval nil)]}}
  ;; ... other stuff here ...
  )
```


## Example

### Errors

For one reason or another, many core Clojure functions have bad error messages.
Here's one example.

```
user=> (first 1)
IllegalArgumentException Don't know how to create ISeq from: java.lang.Long  clojure.lang.RT.seqFrom (RT.java:505)

user=> (clojure.repl/pst)
IllegalArgumentException Don't know how to create ISeq from: java.lang.Long
  clojure.lang.RT.seqFrom (RT.java:505)
  clojure.lang.RT.seq (RT.java:486)
  clojure.lang.RT.first (RT.java:578)
  clojure.core/first (core.clj:55)
  user/eval52988 (NO_SOURCE_FILE:1)
  clojure.lang.Compiler.eval (Compiler.java:6619)
  clojure.lang.Compiler.eval (Compiler.java:6582)
  clojure.core/eval (core.clj:2852)
  clojure.main/repl/read-eval-print--6588/fn--6591 (main.clj:259)
  clojure.main/repl/read-eval-print--6588 (main.clj:259)
  clojure.main/repl/fn--6597 (main.clj:277)
  clojure.main/repl (main.clj:277)
```

The error originates from passing bogus arguments to `first`, yet we get an error
deep in `first`'s implementation. This is simply pragmatism as such error checking would
incur a performance penalty, but sometimes we just want a better error above all else.

After running `(dynalint.lint/lint)` we can achieve better errors.

```clojure
user=> (require '[dynalint.lint :as dyn])
nil
user=> (dyn/lint)
:ok
user=> (first 1)
ExceptionInfo ERROR (Dynalint id 1): First argument to clojure.core/first must be seqable: 1 (instance of class java.lang.Long)  dynalint.lint/error (lint.clj:63)

user=> (clojure.repl/pst)
ExceptionInfo ERROR (Dynalint id 1): First argument to clojure.core/first must be seqable: 1 (instance of class java.lang.Long) {:dynalint.lint/dynalint true, :dynalint.lint/error true, :dynalint.lint/id 1}
  dynalint.lint/error (lint.clj:63)
  clojure.core/first (core.clj:49)
  clojure.lang.AFunction$1.doInvoke (AFunction.java:29)
  user/eval2098 (NO_SOURCE_FILE:1)
  clojure.lang.Compiler.eval (Compiler.java:6619)
  clojure.lang.Compiler.eval (Compiler.java:6582)
  clojure.core/eval (core.clj:2852)
  clojure.main/repl/read-eval-print--6588/fn--6591 (main.clj:259)
  clojure.main/repl/read-eval-print--6588 (main.clj:259)
  clojure.main/repl/fn--6597 (main.clj:277)
  clojure.main/repl (main.clj:277)
  clojure.tools.nrepl.middleware.interruptible-eval/evaluate/fn--660 (interruptible_eval.clj:56)
```

Dynalint tries to cover it's tracks in the stack trace. Notice it seems `dynalint.lint/error`
is directly called by `clojure.core/first`. This is not really the case, but the stack trace
is transformed to be as understandable as possible. 

If you happen to get a stack trace not thrown
by dynalint, you will notice calls to strange looking functions like 
`dynalint.lint$clojure.core_SLASH_first$wrapper__8482`. This should be interpretted as dynalint's
wrapper for `clojure.core/first`, and for most purposes exactly the same as a stack entry calling
`clojure.core/first`.

### Configuring the linter

Use `dynalint.lint/configure-linting!` to customise your linting experience. Note that
`dynalint.lint/lint` also takes the same arguments, but should only usually be run once.


## Rules for Dynalint errors and warnings

The eventual goal is to have Dynalint modifications for all functions
and macros in Clojure, even if the only change is to give a different
warning for the wrong number of arguments (e.g. `clojure.core/meta`,
`clojure.core/number?`, and others).

Dynalint should give an error only when the original function/macro
would throw an exception, or is known to go into an infinite loop.
For all other conditions, Dynalint issues a warning.


## Contributions

Clojure CA required please.

## License

Copyright © 2014 Ambrose Bonnaire-Sergeant

Distributed under the Eclipse Public License, the same as Clojure.
