# Contributing to Lepton

Thank you for your interest in helping to make Lepton better!  There
are many ways that you can make a difference.

If you have any questions, you can jump into the Lepton [Gitter
channel](https://app.gitter.im/#/room/#Lepton-EDA_Lobby:gitter.im) or get in touch via the
[geda-user mailing list](http://www.delorie.com/listserv/).

Please remember that all contributors are expected to follow our [Code
of Conduct](CODE_OF_CONDUCT.md).

## Bug reports

Bugs are unfortunate, but they happen sometimes.  We can't fix a bug
if we don't know about it, so please report any bug you come across.
Please feel free to file bug even if you're not sure if the behaviour
you're seeing is actually a bug or not.

If you have the chance, before reporting a bug, please [search
existing
issues](https://github.com/lepton-eda/lepton-eda/search?utf8=%E2%9C%93&q=&type=Issues),
because someone else may have already reported your error.  Sometimes
it's hard to know exactly what to search for, and sometimes the search
doesn't show relevant results, so we don't mind if you accidentally
file a duplicate report.

You can open a new issue by following [this
link](https://github.com/lepton-eda/lepton-eda/issues/new) and filling
in the fields.  A bug report might look something like the following
template, but it's okay not to follow it exactly:

    <short summary of the bug>

    I was trying to do this:

    <description of what you were doing when the problem occurred>

    I expected to see this happen: <explanation>

    Instead, this happened: <explanation>

    I am running this version of Lepton:

    <put the output of running `lepton-cli --version` here>

    Here is the related log output:
    ...

The most important things to include are: 1) what you did, 2) what
you expected, and 3) what happened instead.  Please include the
output of `lepton-cli --version`, which includes important
information about exactly what version of Lepton you are using.

Sometimes, there are log messages that are related to the problem;
including them in your report is very helpful.
Log files can be found in the `$XDG_CACHE_HOME/lepton-eda/logs/`
directory (`$XDG_CACHE_HOME` usually expands to `$HOME/.cache`).

## Pull Requests

Pull requests are the main way that changes to Lepton are accepted.
GitHub has some [detailed
documentation](https://help.github.com/articles/about-pull-requests)
on the use of Pull Requests.

Please make pull requests against the `master` branch.

Before opening a pull request, please make sure that `make distcheck`
is successful with your changes.  This `make` target will create
release tarball, compile Lepton from the release tarball, run all
tests, and make sure that Lepton can be cleanly installed and
uninstalled.  It takes a long time to run; during normal development,
`make check` will run almost all of the tests.

All pull requests are reviewed by another person.  Most proposed
changes Lepton will require some revisions before they're accepted.
The feedback left by reviewers is intended to make sure that Lepton
continues to be high quality, stable and reliable software for all its
users.

## Tips for successful pull requests

Some general suggestions:

- If your changes are extensive, try to split them up into a series of
  small, logical steps, and use a separate git commit for each

- Write [clear, explanatory commit
  messages](https://chris.beams.io/posts/git-commit/)

- Provide updated tests and documentation

- Do not leave trailing spaces in edited files

When writing C code:

- Do not use C++-style comments `// ...`; use C-style `/* ... */`
  comments instead.

- Avoid using macros for definition of function names, they
  obfuscate the code and make git-grepping over it harder for the
  developers.

- This set of options to GNU `indent` approximates the lepton-eda C
  indentation style: (note the `-nut` option, which disables
  the use of tab characters: please use spaces for code indentation)

      -br -blf -bls -cdw -ce -cs -ts2 -sc -nut -bap -pcs -psl -lp l80 -bbo

When writing Scheme code:

- When you need to iterate over a list, it's often clearer to use
  [`map`, `for-each`](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_idx_558)
  or [SRFI-1 `fold`](http://www.gnu.org/software/guile/manual/html_node/SRFI_002d1-Fold-and-Map.html#index-fold-3609)
  than to use a recursive function.

- Predicate functions, i.e. functions that test something, should have
  names ending in `?`, e.g. `object?`; destructive functions, that
  modify one of their arguments or global state, should have names
  ending in `!`, e.g. `set-config!`.
  When implementing such a functions in `C`, please follow the naming
  convention: for Scheme names with `?`, corresponding C functions'
  names should have `_p` suffix (e.g. `object_p`), for Scheme names with
  `!` - `_x` suffix (e.g. `set_config_x`).

- When defining a function please use the
  ["implicit `define`" form](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-8.html#%_sec_5.2):

      (define (<name> <formals>) <body>)

- Code that uses `format` to format strings is usually a lot clearer
  than a `string-append`, `display` and `newline`.  You can always use
  the [Guile `(ice-9 format)` library](http://www.gnu.org/software/guile/manual/html_node/Writing.html#index-simple_002dformat-2052).

When writing Makefile code:

- Do not use `$<` ("implied source") variable in explicit rules.
  In implementations of `make` other than GNU, it may be defined only
  in implicit (i.e. suffix-transformation) rules.

- When defining a makefile variable that contain a long list of files,
put each file name on its own line.
