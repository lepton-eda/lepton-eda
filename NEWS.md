Recent Changes in Lepton EDA
============================

This file documents important user-visible changes in Lepton EDA.  For
more information, please consult the `ChangeLog` file.

Notable changes in Lepton EDA 1.9.5
-----------------------------------

### Changes when building from source:
- The version of the `libleptonrenderer` library, mistakenly
  broken in the previous release, has been fixed.

- Fixed build on systems with several versions of `guile` package
  installed. On such systems, the user can specify the right
  program binary on the `configure` stage, e.g., `./configure
  GUILE=/usr/bin/guile-2.0`.

### Changes in `lepton-schematic`:

- Log window scrolling has been fixed so that the last line of the
  log is shown correctly.

### Changes in `lepton-netlist`:

- A new backend, *tEDAx*, aimed to support for the `pcb-rnd`
  program, has been added.

- The patch preventing using of some `bash`-specific constructs
  has been applied.

- Duplicate backend name output by the `--list-backends` command
  line option has been avoided.

----------------------------------------------------------------

Please see [docs/NEWS-1.9.4.md](docs/NEWS-1.9.4.md) for info on
pre-1.9.5 changes.
