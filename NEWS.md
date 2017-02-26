Recent Changes in Lepton EDA
============================

This file documents important user-visible changes in Lepton EDA.  For
more information, please consult the `ChangeLog` file.

Notable changes in Lepton EDA 1.9.3
-----------------------------------

### Breaking changes:

- `libgeda` has been renamed to `liblepton` and `libgedacairo` has
  been renamed to `libleptonrenderer`.

- Most legacy `gnetlist` API functions have been implemented in Scheme.  Some
  backends may need to be adjusted slightly to take advantage of the
  improved API.

- Direct output file access in `gnetlist` backends is now discouraged.
  Backends should write netlist data to the current Scheme output
  port, unless the backend is generating several files.  The
  `output-filename` parameter to the main backend entry procedure is
  still meaningful; it reflects the output filename requested or is
  set to `#f` if `gnetlist` is generating the netlist to standard
  output.

### General changes:

- Lepton core tools will now automatically rotate per-user log files.
  250 log files per tool will be retained from the last 24 hours, and
  50 log files per tool from all time.

- gEDA/gaf tools now search for data and configuration files in the
  standard XDG directories.  By default, these are:

  - `/usr/local/share/gEDA/` and `/usr/share/gEDA` for system data
  - `/etc/xdg/gEDA/` for system configuration
  - `$HOME/.local/share/gEDA/` for per-user data
  - `$HOME/.config/gEDA/` for per-user configuration.

  However, gEDA/gaf tools will still use `$HOME/.gEDA` for per-user
  files if that directory exists, they still obey the `$GEDADATA` and
  `$GEDADATARC` environment variables, and by default the search path
  includes the installation prefix selected when compiling Lepton.

- Lepton tools will now search for Scheme code in the `scheme`
  subdirectory of the per-user data directory before anywhere else, to
  allow easy low-level Lepton customisation.

- The `world-size` rc function has been deprecated, and no longer does
  anything.

### Changes in `gschem`:

- The default editor view bounds have been expanded to allow symbols
  to be drawn without needing to translate them to the origin.

- The multi-attribute editor now allows navigation between its
  controls with <kbd>Tab</kbd> and <kbd>Shift</kbd>+<kbd>Tab</kbd>, a
  tooltip for hinting how to add a literal linefeed or tab character,
  and a horizontal scrollbar to suppress window resizing.

- You can now launch a Scheme Read-Eval-Print Loop (REPL) in gschem's
  controlling terminal with 'File → REPL' or with <kbd>F</kbd>,
  <kbd>Shift</kbd>+<kbd>R</kbd>.

- Fix a crash when closing schematic pages, and several possible
  crashes when no page is open.

- When undoing an operation the page is zoomed and scaled correctly,
  even if the `gschem` window was resized since the operation being
  undone.

- 'File → Revert' will now detect changes to symbol libraries since
  the the page was loaded.  This is helpful when trying to fix things
  up when symbols weren't found.

- Faster page redraw operations.

- Junction dots are now drawn slightly larger to ensure that they are
  clearly visible.

### Changes in `gnetlist`:

- `gnetlist` will no longer warn if some of the symbols in a merged
  component (i.e. symbols with the same `refdes` attribute) are
  missing an attribute.  For example, you can now attach a `footprint`
  attribute to only one symbol in a merged component without warnings.
  `gnetlist` continues to warn when it finds conflicting values,
  however.

- The `gnetlist` REPL is now compatible with Geiser.  Packages,
  package pins and pin nets can now be pretty-printed in the REPL.

- All rc functions related to hierarchy have been deprecated, and have
  been replaced with config file settings.

- The `drc` backend has been extensively rewritten and improved.

- The `makedepend` backend has been fixed and can now generate Make
  rules for hierarchical schematics.  It can also work with
  non-numbered schematic names.

- The `spice-sdb` backend now emits verbose info to standard error, so
  that verbose messages don't end up in the netlist.

- The `vams` (Verilog AMS) backend now generates entity files
  correctly.

- The `redac` backend no longer duplicates connection info.

- Several backends no longer emit trailing whitespace.

- Changes for backend authors:

  - A new `object-id` function has been added to allow backends to
    access the object identifiers used during netlist generation.

  - The `gnetlist-version` function has been removed.

- Internal changes:

  - A new Scheme test suite has been added, using the SRFI-64
    framework.

  - "Graphical" netlists are no longer used during netlist generation.

  - Schematic page tree creation and transformation procedures are now
    accessible from Scheme.

  - A new `(gnetlist schematic)` module provides procedures for
    working with schematics as first-class objects during netlisting.

  - The new `(gnetlist option)` and `(gnetlist config)` modules
    provide procedures for working with `gnetlist`'s command-line
    arguments and configuration data.

### Scheme API changes

- A new procedure, `log!`, has been added to the `(geda log)` module.
  It allows Scheme code to emit log messages in the same way that the
  tools' C code does.

- Two new Scheme procedures for working with source library have been
  added to the `(geda library)` module: `source-library-contents`
  and `set-source-library-contents!`.

- Accessing Lepton values (objects, pages, etc.) via the Scheme API is
  now much faster.

Please check `info geda-scheme` for full details.

### Changes when building from source:

- Lepton requires Guile 2.0 or later for build.  However, running the
  full test suite requires Guile 2.0.13.

- The Lepton source tarball no longer includes `libintl`.  When
  building Lepton with internationalisation support, GNU `gettext`
  0.18 or later is required.

- The configure script now supports an `--enable-contrib` switch.
  Unless this is specified, the tools in the `contrib` directory will
  not be compiled or installed.

- The configure script now supports an `--enable-relocatable` switch.
  When specified, Lepton tools do not have the configured prefix
  compiled into them.

- The configure script now supports a `--disable-deprecated` switch.
  When specified, various deprecated features and behaviour are
  disabled, which may break backward-compatibility with some
  configurations.

----------------------------------------------------------------

Please see [docs/NEWS-1.9.2.txt](docs/NEWS-1.9.2.txt) for info on
pre-1.9.3 changes.