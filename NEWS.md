Recent Changes in Lepton EDA
============================

This file documents important user-visible changes in Lepton EDA.  For
more information, please consult the `ChangeLog` file.


Notable changes in Lepton EDA 1.9.6
--------------------------------------
### Breaking changes:
- All `(gnetlist module-name)` Scheme modules have been renamed to
 `(netlist module-name)`.

### Changes when building from source:
- More portable gcc flags (namely, `AX_GCC_FLAGS`) are used now.
  `-std=c99` substitutes for `-std=gnu99`, and
  `-Werror=uninitialized` is now used instead of
  `-Werror=maybe-uninitialized`.


### Changes in `lepton-schematic`:

- A new configuration key `text-sizes` in `schematic.gui` group
  has been added. It can be set to a semicolon-separated list of
  text size values. Those values will appear in *Text Entry...*
  ('Add → Text)' and *Text* ('Edit → Text') dialogs instead of
  default built-in values.

- The middle mouse button can now be used to display the popup
  menu. Set `(middle-button "popup")` in `gschemrc` configuration
  file for this to work.

- The maximum number of recent files to show in the `File → Open
  Recent` menu can now be configured using `max-recent-files` key
  in the `schematic.gui` group.

- Processing of `-q` (`--quiet`) command line option has been fixed.

### Changes in `lepton-netlist`:

- `lepton-netlist --list-backends` standard output now contains just
  the list of backend names, making it more script-friendly.

- A bug in the function `get-all-connections`, which led to wrong
  netlists, has been fixed. The function now ignores pins of
  *graphical* components if they are connected to nets.

- Filtering of "no-connect" packages and nets has been added:
  - A new `symbol=nc` attribute has been added to the list of
  processed attributes.  Please see the
  [Attributes](https://github.com/lepton-eda/lepton-eda/wiki/Attributes)
  page in the lepton wiki for more information on how it works and
  about backward compatibility.
  - A new function, `package-nc?` has been added to the `(netlist
  package)` module to filter the "no-connect" packages.
  - The `<schematic>` record in the `(netlist schematic)` module
  has been changed to contain the field `nc-nets` providing the
  names of nets connected to the "no-connect" symbols.
  - Appropriate functions of the `drc2` backend have been modified
  accordingly to support the new attribute.
  - The "no-connect" symbols in the distribution (`nc-*.sym`) have
  been updated to reflect the above change.
  - Some netlister test schematics have been fixed to check for
  the new and legacy "no-connect" symbols.

- Output of the `geda` backend has been changed. Now it outputs
  additional information on graphical symbols and "no-connect"
  symbols.

- Error output of the `bom2` backend has been fixed.

- Log file names are now prefixed with `netlist` instead of
  `gnetlist`.

- Reduced output to log files by removing disclaimer.

- Fixed netlister name in the output of many backends.

- A new command line option, *-w*, has been added to prevent
  "annoying" warnings about missing configuration files.

- A new module, `(netlist error)`, and a new function in it,
  `netlist-error()`, have been added. The function is recommended
  for using in code of backends instead of `error()`, since the
  latter outputs Scheme execution stacks awful for mere users and
  absolutely unnecessary in cases where there are no programming
  error.

- The program now exits with exit code *1* when neither backend
  nor interactive mode are given.

### Changes in `lepton-schdiff`:

- A non-portable invocation of `mktemp` has been fixed.

- Temporary file names are now prefixed with strings
  "lepton-schdiff-old", "lepton-schdiff-new", and
  "lepton-schdiff-diff" to facilitate debugging.

- `lepton-cli export` is now invoked instead of deprecated Scheme
  scripts.

- A new command line option, *-d [VIEWER]*, has been added to
  specify the image viewing program.  By default, `display` from
  the **ImageMagick** package is used.

- The script now uses the schematic font name set in the
  `schematic.gui::font` configuration key to output images. By
  default, if that key is not set, *Arial* is used.


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
