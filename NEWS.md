Recent Changes in Lepton EDA
============================

This file documents important user-visible changes in Lepton EDA.  For
more information, please consult the `ChangeLog` file.

Notable changes in Lepton EDA 1.9.4
-----------------------------------

### Breaking changes:

- `gaf` has been renamed to `lepton-cli`, `gschem` has been
  renamed to `lepton-schematic`, `gnetlist` has been renamed to
  `lepton-netlist`, `gsch2pcb` has been renamed to
  `lepton-sch2pcb`, `gsymcheck` has been renamed to
  `lepton-symcheck`, `grenum` has been renamed to `lepton-renum`,
  `refdes-renum` has been renamed to `lepton-refdes_renum`,
  `tragesym` has been renamed to `lepton-tragesym`,
  `pcb_backannotate` has been renamed to
  `lepton-pcb_backannotate`, `garchive` has been renamed to
  `lepton-archive`, `gsymfix` has been renamed to `lepton-symfix`,
  `schdiff` has been renamed to `lepton-schdiff`, `gschlas` has
  been renamed to `lepton-schlas`, `gxyrs` has been renamed to
  `lepton-xyrs`, `gattrib` has been renamed to `lepton-attrib`.

- By default, backward compatibility symlinks are created for
  major renamed tools, namely, `gaf`, `gattrib`, `gnetlist`,
  `gsch2pcb`, `gschem`, and `gsymcheck`.  A new configure option,
  `--disable-compat-symlinks`, can be used to disable their
  creation.

### General changes:

- `lepton-eda` can now be compiled in C++ mode basically by using
  `./configure CC=g++ && make CC=g++`.

- The same readline history file ".lepton_history" residing in the
  user configuration directory is used for `lepton-schematic`,
  `lepton-netlist`, and `lepton-symcheck`.

- A new section about 'Makefile' creation has been added to
  CONTRIBUTING.md to facilitate non-gnu-make builds.

- Fixed non-working Scheme API procedure `reset-source-library`.

### Changes in `lepton-cli`:

- Fixed Postscript output in landscape orientation.

- Fixed `--layout` option processing for export.

### Changes in `lepton-netlist`:

- All `lepton-netlist` (previously known as `gnetlist`)
  functionality has been reimplemented in Scheme.

- Fixed processing of the `graphical` attribute.

- Fixed a regression in the `verilog` backend.

- A new example and
  [a wiki page for it](https://github.com/lepton-eda/lepton-eda/wiki/Verilog-example)
  have been added for the `verilog` backend.

### Changes in `lepton-schematic`:

- A new widget for changing color scheme has been added. The user
  can now open 'View → Color scheme editor...', choose the colors
  she wants, and save the resulting color scheme under appropriate
  file name.

- File save dialog now displays proper names for new files.

- File save dialog is no longer opened after 'File → Save' for
  existing files if they have already been saved under default
  name.

- Some duplicated log messages and superfluous newlines are no
  longer displayed in the log window.

- Orphaned pages visible only in Scheme are no longer created
  after 'Page → Revert'.

- Tabbed GUI support has been added to `lepton-schematic`: each
  schematic page can be now displayed in its own tab.  By default,
  it is disabled and can be turned on by setting
  `schematic.gui::use-tabs` configuration key to `true`.  Two
  other configuration keys related to tabbed GUI are
  `schematic.tabs::show-close-button` and
  `schematic.tabs::show-up-button`. They determine whether to show
  "close" and "hierarchy up" buttons on each tab, respectively.

- Non-working options "-r" and "--config-file" have been removed.

- A new hook, `switch-action-mode-hook`, has been added, which can
  be used to yield user subroutines on mode switch. Please check
  `info geda-scheme` for details.

- 'Text Entry...' dialog now respects the `text-size` option
  value.

- Freestyle colors have now appropriate names in GUI.

- A new configuration key `modify-viewport` in `schematic.undo`
  group has been added. It allows to change panning and zooming on
  undo/redo operations if `undo-panzoom` is set to "disabled" in
  `gschemrc` configuration file.  The default value is "false".

- Fixed naming of exported files in non-UTF-8 locales.

- Warnings about missing standard menu items no longer clutter the
  log window when they are commented out in `system-gschemrc`.

- A new widget to check symbols using `lepton-symcheck`
  functionality has been added.  Use 'Attributes → Find Specific
  Text...' or <kbd>T</kbd>, <kbd>Shift</kbd>+<kbd>F</kbd>, choose
  'Check Symbol:' on the combo-box at left, and press the button
  'Find'. A message dialog with info about wrong floating
  attributes will appear.  Closing it with the 'OK' button will
  lead to opening a window with info on objects having errors or
  warnings if such objects exist. If you select any of them, the
  canvas will be panned and zoomed to have it centered on the
  page.

- Support for showing widgets as dialogs has been added. Now the
  users may decide, what type of GUI they prefer to use: dialog
  boxes (as it was before 1.9.2) or docking widgets.  The type of
  GUI is controlled by the `use-docks` configuration key in the
  `schematic.gui` group. By default it's `true`: use docking
  widgets. If it's `false` then the widgets will be shown as
  dialog boxes.

- A new module, `(schematic undo)` has been added, which contains
  a new procedure, `undo-save-state`.  It saves current state onto
  the undo stack.  Now it's possible to support undo/redo
  operations while modifying a schematic by scripts written in
  Scheme.

- Filtering support has been added to hotkeys dialog.  The user
  can quickly search by a desired keystroke or an action name in
  the `Filter:` entry.  When searching for hotkeys, the user has
  to enter space between letters.

- A regression in the multiattrib dialog box, resulted in the
  wrong height of the value textview, has been fixed.

- A new `font` configuration key in the `schematic.log-window`
  group is now used to select the font used in the log window.

- Scrollbars in the log window are no longer shown if the text
  fits in the window.

- Automatic scrolling is now used in text properties and object
  properties widgets.

- Color selection combo box keeps updated in line with the current
  color scheme.

### Changes in `lepton-symcheck`:

- `lepton-symcheck` (previously known as `gsymcheck`)
  functionality has been fully reimplemented in Scheme.

- Fixed wrong evaluation of amount of failed checks.

- No Scheme rc files are parsed for the utility any more.  Useless
  rc procedures `gsymcheck-version`, `quit` and `exit` have been
  removed.

- A new option, `--interactive`, has been added, which allows
  working with the tool functions in an interactive REPL.

- Improvements and new checks in the utility:

  - Improved error messages about various objects.

  - Added checks for zero sized primitives.

  - Added checks for forbidden objects inside symbols (nets,
    buses, components).

  - Improved checks for duplicated floating attributes.

  - An incorrect warning message about a trailing backslash in
    text objects has been fixed.

  - Improved checks for the `pinseq` attribute multiplicity.

  - Improved special checks for `device` and `graphical`
    attributes.

  - Output messages about required floating attributes (`refdes`,
    `device`, and `footprint`) have been unified and have now
    severity `'warning` for all of them.

  - Improved checks for symbol slotting: added checks for the
   `numslots` attribute and for duplicate pin numbers in the
   `slotdef` attributes.  Checks for duplicates in wrongly formed
   slots are avoided.

  - Refactored checks for duplicates in `slotdef`, `net`, and
    `pinnumber`.

  - Misleading warnings about matching pin number in `net` and
    `pinnumber` attributes are removed.

### Changes in `lepton-sch2pcb`:

- Environment variable `GNETLIST` is no longer used in
  `lepton-sch2pcb` to custom netlister executable name.  It has
  been replaced with `NETLISTER`.

- New options, `--backend-cmd`, `--backend-net`, and
  `--backend-pcb` can be used to customize backend names. Default
  backend names are *pcbpins*, *PCB*, and *gsch2pcb*, respectively.

### Changes in `gmk_sym`:

- Using of both dot and clock in generated symbols for the
  `verilog` backend is now allowed. The user should use a new
  directive *BOTH* to make this work.

### Changes when building from source:

- Lepton now requires GTK+ 2.24.0, Glib 2.25.0, Gio 2.25.0,
  Gdk-pixbuf 2.21.0 or later versions for build.

- Fixed option `--enable-contrib` of the *configure* script.


### Changes related to building under Cygwin:

- An error in Cygwin port for Windows has been fixed.  Information
  on how to build `lepton-eda` under Cygwin can be found
  [in the lepton-eda wiki](https://github.com/lepton-eda/lepton-eda/wiki/Lepton-EDA-and-Cygwin).


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
  compiled into them. This option is not supported on BSD systems.

- The configure script now supports a `--disable-deprecated` switch.
  When specified, various deprecated features and behaviour are
  disabled, which may break backward-compatibility with some
  configurations.

----------------------------------------------------------------

Please see [docs/NEWS-1.9.2.txt](docs/NEWS-1.9.2.txt) for info on
pre-1.9.3 changes.
