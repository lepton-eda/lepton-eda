Recent Changes in Lepton EDA
============================

This file documents important user-visible changes in Lepton EDA.  For
more information, please consult `git log` history.

Notable changes in Lepton EDA 1.9.8
-----------------------------------

### General changes:

- The module `(geda library)` has been renamed to `(lepton library)`.
  All the functions and variables available are still re-exported
  in the former one but its using is discouraged.

- Warnings about deprecated and "dead" RC functions are now more
  verbose, and contain link to Configuration Settings wiki page.

### Changes in `lepton-netlist`:

- A new record, `<schematic-component>`, has been added. It is now
  the basic record for using in the backends that work with
  components such as **spice** ones.

- `<schematic>` record now contains a new field, `packages`,
  containing the list of schematic *packages*, where any element
  of the list is a `<package>` record.  The `<package>` records
  now contain sets of components with same refdes. The field
  *packages* is intended for use in backends working with
  multi-slot packages, e.g. **PCB**.

- The `schematic-netlist` field of the `<schematic>` record has
  been renamed to `schematic-components` to more accurately
  reflect destination of the field.

- Common `<package>` structures for `<schematic-component>`'s with
  default "refdes" attribute value are no longer
  created. Component's "refdes" value is considered to be default
  if the component has no attached "refdes", or its attached
  "refdes" is the same as the inherited one and has the suffix
  "?".

- A new module, `(netlist schematic-connection)`, has been added.
  The module has to provide records and functions for working with
  schematic connections, that is, directly or indirectly connected
  groups of nets.

- The `composite` field in the `<schematic-component>` record,
  inherited from the `<package>` structure, has been renamed to
  `sources`.  The field can be used to get source file names.

- The obsolete procedure `get-uref()` has been replaced with a
  local procedure and its using is discouraged.

- A new module, `(netlist schematic toplevel)`, has been added. It
  contains accessors for `<toplevel-schematic>` record which is
  now obtained as a set of schematic files given on
  **lepton-netlist** command line.  The `toplevel-schematic`
  variable has been replaced with same named procedure.

- Arguments for `make-toplevel-schematic()` are now filenames
  instead of pages.  This will allow reusing this function to
  encapsulate subschematics in future.

- A new module, `(netlist deprecated)`, has been added to isolate
  legacy code that is a subject for deprecation in the next stable
  release.

### Changes in `lepton-schematic`:

- The **macro widget** has been improved in several ways:

  - A new configuration group, `schematic.macro-widget`, has been
    added for setting up the properties of the widget.

  - The history of Scheme expressions executed via the `:` prompt
    is now saved in the configuration file in the *cache*
    configuration context (usually
    `$XDG_CACHE_HOME/lepton-eda/gui.conf`). The commands are
    stored in the `history` key and can be accessed by choosing
    appropriate items from drop-down list in the macro widget. The
    history length is set in the `history-length` configuration
    key in the `schematic.macro-widget` group in the *user*
    configuration context. The default value is 10.

  - The macro widget now supports auto-completion of text.

  - A new configuration key, `font`, in the
    `schematic.macro-widget` group in the *user* configuration
    context can now be used for setting up the font of the widget.

- New accelerator keys have been added to facilitate keyboard
  navigation in the following dialogs:
  - `Single Attribute Editor`
  - `Edit Attributes`
  - `Select Component`
  - `Add Text`
  - `Edit Text`
  - `Object Properties`
  - `Options`
  - `Arc Params`

### Changes in `lepton-attrib`:

- Separate menu description *XML* file has been removed. Now the
  menu structure is defined in the source code only.

### Scheme API changes
- Three legacy rc procedures, `attribute-promotion`,
  `promote-invisible`, and `keep-invisible`, have been adjusted to
  return their boolean values if used with no arguments, instead
  of just `#t`. This allows using the results in Scheme
  plugins. The procedure `bitmap-directory` now returns its string
  value if used with no arguments.  If no value has been
  previously set, it returns `#f`.


Notable changes in Lepton EDA 1.9.7
-----------------------------------

### Changes in `lepton-schematic`:

- A new action, `edit-object-properties`, has been added. It is a
  replacement for four legacy actions which opened the same
  dialog: `edit-color`, `edit-linetype`, `edit-filltype`,
  `edit-pin-type`. The action is now bound to the shortcut
  <kbd>E</kbd>-<kbd>O</kbd>.

- Fixed icon name for "Down Symbol" entry in pop-up menu.

- Improved diagnostics of input file errors:

  - If `--verbose` (`-v`) command line option is passed to
    `lepton-schematic`, it prints input files line by line as they
    are parsed in the terminal, which facilitates debugging of
    broken schematics.

  - "Failed to load file" dialog error message has been
    improved. It now reports line number in the schematic file
    where parsing was stopped due to an error.

- Several changes in the `lepton-schematic` menu make it more
  organized and easier to navigate through:
  - A new shortcut, <kbd>O</kbd>-<kbd>F</kbd>, for 'Options →
    Font', has been added.
  - A new shortcut, <kbd>T</kbd>-<kbd>I</kbd>, for 'Attributes →
    Show/Hide Hidden Text', has been added while retaining the
    legacy one, <kbd>E</kbd>-<kbd>N</kbd>.
  - Shortcut for 'Help → Find Component Documentation' has been
    changed to <kbd>H</kbd>-<kbd>O</kbd>.
  - Shortcut for 'Options → Options' has been changed to
    <kbd>O</kbd>-<kbd>O</kbd>.
  - Shortcut for 'Attributes → Find Text' has been changed to
    <kbd>T</kbd>-<kbd>F</kbd>.
  - Obsolete 'Edit Pin Type' entry in pop-up menu has been
    replaced with 'Object Properties'.

- Some issues with using of 'Help' menus have been fixed:
  - Fixed doc searching bug in `lepton-schematic`'s module
    `gschemdoc` which failed due to wrong guessing on the
    installed documentation paths.
  - Fixed blocking of `lepton-schematic` GUI on using 'Help' menu
    entries on systems where documentation viewer may not return
    control until the web browser is closed.

### Changes in `lepton-netlist`:

- A new test schematic for the `verilog` backend has been
  added. It allows checking changes in verilog module
  declarations.

- Log initialisation has been moved from the `(netlist)` module to
  the `lepton-netlist` executable file to prevent repetition
  of the initialisation on every loading of the module and using
  the wrong log domain when the module is loaded, e.g., in
  `lepton-schematic` GUI.

- Output of `lepton-netlist` version has been unified and placed
  into one procedure, `lepton-netlist-version`.  Version is now
  output once per run, and the procedure can be used interactively
  in `lepton-netlist` REPL.

- `lepton-netlist` interactive mode can now be used without
  specifying any file name on command line; the user can now load,
  traverse, and introspect arbitrary schematics data in its REPL.

- Some backward compatibility variables have been provided to
  ensure compatibility with legacy geda backends.

- `schematic-packages` and `schematic-non-unique-packages` fields
  have been removed from the `<schematic>` record since they are
  derivatives from its other structures. Two substitution
  procedures retrieving those data, `schematic-package-names` and
  `schematic-non-unique-package-names`, have been provided to
  provide the same functionality.

- New procedure `netlist-error` is now used instead of `error` in
  many backends for reporting errors. This simplifies error output
  by avoiding improper output of the program execution stack where
  it is not necessary.

- The program now catches errors in loaded code (specified by
  options `-g`, `-l`, `-m`, `-c`) and exits with appropriate error
  message.


### Changes in `liblepton`:

- The library has now facilities to load schematics in a verbose
  manner, which is now used in `lepton-schematic` (see above) for
  debugging.


Notable changes in Lepton EDA 1.9.6
-----------------------------------
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

- By default, backward compatibility symlinks are not created for
  major renamed tools, namely, `gaf`, `gattrib`, `gnetlist`,
  `gsch2pcb`, `gschem`, and `gsymcheck`, though the user can
  change that.  A new configure option,
  `--enable-compat-symlinks`, can be used to enable their
  creation.

- The users have now use `info lepton-scheme` to open texinfo
  manual on Lepton EDA Scheme API instead of `info geda-scheme`.

### General changes:

- Compatibility with various non-Linux systems has been improved
  by avoiding of hardcoding paths for such interpreters as Perl,
  Python, or bash.

- `lepton-eda` can now be compiled in C++ mode basically by using
  `./configure CC=g++ && make CC=g++`.

- The same readline history file ".lepton_history" residing in the
  user configuration directory is used for `lepton-schematic`,
  `lepton-netlist`, and `lepton-symcheck`.

- A new section about 'Makefile' creation has been added to
  CONTRIBUTING.md to facilitate non-gnu-make builds.

- Fixed `--disable-deprecated` configure option.

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

- view-find-text-state action has been added ('View → Find Text State'). It
helps when docking windows GUI is turned off (in that case all widgets
are not always visible).

- "Freestyle" colors gain default values. Now there is no need to
share the color schemes if one use that colors in their schematics.

- A new widget that allows the users to change the schematic font has
been added ('Options → Font...'). The user can click the "Apply" button
to preview the chosen font and then cick the "Save..." button to save
the settings either to the local or user configuraion file. It
changes `font` key in `schematic.gui` group mentioned below.

- A new configuration key `font` in `schematic.gui` group has been
  added. The user can now choose the font for schematic text.

- Program-specific settings are now stored in separate configuration
  file in `$XDG_CACHE_HOME` directory.

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
  `info lepton-scheme` for details.

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

- Several new statusbar settings can now be used to adjust the
  statusbar appearance. The user can change one of the settings in
  the `schematic.status-bar` configuration group in order to get
  her favorite view:

  - Rubberband and magnetic net mode indicators can be now turned
    on by setting `show-rubber-band` and/or `show-magnetic-net`
    configuration keys to `true`, accordingly.

  - Indication of mouse button assignment can be switched off by
    setting `show-mouse-buttons` configuration key to `false`.

  - The user can now affect the font style used for the active
    action text with two new keys: `status-active-color` and
    `status-bold-font`.

   Please see
   [the Configuration Settings HowTo](https://github.com/lepton-eda/lepton-eda/wiki/Configuration-Settings)
   for more information on the above settings.

- All status bar elements have now tooltip descriptions.


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

Please see [NEWS-1.9.2.txt](docs/NEWS-1.9.2.txt) for info on
pre-1.9.3 changes (before *Lepton EDA* has forked from *geda-gaf*).
