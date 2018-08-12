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

----------------------------------------------------------------

Please see [docs/NEWS-1.9.3.md](docs/NEWS-1.9.3.md) for info on
pre-1.9.4 changes.
