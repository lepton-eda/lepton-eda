Recent Changes in Lepton EDA
============================

This file documents important user-visible changes in Lepton EDA.  For
more information, please consult `git log` history.


Notable changes in Lepton EDA 1.9.19 (upcoming)
-----------------------------------------------

### General changes:

- **SMOB** machinery deprecated in Guile has been fully replaced
  with using modern foreign function interface (FFI) in all
  libraries available in the projects.  Detailed changes on type
  renamings and new modules are described below.

- All Lepton tools, including `lepton-schematic` do now reside in
  the toplevel directory `/tools` of the repository.

### Changes when building from source:

- Several deprecated GTK3 functions related to GUI have been
  replaced with newer ones or removed, thus eliminating many
  compilation warnings when Lepton is compiled with the
  `--with-gtk3` option.

### Changes in `liblepton`:

- A new Scheme type, `<toplevel>`, has been introduced.  This is a
  wrapped foreign pointer type in Scheme representing the
  `LeptonToplevel` structure in C.  Three procedures in the module
  `(lepton toplevel)` are now exported for transforming instances
  of the type *to* or *from* pointers and for checking the type.
  Those are `toplevel->pointer`, `pointer->toplevel`, and
  `toplevel?`.

- Three procedures in the module `(lepton toplevel)`,
  `%current-toplevel()`, `%make-toplevel()`, and `%with-toplevel`,
  have been renamed.  The procedures had the prefix `%` as they
  were previously defined in C code.  Now, when this is no longer
  the case, the prefix has been dropped.

- Log rotation has been moved from `geda.scm` to the config file
  `system-gafrc` in which system administrators can disable or
  enable it.

- Three previously available SMOB types, `<geda-toplevel>`,
  `<geda-page>`, and `<geda-object>` have been replaced with three
  wrapped foreign pointer types, `<toplevel>`, `<page>`, and
  `<object>`, respectively.  In order to achieve this, three new
  modules with the definitions of the types and supporting
  functionality have been created: `(lepton toplevel foreign)`,
  `(lepton page foreign)`, and `(lepton object foreign)`

- Functions dealing with C `GList` lists in Scheme have been
  unified.

- The module `(lepton toplevel)` now has a new procedure,
  `set-current-toplevel!()`, which may be used to unconditionally
  set the current toplevel fluid to a given value, which may be
  convenient for working with Lepton modules in REPL.

- Several new checks have been added for `make-page()`:
  - the procedure now raises an error if if its filename argument
    is an empty string;
  - it now raises an error if current toplevel is not set.

### Changes in `libleptongui`:

- The module `(schematic core gettext)` has been renamed to
  `(schematic gettext)`.

- A new Scheme module, `(schematic window foreign)`, has been
  introduced.  It contains foreign wrappers and functions for
  toplevel schematic window structure similar to what we have in
  `(lepton toplevel foreign)` for `<toplevel>` structure.

- `current-window()` now returns a Scheme wrapped pointer to an
  instance of C type `GschemToplevel`.

- A syntax rule has been added to check the result of
  `current-window()` in the module `(schematic builtins)`.

- A wrapper function for the `g_signal_connect()` C macro has been
  added to allow for connecting callbacks written in Scheme to
  signals defined in C code, especially for widgets.

- The signal "delete-event" of `lepton-schematic` is now connected
  in Scheme code.

- Key event processing callback is now assigned in Scheme.

- A new Scheme function, `make-schematic-window()`, has been
  factored out for creating a new main `lepton-schematic`
  window. The function now lives in the module `(schematic
  window)`. Many top level widgets are now also set up in Scheme
  and their pointers are available for processing in Scheme.

- A separate Scheme module for Slot edit dialog code, `(schematic
  dialog edit-slot)`, has been added to localize the functionality
  of the dialog.

- Several actions in the module `(schematic builtins)` have been
  simplified so that a few intermediate `i_callback_*()` functions
  are no longer used.

### Changes in `lepton-schematic`:

- Porting the program to the stable GTK version 3.24 has been
  completed.  Deprecated functions are no longer used in the code.

- Two Scheme scripts, `auto-refdes.scm` and `auto-uref.scm`, used
  in the program have been transformed into a new module,
  `(schematic refdes)`.  Main functions of the scripts are now
  exported in the new module.

- A regular expression in the function `auto-refdes-reset!()` has
  been fixed so it now correctly translates refdeses like
  **Uxyz123** to **Uxyz?** instead of just **z?**.

- The file `geda-deprecated-config.scm` has been transformed into
  the module `(geda deprecated-config)`.  This reduces a little
  start-up time of the program when launched first time.

- The file `pcb.scm` has been transformed into the module
  `(schematic pcb)`.  This also reduces start-up compilation time
  of the program.

- A bug in the GTK3 port of the program related to cursor warping
  has been fixed.  When the setting `warp-cursor` in the
  `schematic.gui` configuration group was set to `true`, rotating
  the mouse wheel in both directions only led to zooming in.  The
  issue was related to GTK3 smooth scrolling introducing in the
  release 1.9.17.  Since it seems to be an GTK3 issue (see [this
  bug description](https://gitlab.gnome.org/GNOME/gtk/-/issues/2048)),
  it has been fixed by disabling smooth events if the mouse cursor
  warping is enabled in configuration

- A regression introduced in 1.9.18, consisting in missing icons
  in the tab menu in the GTK2 port, has been fixed.

- The slot editing action has been improved.  Previously, the
  action code worked on the first selected object no matter the
  type it had.  The action did nothing if the returned object was
  just a graphical primitive despite of the fact if the selection
  had components or not, it silently ignored user input in such
  situations.  The empty selection list was ignored as well.  Now,
  the list of objects is filtered and if it contains one
  component, it works with it.  The user is now warned if either
  no component is selected or when the selection contains more
  than one component.

### Changes in `lepton-archive`:

- The program now outputs its basename instead of the full path
  when the option `--help` is given.

- The program does no longer copy same files several times.

- In some cases, the `gafrc` file to be updated may have no write
  permissions for the user.  The program now solves this as
  follows: instead of trying to append anything to it, a new file
  with updated contents is created after deleting the existing
  one.

- In cases, when renaming a file is failing, the user will now be
  warned that the archive created was left alone in the temp
  directory instead of reporting that anything was done
  successfully.


Notable changes in Lepton EDA 1.9.18 (20220529)
-----------------------------------------------

### General changes:

- The minimal required version of GTK3 has been bumped to 3.22.

- Several deprecated GTK3 functions related to GUI have been
  replaced with newer ones or removed, thus eliminating hundreds
  of compilation warnings when Lepton is compiled with the
  `--with-gtk3` option.

- `lepton-symcheck` tests have been transformed into `srfi-64`
  ones and included in the common integration test suite, thus
  reducing the number of different Lepton test suites by one.

- Fixed the issue with `make check` which failed if the command
  was launched after the tools were built in the C++ mode.

- The vulnerable Glib function `g_memdup()` has been replaced with
  `g_memdup2()` in the tools.  A workaround has been added for
  Glib versions before 2.68 that do not support the latter.


### Changes when building from source:

- Scheme libraries do no longer depend on functions defined in the
  C code of Lepton libraries, so native compilation of all Scheme
  code without Guile snarfing is now supported.  See below for
  more.

- A new `configure` option, `--enable-guild`, has been added to
  enable beforehand compilation of Guile sources of the tools.
  Please see [README](README.md) for more information.


### Changes in `liblepton`:

- All so named `core` Scheme modules defined in C and their
  functions have been reimplemented using Scheme foreign function
  interface, thus making the library independent of Guile snarfing
  and clearly separating C and Scheme interface as well as
  allowing compilation of Scheme modules without applying several
  special tricks as it used to be previously.  Some artifacts and
  dependence on Guile library are still present on the C side of
  `liblepton`. In spite of it, the Scheme modules that previously
  were dependent on the `core` modules can now be simply compiled
  with generic Guile compiler `guild`.

- The module `(lepton core gettext)` has been renamed to `(lepton
  gettext)`.  It does not belong to the set of modules defined in
  C described above.

- The functions that were previously available in the module
  `(lepton core toplevel)` reside now in `(lepton toplevel)`.

- Functions, used for exporting a current color map for later
  including in Scheme RC scripts (typically, `gschemrc`), now
  support output of translucent colors by adding alpha channel
  values for colors that are not fully opaque.

- A new module, `(lepton autoplace)`, has been created from three
  legacy scripts: `auto-place-attribs.scm`,
  `auto-place-netname.scm`, and `default-attrib-positions.scm`.
  The module merges functionality of the scripts and exports three
  legacy functions that can be used for automatic placement of
  attributes in `lepton-schematic` GUI using Scheme hooks:
  - `autoplace-pin-attributes`
  - `autoplace-object-attributes`
  - `place-netname-attribute-handler`

  Please see examples of using the functions in the configuration
  file `conf/schematic/deprecated.scm` usually living in the
  directory `${prefix}/share/lepton-eda/scheme}`.  Two previously
  available variables defining the behavior of the auto-placement
  code have been moved to the new module and are not currently
  available in any Lepton configuration files:
  `default-position-of-text-attributes` and
  `autoplace-attributes-grid`.  Some better mechanism is desirable
  to make the settings available in the current configuration.

- Provisions have been added against potential memory leaks in
  export functions in case they are called multiple times in
  Scheme scripts, which also eliminated several build warnings.

### Changes in `libleptongui`:

- The library does no longer depend on Guile snarfing and Scheme
  functions defined in C code, which allowed for eliminating so
  named `core` Scheme modules.  This is the same change as
  described above in the section for `liblepton`.


### Changes in `lepton-schematic`:

- A new color chooser widget for the GTK3 version of the program
  has been introduced.  Apart from stock functionality and a
  combobox for color selection, as in the version for GTK2, it
  has two buttons: *Apply* and *Toggle Editor*.  The former is
  used to apply a selected color to immediately see the changes on
  the canvas, and the latter is for toggling between palette
  swatch view and color editor in the widget.

- Both color chooser versions, for GTK2 and GTK3, now support
  selecting and displaying translucent colors.

- Color swatches for color selection in the color chooser widget
  and various other widgets now display non-opaque, translucent
  colors by rendering them applied to a checkered background
  pattern.

- Previously, the export image GUI function in the GTK3 version of
  the program produced images with wrong fonts as it utilized some
  default font name.  Now it has been fixed so that it outputs the
  fonts set up in the configuration files.

- The following rest Scheme modules defined in C have been
  rewritten using Scheme FFI and removed:
  - `(schematic core attrib)`
  - `(schematic core hook)`
  - `(schematic core keymap)`
  - `(schematic core selection)`
  - `(schematic core undo)`
  - `(schematic core util)`
  - `(schematic core window)`
  While some parts of the C code of the program still depend on
  `libguile`, this change, in general, untangles many
  interdependencies in the code by separating its C and Scheme
  parts.

- The module `(schematic gschemdoc)` has been renamed to
  `(schematic doc)`.

- The hook `complex-place-list-changed-hook()` has been amended so
  it now works with a list of objects instead of a single
  component object.  The argument list usually includes an
  inserted component and its toplevel (floating) attributes, so
  the users can handle all them directly in their custom code.
  The configuration example code for automatic relocation of
  attributes around their component on insertion has been amended
  according to the change.

- Previously, the code for automatic placement of attributes could
  place them at arbitrary coords, even inaccessible in the program
  GUI, or at a wrong place around the object they should be
  attached to.  It depended on some conditions: if and how many
  objects are selected, where the mouse pointer is, and if the
  user used hotkey or mouse click on menus for adding an
  attribute.  These bugs have been fixed.

- Three potential memory leaks related to the File selection
  dialog code have been fixed.

### Changes in `lepton-attrib`:

- Porting the program to the stable GTK version 3.24 has been
  completed.


Notable changes in Lepton EDA 1.9.17 (20211219)
-----------------------------------------------

### General changes:

- All Lepton tools now support attributes with empty values.

### Changes when building from source:

- Support for Guile 2.0 has been dropped.

- Lepton now requires GTK 3.2.0 or later versions for build with
  GTK3 support.

### Changes in `liblepton`:

- Support for dlopening foreign libraries used in Lepton by Scheme
  code has been improved so that Lepton could find the necessary
  libraries even if there is no development files for them
  installed on user's system.  This fixes the issue of finding
  libraries by Guile on systems like Debian and its derivatives.
  Guile requires library names to end up with standard exstensions
  like `.la` or `.so`.  On Debian, for example, library names may
  have some version numbers appended to them and symlinks with
  canonical names are distributed in separate development
  packages.  One portable solution of the issue is using
  `ldconfig` for obtaining the names of the existing library
  files.  The tool can be now used on the `./configure` stage of
  Lepton compilation to hardcode the names in Scheme.  A new
  environment variable, `USE_LDCONFIG`, has been introduced to
  make use of this approach.  To trigger retrieving the names,
  just do: `./configure USE_LDCONFIG=/sbin/ldconfig ...`

- The following core Scheme modules written in C have been removed
  and their functions have been reimplemented using Scheme FFI:
  - `(lepton core attrib)`
  - `(lepton core component)`
  - `(lepton core config)`
  - `(lepton core page)`
  - `(lepton core rc)`

### Changes in `lepton-schematic`:

- Support for GTK3 *smooth* scrolling events has been added in the
  GTK3 version of the program.

### Changes in `lepton-cli`:

- The program has been rewritten in Scheme and splitted up into
  four separate utilities that provide different functionality
  which was previously built-in in one binary of the tool:
  - `lepton-config` provides facilities for working with Lepton
    configuration system,
  - `lepton-export` deals with exporting of schematics and symbols
    into various image formats,
  - `lepton-shell` provides the interactive Scheme REPL and,
  - `lepton-cli` is a tool that can launch all the above utilities
    using corresponding commands `config`, `export`, and `shell`,
    as before.

- The new Scheme `lepton-cli` utility, unlike the previous code,
  does not allow for merely skipping `--` just after command name
  in the command line and behaves the same way like Guile does,
  that is, interprets all options after `--` as file names.

- `lepton-cli config` now does check if the specified directory
  exists and reports if it does not, returning non-zero exit
  status.

- Redundant arguments on the `lepton-cli config` invocation are no
  longer ignored.  Instead, the command prompts the user of what's
  happening and exits with non-zero exit status.

- More consistent processing of the option `-p` has been
  implemented in `lepton-cli config`.  `-p` can be now used in two
  forms, `-pPATH` or `-p PATH`, that is, it is processed the same
  way with or without space.  On the contrary, its long version,
  `--project`, can only be given as `--project=PATH`.  Ambiguous
  form `-p=PATH` which may suggest that a project directory name
  is equal to `=PATH`, is now disallowed as an extension not
  conforming to POSIX and GNU standards.  On the other hand, the
  ambiguity of the expression `-p path` has been eliminated: if
  the option `-p` is not the last command line argument, the next
  argument is considered to be belonging to it and means a project
  directory path, *not* a configuration group name.

- `lepton-cli config` now warns the user if the directories given
  in the command-line are missing.  Previously, it could output
  misleading messages about other errors, e.g., that the user may
  specify only one configuration store.

- In `lepton-cli config`, an argument is now required by the
  options `-s` and `-u`, so using any combination of them in
  series without a file name argument leads to errors about wrong
  file names.

### Changes in `contrib`:

- Utilities `gsymupdate` and `gschupdate` have been replaced with
  a new utility, `lepton-update`, written in Scheme.  The old
  utilities parsed schematic and symbol files directly, which
  sometimes could lead to wrong results, and were broken since
  introducing `lepton-embed`.  The new tool, as many others in the
  suite, uses `liblepton` and does better job when updating
  obsolete files.  Please see *lepton-update(1)* for more.


Notable changes in Lepton EDA 1.9.16 (20210731)
-----------------------------------------------

### General changes:

- Compilation warnings on running Lepton utilities written in
  Scheme, which shown up in the previous release, have been fixed.

- Several symbols in the `sym/analog` have been fixed by adding
  missing `footprint`, `value`, and `symversion` attributes.

- Test schematics for netlister have been made self-sufficient by
  adding symbols they depend on.

- A new integration test suite written in Scheme has been added to
  unify testing of all Lepton utilities in future.

### Changes when building from source:

- Versions of `Cygwin` libraries are now updated automatically.

### Changes in `liblepton`:

- Several Scheme functions related to source libraries have been
  fixed or improved as follows:
  - The function `source-library-search()` no longer adds wrong
    directories with duplicated root name.
  - The functions `source-library()`, `source-library-search()`,
    `reset-source-library()` now return the value of the default
    source library record.
  - The functions now check the type of their arguments and raise
    `'wrong-type-arg` exception if it is wrong.


### Changes in `lepton-schematic`:

- Several status bar indicators have been made interactive.  Now,
  the user can change the *rubber band* and *magnetic net* modes
  and sizes of grid and snap simply by clicking on the indicators.

### Changes in `lepton-netlist`:

- The output of the `switcap` netlister backend which was broken
  for several years is now fixed according to its documentation.


Notable changes in Lepton EDA 1.9.15 (20210626)
-----------------------------------------------

### General changes:

- The test suite has been fixed and now `make distcheck` can
  be run under the `root` user account without errors.

- Lepton now fully supports GTK 3.0.0:
  - Several warnings on functions deprecated in GTK/GDK 3.0.0 have
    been fixed.
  - See also changes in `lepton-schematic` below.

- A bug with visual representation of arcs in raster images and on
  canvas of `lepton-schematic` has been fixed.

- All Lepton utilities, even the GUI ones, no longer use
  `liblepton` as a Scheme extension.  Now the libraries are
  properly dynamically loaded by Scheme as any other libs.


### Changes when building from source:

- Fixed building with `--disable-attrib`: the `bin/lepton-attrib`
  script is no longer installed.

- Versions of `Cygwin` libraries have been updated to match actual
  versions of `liblepton`, `libleptongui` and `libleptonattrib`.

### Changes in `liblepton`:

- Several core C structs in the library have been amended to
  contain only relevant information about primitive objects.

- The following core Scheme modules written in C have been removed
  and their functions have been reimplemented using Scheme FFI:
  - `(lepton core deprecated)`
  - `(lepton core object)`
  - `(lepton core os)`

- A new module, `(lepton object type)`, has been introduced.  It
  now contains functions from `(lepton object)`, and exported in
  that module as well, that query the type of primitive Lepton's
  objects.

- Definitions of foreign functions are now residing in a new
  module, `(lepton ffi)`.  The module contains definitions of
  Scheme counterparts of C functions in the `liblepton` library.

- The Scheme function `platform()` no longer outputs `'(carbon)`.
  There is no reasonable replacement yet, so the function outputs
  `'(unknown)` on those Mac machines on which it usually did so.

### Changes in `lepton-schematic`:

- Snap size and grid size indicators now have separate labels
  in the status bar. The snap size label changes its text color
  and decoration to draw user's attention when snap size currently
  set differs from the value in configuration. It helps avoiding
  mistakes when constructing new symbols.

- Lepton now fully supports GTK 3.0.0.  Apart from fixes mentioned
  above the following changes have been made:
  - A bug of not showing text of hidden attributes in the
    **Multiattrib** dialog of `lepton-schematic` has been fixed.
  - Displaying of tabs has been improved.
  - Displaying of the **Object properties** dialog in the docked
    view of the program has been fixed so all widgets are shown
    properly now.

Notable changes in Lepton EDA 1.9.14 (20210407)
-----------------------------------------------

### General changes:

- The output of `--help` and `--version` command line options for
  all utils has been unified and simplified: reduced long lines,
  added links to Lepton's issue tracker and home page as well as
  copyright information.  The options have been added to the
  programs that missed them.

- Several changes have been made in the directory layout of the
  repository:
  - Directory with examples for `gmk_sym` has been moved to
    `contrib/` in where the script actually resides.
  - The program `lepton-renum` has been moved to `contrib/`.
  - The following scripts have been moved with tests, examples and
    documentation to dedicated directories under `utils/`:
    - `lepton-archive`
    - `lepton-attrib`
    - `lepton-cli`
    - `lepton-embed`
    - `lepton-netlist`
    - `lepton-pcb_backannotate`
    - `lepton-refdes_renum`
    - `lepton-sch2pcb`
    - `lepton-schdiff`
    - `lepton-symcheck`
    - `lepton-symfix`
    - `lepton-tragesym`
    - `lepton-upcfg`
  - `m4` files used by `autoconf` have been renamed.
  - `ChangeLog` files for Lepton tools are no longer generated.  A
    toplevel `ChangeLog` file has been added with description of
    where and how to get the information on changes.
  - Empty `ChangeLog` files have been removed from `po/` directories.
  - `.gitignore` files have been cleaned up.
  - Several other documentation files have been moved from
    `utils/` to `contrib/` or `docs/`, or removed.
  - The contents of the `symbols/` directory has been reorganized
    as well:
    - Most of symbols have been moved to `symbols/sym`.
    - Added the subdirectory `docs/` for documentation.
    - Added the subdirectory `scheme/` for Scheme files.
    - Directories with symbols that have names conflicting with
      those living in the `sym/` subdirectory have been renamed
      and are now prefixed with `sym-`.

### Changes when building from source:

- Fixed building of *doxygen* documentation.

### Changes in `docs`:

- Relatively new Scheme functions are now documented in the Lepton
  EDA Scheme manual: `config?()`, `user-cache-dir()`,
  `cache-config-context()`, and `anyfile-config-context()`.

- Lepton EDA reference manual has been updated with info on
  `lepton-cli` usage.

- Fixed a link in Lepton EDA reference manual to the Lepton EDA
  Scheme API manual in HTML format.

- The contents of the file `nc.doc` about no-connection symbols
  has been moved to the Lepton EDA reference manual.

### Changes in `liblepton`:

- The module `(lepton core smob)` written in C does no longer
  exist.  Its exported functions have been reimplemented using
  Scheme FFI.

- Many of the basic Scheme functions in the `(lepton core object)`
  module defined in the C code have been rewritten using plain
  Scheme FFI.

- Foreign functions in Scheme code have been renamed accordingly
  to their C counterparts.

- The files which names started with `geda_` have been renamed and
  this prefix has been dropped.

- The types which names were prefixed with `Geda` have been
  renamed and now have the prefix `Lepton`.  Their sibling types,
  e.g. `OBJECT` for `LeptonObject` have been removed.

- The functions which names were prefixed with `geda_` have been
  renamed so that their name are now start with `lepton_`.  Many
  other functions related to objects have been renamed in similar
  way as well.

- The `component_embedded` field of the `LeptonObject` structure
  has been renamed to `embedded` and moved to `LeptonComponent`.
  Accessors for this field have been added to not use it directly.

- Many new checks for object type have been added in functions
  dealing with objects.

- Object types are no longer queried explicitly in the code.  Type
  accessors have been added which are used both in C and Scheme
  FFI code.

- Accessors for the `CHANGED` field of the renamed structure
  `LeptonPage` have been added to facilitate using it in Scheme.

- The following redundant types have been eliminated: `BOX`,
  `OBJECT_END`, `OBJECT_FILLING`, `OBJECT_TYPE`, `PATH`,
  `PICTURE`, `SELECTION`, `TEXT`, `TOPLEVEL`, `UNDO`, `sPOINT`.

- The type `LeptonStroke` has been factored out of `LeptonObject`.
  Its inherited fields have been renamed to match to corresponding
  Scheme functions.  C accessors for each member of the new
  structure have been added as well.

- Several accessors for `LeptonObject` structure members have been
  added.

- Accessors for all fields of picture objects have been added.

- The ad hoc object type `OBJ_PLACEHOLDER` has been eliminated.

- Previously, some of the functions for new object creation had
  the `type` argument.  The argument has been removed as
  superfluous since it is always known which object is created.

- Embedding code for pictures and components has been splitted up,
  moved to appropriate places, and reduced.

- Four unit tests that previously worked well with Guile 3.0.4
  started to fail with Guile 3.0.5 on Debian.  The tests have been
  fixed so they pass with either of the versions.

### Changes in `libleptongui`:

- Scheme module `(schematic core builtins)` defined in C code has
  been removed.  Its functions are now defined in the module
  `(gschem deprecated)`.

- Gettext domain for the library has been renamed from
  **lepton-schematic** to **libleptongui** to be consistent with
  other tools.

### Changes in `lepton-netlist`:

- Fixed exporting of necessary modules in several Scheme files.

- Several superfluous and erroneous `gafrc` files have been
  removed from the `examples/` subdirectory.

### Changes in `lepton-attrib`:

- The program used to use colors to designate the visibility of
  affected attributes in schematic editor.  A status bar with a
  color legend has been added prompting the user of the meaning of
  the colors used.

- The current file name is now displayed in the title of the
  program window.  If several files are open, "Multiple files" is
  shown in the caption instead.  If there were changes in any of
  the opened files, this is marked by an asterisk prepended to the
  title caption.

- Slot numbers are now explicitly displayed in parentheses with
  the word "slot" after reference designators, e.g. **U1 (slot
  3)** instead of **U1.3** as it was previously.  This is to
  prevent confusion if the user actually wants to use dot
  separators in refdeses.

- Only those components that really have the `slot=` attribute are
  now shown as slotted.

- The overwrite confirmation dialog will now displayed on *File* →
  *Export CSV* if an already existing file is selected for output.

- Default response in the *Delete Attribute* dialog has been
  changed to "No".

- The *About* dialog has been improved and now it looks like one
  in `lepton-schematic`.

- Modification status update on saving has been fixed.

### Changes in `lepton-schematic`:

- The long command line option `--command` has been introduced as
  an analog of the short option `-c`.

- The program used to output backtraces on wrong command line
  options.  Now backtraces are avoided.  Instead, a short error
  message and a useful prompt are shown.

- Fixed crashes when the command line contains the text as
  follows: `-c '(quit)'`.

- The behaviour of the combobox for choosing the way of symbol
  insertion in the component selection dialog was broken in the
  previous version and now has been fixed.

- Text in several dialog titles has been improved, fixed, or
  added.  In particular, the title of the close confirmation
  dialog now shows the name of the program instead of "unknown".

- Default response in the overwrite confirmation dialog is now
  "No" to avoid accidental overwriting of files.

- Color scheme error dialog was not shown previously. This has
  been fixed.

- The bus drawing tool supposed to behave the same way as the net
  tool.  A click with the right mouse button cancels the net
  action.  This did not work with bus actions, which is now fixed.

- Modified schematic pages used to be shown as marked with an
  asterisk in tabbed view.  Now, "Save" buttons are shown instead,
  which allows the user to quickly save files with the mouse if
  toolbars are turned off.

- The tab buttons, "Save" and "Close" have now tool-tips
  describing their functions.

- The edit attribute dialog has been improved: now its section
  "Add attribute" is collapsible, that is, it may get folded or
  unfolded, which allows to see more attributes at once.  This
  state is preserved during the program run.

### Changes in `lepton-renum`:

- Build date and time are no longer hard-coded in the sources to
  aid reproducible builds.

### Changes in `lepton-pcb_backannotate`:

- Previously, the script code searched for the "Id" string in
  itself and, being unable to find it, exited with the error
  message "Could not determine version".  Now the script prints
  Lepton version and copyright instead.

- Refdes renaming has been fixed by exchanging parameters of the
  `%cmd_rename` hash.

- Deprecated `defined` construct that raised a fatal error since
  Perl version 5.22 has been removed.

- Man page of the script has been improved.

Notable changes in Lepton EDA 1.9.13 (20201211)
-----------------------------------------------

Bugfix release.

### Changes when building from source:

- Build for Cygwin has been fixed in that the Lepton GUI tools now
  actually work.

- Fixed installation of images into the info directory.

- File `color-map.scm`, removed in the previous release, has been
  restored for backward compatibility.

### Changes in `lepton-attrib`:

- Two long standing bugs in the program, that manifested
  themselves when the user deleted an attribute column in the
  spreadsheet, have been fixed:
  - The program did not prompt the user on unsaved changes on
    exit.
  - All invisible attributes became visible.

### Changes in `lepton-gui`:

- Fixed processing of options `-c` and `-s` broken in the previous
  release.

- Reduced the number of Help menu items pointing to obsolete wiki
  documentation.


Notable changes in Lepton EDA 1.9.12 (20201204)
-----------------------------------------------
### General changes:

- Started creation of Lepton EDA reference manual.  The manual can
  be opened from the `lepton-schematic` GUI by using <kbd>H</kbd>
  <kbd>M</kbd> keyboard shortcut.

- The empty `local` subdirectory has been removed from
  the list of component libraries.

- Old ChangeLog files (<=2007) are no longer installed.  They
  hardly provide some any useful information for end users now and
  any info can be easily received using git.

### Changes when building from source:

- Guile 3.0 support has been added.

- Experimental GTK 3.0 support has been added.  A new `configure`
  option, `--with-gtk3`, has been added to enable building
  programs with GTK3.  Please see [README](README.md) for more
  information.

- Build with `-Wl,--no-undefined`, broken in the previous release,
  has been revamped by merging two libraries, `liblepton` and
  `libleptonrenderer`.

- Build for Cygwin has been fixed as well by the above change.

- The build option `--disable-attrib` broken in the previous
  release has been fixed.

- The obsolete and pretty useless script `bom_xref.sh` has been
  removed from the toolset.

### Changes in `liblepton`:

- Code from the library `libleptonrenderer` has been merged into
  `liblepton`.

- A new module, `(lepton eval)`, has been added. The new module
  exports Scheme versions of functions `eval-protected()` and
  `eval-string-protected()` previously defined in C code.

- A new module, `(lepton ffi)`, has been introduced.  It contains
  general definitions related to Scheme FFI.

- Refactored color system code:
  - A new module, `(lepton color-map)` has been added.
  - Procedures `print-color-map()`, `display-color-map()`, and
    `display-outline-color-map()` have been moved to the module.
  - The procedures now return their color-map values instead of
    just `#t`.
  - The module additionally exports the variable
    `%color-name-map`.  It returns the set of valid symbols for
    color names.

- Print color maps for dark and light color schemes have been
  fixed:
  - The `grid` color name has been replaced with `dots-grid`.
  - Missing colors `mesh-grid-major` and`mesh-grid-minor` have
    been added.

- Scheme functions working with version of the tools have been
  added or refactored:
  - Added procedure `lepton-version-data()` which can return the
    list of version strings by requested symbol names.
  - Added procedure `lepton-version-ref()` which retrieves one
    version data element by its symbol name.
  - Added generic procedure `display-lepton-version()`.
  - `lepton-version()` has been refactored.  Please see its
    docstring for more.

### Changes in `lepton-schematic`:

- C source code has been transformed into a new library,
  `libleptongui`.  `lepton-schematic` is now an executable Scheme
  script which uses Guile FFI to make the job done, allowing to
  play with it without recompilation of the underlying C code.

- Setting of the environment variable `LD_LIBRARY_PATH` may be
  needed for users installing Lepton from sources if the system
  configuration does not include the path to `libleptongui`.

- Two new Scheme modules, `(schematic ffi)` and `(schematic ffi
  gtk)`, have been added to the suite.  They provide Scheme FFI
  bindings for GTK functions.

- A new environment variable, **LIBLEPTONGUI**, has been added in
  order the user to redefine the path to the `libleptongui`
  library "on the fly".  This can be used, e.g., for test suite or
  in development for testing changes without installation of the
  library.

- Scheme builtin action procedures that were defined in C code and
  that were not available in some circumstances, e.g. in remote
  REPL, are now available in the module `(gschem deprecated)`.

- Three C functions for creating dialogs and their corresponding
  Scheme counterparts have been re-implemented in Scheme using FFI
  and renamed.  The new functions are exported in a new Scheme
  module, `(schematic dialog)`. Those functions are:
  - `schematic-message-dialog` (former `gschem-msg`)
  - `schematic-confirm-dialog` (former `gschem-confirm`)
  - `schematic-fileselect-dialog` (former `gschem-filesel`)

- Guile module `(srfi srfi-37)` is now used instead of `(ice-9
  getopt-long)` for command line option processing.

- A new option, `--command`, can now be used as an alias for `-c`.

- The keyword `G_` is now used for translation of strings in
  Scheme code in order to prevent conflicts with other Guile
  functions such as, e.g., `match()`.  It's the same change as one
  that have been recently made in Guile 3.0.

- Several new additional shortcuts have been added which are used
  in many other programs to ease working with the tools for
  newbies:
  - <kbd>Control</kbd>+<kbd>N</kbd> for opening a new file
  - <kbd>Control</kbd>+<kbd>O</kbd> for opening an existing file
  - <kbd>Control</kbd>+<kbd>S</kbd> for file saving
  - <kbd>Control</kbd>+<kbd>P</kbd> for printing the contents of a file
  - <kbd>Control</kbd>+<kbd>Shift</kbd>+<kbd>Z</kbd> for redo (as
    an inversion for <kbd>Control</kbd>+<kbd>Z</kbd> for undo)

- Another bunch of additional shortcuts has been added to simplify
  zooming for the same reasons:
  - <kbd>=</kbd>, <kbd>Control</kbd>+<kbd>=</kbd>, and
    <kbd>KP_Add</kbd> for zooming in
  - <kbd>-</kbd>, <kbd>Control</kbd>+<kbd>-</kbd>, and
    <kbd>KP_Subtract</kbd> for zooming out
  - <kbd>0</kbd>, <kbd>Control</kbd>+<kbd>0</kbd>, and
    <kbd>KP_Multiply</kbd> for zooming to extents

- Filters behavior in the file chooser dialogs has been
  improved. In the `Save As` dialog the filter is set based
  on the current schematic's file name. Extensions containing
  letters in different case are now correctly recognized,
  filters can work with file names matching the following
  shell patterns: `*.[sS][cC][hH]` and `*.[sS][yY][mM]`.

- It is now possible to change a color of net and bus objects
  in the `Object Properties` dialog.

- All available colors are now shown in the colors combo box,
  disabled ones are designated as "[ disabled ]".

- Two new keywords, `symbol-attribs` and `pin-attribs`, have been
  added to the `[schematic.attrib]` configuration group in order
  to replace the functionality of the legacy procedure
  `attribute-name()`.  The procedure is now exported in the module
  `(schematic attrib)`.  See updated Lepton reference manual for
  more information.

- Fixed a regression in font displaying that appeared in the
  previous Lepton version 1.9.11.  Now lower aligned text objects
  are displayed the same way as before, their anchor points now
  specify positions of their baselines rather than bottom lines of
  the text logical rectangles.

### Changes in `lepton-netlist`:

- Two issues in the `tEDAx` backend have been fixed:
  - Parametric attribute values containing spaces between the
    parameters were passed un-escaped in the backend output.  Now
    they are escaped according to [the tEDAx
    spec](http://repo.hu/projects/tedax/syntax.html).
  - Support for output of `nettag` net attributes, which had been
    missing before, has been added.

### Changes in `lepton-attrib`:

- The source code of the program has been fixed so that it no
  longer use a big change of obsolete third party spreadsheet
  code.  Instead, a dependency on another up-to-date project,
  `gtkextra`, has been added.

- After the above change, another feature has been added.  Now,
  you can try to install a new, experimental GTK3 interface of the
  program.  Please see Lepton reference manual for more info.

Notable changes in Lepton EDA 1.9.11 (20200604)
-----------------------------------------------
### General changes:
- Executable scripts `lepton-netlist` and `lepton-symcheck` have
  been moved to the *utils/* directory.  Their Scheme modules have
  been moved to the directory with Scheme files of `liblepton`.

- Separate *gettext* domains of `lepton-netlist` and
  `lepton-symcheck` have been removed.

- As a side effect of this transition, the messages by symbol
  checker are now translated in `lepton-schematic` GUI.

- More Scheme strings have been added to translation.

- Two Scheme test suites, `liblepton`'s and `lepton-netlist`'s
  ones, have been merged into one.

- Unit test for promotable attributes has been added.

- Old changelog files (*ChangeLog-1.0*) have been moved into one
  directory and renamed.

### Changes when building from source:
- Build order has been changed and now `libleptonrenderer` is
  built before `liblepton`.

### Changes in `lepton-cli`:
- The tool used to silently skip missing symbols on export without
  rendering any placeholders. Rendering of placeholders has been
  fixed.

- In some cases, for instance, when a Scheme script was evaluated
  by the tool and no renderer was available, it output assertion
  failures for text objects during calculation of their bounds,
  which has been fixed.

### Changes in `lepton-netlist`:

- Localization support has been added.  The utility had no
  localization support since rewriting it in Scheme.

- The famous Scheme backend `spice-noqsi` has been added to the
  list of backends. The directory with examples contains now its
  documentation and example schematics.

### Changes in `lepton-symcheck`:

- Localization support has been added.  The utility had no
  localization support since rewriting it in Scheme.

### Changes in `lepton-schematic`:
- Log messages about `symversion` mismatch are no longer emitted
for missing schematic symbols.

- `symversion` attribute changes are now detected when a page is
opened. Previous behavior was to check just the first page passed
on the `lepton-schematic` command line.

- Log messages related to `symversion` have been changed to be
more compact and comprehensible.

- Several dialogs and dock widgets now have shorter and more
  uniform titles.

- The "Major symbol version changes" dialog has been improved by
making it more informative and non-modal. A new dialog instance
is opened for each page with symbol version changes.

- The dialogs "Add picture", "Select picture" and "Execute script"
  now have file filters, which eases selection of supported files.

- A new boolean configuration parameter has been added to the
`schematic.gui` group: `use-toplevel-windows` ( `false` by default).
Setting it to `true` allows the following widgets to behave like
top-level windows (i.e. do not stay on top of the main window):
  - Page manager
  - Log window
  - Edit text
  - Color scheme editor
  - Object properties
  - Options
  - Font selector
  - Find text results

- `lepton-schematic` now uses new (more compact) placeholders
for missing schematic symbols. Traditional placeholders (giant
red triangles with an exclamation mark and two lines of text)
can be turned back on by setting the `small-placeholders` key
in the `schematic.gui` group to `false`.

- The same font, defined in the `font` configuration key of the
  `schematic.gui` group, is now used for printing and rendering of
  objects in GUI.

- Several rendering issues, manifesting themselves on some modern
  distributions where new Pango library (1.44) is used, have been
  fixed:
  - Different scaling of overbars on zooming.
  - Wrong alignment and position of lower aligned text in printed
    output.
  - Changing of vertical spacing between paragraphs of text
    separated with empty lines on zooming.

- Historically, text lines of the same object size with different
  size of glyphs, for instance, all uppercase and all lowercase,
  might be rendered as if they have different baseline position.
  For example, `-` (*minus*) and `_` (*underscore*) could be
  rendered on the same height, depending on alignment.  This has
  been changed, and now logical extents of text are used to
  calculate and properly set its position.

- Showing/hiding of hidden text can now be set individually on per
  page view basis.

### Changes in `lepton-upcfg`:
- Fixed a logging issue when the utility is called with some file
  path as a command line parameter.

- The utility now has its own manual page `lepton-upcfg(1)`.

### Changes in `lepton-attrib`:
- The source code of the program has been transformed into a
  library, `libleptonattrib`, and an executable script written in
  Scheme.  Thus, C functions of the new library can now be used in
  other tools.

- The option `--quiet` has been removed.

### Changes in `liblepton`:

- The procedure `process-gafrc()` from the module `(netlist)` has
  been moved to the module `(lepton rc)` to be available for other
  utilities.

- A new Scheme function, `anyfile-config-context()`, and its C
  counterpart, have been added to the library.  In future, they
  can be used to support additional configuration files, for
  example, files containing color schemes or component library
  descriptions.

Notable changes in Lepton EDA 1.9.10 (20200319)
-----------------------------------------------
### General changes:
- Utilities written in Python, `lepton-archive`, `lepton-tragesym`
  and others (please see below for more information), have been
  rewritten in Scheme or removed, so there is no dependency on
  Python any more.

- C source files, Scheme modules, types, functions, variables,
  structure fields have been renamed to avoid using of ambiguous
  word "**complex**" since it is used in Scheme to designate
  complex numbers. The Scheme object type `'complex` has not been
  renamed for backwards compatibility.

- Two unused old scheme scripts `print-NB-attribs.scm` and
  `list-keys.scm` have been removed.

- Several other cleanups have been made in the source tree.

- The Scheme auto-load subdirectory `gafrc.d`
has been renamed to `autoload`.

- Obsolete environment variables `GEDADATA` and `GEDADATARC` are
  no longer used in Lepton.  Please use Scheme procedures `load`
  or `primitive-load` in *gafrc* files to load your extensions.

- The directory *$HOME/.gEDA/* is no longer used for storing of
  user configuration files.  The new location for user config
  files is *$XDG_CONFIG_HOME/lepton-eda/* (usually,
  it is *$HOME/.config/lepton-eda*).

- Legacy `geda*.conf` configuration files are no longer used.
  Settings are now read from `lepton*.conf` files.
  To convert your settings to new format, please use the
  `lepton-upcfg` utility.

- The obsolete and undocumented script `bompp.sh` has been
  removed.

### Changes when building from source:
- The following obsolete `configure` options have been removed:
  - `--with-kdedatadir` (for KDE 3)
  - `--enable-relocatable` (non-portable)
  - `--with-rcdir` (no longer usable)
  - `--disable-deprecated` (no longer usable)
  - `--enable-compat-symlinks` (does not make sense any more)

- Detection of installed `Guile` run-time and development packages
  has been improved. Previously, after introducing support for
  Guile 2.2, some users could encounter difficulties with building
  from source when Guile development headers were distributed in
  separate packages in their distribution and there was two
  different Guile versions (2.0 and 2.2) installed on their
  systems.  They could get mysterious error messages about
  applying a wrong object type.  Point is that Guile 2.0 and Guile
  2.2 use incompatible binary code format for compiled programs.
  Now Lepton build system checks what development packages are
  installed on user's system and chooses a correct version of
  Guile for compilation.  If it is impossible due to missing
  required package, it reports the error on the configure stage.

- The `autogen.sh` script, while still available, is no longer
  required for installation from the git repository.  Please use
  `autoreconf -ivf` instead.

- Building of the Scheme API HTML documentation with multiple
  make jobs on FreeBSD has been fixed.

- `icon-theme-installer` script has been fixed by simplifying
  its command line arguments checks, which makes it more portable
  among various build environments.

- The command `make maintainer-clean` no longer removes
  *configure* and its helper files, as well as *ChangeLog* and
  *Makefile.in* files.

### Scheme API changes:
- The module `(geda log-rotate)` has been renamed to `(lepton
  log-rotate)`.

- Several `(geda *)` modules have been renamed to `(lepton *)`
  ones.  All the functions and variables available are still
  re-exported in the former ones but their using is discouraged.
  The following renames have been made:
  - `(geda attrib)` => `(lepton attrib)`
  - `(geda config)` => `(lepton config)`
  - `(geda log)` => `(lepton log)`
  - `(geda object)` => `(lepton object)`
  - `(geda os)` => `(lepton os)`
  - `(geda repl)` => `(lepton repl)`

- The following *core* (written in C) Scheme modules have been
  renamed:
  - `(geda core attrib)` => `(lepton core attrib)`
  - `(geda core complex)` => `(lepton core component)`
  - `(geda core config)` => `(lepton core config)`
  - `(geda core deprecated)` => `(lepton core deprecated)`
  - `(geda core gettext)` => `(lepton core gettext)`
  - `(geda core log)` => `(lepton core log)`
  - `(geda core object)` => `(lepton core object)`
  - `(geda core os)` => `(lepton core os)`
  - `(geda core page)` => `(lepton core page)`
  - `(geda core smob)` => `(lepton core smob)`
  - `(geda core toplevel)` => `(lepton core toplevel)`

- Apart from expanding environment variables, the function
  `expand-env-variables` from the `(lepton os)` module now replaces
  **~/** (user home directory prefix) in file names in order to
  make such names understandable for other functions.  This allows
  using this prefix in functions like `component-library` and
  `source-library`.

- A couple of functions have been added to the `(lepton object)`
  module to work with object's "embedded" state:
  `object-embedded?` and `set-object-embedded!`.

- The **cache** configuration context can now be accessed in
  Scheme code: `cache-config-context` function has been added
  to the `(lepton config)` module.

- A new function, `component-filename`, has been added to the
  `(lepton object)` module. It returns the full component's symbol
  file path.

- `config-load!()` function in the `(lepton config)` module has been
  modified to accept a new optional `boolean` parameter `force-load`,
  which is `false` by default. Configuration is no longer reloaded
  each time this function is called, unless you explicitly request
  it by `#:force-load #t`.

### Changes in `liblepton`:
- The module `(lepton library component)` has been amended to
  support new Scheme layer around internal `%component-library` in
  the `component-library` procedure, which prevents loading of
  duplicate component libraries.

- The `bitmap-directory` `gafrc` option has been removed.

- The `gafrc` options listed below have been deprecated
and replaced with new configuration settings.
Please refer to the [Deprecated Settings](https://github.com/lepton-eda/lepton-eda/wiki/Deprecated-Settings#user-content-deprecated-gafrc) wiki document.
  - `bus-ripper-symname` => `[schematic]::bus-ripper-symname`
  - `always-promote-attributes` => `[schematic.attrib]::always-promote`
  - `keep-invisible` => `[schematic.attrib]::keep-invisible`
  - `attribute-promotion` => `[schematic.attrib]::promote`
  - `promote-invisible` => `[schematic.attrib]::promote-invisible`
  - `make-backup-files` => `[schematic.backup]::create-files`

- Fixed rotation of logs broken in Lepton 1.9.8.

- Fixed two long standing bugs in the procedure
  `component-library-search`:

  - Failure of the procedure in some cases when the root directory
    processed by the procedure contained symbol files.

  - Failure of the procedure with the `'out-of-range` exception if
    the processed path contained 2 identical components at the end
    and trailing slash (e.g. "/path/sym/sym/").

### Changes in `lepton-netlist`:

- The module `(netlist rename)` has been removed.  All previous
  renaming functionality has been wholly thrown away.

- The module `(netlist pin-net)` has been removed.  `<pin-net>`
  records are no longer used in netlisting.

- The module `(netlist traverse)` has been removed. All its
  functions have been moved to other appropriate modules.

- New data structures, *hierarchical*, or *over-port*, or
  *vertical*, connections, have been introduced.  They are used to
  produce net names for schematics with subcircuits.  This fixed
  the issue with toplevel unnamed connections, using of which lead
  to wrong netlists, and resulted in the following changes in output
  netlists:

  - The `geda` backend no longer displays intermediate renamings
    the same way it is used for unnamed nets shown in netlists.
    Renamed unnamed nets of a subschematic are shown by the geda
    backend differently, e.g.,
    "*hierarchical/subciruit/name/unnamed_net_at_XxY*", where X
    and Y are coords of one of their members.

  - If a net has more than one preferred attribute attached
    ("*net=*" or "*netname=*"), the lowest one by means of the
    predicate `refdes<?` is chosen then.

  - The sequence numbers of unconnected pins and unnamed nets have
    been reduced due to removing of internal ones from the
    sequence.

- During netlisting, page contents is filtered once to make
  connections.

- Hierarchical names are now used for subschematics.

- A new field, `parent`, has been added to the
  `<schematic-component>` record to get hierarchical data.  For a
  component, it contains the link to the `<subschematic>` instance
  the component belongs to.

- The `<schematic-connection>` record now also contains a new
  field, `parent`.  For a connection, it contains the link to the
  `<subschematic>` instance the connection belongs to.

- The `<subschematic>` record now also contains a new field,
  `parent`.  It contains the component the `<subschematic>`
  belongs to, if any.

- The unused `sources` field has been removed from the `<package>`
  record.

- The unused `tag` field has been removed from the `<package>`
  record.

- The <package-pin> record now contains a new field,
  `port-connection`, which holds a hierarchical
  `<schematic-connection>` a pin belongs to.

- Crashes, when source file cannot be found, are now avoided.

- Fixed crashes on power symbols (the symbols having one pin and
  no *"refdes="* attribute) having a *"net="* attribute but no
  *"pinnumber=* attribute attached to its pin.

- Fixed crashes in the `spice-sdb` backend when processing I/O
  components.

- Removed output of version in the `spice-sdb` backend.  It has
  not been updated since 2007 and does not reflect the changes in
  the backend code.

- Fixed a link to documentation in the output of the `spice-sdb`
  backend.

- A new module, `(netlist mode)`, has been added. It contains
  accessor functions for current netlisting mode.

- A new module, `(netlist schematic-port)` has been added. It
  defines a new record, `<schematic-port>`, which contains info
  about connections via ports between hierarchical levels.

- A new module, `(netlist schematic-named-connection)`, has been
  added. It contains functions for merging connections by net
  name.

- A new module, `(netlist subschematic)`, has been added. It
  defines a new record, `<subschematic>`, which extends
  `<#geda-page>` internal C structure and contains various netlist
  related fields.

- The `<package-pin>` record now has several new fields:
  - `net-map` is defined for virtual pins created from the
    *"net="* attribute.
  - `parent` contains link to the parent component of a pin.

- A new field, `pins`, has been added to the
  `<schematic-connection>` record. For a connection object, it
  contains `<package-pin>` objects that belong to the connection.

- A new field, `subschematic`, has been added to the `<schematic>`
  record. It contains toplevel multi-page `<subschematic>` of
  `<schematic>`, thus allowing recursive access to low level
  subschematics.

- A new field, `subschematic`, has been added to the
  `<schematic-component>` record. It contains its parent
  `<subschematic> record.

- A new field, `port`, has been added to the
  `<schematic-component>` record. The field contains port
  (`<schematic-port>` record) for components being port components
  in subcircuits.

- A new field, `net-maps`, has been added to the
  `<schematic-component>` record.  It provides info on net mapping
  for the component pins.

- A new field, `named-connection`, has been added to the
  `<package-pin>` record, which contains the parent
  `<named-connection>` of a pin instance.

- Fixed wrong usage of the *"netname="* attribute instead of
  *"net="* when the `netname-priority` config setting is set to
  false (`#f`), that is, when *"net="* should be preferred.

- Fixed issue with output of wrong warnings about renaming nets
  with the same name (shorted to themselves).

- Previously, the program used to exit with error if some source
  file was not readable or missing, even in the interactive mode.
  Now, `lepton-netlist` just reports such errors as critical
  without exit.

- The module `(netlist page)` has been eliminated in favour of the
  `(lepton page)` module.  All functions from the former have been
  moved to the latter.  The procedure `filename->page()` has been
  renamed to `file->page()`.

- The unused Scheme module `(netlist repl)` has been removed.

- Netlist backends can now set the desired netlist mode by defining
  a `request-netlist-mode` function, which should return either `'geda` or
  `'spice`. `lepton-netlist` no longer relies on the backend's filename
  when setting the operation mode.

- The `spice-sdb` backend has been fixed to generate SPICE
`.MODEL` lines correctly.

- The `--list-backends` command line option now has a short
equivalent: `-b`.

### Changes in `lepton-archive`:
- The initial Python script has been rewritten in Scheme.  It was
  broken at least since 2007, when *system-gafrc* was refactored by
  removing `component-library` calls from it.  It parsed that file
  for the "component-library" strings, but could not find and save
  any stock symbols.  The new Scheme script has the following
  advantages:

  - It eliminates issues with Python versions incompatibilities
    due to moving in most distributions to the more recent
    versions.

  - The previous script could only expand one environment variable
    in a file name. Now all environment variables are expanded
    recursively in file names, the tilda prefix (**~**) is
    expanded, too.

  - OS specific file name separator is now supported.

  - The script replaces using of system utilites *cp*, *mv*, *rm*
    in most cases with appropriate Scheme procedures.

  - Instead of direct parsing of component lines in schematic
    files (**"C x y ..."**), which may be wrong in some cases, it
    uses info on internal schematic provided by `liblepton`
    modules.

  - The new script eliminates direct setting of any environment
    variables to the directory where `system-gafrc` should be
    placed, and parsing of the lines containing
    `component-library` and `component-library-search` in system
    rc files, as it is no longer required.

  - `gafrc` is now used instead of `gschemrc` as rc file to
    update; local project's `gafrc` is processed to get info on
    component libraries used in the project.  `gafrc` in the
    resulting archive is changed in that to load symbols from its
    local cache directory.

  - Symbol and SPICE file cache directory name has been changed
    from `gschem-files` to `cache`.

  - The program can now collect and cache all symbols and
    subschematics in a hierarchical schematic, as well as
    subcircuits mentioned in the *"file="* attributes of
    components.

  - RC file listing files to be archived is no longer used. The
    file set by the option `--files-from` (`-f`) is used
    instead. The script copies all files mentioned in that file
    and in command line to the archive, including files not living
    in the project directory.

  - Lowercase archive name is now used by default:
    `project-archive.tar.gz` instead of
    `ProjectArchive.tar.gz`. It is to avoid issues with case
    sensitive filesystems.

  - The program options now have long equivalents, please see
    `lepton-archive --help` for more information.

  - The program now supports both absolute and relative source
    filenames (such as "*/tmp/simulation.cmd*") or using relative
    paths (like "*./*" or "*../*").

  - Files missing in the source library are reported.

  - The output archive file name can be specified without the
    "*.tar.gz*" suffix.  The users can just use `-o
    project-archive` to save their project to the
    *project-archive.tar.gz* file.

### Changes in `lepton-schlas`:
- This utility has been rewritten in Scheme and renamed to `lepton-embed`.
- Its functionality has been extended: it can now embed/unembed pictures (`-p`)
  or components (`-c`) only, and may be instructed to leave source files intact,
  saving its output to a separate files with a given suffix (`-x`).

### Changes in `lepton-cli`:
- A new command line option has been added for the `export`
  command: `--paper-names` (`-P`). It displays a list of paper
  size names suitable for the `--paper` option.

- The default paper size is now defined in the system configuration
  files: the `paper` configuration key in the `export` and
  `schematic.printing` (`gschem.printing`) groups is set to `iso_a4`.

- `lepton-cli config` can now read and write configuration
  stored in the **cache** configuration context: a new
  command-line option `--cache` (`-c`) has been added.

### Changes in `lepton-tragesym`:
- The initial Python script has been rewritten in Scheme. The
  program now does anything the previous version did, and more:

  - `liblepton` functions are now used to create objects instead
    of dealing with symbol text format directly.

  - Fixed calculation of symbol width so that pins always touch
    symbol box even if the user specified too little width in the
    template file.

  - Dealing with "spacer"s gives better results/notifications.

- The man page of the program has been updated to provide
  information about the source file format.  Please see
  *lepton-tragesym(1)* for more information.

### Changes in `lepton-schematic`:
- Several `(gschem *)` modules have been renamed to `(schematic
  *)` ones.  All the functions and variables available are still
  re-exported in the former ones but their using is discouraged.
  The following renames have been made:
  - `(gschem action)` => `(schematic action)`
  - `(gschem attrib)` => `(schematic attrib)`
  - `(gschem builtins)` => `(schematic builtins)`
  - `(gschem gschemdoc)` => `(schematic gschemdoc)`
  - `(gschem hook)` => `(schematic hook)`
  - `(gschem keymap)` => `(schematic keymap)`
  - `(gschem selection)` => `(schematic selection)`
  - `(gschem symbol check)` => `(schematic symbol check)`
  - `(gschem util)` => `(schematic util)`
  - `(gschem window)` => `(schematic window)`

- The following modules were simply renamed without providing any
  backwards compatibility:
  - `(gschem core gettext)` => `(schematic core gettext)`
  - `(gschem repl)` => `(schematic repl)`

- The following *core* (written in C) Scheme modules have been
  renamed:
  - `(gschem core attrib)` => `(schematic core attrib)`
  - `(gschem core builtins)` => `(schematic core builtins)`
  - `(gschem core hook)` => `(schematic core hook)`
  - `(gschem core keymap)` => `(schematic core keymap)`
  - `(gschem core selection)` => `(schematic core selection)`
  - `(gschem core util)` => `(schematic core util)`
  - `(gschem core window)` => `(schematic core window)`

- Grips can now be turned on and off at run-time.
  Use the new 'Options → Grips: On/Off' menu item or
  <kbd>O</kbd>-<kbd>I</kbd> keyboard shortcut.

- In the 'Select Component' dialog, it is now possible to copy
  names and values of attributes to clipboard.

- In tabbed GUI, tabs can now be rearranged by dragging them
  with mouse.

- A context menu with common actions (`New`, `Open`, `Save`,
  `Save As`, `Page Manager`, and `Close`) has been added to
  the active tab's header widget.

- The main menu code has been refactored and changed so that the
  type of selected objects is taken into account.  Thus, some menu
  entries are not sensitive in some cases where it is
  inappropriate.

- If *Help* → *Find Component Documentation* cannot find any
  documentation, a message box is displayed. No backtrace, nor
  Guile errors are printed to the log window.

- All the `gschemrc` options are now deprecated, the settings have been migrated
  to new configuration system (`*.conf` files). Use `lepton-cli config` to read
  and modify them. New options have the same names and possible values (use
  `true`/`false` for booleans instead of `"enabled"` and `"disabled"`). Please refer
  to the
  [Configuration Settings](https://github.com/lepton-eda/lepton-eda/wiki/Configuration-Settings)
  document ("Deprecated Settings") for more information.

- In the `Save As` dialog, the filter field now shows
file extensions (`.sch`, `.sym`). A file's extension in the
`Name` box is updated when the filter is changed.

- Tree view columns in the `Page Manager` window are now
automatically resized. It helps when displayed paths are long:
the `Changed` column becomes visible again when you uncheck
the `Show full paths` checkbox.

- The `file-preview` option (`[schematic.gui]` group) processing
has been fixed. The user can turn the preview area in the
`Open...` dialog on and off.

- The user can now change the paper size and orientation in the
`Print` dialog. The combo boxes with these settings used to be set
to default values and disabled.

- The context menu has been reorganized and updated.

- Informational messages have been added to several
lepton-schematic dialog boxes.

- An "untitled" page can no longer be reverted (`Page → Revert`).
This operation doesn't make sense for it and has been disabled.

- A new configuration parameter has been added: `show-tooltips` in
the `schematic.tabs` group (`boolean`, `true` by default). It
enables or disables tooltips for tab header widgets in tabbed GUI.

- The `Single Attribute Editor` dialog can now be accepted by
pressing the `Enter` key when the `Name` field is focused.

- The save font configuration dialog has been improved so now it
  shows the name of selected font and full path names for
  configuration files.

- The files *lepton-gtkrc* are now read from the system and user
  configuration directories. They can be used to customize the
  appearance of the program.

- Fixed crashes on printing via the menu 'File → Print...' on some
  systems.

- Fixed segfaults when the locale set by environment variables was
  not installed.

- Fixed issue of incorrect main menu creation if system
  configuration file is missing.

### Changes in `examples`:
- Three new examples of analog simulation have been added. They
  reside in the directory *netlist/examples/analog/*.

- `RF_Amp` and `TwoStageAmp` examples have been updated to make
  SPICE simulation work properly.

- `gTAG` example scripts and configuration files have been fixed.

- Several `README` files have been updated.

- The obsolete Python script `bom` has been removed.

Notable changes in Lepton EDA 1.9.9 (20191003)
----------------------------------------------
### General changes:
- The version of the `libleptonrenderer` library has been fixed.

- The utility `lepton-xyrs` has been removed from the distribution
  since it has not been maintained or changed for many years and
  it does not belong to schematic capture.

### Changes in `liblepton`:
- Static gettext domain name is now used for translation files.

### Changes in `lepton-netlist`:
- Two new procedures, `page-list->schematic` and
  `file-name-list->schematic` have been added to the `(lepton
  schematic)` module to provide users with more intuitive way of
  `<schematic>` record creation. The previously present procedure
  `make-toplevel-schematic` is still there, and has been made an
  alias of the latter. Its `netlist-mode` argument has been made
  optional and is `'geda` by default.

Notable changes in Lepton EDA 1.9.8 (20190928)
----------------------------------------------
### Breaking changes:
- The configure switch `--disable-gattrib` has been renamed to
  `--disable-attrib`.

### General changes:

- A new configuration upgrade tool, `lepton-upcfg`, and C and
  Scheme infrastructure for it have been added to simplify moving
  to Lepton's new configuration system.  The new tool is aimed to
  convert legacy `geda*.conf` files into new `lepton*.conf` files
  by importing and renaming the settings in them.  For example, in
  order to produce `lepton-user.conf` from `geda-user.conf`, the
  user can use the command `lepton-upcfg --user`.  Additionally,
  the user can switch between using of legacy and new
  configuration system using a new function,
  `config-set-legacy-mode!()`, which may be set in some of Scheme
  files loaded by the tools.

- The module `(geda library)` has been renamed to `(lepton library)`.
  All the functions and variables available are still re-exported
  in the former one but its using is discouraged.

- Warnings about deprecated and "dead" RC functions are now more
  verbose, and contain link to Configuration Settings wiki page.

- Several clean-ups in the code and documentation have been made
  in both libraries' and GUI tools' code, removed some unused
  function arguments.

- A main category has been added into Lepton desktop
  files. According to
  https://specifications.freedesktop.org/menu-spec/latest/apa.html,
  previously there was no valid main category in the desktop
  files.  The problem is that without a main category, it is not
  possible to know for sure if a freedesktop compliant menu will
  show the applications the desktop files were made up for.  This
  has been fixed by adding "Development" as main category for
  Lepton. Two additional categories have been rearranged in order
  "Electronics" to have priority over "Engineering".

- The following rc procedures are now exported in the module
`(lepton library)`:

  - `component-library-command`
  - `component-library-funcs`
  - `component-library-search`
  - `component-library`
  - `reset-component-library`

- The following rc procedures are now exported in the module
`(geda deprecated)`:

  - `always-promote-attributes`
  - `attribute-promotion`
  - `bitmap-directory`
  - `bus-ripper-symname`
  - `keep-invisible`
  - `make-backup-files`
  - `print-color-map`
  - `promote-invisible`
  - `scheme-directory`

- Symbols with duplicated names have been moved to separate folders:

  - `share/lepton-eda/sym-gnetman`
  - `share/lepton-eda/sym-verilog`
  - `share/lepton-eda/sym-vhdl`

  Now it's possible to use `gafrc` function `component-library-search`
  with `share/lepton-eda/sym` without getting tons of warnings about
  duplicated symbols in the log.

- System-wide configuration file, `lepton-system.conf`, has been
  added to the distribution with default settings applicable for
  most installations.  Its legacy predecessor, `geda-system.conf`,
  has been added as well for backwards compatibility.

- The location of log files has been changed to `$XDG_CACHE_HOME/lepton-eda/logs/`.
  Previously they were stored in the user's configuration directory.

### Changes when building from source:

- Building of the tools with Guile 2.2 is now supported.

- Lepton now requires Glib 2.38.0 or later versions for build.

- It is now possible to accelerate the tools some parts of which
  are written in Scheme when they are launched for the first time.
  Previously, on first running much time would be spent on
  compilation of Scheme code involved.  The startup time can be
  reduced if the code is compiled and installed in advance.  To
  make it work, a new `make` target, `precompile` has been added.
  While the command `make precompile` can be used for local
  builds, it could be much more convenient for end users if
  package managers working on packaging Lepton for some
  distributions "precompile" the code on the package building
  stage.  The technik of "precompilation" has been already
  implemented to make packages for FreeBSD:
  [FreeBSD port](https://github.com/graahnul-grom/freebsd-lepton-eda/tree/master/wip.pc)
  and [binary package](http://graahnul.beget.tech/lepton-eda-pc/lepton-eda-1.9.7.txz)
  made from it.  In essence, it included two steps:

  - A relevant section has been added to the port's
    [`Makefile`](https://github.com/graahnul-grom/freebsd-lepton-eda/blob/master/wip.pc/Makefile), and
  - The list of compiled files has been added to the port's [`pkg-plist`](https://github.com/graahnul-grom/freebsd-lepton-eda/blob/master/wip.pc/pkg-plist).

  Package managers of other distributions could adopt this
  technik.  Please see discussion at #339 for more information.

- Fixed and improved *VPATH* (out-of-source) builds on some
  systems. It used to be that Lepton did not install files from
  the `docs/wiki/` subdirectory when the command `make install`
  was used, because it was assumed that the directory contained
  only regular files.  This has been fixed by changing appropriate
  Makefiles to search for symlinks as well.

- Build errors on FreeBSD 13.0-CURRENT on the link stage in the
  `utils/gschlas/` directory have been fixed.  In earlier FreeBSD
  version Lepton EDA builds without errors.  The issue happened to
  appear due to changing of the default linker from GNU `ld` to
  `ld.lld` from the `llvm` distribution.

- Fixed FreeBSD build errors on the `powerpc64` architecture with
  base system compiler gcc-4.2: (FreeBSD bug report
  [#239311](https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=239311)).

- Fixed build failures on Gentoo with gcc-8.2.0.

- Several C functions in `liblepton`, various Lepton tools,
  contributed code (`olib`, `sarlacc_schem`, `convert_sym`,
  `gmk_sym`) as well as PO-files have been fixed to avoid
  compilation warnings and errors.  Now `lepton-eda` compiles
  without warnings with gcc/g++ on several platforms.  Several
  warnings reported by clang++/llvm have been fixed as well.

- The like changes as above have been done in the Scheme part of
  code to prevent various compilation and deprecation warnings in
  several modules and tools when compiled with guile-2.0 or
  guile-2.2.

- LaTeX files are no longer generated by `make doxygen` since it
  was reported that several Ubuntu versions do not do TRT when
  building LaTeX documentation.

- **Doxygen** subsystem has been cleaned up to get rid of unused
  files since the tools rewritten in Scheme are not covered by its
  functionality any more.

- Fixed some portability issues in documentation building.

- Building of documentation no longer depends on the `fig2dev`
  program.  The dependency has been eliminated.

- **Gettext** translation subsystem has been changed for most of
  the tools in that to avoid unintentional and unwanted updates of
  their translation *PO* files when the tools are built from
  sources.

- Now *Lepton EDA Scheme Reference Manual* in the `HTML` format is
  built and installed with other files on `make && make install`.
  A CSS file has been added to prettify the generated HTML pages.

- Some empty application-specific directories that previously
  could not be removed after uninstalling all files by `make
  uninstall` are now correctly deleted.

### Changes in `liblepton`:

- `liblepton` configuration C and Scheme API has been extended
  with functions for removing configuration keys and groups.

- A new Scheme module, `(lepton file-system)`, has been added. It
  exports functions that previously were global:
  `regular-file?()`, `directory?()`, and `file-readable?()`.  The
  users may need to fix their scripts in which those functions
  were used by adding corresponding `(use-modules ...)` line..

- A new Scheme module, `(lepton rc)`, has been added.  It exports
  functions and variables that previously were global: build-path,
  `geda-data-path()`, `geda-rc-path()`, `path-sep()`,
  `load-scheme-dir()`, and `load-rc-from-sys-config-dirs()`.  The
  users may need to fix their scripts in which those functions and
  variables were used by adding corresponding `(use-modules ...)`
  line.

- Fixed `'config-keys` unit test which previously did not work
  correctly on some Ubuntu versions.

### Changes in `libleptonrenderer`:
- Default font name has been changed from `Arial` to `Sans`.

- Changes in path rendering:

  - Previously, the library was not able to render zero line width
    paths properly since the zero value for the line width field
    in the path definition was interpreted as a request for
    the default line width.  Therefore the path objects were always
    enlarged by some amount in size. Now, when line width of a
    path is set to zero, the stroke is not rendered, thus allowing
    drawing of filled figures without increasing their size by a
    half of the line width, so the object sizes strongly
    correspond to what the user wanted.  No line is drawn when
    exported with `lepton-cli export` command. In the
    `lepton-schematic` GUI, 1 screen pixel line is used for
    rendering zero line width path objects to provide visual
    feedback to the user.

  - Now, path objects can be rendered with different line cap
    styles when the line width is non-zero.  Previously, the
    default `square` style was always used.

### Scheme API changes
- Three legacy rc procedures, `attribute-promotion`,
  `promote-invisible`, and `keep-invisible`, have been adjusted to
  return their boolean values if used with no arguments, instead
  of just `#t`. This allows using the results in Scheme
  plugins. The procedure `bitmap-directory` now returns its string
  value if used with no arguments.  If no value has been
  previously set, it returns `#f`.

- A new hook, `open-page-hook`, has been added to the `(gschem
  hook)` module.  It allows to evaluate Scheme code when an
  existing schematic page is loaded.

- New Scheme API functions for working with object selectable
  status have been added to the `(geda object)` module:
  `object-selectable?` and `set-object-selectable!`.  Together
  with page hooks they can allow to save locking status of
  primitive objects without changing the current file format.

- The function `arc-end-angle` has been fixed so it now correctly
  returns the end angle of an arc.  Since its creation, it gave
  improper results because it was a sibling of a C function
  returning the sweep angle of an arc. A new function,
  `arc-sweep-angle`, has been added to maintain that
  functionality.

### Changes in `lepton-netlist`:

- A few improvements have been made in option processing:

  - Getopt-long functions are no more directly used inside
    netlister modules, so the modules can now be loaded in other
    programs having other options. This affects unit testing and
    `lepton-schematic` GUI, as in those cases another set of
    options may be used.

  - Processing of some options yielding lists when several of such
    options are used, no more results in reversed lists.

  - Processing of the `-c` option has been fixed so there is now
    no errors when it used more than once.

- The option *-w* introduced in Lepton EDA 1.9.6 has been
  removed. It was used to suppress warnings about missing
  configuration files.  This is no longer needed, since the
  function `config-load!()` has been fixed to not complain if
  those files are absent.

- A few improvements have been made in the SRFI-64 unit-test suite:

  - There is no more limit for backtrace length if an error
    occurs, the unit-test script uses default stack size which
    makes debugging a bit more convenient.

  - Backtrace output is now formatted properly without newline
    mangling.

  - Directories containing modules `lepton-netlist` depends on
    have been added to the unit-test script load path. Modules
    depending on `liblepton` and `symcheck` can now be used with
    the script.

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

- A new function, `set-netlist-option!`, has been added to the
  `(netlist option)` module.  It lets the users set arbitrary
  default netlister option values when the netlister modules are
  used by other tools, e.g., when they're loaded while working in
  `lepton-schematic` GUI.

- A new module `(netlist page)` has been added.  The new module
  now contains an only procedure, `filename->page()`, which opens
  a schematic page by filename.  Depending on the form of its
  call, it can return either an already opened page for a given
  filename, or a new page, if the user so desires.

- Several issues related to hierarchical names have been fixed:

  - Reverse order setting for refdes attribute no longer affects
    netnames.

  - Now netnames are formed using true reversed hierarchical name,
    before they were formed from a string representing
    hierarchical refdes and local netnames. Since the hierarchical
    refdes string might be not reversed depending of the reverse
    order setting for refdes, the resulting strings did not
    reflect hierarchy correctly.

  - For the same reason, the separator for the "net=" and
    "netname=" attributes did not affected resulting net names,
    which has been fixed as well.

  - Removing of refdes mangling was previously possible only if
    the refdes separator was "/".  This has been fixed to support
    other separators.

- The following fixes have been made in the VAMS example:
  - Deprecated gnetlistrc settings have been replaced with new
    configuration options.
  - Wrong netlister name has been fixed.
  - generate_netlist.scm has been moved to the example directory.

- The `vams` backend code has been fixed as follows:

  - The code does no longer emit Scheme return values of
    procedures, which previously mangled the output.

  - The output of generic and port definitions is now going into
    appropriate place.

  - A list of attributes that cannot form generics does no longer
    appear in the output.  Two internal (or abstract) attributes,
    `slot=` and `net=`, are no longer used in forming VHDL generic
    maps, since they are intended for internal use by netlister
    itself.

- The `allegro` backend has been refactored so obsolete procedures
  are no longer used in it.

- Two obsolete scripts written in **sh** and **awk**,
  `annotate.sh` and `unannotate.sh`, have been removed from the
  distribution.

- Three **sh** scripts, `bompp.sh`, `bom_xref.sh`, and
  `sch2eaglepos.sh`, have been moved to the *contrib/* directory.

- The directory *netlist/utils/* and an only pretty much obsolete
  utility, `mk_verilog_syms`, contained in it, have been removed
  from the distribution. The script was used to initially create
  *verilog* symbols, and output symbols in obsolete format with
  limited set of pin attributes. By now, initially created verilog
  symbols have been modified in various ways, including manual
  intervention and generating the symbols with some new version of
  the program that did not however go to the repository, so the
  removing is safe.

- Netlister backends and associated files have been moved to a new
  subdirectory `share/lepton-eda/scheme/backend`.

- An arbitrary backend file can be loaded by specifying its path
  via the new `-f` command line switch to `lepton-netlist`.
  The file should be named like `gnet-NAME.scm`, where `NAME` is the
  backend's name. It's useful for testing new and 3rd party
  backends, as well as running backends from different
  installation paths.

### Changes in `lepton-schematic`:

- The command line option *-p* has been removed. It was used to
  automatically place the main window with some default widget
  sizes.

- Several configuration settings have been changed to ensure
  reasonable default values:
  - The **rubber band** mode and **magnetic net** mode status bar
    indicators are now shown by default
    (`schematic.status-bar::show-rubber-band=true`,
    `schematic.status-bar::show-magnetic-net=true`).
  - The tabbed GUI is enabled (`schematic.gui::use-tabs=true`).
  - Monospace font is used in the log window
    (`schematic.log-window::font=Monospace 11`).
  - Monospace font is used in the macro widget entry
    (`schematic.macro-widget::font=Monospace 11`).

- The attribute attachment code has been rewritten in Scheme,
  changed and fixed in several ways:

  - Now attributes can be attached to any non-text objects.

  - Only visible attributes can be now attached.

  - If attributes are already attached or no attribute is
    selected, no action is undertaken.

  - Information about successful attachment is now printed to the
    log.

  - Successfully attached attributes and the object they are
    attached to are deselected after the attachment operation.

- The attribute detachment behaviour has been changed.  Before,
  attributes of selected objects (visible and invisible, selected
  or not) were all unconditionally stripped.  The detachment code
  has been rewritten in Scheme, changed and fixed in several ways:

  - Now only selected visible attributes are detached from their
    respective parent objects.

  - There is no need to select parent objects in order to detach
    their attributes any more.

  - The info about successfully detached attributes is now output to
    the log.

  - If there is no selection, the selected attributes are not
    attached to any object, or selection does not include any
    attributes, nothing is undertaken.

  - Just detached attributes get unselected and their color is
    changed to the color of detached attributes.

- Several improvements/fixes with regard to locking/unlocking of
  components and their attributes have been introduced:

  - Previously, when a component was locked, its attributes were
    also locked, but they changed the locking status after saving
    and reopening of schematic.  Now, they become locked along
    with the component they're attached to.

  - Previously, toggling of locking status of a component and its
    attributes required selecting them all to do it properly.
    Otherwise, the locking status of not selected attributes would
    not change.  Now, it is sufficient to select only the
    component to lock or unlock it together with the attributes,
    without having to additionally select them.

- The action 'File → Save All' and processing of "untitled"
  schematics have been improved:

  - It used to be that on saving all files, all opened pages were
    silently saved, even if there were new "untitled" pages among
    them, that is, the pages newly created under the default file
    name and not yet saved to disk.  Now such files are skipped
    from saving.

  - When constructing file name for a new "untitled" page, the
    names of already opened files and existing files in the current
    directory are taken into account. These names are not reused
    to prevent possible consequent data loss.  The skipped names
    are printed to the log.

  - A message indicating the result of the "save all" operation is
    now shown in the status bar.

  - If tabbed GUI is enabled in configuration, the header of each
    tab widget correctly updates to reflect the status of its
    page, if it is saved or not.

  - "Save As" dialog box is now opened for new "untitled" pages on
    the "save all" operation, prompting the user to save them.

- In the 'Edit Attributes' dialog, it is now possible to copy names
  and values of inherited attributes to clipboard. The feature was
  first requested on *launchpad* in
  [#1405314](https://bugs.launchpad.net/geda/+bug/1405314).

- Distinct parts of the `system-gschemrc` configuration file are
  moved to separate files under the `conf/schematic/` subdirectory
  to simplify dealing with certain aspects of `lepton-schematic`
  behaviour and searching for appropriate settings. The new files
  are loaded from `system-gschemrc`:
  - `attribs.scm`
  - `deprecated.scm`
  - `keys.scm`
  - `menu.scm`
  - `options.scm`
  - `stroke.scm`

- The following Scheme functions are now deprecated and do nothing:
  - `gschem-image`
  - `gschem-pdf`
  - `gschem-use-rc-values`

- Several `gschemrc` settings has been changed as follows:
  - `add-attribute-offset` is now deprecated and does nothing.
  - `image-color` is now deprecated and does nothing.
  - `image-size` is now deprecated and does nothing.
  - `logging-destination` is now deprecated and does nothing.
  - `log-window-type` is now deprecated and does nothing.
  - `raise-dialog-boxes-on-exp` is now deprecated and does nothing.
  - `window-size` is now deprecated and does nothing.
  - `warp-cursor` is now "disabled" by default.
  - `fast-mousepan` is now "disabled" by default.
  - `log-window` is now "later" by default.
  - `undo-levels` is now "20" by default.
  - `undo-panzoom` is now "disabled" by default.

- The `sys-doc-dir()` function code in the module `(gschem
  gschemdoc)` module responsible for searching for system
  documentation directories has been known to fail in some
  circumstances when it tried to guess where the documentation
  files are installed. The guess-work has been eliminated by using
  fixed compile-time variables in the code.

- The geometry (size and position) of `lepton-schematic`'s main
  window can now be stored in the **cache** configuration context,
  in the `schematic.window-geometry` group. The geometry is saved
  on exit and restored on startup if the (new)
  `[schematic.gui]::restore-window-geometry` configuration key is
  set to `true` (which is so by default).

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

  - As a side effect of refactoring of the widget, the program no
    longer crashes on evaluating of the command `(file-quit)` in
    the prompt.

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

- The log window can now be cleared by choosing "Clear Log Window"
  menu item in the context menu.  The user can cancel the action
  in the confirmation dialog.

- Long lines in the log window can now be wrapped by clicking the
  "Wrap Long Lines" menu item in the log window's context menu.

- Schematic page filename and more verbose information is now
  displayed in the page revert dialog.

- If you select 'Print to File' option in the print dialog, the
  output PDF file name is now derived from the name of the current
  page instead of previous default name "output.pdf".

- File filters in the dialog boxes 'Open...' and 'Save As...' now
  keep their last chosen values while the program is running.
  Previously, the filters were always set to choosing schematic
  files when the user opened the dialogs.

- Wrong behaviour of the program when the value of **draw-grips**
  is set to "disabled" in `gschemrc` has been fixed, so now this
  setting prohibits drawing grips of selected objects still
  allowing the user to resize them.

- A new menu allowing exporting of netlists has been added to the
  program.  As of now, the only item in the menu allows exporting
  of `allegro` netlists. Messages about processing are sent to log
  to provide the user with some feedback information in the log
  window.

- Fixed segmentation faults in the `always-promote-attributes` rc
  function (called from the `gafrc` configuration file) when used
  with improper arguments.

- Avoided GTK warnings on output of some message strings to the
  status bar containing characters which are treated specially by
  GTK, such as '<' and '>'.

- Page "changed" indicator is now shown in the tab headers as
  well: the file name shown is output in bolder font and an
  asterisk is added to emphasize that the file has been changed.

- Fixed font preview in the font selection dialog. Previously, if
  a custom font name was specified in the configuration key `font`
  in the `schematic.gui` configuration group, the text became
  invisible in the preview widget of the dialog, because the
  "Size" field was always initially set to zero.  This has been
  fixed by setting the initial font size to non-zero value.

- Fixed page "changed" status indication.  Previously, after
  successful *Edit* → *Embed* and *Edit* → *Unembed* operations
  the "changed" status of the page would not reflect in the page
  manager and in the window's title.  Now, both are correctly
  updated so that the status is visible to the user.

- Page manager visual appearance has been improved in several ways:

  - When working on hierarchical schematics, full schematic paths
    used to clutter the view most of the time. Now they can be
    shown at any time or hidden away by checking the "Show full
    paths" checkbox. By default, they are hidden.  The state of
    the "Show full paths" checkbox is stored in the
    `show-full-paths` key of the `schematic.page-manager` group in
    the **cache** configuration context.

  - Minimal "Filename" column width has been reduced.

  - Scrollbars are now shown only when needed.

  - Superfluous "Right click on the filename for more options..."
    label has been removed.

  - The "Update" button has been removed from the page manager
    dialog, and a similar item has been added to the context menu.

- Fixed a broken output of translated strings to the log when the
  program is compiled using guile-2.2.  It is a [known
  issue](https://lists.gnu.org/archive/html/bug-guile/2017-01/msg00020.htm)
  in guile-2.2.  The function `scm_puts()` broke translated
  Unicode strings and output them as sets of bytes.  Their further
  transformation by gtk functions resulted in unreadable text in
  the log window.

- Fixed segfaults triggered on some systems by the sequence
  <kbd>Control</kbd>+<kbd>x</kbd> <kbd>Control</kbd>+<kbd>v</kbd>
  when a component was selected.

- Fixed errors triggered by the function `file-quit` on startup,
  for example, using the following command: `lepton-schematic -c
  '(file-quit)'`.

- Fixed a bug of searching for `gschem-gtkrc` in wrong system
  config directories.

- The file `gschem.scm`, that defined some previously global
  Scheme functions, has been transformed into two modules —
  `(schematic gui keymap)` and `(schematic gui strokes)`.  While
  this simplifies Lepton code a little and eliminates a
  superfluous step of loading the file with auxiliary functions,
  the functions are no more global, so the users may have to fix
  up their extensions, if they want to use those functions, by
  making sure the necessary modules are loaded within them.

- Exceptions in the (gschem gschemdoc) module are now catched and
  reported to the user in a message box.

- The menu item *Help* → *Find Component Documentation* is now
  sensitive only if a component is selected, providing the user
  with a useful feedback.

- The action `&repeat-last-action` (the /repeat/ key *.*) has been
  improved, so it now does as following:

  - It now displays a log message when there is no last action to
    repeat rather than warning about an invalid action.

  - If an invalid action is requested, instead of using the Scheme
    function `error()` to report the failure, it just prints a
    warning to the log.

- Deprecated export scripts `image.scm`, `print.scm` and the
  `--output` command line option have been removed. Schematics can
  be exported by using `lepton-cli export` or via the 'File →
  Write Image...' dialog box.

- Current schematic's full file path can be shown in the main window's
  title if the `title-show-path` configuration key (boolean) in the
  `schematic.gui` group is set to `true`.

- Close confirmation dialog has been improved:

  - It can be resized.
  - "Show full paths" checkbox has been added.

- The `Write Image...` dialog box has been improved as follows:

  - Image color mode can now be selected (greyscale or color).
  - 1024x768 has been added to the list of image sizes.
  - Selected directory, image size, image type and color mode
    are saved and restored when this dialog is opened next time.

- New names are now used for temporary files created by undo
  subsystem in `$TMP`: `lepton-schematic.save*` instead of
  `gschem.save*`.

### Changes in `lepton-attrib`:

- The program no longer loads `gattribrc` files since for many
  years they provided no useful settings for the user.  Previously
  available Scheme functions `quit()`, `exit()`, and
  `gattrib-version()` have been removed as well.

- Separate menu description *XML* file has been removed. Now the
  menu structure is defined in the source code only.

- The size and position of the main window is now saved in the
  **cache** configuration context, in the group
  `attrib.window-geometry`, and restored on launching of the
  program.

### Changes in `lepton-symcheck`:

- The program no longer requires specifying any file name when
  interactive mode is requested, which may be convenient, for
  example, for debugging its functions.

- A simple error is output instead of Scheme backtrace when
  neither interactive mode nor input files are specified on
  command line.

### Changes in `lepton-cli`:
- Default font name for the `export` command has been changed from
  `Arial` to `Sans` (`export::font=Sans`).

### Changes in `lepton-schlas`:
- The program no longer loads `system-gschlasrc` since for many
  years it provided no useful settings for the user.  Previously
  available Scheme functions `quit()`, `exit()`, and
  `gschlas-version()` have been removed as well.

### Changes in `lepton-schdiff`:
- Default font name has been changed from `Arial` to `Sans`.

### Changes in `gmk_sym`:

- Non-working `-d` (debug) option has been removed.

Notable changes in Lepton EDA 1.9.7 (20181211)
----------------------------------------------

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


Notable changes in Lepton EDA 1.9.6 (20181101)
----------------------------------------------
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


Notable changes in Lepton EDA 1.9.5 (20180820)
----------------------------------------------

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


Notable changes in Lepton EDA 1.9.4 (20180812)
----------------------------------------------

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


Notable changes in Lepton EDA 1.9.3 (20170226)
----------------------------------------------

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

Please see [NEWS-1.9.2.txt](docs/news/NEWS-1.9.2.txt) for info on
pre-1.9.3 changes (before *Lepton EDA* has forked from *geda-gaf*).
