Recent Changes in Lepton EDA
============================

This file documents important user-visible changes in Lepton EDA.  For
more information, please consult `git log` history.

Notable changes in Lepton EDA 1.9.10
------------------------------------
### Changes when building from source:
- Building of the Scheme API HTML documentation with multiple
  make jobs on FreeBSD has been fixed.

- `icon-theme-installer` script has been fixed by simplifying
  its command line arguments checks, which makes it more portable
  among various build environments.

### Scheme API changes:
- Apart from expanding environment variables, the function
  `expand-env-variables` from the `(geda os)` module now replaces
  **~/** (user home directory prefix) in file names in order to
  make such names understandable for other functions.  This allows
  using this prefix in functions like `component-library` and
  `source-library`.

### Changes in `liblepton`:
- The module `(lepton library component)` has been amended to
  support new Scheme layer around internal `%component-library` in
  the `component-library` procedure, which prevents loading of
  duplicate component libraries.

### Changes in `lepton-netlist`:
- Fixed crashes on power symbols (the symbols having one pin and
  no *"refdes="* attribute) having a *"net="* attribute but no
  *"pinnumber=* attribute attached to its pin.

- Fixed crashes in the `spice-sdb` backend when processing I/O
  components.

- A new module, `(netlist mode)`, has been added. It contains
  accessor functions for current netlisting mode.

- The module `(netlist page)` has been eliminated in favour of the
  `(lepton page)` module.  All functions from the former have been
  moved to the latter.  The procedure `filename->page()` has been
  renamed to `file->page()`.

### Changes in `lepton-archive`:
- The initial Python script has been rewritten in Scheme.  It was
  broken at least since 2007, when system-gafrc was refactored by
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

  - The new script eliminates direct setting of
    `GEDADATA`/`GEDADATADIR` to the directory where `system-gafrc`
    should be placed, and parsing of the lines containing
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

### Changes in `lepton-cli`:
- A new command line option has been added for the `export`
  command: `--paper-names` (`-P`). It displays a list of paper
  size names suitable for the `--paper` option.

### Changes in `lepton-schematic`:
- Grips can now be turned on and off at run-time.
  Use the new 'Options → Grips: On/Off' menu item or
  <kbd>O</kbd>-<kbd>I</kbd> keyboard shortcut.

Notable changes in Lepton EDA 1.9.9
-----------------------------------
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


Notable changes in Lepton EDA 1.9.8
-----------------------------------
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
