#! /usr/bin/env python
#
# Copyright (C) 2003 Stuart Brorson <sdb@cloud9.net>
#
#------------------------------------------------------------------
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
#-------------------------------------------------------------------

"""
This program is used to create a gEDA design archive.  It operates in two 
modes: archive mode and extract mode.  In archive mode it creates a
project archive from a bunch of project files, and in extract mode it 
extracts the files from the archive and places them in the local dir.

Detailed description:

Information about program invocation is held in the Usage string below.

--- Archive mode algorithm:
1.  It gets the local directory name, as well as a bunch of info from the
    command line & stores all the info in the object "Args".  Then it
    cd's into /tmp (ScratchDirectory).
2.  It gets the symbol library search path from $(GEDADATADIR)/ 
    gschemrc-common and ./gschemrc & creates a list of all search
    paths.  Note that the system-dependent search path is hardcoded into
    the script by "make" when the file is installed.
3.  It reads the list of files to archive from ./garchiverc (or the -f
    substitute) and the command line and creates an internal list of files to
    archive.
4.  It finds the .sch files among the archive list, and creates an internal
    list of schem files process/archive.
5.  For each schem file in the list, it opens the file, runs through it,
    and builds a list of symbols used in that file.  Naturally, duplicate
    symbols are ignored.
6.  It opens a directory called /tmp/garchive-symbols.  For each symbol 
    file in the symbol file list, it gets the file from $(GEDADATADIR)/sym
    and sticks it into ./garchive-symbols.
7.  It takes the local gschemrc, and munges it so that gschem will find 
    all symbols in ./garchive-symbols.
8.  It then creates a tar archive of the following:
    *  All files listed in garchiverc and/or the command line.
    *  The entire ./garchive-symbols directory
    *  gschemrc
    *  garchiverc
9.  The prog then gzips the tar archive, renames it to the name
    specified by the user, and sticks it in the user's directory
10. It then cd's back to the local directory & deletes the leftover
    cruft from /tmp (or ScratchDirectory).

  Important data structures during creation of archive:
  *  LibraryPathList -- list of paths to symbol libraries.
  *  ArchiveFileList -- list of all files to archive.  Includes gschemrc and
     garchiverc, as well as garchive-symbols/
  *  SchemFileList -- list of schematic files found.
  *  SymbolFileList -- list of symbol files found.  Duplicates are ignored.

--- Extract mode algorithm:
1.  Copy archive file into /tmp (or ScratchDirectory).
2.  Create a list of archive's contents using "tar -t -f ProjectArchive"
3.  Extract files into /tmp
3.  Loop on file list.  Move each file into user's directory.
    Before each move, make sure that no overwrite of existing files will occur.
    After each iteration, clean up the cruft left in /tmp.

Note:  File names used internally have no / prefix.  Dir names have no / suffix.
Therefore, must include / manually in each case.

---  TBD suggestions from users:
None right now . . . .

---  Revision history:
20031114 -- First (alpha) version by SDB.
20031121 -- Incorporated SPICE file archiving.  Now get /tmp dir from
            environment.  Presence of RC files is now optional -- user
            is queried about creating them if they don't exist.
20031204 -- Changed program so that it keeps .tar.gz as the archive file
            suffix due to popular demand.
"""
############################################################################
import sys, copy, string, getopt, re, os, commands

############################################################################
#  Helper fcns and data structures.                                        #
############################################################################

#---------------------------------------------------------------------------
#  This is the help string.
Usage =\
"""
garchive -- the gEDA archiving utility.  Used to create as well as extract
gEDA designs from an archive.  The two modes of operation are "archive mode"
(archive creation) and "extract mode".  Archive mode is the default.

Command line switches:
  -f <filename>  -- Optional.  Used in archive mode.  Read files to archive from
                    <filename> instead of garchiverc.
  -v             -- Optional.  Verbose mode.  Used in both archive and extract mode.
                    Spews lots of info about what the prog is doing.
  -e             -- Mandatory if you want to extract.  Extract mode.  
                    Open up the archive specified on the command line.
  -a             -- Optional.  Archive mode.  Archive mode (i.e. create a project
                    archive) is the default; this flag exists to give people 
                    the warm and fuzzies.
  -o <outfile>   -- Optional.  Used in archive mode.  Specifies the name of
                    the output archive file.  The output file extension
                    should be ".tar.gz".  If this flag is not specified, the
                    output file name is "ProjectArchive.tar.gz".

Example usage:
   Create an archive named MyArchive.tar.gz (files to store listed in garchiverc):
   garchive -o MyArchive.tar.gz

   Verbosely create an archive (archives files listed on cmd line
   as well as those listed in garchiverc):
   garchive -v -o MyArchive.tar.gz README Schematic1.sch Schematic2.sch Schematic3.sch

   Extract an archive:
   garchive -e ProjectArchive.tar.gz

Copyright (C) 2003 by SDB.  Released under the GPL.
"""

#---------------------------------------------------------------------------
def DebugSpew(Args, String):
    """
    This prints out String when the -v flag is set, otherwise is silent
    """
    if (Args.VerboseMode == "verbose"):
        print("---- "+ String)
    return

#---------------------------------------------------------------------------
def CheckFilename(Filename):
    """
    This checks a string to make sure that it is a valid filename.
    It currently doesn't do very much. . . .
    """
    if (re.search('\.tar\.gz$', Filename)):
        return 1
    else:
        return 0


#----------------------------------------------------------------------
def NormalizePath(SearchDir, DirName):
    """
    This fcn expands environment variables like ${GEDADATA} and ${HOME}
    in full absolute file/directory names.
    Environment vars must be in the form ${foo}.
    It expects to see a directory or file name like "${foo}/bar/baz/".
    Only one env var may be expanded at a time. SearchDir is the base
    directory from which all relative paths are normalized.
    """
    #  First replace environment var
    if re.match('\${[\w]+}', DirName):
        Match = re.search('(\${)([\w]+)(}/)([\w/]+)', DirName)
        EnvVar = Match.group(2)
        
        #  Need to make sure that something exists beyond ${ENVVAR}
        try:
            Remainder = Match.group(4)
        except KeyError:
            Remainder = ""

        # Check if env variable is ${GEDADATA}.  If so, just replace it with Args.GedaDataDir
        if (EnvVar == "GEDADATA"):
            EnvVar = Args.GedaDataDir
        else:
            #  Try to get environment variable
            try:
                EnvVar = os.environ[EnvVar]
            except KeyError:
                print ("Env variable "+EnvVar+" not defined in current environment")
                print ("Try setting it. . . .")
                sys.exit(1)

        DirName = EnvVar+"/"+Remainder

    # Now cd into SearchDir and perform a filename normalization
    # (to get rid of .. . and other relative file paths . . .)
    # Then cd back.
    CurrentDir = os.getcwd()
    os.chdir(SearchDir)
    DirName = os.path.abspath(DirName)
    os.chdir(CurrentDir)

    return DirName
    

#---------------------------------------------------------------------------
class CmdLineArgs:
    """
    This class holds info about the environment and cmd line args passed to the
    program.  It has only one method:  the constructor, which gets the args
    and fills out the public vars.  The public vars are:
    
    ProgramMode = "archive", "extract"
    OutputFileName (archive only) = the name of the archive.  default = ProjectArchive.tar.gz
    InputFileNames = [list of input files on command line]
    RcFileName (archive only) = name of garchiverc to use (instead of garchiverc)
    VerboseMode = "quiet", "verbose"
    FilesToArchiveList (archive only)
    UserDir = Directory holding files to archive.  (Directory where garchive was invoked.)
    ScratchDir = "/tmp" by default
    FileArchiveDir = "gschem-files"  by default
    GedaDataDir = directory holding GEDA data & files such as "system-gafrc"
    """
    def __init__(self):
        """
        Constructor: parse through cmd line args and fill out vars.
        """
        self.VerboseMode = "quiet"                      # default
        self.ProgramMode = "archive"                    # default
        self.GarchiveRcFileName = "garchiverc"          # default
        self.GschemRcFileName = "gschemrc"              # default
        self.OutputFileName = "ProjectArchive.tar.gz"   # default
        self.UserDir = os.path.abspath(os.getcwd())
        self.FileArchiveDir = "gschem-files"            # default

        #  Get GedaDataDir -- this will be set by the Makefile, which does
        #  an sed replacement of GEDADATADIR
        self.GedaDataDir = "GEDADATADIR"
        # self.GedaDataDir = "/home/binaries/geda/share/gEDA"       #  Used for debug

        #  Get ScratchDir, either from environment, or just use /tmp as default.
        for EnvVar in ["TMP", "TMPVAR", "TEMP"]:
            try:
                TempDir = os.environ[EnvVar]
            except:
                continue                      # Not present, continue looping
            else:
                self.ScratchDir = TempDir     # Got it!
                break
        else:
            self.ScratchDir = "/tmp"          # no env var set, use default

        #  Get and process command line args
        try:
            OptList, Args = getopt.getopt(sys.argv[1:], 'aef:ho:v')
        except getopt.error:
            print Usage                # print out usage string if
                                       # user uses invalid flag.
            sys.exit(1)

        # First pass through args.  Get switch settings & set program modes.
        for Option, Value in OptList:
            
            if Option == '-a':
                self.ProgramMode = "archive"
                    
            if Option == '-e':
                self.ProgramMode = "extract"
                        
            if Option == '-v':
                self.VerboseMode = "verbose"
                            
            if Option == '-h':
                print Usage
                sys.exit(0)
                
        # Second pass.  Do sanity checking and get configured filenames.
        for Option, Value in OptList:

            if Option == '-a':
                if self.ProgramMode == "extract":          # sanity check
                    raise SyntaxError("Incompatible command line args")

            if Option == '-e':
                if self.ProgramMode == "archive":          # sanity check
                    raise SyntaxError("Incompatible command line args")

            if Option == '-f':
                if self.ProgramMode == "extract":          # sanity check
                    raise SyntaxError("Incompatible command line args")
                try:
                    os.stat(Value)
                except OSError:
                    print("Resource file "+Value+" doesn't exist.  Exiting.")
                    sys.exit(1)
                else:
                    self.GarchiveRcFileName = Value                #strcopy?

            if Option == '-o':
                if self.ProgramMode == "extract":          # sanity check
                    raise SyntaxError("Incompatible command line args")
                if CheckFilename(Value):
                    self.OutputFileName = Value            #strcopy?
                else:
                    print("Warning -- output file suffix is not \".tar.gz\" -- the")
                    print("extractor won't know how to deal with your archive.")
                    Input = raw_input("Continue? [y/N] ")
                    if ( (len(Input) == 0) or (Input[0] != "y") ):
                        sys.exit(1)
                    else:
                        self.OutputFileName = Value         

        # Third step: Create list of files remaining on command line, and create output
        # base file name.
        self.CmdLineFileList = Args

        self.OutputFileNameBase = re.sub('\.tar\.gz', '', self.OutputFileName)
        
        return


#---------------------------------------------------------------------------
def GetLibraryPath(Args):
    """
    This fcn takes the library search path from the local dir and the system
    gschem-gafrc.
    """
    DebugSpew(Args, "Now in GetLibraryPath.")
    LibraryPathList = []

    LocalRCFileName = Args.UserDir+"/"+Args.GschemRcFileName
    SystemRCFileName = Args.GedaDataDir+"/system-gafrc"

    # Now read in system rc file and create sym lib path
    DebugSpew(Args, "Processing system resource file "+SystemRCFileName)
    try:
        SysFile = open(SystemRCFileName, "r")
    except:
        print("Unable to find system resource file "+SystemRCFileName+".")
        sys.exit(1)

    for line in SysFile.readlines():
        #  Match "(component-library " string. . . .
        if re.match('^\(component-library ', line):
            Match = re.search('(")([{$}\w/]+)(")', line)
            Dir = Match.group(2)
            Dir = NormalizePath(Args.GedaDataDir, Dir)    #  Expand any env variables such as ${GEDADATA}. . .

            # DebugSpew(Args, "Sticking "+Dir+" into LibraryPathList")
            LibraryPathList.append(Dir)

        #  Match "(component-library-search " string. . . .            
        if re.match('^\(component-library-search', line):
            Match = re.search('(")([{$}\w/]+)(")', line)
            Dir = Match.group(2)
            Dir = NormalizePath(Args.GedaDataDir, Dir)    #  Expand any env variables such as ${GEDADATA}. . .

            if Dir in LibraryPathList:
                pass
            else:
                # DebugSpew(Args, "Sticking "+Dir+" into LibraryPathList")
                LibraryPathList.append(Dir)

    SysFile.close()

    # Next read in LocalRCFileName
    DebugSpew(Args, "Processing local resource file "+LocalRCFileName)
    try:
        LocalFile = open(LocalRCFileName, "r")
    except:
        Input = raw_input(LocalRCFileName+" doesn't exist.  Create empty version in local dir? [Y/n] ")
        if ( (len(Input) == 0) or (Input[0] != "n") ):
            os.system("touch "+LocalRCFileName)
        else:
            print("You need "+LocalRCFileName+" to create archive.  Aborting.")
            sys.exit(1)

    for line in LocalFile.readlines():
        if re.match('^\(component-library ', line):
            Match = re.search('(")([\S]+)(")', line)    # Note additional . to search for
            Dir = Match.group(2)
            Dir = NormalizePath(Args.UserDir, Dir) #  Expand any env variables and ./ 

            if Dir in LibraryPathList:
                pass
            else:
                # DebugSpew(Args, "Sticking "+Dir+" into LibraryPathList")
                LibraryPathList.append(Dir)

    LocalFile.close()

    # Reverse list because that's how it is searched
    LibraryPathList.reverse()
    
    return LibraryPathList
    
#---------------------------------------------------------------------------
def CreateArchiveFileList(Args):
    """
    This creates the list of files in the archive.  It starts with
    known files, and then adds the names of the files to archive,
    given either at the command line or in the garchiverc file.
    """
    DebugSpew(Args, "Now in CreateArchiveFileList.")
    ArchFileList = []
    
    # Start with known file names 
    PotentialArchFileList = [Args.UserDir+"/"+Args.GschemRcFileName, Args.UserDir+"/"+Args.GarchiveRcFileName]  # Could use map here. . .
    
    #  Make sure each file exists and can be saved
    for FileName in PotentialArchFileList:
        if (os.path.isfile(FileName) or os.path.isdir(FileName)):
            FileName = NormalizePath(Args.UserDir, FileName)    # Just make sure filename is kosher. . . .
            ArchFileList.append(FileName)
        else:
            Input = raw_input(FileName+" doesn't exist.  Create empty version in local dir? [Y/n] ")
            if ( (len(Input) == 0) or (Input[0] != "n") ):
                print("Creating "+FileName+" in archive.")
                os.system("touch "+FileName)
                ArchFileList.append(FileName)
            else:
                print("You need "+FileName+" to create archive.  Aborting.")
                sys.exit(1)

    #  Add the gschem-files dir /tmp/gschem-files
    ArchFileList.append(Args.ScratchDir+"/"+Args.FileArchiveDir)     #  We build the archive dir in /tmp

    # Now get names of all schematics and other files to archive.

    # First get file names from command line
    DebugSpew(Args, "Examining files listed on command line")
    for File in Args.CmdLineFileList:
        File = NormalizePath(Args.UserDir, File)
        DebugSpew(Args, "Examining "+File+" for inclusion in archive")
        if (File in ArchFileList):
            break       #  Don't include file if it's already there.
        try:
            os.stat(File)
        except OSError:
            print("File "+File+" listed in command line doesn't exist.  Ignoring. . .")
            continue
        else:
            ArchFileList.append(File)

    # Next get file names from file, if specified.
    GarchiveRCFile = open(Args.UserDir+"/"+Args.GarchiveRcFileName, "r")
    DebugSpew(Args, "Examining files listed in "+Args.GarchiveRcFileName)
    while 1:
        FileName = GarchiveRCFile.readline()
        if not FileName:
            break

        FileName = re.sub('[\n\s]+', '', FileName)    #  Strip out \n chars & whitespace
        FileName = NormalizePath(Args.UserDir, FileName)
        DebugSpew(Args, "Examining "+FileName+" for inclusion in archive")

        try:
            os.stat(FileName)
        except OSError:
            print("File "+FileName+" listed in "+Args.GarchiveRcFileName+" doesn't exist.  Ignoring. . .")
            continue
        else:
            FileName = NormalizePath(Args.UserDir, FileName)
            if (FileName in ArchFileList):
                pass
            else:
                ArchFileList.append(FileName)
    
    return ArchFileList

#---------------------------------------------------------------------------
def CreateSchemFileList(Args, FileList):
    """
    This creates the list of schem files to search.  Right now I just
    run through FileList and pull out all files ending in .sch.
    Files are saved in list with basename (no path).
    """
    DebugSpew(Args, "Now in CreateSchemFileList.")
    SchemFileList = []
    for File in FileList:
        #  Match *.sch
        if re.search('\.sch$', File):     # re.search matches occurance anywhere

            # Need to make sure schem file actually exists
            # There is probably a better way to do this using os.access, but I was
            # not able to get it to work. . . . .
            try:
                TestFile = open(File, "r")
            except IOError:
                print("Can't access "+File+" for reading.  Exiting . . . .")
                sys.exit(1)
            TestFile.close()

            # Next we need to make sure that this file is not already in the list.
            if File in SchemFileList:
                pass
            else:
                SchemFileList.append( os.path.basename(File) )
            
    return SchemFileList

#---------------------------------------------------------------------------
def CreateSymbolFileList(SchemFileList, LibraryFileList):
    """
    This fcn opens each .sch file found and looks for symbol files
    (typically lurking in lines like "C 32400 53000 1 0 0 resistor-1.sym").
    When it finds a symbol file, it looks up the file's entire path, and then
    sticks it in the SymbolFileList.
    """
    DebugSpew(Args, "Now in CreateSymbolFileList.")
    SymbolFileList = []  # List starts as empty

    for SchemFileName in SchemFileList:
        SchemFile = open(SchemFileName, "r")
        for line in SchemFile.readlines():
            #  Match component line C 32400 53000 1 . . . . .
            if re.match('^C ', line):
                Match = re.match('(C )([\d]+ )([\d]+ )([\d]+ )([\d]+ )([\d]+ )([\d\w\-\./]+)', line)
                SymFile = Match.group(7)
                
                # DebugSpew(Args, "Found "+SymFile+" in schematic "+SchemFileName)

                #  Now find path for symbol file & stick it in list
                for LibPath in LibraryFileList:
                    AbsSymFileName = os.path.abspath(LibPath+"/"+SymFile)
                    if os.path.isfile(AbsSymFileName):
                        # Insert in list if not already there.
                        if AbsSymFileName in SymbolFileList:
                            pass
                        else:
                            SymbolFileList.append(AbsSymFileName)

    return SymbolFileList

#---------------------------------------------------------------------------
def CreateSPICEFileList(Args, SchemFileList):
    """
    This fcn opens each .sch file found and loops through it.
    While looping, it looks for SPICE files (typically lurking in lines like
    "file=/path/to/spice/models/circuit.cir".  When it finds a SPICE
    file, it sticks it in the SPICEFileList.
    The SPICE file names found are returned as absolute paths.
    """
    DebugSpew(Args, "Now in CreateSPICEFIleList.")
    SPICEFileList = []  # List starts as empty

    SavedLine = []
    for SchemFileName in SchemFileList:
        #  Open file in user dir.
        GschemFile = open(Args.UserDir+"/"+os.path.basename(SchemFileName), "r")
        for Line in GschemFile.readlines():
            if (re.match('^file=', Line)):
                Match = re.match('(file=)(\S+)', Line)
                SPICEFile = Match.group(2)
                #  This needs to be more sophosticated 
                # SPICEFile = os.path.normpath(SPICEFile)
                SPICEFile = os.path.abspath(SPICEFile)                    
                DebugSpew(Args, "Found "+SPICEFile+" in schematic "+SchemFileName)

                # Next we need to make sure that this file is not already in the list.
                if SPICEFile in SPICEFileList:
                    pass
                else:
                    SPICEFileList.append(SPICEFile)
                    
    return SPICEFileList

#---------------------------------------------------------------------------
def UpdateSchemFiles(Args, SchemFileList):
    """
    This fcn opens each .sch file found and loops through it.
    It stuffs each file line found into a list of lines.  While
    looping, it looks for SPICE files (typically lurking in lines like
    "file=/path/to/spice/models/circuit.cir".  When it finds a SPICE
    file, it substitutes the line found with "file=./gschem-files/circuit.cir".
    After running through the file, it closes
    the file, re-opens it as write-only, and outputs the changed file.
    Yes, this operation could take place in CreateSPICEFileList, but I thought
    it better conceptually & architecturally to split it off to a separate fcn.
    """
    DebugSpew(Args, "Now in UpdateSchemFileList.")

    SavedLine = []
    for SchemFileName in SchemFileList:
        #  Open file in user dir.
        GschemFile = open(Args.ScratchDir+"/"+os.path.basename(SchemFileName), "r")
        while 1:
            Line = GschemFile.readline()
            if not Line:
                break
            else:
                if (re.match('^file=', Line)):
                    Match = re.match('(file=)(\S+)', Line)
                    SPICEFile = Match.group(2)
                    DebugSpew(Args, "Found "+SPICEFile+" in schematic "+SchemFileName)
                    SPICEFile = Args.FileArchiveDir+"/"+os.path.basename(SPICEFile)
                    DebugSpew(Args, "Updating line to point to "+SPICEFile)
                    SavedLine.append("file="+SPICEFile+"\n")
                else:
                    SavedLine.append(Line)
    GschemFile.close()

    #  Now write out list in place of file.
    GschemFile = open(Args.ScratchDir+"/"+os.path.basename(SchemFileName), "w")
    for Line in SavedLine:
        GschemFile.write(Line)

    GschemFile.close()

    return 

#---------------------------------------------------------------------------
def SaveSymbols(SymFileList, LibraryFileList, ArchiveDirectory):
    """
    This fcn loops through all symbols in the list, 
    and  copies the file into the local
    archive.  
    """
    DebugSpew(Args, "Now in SaveSymbols.")
    for SymFileName in SymFileList:
        DebugSpew(Args, "Saving symbol "+SymFileName+" into archive "+ArchiveDirectory)
        os.system("cp "+SymFileName+" "+ArchiveDirectory+"/"+os.path.basename(SymFileName) )
                
    return 
#---------------------------------------------------------------------------
def SaveSPICEFiles(SPICEFileList,  ArchiveDirectory):
    """
    This fcn loops through all SPICE files in the list, finds the corresponding
    file somewhere in the directory tree, and then copies the file into the local
    archive.
    """
    DebugSpew(Args, "Now in SaveSPICEFiles.")
    for SPICEFileName in SPICEFileList:
        DebugSpew(Args, "Saving SPICE file "+SPICEFileName+" into archive "+ArchiveDirectory)
        os.system("cp "+SPICEFileName+" "+ArchiveDirectory+"/"+os.path.basename(SPICEFileName) )
                
    return 

#---------------------------------------------------------------------------
def UpdateRC(Args):
    """
    This fcn takes the gschemrc and updates it.
    It runs through the file, and comments out any
    occurance of (component-library. . . .  Then it appends
    a pointer to the local gschem-files directory
    """
    DebugSpew(Args, "Now in UpdateRC.")
    FileName = os.path.basename(Args.GschemRcFileName)

    #  First run through the file, reading the lines and building a list
    #  the lines found.
    SavedLine = []
    GschemRCFile = open(FileName, "r")
    while 1:
        Line = GschemRCFile.readline()
        if not Line:
            break
        else:
            if (re.match('^\(component-library', Line)):
                SavedLine.append(";; "+Line)    #  Comment out any (component-library lines found
            else:
                SavedLine.append(Line)

    GschemRCFile.close()

    #  Now write out list in place of file.
    GschemRCFile = open(FileName, "w")
    for Line in SavedLine:
        GschemRCFile.write(Line)

    #  Write pointer to new lib into file
    GschemRCFile.write("(component-library \"./"+Args.FileArchiveDir+"\")\n")

    GschemRCFile.close()

    return

#---------------------------------------------------------------------------
def IsSimpleFile(File):
    """
    This fcn returns 1 if file is simple file name ("gschemrc"), or is a
    simple directory name ("gschem-files").  It returns 0 if File is a
    compound file name ("gschem-files/symbol-1.sym").
    """
    if (os.path.basename(File) == File):
        return 1                        # Simple file
    elif (os.path.isdir(File)):
        return 1                        # Directory
    else:
        return 0


############################################################################
#  Body of archiver lives here                                             #
############################################################################
def Archive(Args):
    """
    This is the main archiver.  Program algorithm is documented above.  Primary
    data structures are a bunch of lists holding various file names.
    """
    #  First check that ScratchDir is writable by the user.  We will CD there
    #  to do real work later.
    try:
        TestFile = open(Args.ScratchDir+"/gschem_test", "w")
    except IOError:
        print("Can't work in "+Args.ScratchDir+" directory.  Check that you have write permission there.")
        sys.exit(1)
    else:
        TestFile.close()
        os.remove(Args.ScratchDir+"/gschem_test")
    
    #  Create list of files (and directories) to stick into archive.  Returned paths point
    #  to the absolute paths of the files.
    ArchiveFileList = CreateArchiveFileList(Args)

    # print
    # print "ArchiveFileList = ",
    # print ArchiveFileList

    #  Create list of paths to various library files.  Returned paths are absolute path names
    LibraryPathList = GetLibraryPath(Args)

    # print
    # print "LibraryPathList = ",
    # print LibraryPathList

    #  Create list of schematic files to open and search.  Returned paths 
    #  give only the base name (i.e. no path)
    SchemFileList = CreateSchemFileList(Args, ArchiveFileList)

    # print
    # print "SchemFileList = ",
    # print SchemFileList

    #  Now run through SchemFileList and create list of symbols.  Symbols are returned
    #  with only base file name (i.e. no path).
    SymbolFileList = CreateSymbolFileList(SchemFileList, LibraryPathList)

    # print
    # print "SymbolFileList = ",
    # print SymbolFileList

    #  Now run through SchemFileList and create list of pointers to spice files
    # ("file" attributes).  SPICEFiles are returned using absolute paths.
    SPICEFileList = CreateSPICEFileList(Args, SchemFileList)

    # print
    # print "SPICEFileList = ",
    # print SPICEFileList

    #  Now cd into /tmp dir and copy all files over to /tmp directory for processing.
    os.chdir(Args.ScratchDir)

    DebugSpew(Args, "Cd into "+Args.ScratchDir+" for remainder of work.")
    for File in ArchiveFileList:
        if (os.path.dirname(File) == Args.UserDir):
            os.system("cp "+File+" "+Args.ScratchDir)

    #  Now run through SchemFileList and update .sch file by stuffing names
    #  of SPICE files into them.  Save the resulting .sch files in the /tmp directory.
    UpdateSchemFiles(Args,SchemFileList)

    #  Open gschem-files directory and stick symbol & SPICE files into into it
    try:
        Dir = NormalizePath(Args.ScratchDir, Args.FileArchiveDir)
        os.mkdir(Dir)
    except:  # Directory exists.
        os.system("rm -fR "+Dir)        # Remove contents of old dir
        os.mkdir(Dir)                   # Replace with new dir.

    SaveSymbols(SymbolFileList, LibraryPathList, Dir)
    SaveSPICEFiles(SPICEFileList,  Dir)
            
    #  Now create tar file.  We copy remaining files over to /tmp, and then tar them
    #  all up using a local, relative file prefix.

    #  Create string of files to archive
    ArchiveString = ""
    for File in ArchiveFileList:
        # if (os.path.dirname(File) == Args.UserDir):
        #    os.system("cp "+File+" "+Args.ScratchDir)
        ArchiveString = ArchiveString+" "+os.path.basename(File)

    DebugSpew(Args, "Files to archive: "+ArchiveString)

    # Update copy of gschemrc
    UpdateRC(Args)

    DebugSpew(Args, "Creating archive in "+Args.ScratchDir+" directory.")

    #  Now use this in tar command.
    os.system("tar -cf "+Args.OutputFileNameBase+".tar "+ArchiveString)
    os.system("gzip "+Args.OutputFileNameBase+".tar")

    # Now try to move completed archive back to user directory.
    DebugSpew(Args, "Moving archive into local directory.")
    try:
        os.stat(Args.UserDir+"/"+Args.OutputFileName)
    except OSError:                   #  archive is not in user directory yet, no need to force it.
        os.system("mv "+Args.OutputFileName+" "+Args.UserDir)
    else:                          # Directory already exists
        Input = raw_input(Args.UserDir+"/"+Args.OutputFileName+" already exists.  Overwrite? [y/N] ")
        if ( (len(Input) == 0) or (Input[0] != "y") ):
            print("Preserving existing archive in local directory.")
            print("Your new archive lives in "+Args.ScratchDir+"/"+Args.OutputFileName)
        else:
            os.system("rm -fR "+Args.UserDir+"/"+Args.OutputFileName)   # Remove old archive
            os.system("mv "+Args.OutputFileName+" "+Args.UserDir)
            print("gEDA archive "+Args.UserDir+"/"+Args.OutputFileName+" created successfully!")

    #  Clean up remaining mess
    os.system("rm -fR "+ArchiveString)
    os.chdir(Args.UserDir)

    return    #  End of fcn . . .

############################################################################
#  Body of extracter lives here                                            #
############################################################################
def Extract(Args):
    """
    This fcn extracts the archive.  It tries to do it very carefully, and won't
    overwrite anything you don't want it to.  Algorithm:
    1.  copy archive file into /tmp
    2.  list its contents
    3.  Extract files indivdually, and check that each one is not present in the
        destination dir before moving it.
    """
    if (len(Args.CmdLineFileList) == 0):
        print("Must specify a filename for extraction.")
        sys.exit(1)


    for FileName in Args.CmdLineFileList:
        DebugSpew(Args, "Trying to extract archive "+FileName+".")

        try:
            os.stat(FileName)
        except OSError:
            print("File "+FileName+" doesn't exist.  Ignoring")
            continue

        try:
            os.system("cp -f "+FileName+" "+Args.ScratchDir)
        except IOError:
            print("Can't work in the "+Args.ScratchDir+" directory.  Check that you have write permisison there.")
            sys.exit(1)
            
        os.chdir(Args.ScratchDir)

        # Change name of file so it can be gunziped.
        if not CheckFilename(FileName):
            print( """
Error -- the file suffix is not \".tar.gz\"; garchive can't do extraction.
If this archive was created using garchive, you can rename it using
.tar.gz as suffix and try again.  Otherwise, just gunzip and tar -xvf
the file manually.
            """)
            sys.exit(1)     

        # Now gunzip the file, then change File name to reflect new status (.tar)
        os.system("gunzip -f "+FileName)
        NewFileName = re.sub('\.gz', '', FileName)

        # Get list of files in archive.  Then open up archive
        ReturnString = commands.getoutput("tar -t -f "+NewFileName)
        FileList = re.split('\s+', ReturnString)

        for File in FileList:
            DebugSpew(Args, "Extracting "+File)
            os.system("tar -f "+NewFileName+" -x "+File)

        #  We need to treat directories carefully.  For each file, check
        #  if it is a simple file, a directory name, or a compound file.
        for File in FileList:
            if (IsSimpleFile(File)):
                try:
                    os.stat(Args.UserDir+"/"+File)
                except OSError:
                    os.system("mv "+File+" "+Args.UserDir)
                else:
                    Input = raw_input(Args.UserDir+"/"+File+" already exists.  Overwrite? [yN] ")
                    if ( (len(Input) == 0) or (Input[0] != "y") ):
                        print("Preserving existing "+File+" in local directory.")
                    else:
                        os.system("rm -fR "+Args.UserDir+"/"+File)
                        os.system("mv -f "+File+" "+Args.UserDir)

        #  Now clean up /tmp directory
        os.system("rm -fR "+NewFileName)
        os.system("rm -fR "+FileName)
        os.chdir(Args.UserDir)

    return     #  End of fcn . . . . .

############################################################################
############################################################################
#  Main prog begins here                                                   #
############################################################################
############################################################################
# First get and parse command line args
Args = CmdLineArgs()  #  Creates Args object holding command line args info.

if Args.ProgramMode == "archive":
    Archive(Args)
    sys.exit(0)
elif Args.ProgramMode == "extract":
    Extract(Args)
    sys.exit(0)
else:
    raise RuntimeError("Unknown program mode found.")

#  That's it -- very simple!!








