#!/bin/bash
# gEDA - GPL Electronic Design Automation
# Copyright (C) 2007-2008 Paul Tan (pt75234 at users.sourceforge.net)
#
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
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
# ========================================================
# Description:  
#   Generate non-flatten hierarchical Verilog netlist
# gnet_hier_verilog.sh
# version 0.0.2
# Usage: 
#   [path]gnet_hier_verilog.sh [path]FileName.sch
# Requires gawk and gEDA's gnetlist gnet-verilog.scm
# Please set tab = 2 for readability
# Written by Paul Tan
# ========================================================
# 1) This is a simple draft bash script to produce a
#    hierarchical verilog netlist in a single file. 
#    It gathers hierarchical information from a list of 
#    unique symbols/schematics originating from the top level
#    schematic all the way down to the lowest level of the 
#    design hierarchy. It then successively invokes the
#    existing gEDA verilog netlister to produce each single 
#    level netlists, and concatinates all the unique 
#    module netlists into one single hierarchical netlist 
#    file. 
# 2) Currently, it assumes that one or more hierarchical symbol
#    can be represented by a single schematic file. If needed, 
#    feature for mutiple schematic files mapped to a single symbol 
#    can be easily added. In that case, multiple source attrib
#    are used in that symbol.
# 3) It checks the follwoing errors while traversing down 
#    the hierarchy and terminates the netlisting if error 
#    is found.
#    a) if a symbol's source attribute indicated schematic can not
#       be found in the search paths defined by gafrc.
#    b) if a symbol's device attribute value does not match its
#       corresponding schematic's module_name attribute value.
# 4) This script assumes that there are no other errors in the 
#    entire hierarchy, and that the user has already run the
#    DRC. Moreover, it assumes that the user has run the single
#    level verilog netlister on each schematic in the hierarchy
#    without any error. it also assumes that the hierarchy-traverse
#    is disabled in gnetlistrc, thus disables flatten hierarchical 
#    netlist generation.
# 5) Netlist of modules are listed from top down, can easily be 
#    changed to do bottom up.
# 6) Symbol must contain "source=????.sch" attribute to have its
#    schematic netlisted. Otherwise, symbol is treated just as 
#    primitive instances in the netlist.
# 7) It only uses the gafrc file in the folder where the top level 
#    schematic resides. The search path for the symbols and
#    schematics should be defined in that gafrc file. The 
#    current implementation searches from the beginning
#    of the file, it will be changed to conform to the gEDA
#    practice of searching from the bottom first.
# 8) Hierarchy info is output to a report file for reference.
# 9)This script is optimized for readability only, I hope.
# 10)Array variables are suffixed with A, e,g,, HLineA[]
# ========================================================
#             To do list
# -------------------------------------------------------
# 1) Multi-page sch per symbol support
# 2) Use the "file" attrib to include user defined netlist
# 3) Include .gnetlistrc and other config files info to search
# 4) Change search order for sym/sch libs to LIFO (last in fisrt out)
# 5) Recode the script in C or scheme ?
# 6) Add CL args to support VHDL and Spice hier netlist
# 7) Add error checks:
#     a) add ERC (Electrical_Rule_Check) if not already checked
#        by gnetlist DRC.
# ========================================================
#
#                   Notes
#
# ========================================================
# First we create a master list in a record array HierListA[].
# HierListA[] will contain a list of records of all unique symbols
# under the top schematics and all its underlying schematics.
# This master record list will be output to a text file
# for reference.
# Then we'll invoke the gnetlist program to netlist each of 
# the schematics representing the symbols on the list,
# concatinating each netlist into the top level netlist.
#
# Each record of HierListA[] has 5 or more fields:
#  0          1         2          3         4        [5] ...
#  ItemNum    Flag     Sym_Name   Sch_Dir  Sch_Name  [Sch_Name] ..
# -------------------------------------------------------
# Corresponding variables declared in this script
# $iN         $iFlag    $iSym     $iSchDir $iSch
# $CurIndex   $CurFlag  $CurSym   $CurDir  $CurSch   ; In master
# $EndIndex   $Tflag    $Tsym     $TschDir $Tsch     ; To add
# -------------------------------------------------------
# The Flag is used to indicate if a schematic needs to 
# be netlisted.
#    Flag = 0 means NOT to netlist the schematic.
#    Flag = 1 means to netlist the schematic.
#    Flag = 2 means NOT to netlist, since other symbol already
#             has the same schematic listed.
# ----------------------------------------------
# For multiple pages(files) schematic, use more than 5 fields
# in a record.
# ========================================================
#
#          Script starts here
#
# ========================================================
# Configurable items
# ---------------------------------------
# Define the symbol's attribute to be used for its 
#   associated schematic.
src_attrib="source"
# ---------------------------------------
# version
ver=0.0.2
My_Debug=0
# ========================================================
my_error=0
my_ext=""
echo
if [ -z $1 ]
	then my_error=1
fi
# ---------------------------------------
# To be changed: loop thru multiple sch files from CL's args
if [ $my_error == 0 ]; then
	TopSchFile=$1
	my_ext=${TopSchFile#*.}
	if [ "$my_ext" != "sch" ]; then 
		my_error=1
	fi
fi
# ---------------------------------------
if [ $my_error == 1 ]; then
	echo "Error:"
	echo -n "Please include a gEDA schematic file"
	echo " as its argument."
	echo " Usage:"
	echo "   [path]gnet_hier_verilog.sh [path]FileName.sch"
	exit 1
fi
# ---------------------------------------
TopDir=$(dirname $1)
cd $TopDir
TopDir=$PWD
TopBaseFile=$(basename $1 .sch)
CurSchFile=$1
TopVFile="${TopBaseFile}.v"
ReportFile="${TopBaseFile}_Report.txt"
TemptFile="hier_temp.v"
# ---------------------------------------
echo
echo "Starting Hierarchical Verilog Netlister version ${ver} ....."
# ===============================================
if [ $My_Debug == 1 ]
	then
		echo 
		echo '======== Start Debug ==========='
		echo "TopDir = $TopDir"
		echo "TopBaseFile = $TopBaseFile"
		echo "CurSchFile = $CurSchFile"
		echo "ReportFile = $ReportFile"
fi
# ----------------------------------------------
#
# Init all indices:
# Fields position in a record
iN=0; iFlag=1; iSym=2; iSchDir=3; iSch=4
# ----------------------------------------------
# Initialize master list HierListA[]'s first record
CurIndex=0
EndIndex=0 
Tflag=1
Tsym="${TopBaseFile}.sym" 
Tsch="${TopBaseFile}.sch" 
TschDir="."
# Fields in each row of record
HLineA[$iN]=$EndIndex
HLineA[$iSym]=$Tsym
HLineA[$iSch]=$Tsch
HLineA[$iSchDir]=$TschDir
HLineA[$iFlag]=$Tflag
# First row of record in master list
HierListA[0]=`echo ${HLineA[@]:0}`
# ----------------------------------------------
# Initialize other lists
# ----------------------------------------------
# Get gEDA gafrc's symbol and schematic search paths
# into their respective list arrays
SymDirListA=( `
	gawk 'BEGIN {FS="\""}
		(NF >= 1)&&($1 == "(component-library ") {
		printf("%s\n",$2);
	}' gafrc` )
# ----------------------------------------------
SrcDirListA=( `
	gawk 'BEGIN {FS="\""}
		(NF >= 1)&&($1 == "(source-library ") {
		printf("%s\n",$2);
	}' gafrc` )

# ===============================================
if [ $My_Debug == 1 ]; then
	echo "---------------------------"
	echo -n "SymDirList n = "
	echo ${#SymDirListA[*]}
	for i in "${SymDirListA[@]}"; do echo "$i"; done
	echo "---------------------------"
	echo -n "SrcDirList n = "
	echo ${#SrcDirListA[*]}
	for i in "${SrcDirListA[@]}"; do echo "$i"; done
	echo "---------------------------"
fi
# ###############################################
# Process each record from the master list
while (("$CurIndex" <= "$EndIndex"))
do
	# Get a record from the master list
	CurA=( ${HierListA[$CurIndex]} )
	CurFlag=${CurA[$iFlag]}
	CurSchFile=${CurA[$iSch]}
	CurSchDir=${CurA[$iSchDir]}
	if [ "$CurSchDir" == "." ] 
		then CurSchPath=$CurSchFile
		else CurSchPath=`echo "$CurSchDir/$CurSchFile"`
	fi
	CurSchBase=$(basename $CurSchFile .sch)
	# --------------------------------
	# Only Flag=1 entry will get netlisted and processed
	# down the hierarchy
	if [ "$CurFlag" != 1 ]; then
		let "++CurIndex"
		continue
	fi
	# ----------------------------------------------
	# if needed, this is the place to generate netlist
	#	for each record. For now, we netlist at the end
	# after we have generated all the records
	# ----------------------------------------------
	# To be changed: loop thru pages of schs
	# ----------------------------------------------
	# Get all instances of any symbols from current schematic
	# into TempSymListA[]
	TempSymListA=( `
		gawk '(NF==7)&&($1=="C") {printf("%s\n",$7)}' $CurSchPath 
	`)
	# ----------------------------------------------
	# Make a unique symbol list from it
	TSymListA[0]=${TempSymListA[0]}
	found=0
	let "k = 0"
	for i in "${TempSymListA[@]}"; do
		found=0
		for j in "${TSymListA[@]}"; do
			if [ "$i" == "$j" ]; then 
				found=1
				break
			fi
		done # for j
		if [ $found == 1 ]; then continue
		fi
		let "++k"
		TSymListA[$k]=$i
	done # for i
	unset TempSymListA
	# ===============================================
	# Check if Tsym already exist in the master list
	# if not on list, add to the list
	found=0
	for Tsym in "${TSymListA[@]}"; do
		found=0
		for HLineA in "${HierListA[@]}"; do
			HierA=( `echo $HLineA` )
			HierSym=${HierA[$iSym]}

			if [ "$Tsym" == "$HierSym" ]; then
				found=1
				break
			fi
		done

		if [ $found == 1 ]; then continue
		fi
		# ===================================
		# Find the Symbol file and its folder
		found=0
		for i in "${SymDirListA[@]}"; do
			TsymPath=`echo "$i/$Tsym"`
			if [ -e $TsymPath ]; then
				found=1
				TsymDir=$i
				break
			fi 
		done

		if [ $found == 0 ]; then
			echo "Error: "
			echo "  $Tsym file not found. Please check the search path"
			echo "  defined in the gafrc file."
			exit 1
		fi
		# ---------------------------------
		# Find the schematic file name for the symbol from:
		#   1) The "source" attribute in the symbol file of 
		#      the $TsymDir folder.
		#   2) If no "source", use the symbol basename as the schematic
		#      basename?  For now, must use "source". 
		#   3) Note that the sch file must contain "module_name" 
		#      attrubute whose value must match the value of the 
		#      "device" attribute from the sym file. This script
		#      checks for this condition.
		# Given the schematic file name, search its folder path in:
		#   1) Source folders specified in the gEDA rc file
		# ---------------------------------
		TsymBase=$(basename $Tsym .sym)
		TschDir=$TsymDir
		Tflag=0
		# ---------------------------------
		# Get "source" and "device" attributes from sym file
		# and store them in SymAttribListA[]
		SymAttribListA=( `
			gawk -v src=$src_attrib 'BEGIN {FS="="}
				(NF==2)&&($1 == src) {print $0};
				(NF==2)&&($1 == "device") {print $0}
			' $TsymPath` )
		# ---------------------------------
		# To be changed: add a list of sources SymSrcList[]
		#                to support multi-page
		# ---------------------------------
		# Check if source exist in SymAttribListA[]
		Tsch=""
		for i in "${SymAttribListA[@]}"; do
			if [ "${i%=*}" == "$src_attrib" ]; then
				Tsch=${i#*=}; break
			fi
		done
		# ---------------------------------
		if [ "$Tsch" == "" ]; then 
				Tsch="None"; Tflag=0
			else
				# ---------------------------------
				# Found sch name, check if already on master list
				found=0
				for HLineA in "${HierListA[@]}"; do
					HierA=( `echo $HLineA` )
					HierSch=${HierA[$iSch]}
					if [ "$Tsch" == "$HierSch" ]; then	
						found=1
						break
					fi
				done
				# If sch not on master list, need further check
				if [ $found == 0 ]; then
					Tflag=2
				fi
		fi
		# ---------------------------------
		# if sch not on list, search for sch path
		if [ $Tflag == 2 ]; then
			found=0		
			for i in "${SrcDirListA[@]}"; do
				TschPath=`echo "$i/$Tsch"`
				if [ -e $TschPath ]; then
					found=1
					TschDir=$i
					break
				fi 
			done
			# if can't find sch path, error and exit
			if [ $found == 0 ]; then
				echo -n "Error: $Tsch is the $src_attrib attribute in "
				echo -n "$Tsym but NOT in the gafrc "
				echo "source-directory path."
				Tflag=3
				exit 1
			fi
			# ---------------------------------
			# Found sch path, check if sch's
			# sch's module_name attribute = sym's device attribute
			# ---------------------------------
			# Get module_name attrib from sch file
			TModule=""
			TModule=( `
				gawk 'BEGIN {FS="="}
					(NF == 2)&&($1 == "module_name") {
						printf("%s\n", $2); exit}' $TschPath` )
			# ---------------------------------
			# Check if module_name = device from SymAttribListA[]
			# ---------------------------------	
			Tdev=""		
			for i in "${SymAttribListA[@]}"; do
				if [ "${i%=*}" == "device" ]; then
					Tdev=${i#*=}; break
				fi
			done
			# ---------------------------------
			if [[ "$TModule" != "" ]] && [[ "$TModule" == "$Tdev" ]]
				then Tflag=1
				else 
					echo "Error: "
					echo "  $Tsym Symbol's device attribute $Tdev does not match"
					echo "  $Tsch schematic's module_name attribute $TModule."
				exit 1
			fi
			# ---------------------------------
		fi
		# ===================================
		# Add a new record into the master list HierListA[]
		let "++EndIndex"
		HLineA[$iN]=$EndIndex
		HLineA[$iFlag]=$Tflag
		HLineA[$iSym]=$Tsym
		HLineA[$iSchDir]=$TschDir
		HLineA[$iSch]=$Tsch
		HierListA[$EndIndex]=`echo ${HLineA[@]:0}`
		# ---------------------------------
		if [ $My_Debug == 3 ]; then
			echo "Added new record"
			echo ${HierListA[$EndIndex]}
			echo
		fi
		# ---------------------------------
	done	# done. for Tsym
	unset TSymListA
	# ===============================================
	let "++CurIndex"
done	# done generating master list
#
# ###############################################
# Generate a report file
echo "# Hierarchical netlist report for:" >$ReportFile
echo "# $TopSchFile" >>$ReportFile
echo "# ------------------------">>$ReportFile
# comments fields
HLineA[$iN]="Index"
HLineA[$iFlag]="Flag"
HLineA[$iSym]="Sym"
HLineA[$iSch]="Sch"
HLineA[$iSchDir]="Sch_Dir"
echo "# ${HLineA[@]:0}" >>$ReportFile
echo "# ------------------------">>$ReportFile
for i in "${HierListA[@]}"; do
	echo $i >>$ReportFile
done

# ###############################################
# Generate netlist from record in the master list
# Only Flag=1 record will be netlisted
# ###############################################
echo
echo "Generating hierarchical netlist ...... "
let "CurIndex=0"
let "EndIndex=${#HierListA[@]}-1"
# -----------------------------------
if [ $My_Debug == 2 ]; then
	echo
	echo "CurIndex = $CurIndex"
	echo "EndIndex = $EndIndex"
	echo
fi
# -----------------------------------
while (("$CurIndex" <= "$EndIndex"))
do
	# Get a record from the master list
	CurA=( ${HierListA[$CurIndex]} )
	# Get fields of the selected record
	CurFlag=${CurA[$iFlag]}
	CurSchFile=${CurA[$iSch]}
	CurSchDir=${CurA[$iSchDir]}
	if [ "$CurSchDir" == "." ]
		then CurSchPath=$CurSchFile
		else CurSchPath=`echo "$CurSchDir/$CurSchFile"`
	fi
	CurSchBase=$(basename $CurSchFile .sch)
	# --------------------------------
	# Only Flag=1 entry will get netlisted
	if [ $CurFlag != 1 ]; then
		let "++CurIndex"
		continue
	fi
	# --------------------------------
	# To be changed: add multi-page netlist
	# --------------------------------
	if [ "$CurIndex" -eq 0 ]
		then 
			gnetlist -g verilog -o $TopVFile $TopSchFile
		else
			gnetlist -g verilog -o $TemptFile $CurSchPath
			# Skip header portion of each netlist
			gawk 'BEGIN{found = 0}
				(NF >= 1)&&($1 == "module")&&(found == 0){
					found = 1;
					printf("\n/* --------------------------------- */\n");
				};
				(NR > 0)&&(found == 1){
					print $0;
				};
			' $TemptFile >>$TopVFile
	fi
	# --------------------------------
	let "++CurIndex"
done
echo
echo "Hierarchical Verilog netlist successfully completed."
echo

 	  	 
