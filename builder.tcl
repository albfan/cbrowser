# -*- tcl -*-

# $Id: builder.tcl,v 1.2 2000/07/09 19:31:44 cfelaco Exp $

# Cbrowser is a C/C++ source code indexing, querying and browsing tool
# Copyright (C) 1998  B. Christopher Felaco

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

# For more information about Cbrowser and it's author see:
#    URL:http://cbrowser.sourceforge.net/
#
# Feel free to contact me at URL:mailto:cfelaco@users.sourceforge.net
# with enhancements or suggestions.

# $Log: builder.tcl,v $
# Revision 1.2  2000/07/09 19:31:44  cfelaco
# Fixes for NT and freewrap.
#
# Revision 1.1  2000/06/26 19:23:59  cfelaco
# Initial source file revisions for sourceforge.net.
# Existing revision history is based on RCS revisions made by original author
# in private repository.
#
# Revision 0.6  1999/03/15 01:19:16  chris
# Add support for 'xz' backend query type.
# Begin support for NT - not complete until pipe asynch IO problems resolved.
# Reorganize dialog rows.
# Add reserved words toggle for xz.
# Fetch filenames from selected database for rebuilding.
#
# Revision 0.5  1998/07/16 17:07:07  chris
# Build_dialog now prompts for filename first, so it needs the parent of the
# file dialog.
# Magic scrollbars for directory list.
# Minsize for dialogs.
# Use tk_messageBox instead of tk_dialog.
#
# Revision 0.4  1998/06/27 22:35:41  chris
# Initial revision, synched with cbrowser v0.4
#

#----------------------------------------------------------------------------
# This file contains functions that implement the database builder portions
# of cbrowser.  The functions cannot operate unless called from within
# cbrowser.
#----------------------------------------------------------------------------

##############################################################################
#
#  Purpose    : Create or raise a dialog to get information about building
#               a database.
#
#  Parameters : toplevel - the toplevel build dialog to create
#               parent - the parent window of the file selection dialog
#
#  Result     : NONE
#
##############################################################################

proc build_dialog {toplevel parent} {
  global build_dbase build_backend build_which build_source build_include \
      build_status query_backend query_backend_list

  if {[strcmp $build_status "none"] != 0} {
    tk_messageBox -parent $toplevel -title "Error" -type ok -icon error \
        -message "A build is already in progress."
    return
  }

  set new_dbase [select_database $parent 1]
  if {[strlen $new_dbase] == 0} {
    return
  }
  set build_dbase($toplevel) $new_dbase
  set build_backend($toplevel) [database_backend $new_dbase]

  set build_which($toplevel) source

  set build_source($toplevel) {}
  set build_include($toplevel) {}

  if {[strcmp $build_backend($toplevel) "xz"] == 0 &&
      [file exists $new_dbase]} {
    set build_source($toplevel) [build_get_xz_modules $new_dbase]
  }

  if [winfo exists $toplevel] {

    wm deiconify $toplevel
    catch {raise $toplevel}

    build_switch $toplevel source
    return
  }

  toplevel $toplevel

  # Top row contains the target file name
  set row [frame $toplevel.filerow]
  pack $row -side top -fill x -padx 5

  label $toplevel.flabel -text "Database: "
  label $toplevel.filename -textvariable build_dbase($toplevel) \
      -relief groove -padx 3
  eval tk_optionMenu $toplevel.backend build_backend($toplevel) \
      $query_backend_list
  pack $toplevel.flabel -side left -in $row
  pack $toplevel.filename -side left -fill x -expand yes -in $row
  pack $toplevel.backend -side left -padx 3 -in $row

  # Radio buttons and add/remove buttons
  set row [frame $toplevel.radiorow]
  pack $row -side top ; #-fill x -expand yes

  radiobutton $toplevel.source -variable build_which($toplevel) \
      -text "Source" -value source  -command "build_switch $toplevel source"
  radiobutton $toplevel.include -variable build_which($toplevel) \
      -text "Include" -value include -command "build_switch $toplevel include"

  pack $toplevel.source $toplevel.include -in $row -side left

  button $toplevel.add -text "Add" -command "build_add $toplevel" -width 10
  button $toplevel.remove -text "Remove" -command "build_remove $toplevel" \
      -width 10

  pack $toplevel.add    -in $row -side left -padx 5
  pack $toplevel.remove -in $row -side left -padx 5

  # Next row contains the entry field and select button
  set row [frame $toplevel.entryrow]
  pack $row -side top -padx 5 -fill x

  label $toplevel.elabel -text "Entry: "
  entry $toplevel.entry -width 30
  button $toplevel.sselect -text "Select" \
      -command "build_file_select $toplevel.entry"
  bind $toplevel.entry <Return> [list $toplevel.add invoke]
  pack $toplevel.elabel -side left -in $row
  pack $toplevel.entry -side left -fill x -expand yes -in $row
  pack $toplevel.sselect -side left -padx 3 -in $row

  # Next row contains the listbox
  set row [frame $toplevel.list_row]
  pack $row -expand yes -fill both -side top

  listbox $toplevel.list \
      -selectmode extended \
      -exportselection false

  magic_scroll $toplevel.list $row

  # Bindings
  bind $toplevel.list <Delete> [list $toplevel.remove invoke]
  bind $toplevel.list <BackSpace> [list $toplevel.remove invoke]
  bind $toplevel.list <Button-1> \
      "$toplevel.entry delete 0 end
       $toplevel.entry insert 0 \[ %W get \[%W nearest %y\]\]
      "

  # The next row is for toggles
  set row [frame $toplevel.toggles]
  checkbutton $toplevel.force -text "Force Rebuild" \
      -variable build_force($toplevel)
  checkbutton $toplevel.reserved -text "Reserved Words" \
      -variable build_reserved($toplevel)
  pack $toplevel.force $toplevel.reserved -side left -in $row
  pack $row -side top

  # The bottom row should be the buttonbar
  frame $toplevel.buttonbar
  pack  $toplevel.buttonbar -side top;

  button $toplevel.ok -text OK -width 8 \
      -command [concat "wm withdraw $toplevel;" \
                    "build_database $toplevel.progress " \
                    "\$build_dbase($toplevel) " \
                    "\$build_backend($toplevel) " \
                    "\$build_force($toplevel) \$build_reserved($toplevel)" \
                    "\$build_source($toplevel) \$build_include($toplevel)"]
  button $toplevel.cancel -text Cancel -width 8 \
      -command "wm withdraw $toplevel; set build_status none"
  global button_defaults
  if {$button_defaults} {
    $toplevel.ok configure -default active
    $toplevel.cancel configure -default normal
  }
  # The build_status is set so that another procedure can wait on the variable
  # to detect completion of the build.
  button $toplevel.help -text Help -width 8 -command {help_proc building}
  pack $toplevel.ok $toplevel.cancel $toplevel.help -side left \
      -in $toplevel.buttonbar -padx 10

  bind $toplevel <<Cancel>> "$toplevel.cancel invoke"
  bind $toplevel <<Help>> "$toplevel.help invoke"

  wm title $toplevel "Database Build Dialog"
  wm geometry $toplevel 450x350

  # Call build_switch to initialize the listbox
  build_switch $toplevel source
}

proc build_dbase_select {entry} {
  set new_dbase [select_database [winfo toplevel $entry] 1]
  if {[strlen $new_dbase] != 0} {
    $entry delete 0 end
    $entry insert 0 $new_dbase
    cd [file dirname $new_dbase]
  }
}

proc build_file_select {entry} {

  set filetypes {
    {"C/C++ source files" {".c" ".cc" ".C" ".CC" ".cpp" ".h" ".H"}}
    {"All files" {*}}
  }

  set result [tk_getOpenFile \
                  -parent [winfo toplevel $entry] \
                  -filetypes $filetypes \
                  -title "Select File"]

  if {[strlen $result] != 0} {
    $entry delete 0 end
    $entry insert 0 $result
  }
}

##############################################################################
#
#  Purpose    : Perform a database build.
#
#  Parameters : toplevel - the toplevel build progress dialog
#               new_dbase - the new database file to construct
#               unconditional - boolean to force rebuild of database
#               reserved - boolean to include reserved words (xz only)
#               source_list - source files to scan
#               include_list - directories to search for include files
#
#  Result     : NONE
#
##############################################################################

proc build_database {toplevel new_dbase backend unconditional reserved \
                         {source_list {}} {include_list {}}} {
  global env tcl_platform build_status database_file 

  if {[strcmp $new_dbase $database_file] == 0} {
    global query_pipe
    if [info exists query_pipe] {
      catch {close $query_pipe}
      unset query_pipe
    }
  }

  if {![winfo exists $toplevel]} {
    toplevel $toplevel
    label $toplevel.label -text "Build Progress"
    label $toplevel.line -width 50 -relief sunken
    pack $toplevel.label $toplevel.line -side top -fill x
    button $toplevel.abort -text Abort
    pack $toplevel.abort -side top
    wm title $toplevel "Build Progress"
  } else {
    wm deiconify $toplevel
    catch {raise $toplevel}
  }

  # Set an initial value for the progress field.
  $toplevel.line configure -text "(waiting for response)"

  # Switch to the directory of the database so relative paths are set properly
  set cwd [pwd]
  cd [file dirname $new_dbase]

  # Start the build process.  When building, status lines are written to
  # stderr, so the output must be redirected into one stream.
  switch -glob -- $backend {
    "cs*" {
      #set env(SOURCEDIRS) [join $source_list ":"]
      set env(INCLUDEDIRS) [join $include_list ":"]

      set cmd [concat "|$backend -b -f [file tail $new_dbase] " $source_list]
      if {$unconditional} {
        lappend cmd "-u"
      }
    }
    "xz" {
      if {$reserved} {set flag "-w"} else {set flag ""}
      set cmd [concat "|$backend -t 1 $flag -ny -f [file tail $new_dbase] " $source_list]
    }
    default {
      error "Unknown backend program \"$backend\"."
    }
  }

  if {[strcmp $tcl_platform(platform) "unix"] == 0} {
    lappend cmd "|&" "cat"
  }
  if [catch {open "$cmd" "r"} f] {
    global errorInfo errorCode
    set info $errorInfo; set code $errorCode
    wm withdraw $toplevel
    error $f $info $code
  }
  cd $cwd
  if {[strlen $f] == 0} {
    error
  }

  # Disable submit temporarily
  #.submit configure -state disabled

  # Register the handler for the input pipe
  fileevent $f readable "build_handler $toplevel $f"
  set build_status "building $new_dbase"

  update idletasks

  # Set up the abort button on the dialog.  This must be done each time so
  # that the file argument is updated.
  $toplevel.abort configure -command [list build_abort $f]

  # Process file and user events until done or user presses Abort
  tkwait variable build_status

  catch {close $f; fileevent $f readable ""}

  $toplevel.line configure -text "Build Complete"
  after 1000 "wm withdraw $toplevel"

  # Clear the build status flag
  set build_status none
}

##############################################################################
#
#  Purpose    : Add directories matching wildcard pattern to the list of
#                included directories for a database build.
#
#  Parameters : toplevel - the toplevel build dialog
#
#  Result     : NONE
#
##############################################################################

set build_status "none"

proc build_add {toplevel} {
  global build_dbase

  upvar build_which($toplevel) which_list
  upvar build_${which_list}($toplevel) the_list

  # Save the current dir and switch to the database dir
  set cwd [pwd]
  cd [file dirname $build_dbase($toplevel)]

  set pattern [$toplevel.entry get]
  if [catch {glob $pattern} file_list] {
    tk_messageBox -parent $toplevel -title "Error" -type ok -icon error \
        -message "No files match the pattern:\n$pattern"
    cd $cwd
    return
  }
  set junk_list ""
  foreach file $file_list {
    set file [file nativename $file]

    if {[strcmp [file pathtype $file] "relative"] == 0} {
      set file [file join [pwd] $file]
    }

    if {([strcmp $which_list "include"] == 0 && ![file isdirectory $file]) ||
        ![file exists $file]} {
      lappend junk_list $file
      continue
    }

    if {[lsearch $the_list $file] < 0} {
      $toplevel.list insert end $file
      lappend the_list $file
    }
  }
  if {[strlen $junk_list] > 0} {
    tk_messageBox -parent $toplevel -title "Error" -type ok -icon error \
        -message "The following are not valid:\n[join $junk_list \n]"
  }
  # Return to the original directory
  cd $cwd
}

##############################################################################
#
#  Purpose    : Remove an item from the directory list.
#
#  Parameters : toplevel - the toplevel build dialog
#
#  Result     : NONE
#
##############################################################################

proc build_remove {toplevel} {
  upvar build_which($toplevel) which_list
  upvar build_${which_list}($toplevel) the_list

  foreach entry [$toplevel.list curselection] {
    set value [$toplevel.list get $entry]

    # Find the index of the entry
    set index [lsearch -exact $the_list $value]
    # Remove the entry
    set the_list [lreplace $the_list $index $index]
  }

  # Clear the listbox and re-add the elements
  $toplevel.list delete 0 end
  eval $toplevel.list insert 0 $the_list
}

##############################################################################
#
#  Purpose    : Switch the list to the source directories or include dirs.
#
#  Parameters : toplevel - the toplevel build dialog
#               new_value - the new value, either "source" or "include"
#
#  Result     : 
#
##############################################################################

proc build_switch {toplevel new_value} {
  upvar build_${new_value}($toplevel) the_list

  $toplevel.list delete 0 end
  eval $toplevel.list insert 0 $the_list
}

##############################################################################
#
#  Purpose    : Handle status output from a database build.
#
#  Parameters : toplevel - the toplevel build dialog
#               pipe - the file handle of the input pipe
#
#  Result     : NONE
#
##############################################################################

proc build_handler {toplevel pipe} {
  global current_results

  if [eof $pipe] {
    catch {close $pipe}
    global build_status
    set build_status "done"
    return
  }

  gets $pipe line

  $toplevel.line configure -text $line
  update
}

##############################################################################
#
#  Purpose    : Abort a running database build.
#
#  Parameters : pipe - the file handle of the input pipe
#
#  Result     : NONE
#
##############################################################################

proc build_abort {pipe} {
  global build_status

  # The process must be killed, or the file close will hang
  foreach pid [pid $pipe] {
    exec kill $pid
  }
  set build_status "abort"
}

##############################################################################
#
#  Purpose    : Get the list of modules used in a xz database.
#
#  Parameters : dbname - the xz database name
#
#  Result     : NONE
#
##############################################################################

proc build_get_xz_modules {dbname} {

  # Start up xz
  set f [open "| xz -f $dbname" "r+"]

  # Request the list of all modules
  puts $f ".M"

  set modules {}
  while {[gets $f line] > 0} {
    lappend modules [lindex [split $line ","] 0]
  }
  return $modules
}
