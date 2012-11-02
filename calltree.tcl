# -*- tcl -*-

# $Id: calltree.tcl,v 1.3 2000/09/06 13:55:24 cfelaco Exp $

# Cbrowser is a C/C++ source code indexing, querying and browsing tool
# Copyright (C) 1998-2000  B. Christopher Felaco

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

# For more information about Cbrowser and its author see:
#    URL:http://cbrowser.sourceforge.net/
#
# Feel free to contact me at URL:mailto:cfelaco@users.sourceforge.net
# with enhancements or suggestions.

# $Log: calltree.tcl,v $
# Revision 1.3  2000/09/06 13:55:24  cfelaco
# Added message when failed to find function.
#
# Revision 1.2  2000/07/30 07:38:53  cfelaco
# Finally fixed syntax highlighting bug.
# Updated URLs for Jeff Hobbs' stuff.
# Corrected typos in headers (thanks foka).
#
# Revision 1.1  2000/06/26 19:23:59  cfelaco
# Initial source file revisions for sourceforge.net.
# Existing revision history is based on RCS revisions made by original author
# in private repository.
#
# Revision 0.7  1999/03/15 01:20:30  chris
# Begin support for NT - not complete until pipe asynch IO problems resolved.
# Use Tk 8.0 style menus.
# Remove utility functions now in ftcllib.
#
# Revision 0.6  1998/10/14 03:37:17  chris
# Added view file option.
# Modified edit menu.
# Improved function definition search algorithm.
#
# Revision 0.5  1998/07/16 17:04:00  chris
# Switched order of tree and edit menu.
# Added Exit option and removed unusable edit options.
# Added database info row with query activity light.
# Added message bar.
# Fixed calltree_view_node (found wrong locations sometimes).
# Calltree_expand uses set_callees.
#
# Revision 0.4  1998/06/27 22:32:29  chris
# Initial revision, synched with cbrowser v0.4
#

#----------------------------------------------------------------------------
# This file contains functions that implement the hierarchical function call
# viewer portions of cbrowser.  The functions cannot operate unless called
# from within cbrowser.
#----------------------------------------------------------------------------

##############################################################################
#
#  Purpose    : Create or raise a calltree dialog.
#
#  Parameters : toplevel - the toplevel dialog
#
#  Result     : NONE
#
##############################################################################

proc calltree_dialog {toplevel} {
  global argv0 query_backend

  if [winfo exists $toplevel] {
    wm deiconify $toplevel
    catch {raise $toplevel}
    return
  }

  switch -- $query_backend {
    "xz" {
      tk_messageBox -type ok -icon info -title "Sorry" \
        -message "The calltree is currently unsupported when xz is in use."
      return
    }
  }

  toplevel $toplevel
  # Withdraw it until it's done
  wm withdraw $toplevel

  global tcl_version
  if {$tcl_version < 8.0} {
    set file_menu $toplevel.file.menu
    set edit_menu $toplevel.edit.menu
    set tree_menu $toplevel.tree.menu
    set help_menu $toplevel.help.menu

    frame $toplevel.menubar  -borderwidth 2 -relief raised

    menubutton $toplevel.file -menu $file_menu \
        -text "File" -underline 0
    
    menubutton $toplevel.edit -menu $edit_menu \
        -text "Edit" -underline 0

    menubutton $toplevel.tree -menu $tree_menu \
        -text "Tree" -underline 0

    menubutton $toplevel.help -menu $help_menu \
        -text "Help" -underline 0

    # Pack it up
    pack $toplevel.file -in $toplevel.menubar -side left
    pack $toplevel.edit -in $toplevel.menubar -side left
    pack $toplevel.tree -in $toplevel.menubar -side left
    pack $toplevel.help -in $toplevel.menubar -side right

    pack $toplevel.menubar -in $toplevel -side top -fill x
  } else {
    set file_menu $toplevel.file
    set edit_menu $toplevel.edit
    set tree_menu $toplevel.tree
    set help_menu $toplevel.help

    # Use the new menubar property
    set menu [menu $toplevel.mainmenu]
    $menu add cascade -label "File" -underline 0 -menu $file_menu
    $menu add cascade -label "Edit" -underline 0 -menu $edit_menu
    $menu add cascade -label "Tree" -underline 0 -menu $tree_menu
    $menu add separator
    $menu add cascade -label "Help" -underline 0 -menu $help_menu
    $toplevel configure -menu $menu
  }

  # Set up the main File menu
  menu $file_menu
  $file_menu add command -label "Select Database..." \
      -underline 0 -accel <Meta-s> -command "database_prompt $toplevel"
  bind $toplevel <Meta-s> "database_prompt $toplevel"
  $file_menu add command -label "Build Database..." \
      -underline 0 -accel <Meta-b> -command "build_dialog .build $toplevel"
  bind $toplevel <Meta-b> "build_dialog .build $toplevel"
  $file_menu add command -label "View File..." -underline 0 \
      -accel <Meta-v> -command "view_file $toplevel.file_viewer"
  $file_menu add command -label "Edit File..." -underline 0 \
      -accel <Meta-e> -command "edit_selected $toplevel"
  bind $toplevel <Meta-e> "edit_selected $toplevel"
  $file_menu add command -label "Query Browser..." -underline 0 \
      -accel <Meta-q> -command "wm deiconify .; raise ."
  bind $toplevel <Meta-q> "wm deiconify .; raise ."
  $file_menu add separator
  $file_menu add command -label "Close" -underline 0 \
      -command "wm withdraw $toplevel"
  $file_menu add command -label "Exit" -underline 1 \
      -command "quit_cbrowser"

  setup_edit_menu $edit_menu $toplevel.file_viewer

  # However, most entries don't apply
  $edit_menu entryconfig "Cut" -state disabled
  #$edit_menu entryconfig "Clear" -state disabled
  $edit_menu entryconfig "Paste" -state disabled

  # Set up the Tree menu
  menu $tree_menu
  $tree_menu add command -label "Set Root..." -underline 4 \
      -command "calltree_root_dialog $toplevel"
  bind $toplevel <Meta-r> "$toplevel.setroot invoke"
  $tree_menu add command -label "Expand function" -underline 0 \
      -command "$toplevel.hier open \[$toplevel.hier curselection\]"
  $tree_menu add command -label "Collapse function" -underline 0 \
      -command "$toplevel.hier close \[$toplevel.hier curselection\]"

  setup_help_menu $help_menu

  # Database info row lists the selected database
  frame $toplevel.database_row

  label $toplevel.database_label -text "Selected Database: "
  label $toplevel.database -textvariable database_file -relief groove -padx 3
  bind  $toplevel.database <Button-1> "database_prompt $toplevel"

  global history_button
  menubutton $toplevel.dbase_recall -image $history_button
  menu $toplevel.dbase_recall.menu -tearoffcommand "single_tearoff" \
      -postcommand "dbase_menu_post $toplevel.dbase_recall.menu"
  $toplevel.dbase_recall configure -menu $toplevel.dbase_recall.menu

  # Query activity button
  frame $toplevel.activity -background grey80 \
      -width 10 -height 10 -bd 2 -relief sunken ;# -cursor crosshair

  # Pack the database row
  pack $toplevel.database_label -in $toplevel.database_row -side left
  pack $toplevel.database       -in $toplevel.database_row -side left
  pack $toplevel.dbase_recall   -in $toplevel.database_row -side left -padx 5
  pack $toplevel.activity       -in $toplevel.database_row -side right -padx 5

  pack $toplevel.database_row -side top -fill x -padx 2 -pady 2

  frame $toplevel.pane -relief groove
  frame $toplevel.pane.left
  frame $toplevel.pane.right

  # Create the paned window
  Pane_Create $toplevel.pane.left $toplevel.pane.right -orient horizontal \
      -in $toplevel.pane -percent 0.20 -minpercent 0.10

  hierarchy $toplevel.hier \
      -browsecmd calltree_expand \
      -nodelook  calltree_look \
      -command calltree_view_node \
      -root main 

  # When a node is selected with a single click, display the file
  bind $toplevel.hier <Button-1>        "+calltree_click %W %x %y"

  pack $toplevel.hier -in $toplevel.pane.left -padx 4 -expand yes -fill both

  frame $toplevel.file_frame

  # Create a row for the file and line information
  frame $toplevel.info_row
  label $toplevel.file_label -text "File: "
  entry $toplevel.file_field -state disabled \
      -textvariable current_file($toplevel.file_viewer)
  label $toplevel.line_label -text "Line: "
  entry $toplevel.line_field -state disabled -width 4 \
      -textvariable current_line($toplevel.file_viewer)

  pack $toplevel.file_label -in $toplevel.info_row -side left
  pack $toplevel.file_field -in $toplevel.info_row -side left -fill x -expand yes
  pack $toplevel.line_label $toplevel.line_field -in $toplevel.info_row \
      -side left
  pack $toplevel.info_row -in $toplevel.file_frame -in $toplevel.pane.right \
      -side top -fill x -padx 4 -pady 4

  # Create the file_viewer and pack it below the information row
  setup_file_viewer $toplevel $toplevel.file_frame

  # Add the convenience of clicking on a function to select
  $toplevel.file_viewer tag bind matched_text <Button-1> \
      [list calltree_hyper $toplevel.hier %W %x %y]

  pack $toplevel.file_frame -in $toplevel.pane.right \
      -side top -expand yes -fill both -padx 4
  # The 4 pixels of padding allow space for the grip

  pack $toplevel.pane -in $toplevel -side top -expand yes -fill both

  # Message bar
  message $toplevel.message -textvariable status_msg -relief sunken
  bind $toplevel.message <Configure> "%W configure -width %w"
  # To clear the message, click on it
  bind $toplevel.message <Button-1> {set_message ""}

  pack $toplevel.message -in $toplevel -side bottom -fill x -padx 2

  # Convenience keys
  bind $toplevel <<Help>> "help_proc calltree"

  # Clear the cache when the window is destroyed
  wm protocol $toplevel WM_DELETE_WINDOW "calltree_destroy $toplevel"

  wm geometry $toplevel 800x600
  wm deiconify $toplevel
  wm title $toplevel "[wm title .] calltree"
  catch {raise $toplevel}
  tkwait visibility $toplevel

  calltree_select $toplevel.hier main
}

##############################################################################
#
#  Purpose    : Return the display parameters of a hierarchy node.
#
#  Parameters : see -nodelook flag of hierarchy command
#
#  Result     : see -nodelook flag of hierarchy command
#
##############################################################################

proc calltree_look {widget nodepath isopen} {
  global node_open node_closed
  if {$isopen} {
    set image $node_open
    set color DarkGreen
  } else {
    set image $node_closed
    set color DarkBlue
  }
  return [list [lindex $nodepath end] "" $image $color]
}

##############################################################################
#
#  Purpose    : Respond to a single button click in the calltree by calling
#               calltree_view_node.
#
#  Parameters : widget - the widget clicked in
#               x,y - the coordinates of the click
#
#  Result     : NONE
#
##############################################################################

proc calltree_click {widget x y} {
  set nodepath [lindex [$widget get @$x,$y] 0]
  if {[string length $nodepath] > 0} {
    calltree_view_node $widget $nodepath
  }
}

##############################################################################
#
#  Purpose    : Select a node in the calltree hierarchy and view it's node.
#
#  Parameters : widget - the widget clicked in
#               nodepath - the path to the selected node
#
#  Result     : NONE
#
##############################################################################

proc calltree_select {widget nodepath} {
  $widget select clear
  $widget select set $nodepath
  $widget see $nodepath

  calltree_view_node $widget $nodepath
}

##############################################################################
#
#  Purpose    : Act upon the selection of a calltree node.
#               Display the function in the file_viewer.
#
#  Parameters : widget - the hierarchy widget
#               nodepath - the path to the selected node
#
#  Result     : NONE
#
##############################################################################

proc calltree_view_node {widget nodepath {showing {}}} {
  global function_loc

  set root [winfo toplevel $widget]
  set_root_base $root

  set function [lindex $nodepath end]

  if [info exists function_loc($function)] {
    if {[llength $function_loc($function)] == 2} {
      set foundfile [lindex $function_loc($function) 0]
      set foundline [lindex $function_loc($function) 1]
    } else {
      # If the location of the function is out of scope, cleanup and quit
      display_clear $root
      return
    }
  } else {

    # Start the query activity indicator
    activity_start 500 $base.activity

    # Call query_execute, but make sure to handle any errors and reraise later.
    # (I wish Tcl was more like lisp sometimes)
    set failed 0
    if [catch {query_execute Symbol $function} results] {
      global errorInfo errorCode

      activity_finish 500 $base.activity

      tk_messageBox -parent $widget -type ok -title Error -message $results
      return
    }

    # Assume for now that the first entry is always the main function
    foreach line $results {
      foreach {infile infunc inline codeline} $line {}

      if {$infunc == "(null)" ||
          $infunc == "<global>" ||
          $infunc == $function} {

        # If the code contains the function followed by parentheses, 
        # this is probably it.
        # If the function returns a pointer to function, the declaration could
        # get hairy.  Anything more complicated than this will not be 
        # attempted. 
        if {[regexp -- "$function\[ \t\]*\\(" $codeline] ||
            [regexp -- "\\(\[^()\]*$function\[^()a-zA-Z0-9_\]*\\)\[ \t\]*\\(" \
                 $codeline]} {
          set foundfile $infile
          set foundline $inline

          # If there's a semicolon, or the file is a header, it's probably a 
          # declaration, so keep looking, but keep this as a guess in case 
          # nothing else is found.
          if {![string match {*.[hH]} $infile] &&
              [string first ";" $codeline] < 0} {
            break
          }
        }
      }
    }

    activity_finish 500 $base.activity

    if [info exists foundfile] {
      set function_loc($function) "$foundfile $foundline"
    } {
      display_clear $base
      set_message "Could not find definition of $function"

      # Store a null value for this function to skip future attempts
      set function_loc($function) ""
      return
    }
  }

  set_message ""

  global called_by
  if [info exists called_by($function)] {
    set highlights $called_by($function)
  } else {
    set highlights ""
  }

  display_file $root $foundfile $foundline $highlights

  focus $widget
}

##############################################################################
#
#  Purpose    : Respond to button clicks on function names in the calltree
#               file browser by selecting the corresponding function.
#
#  Parameters : treew - the hierarchy widget
#               textw - the text widget clicked in
#               x,y - the coordinates of the pointer
#
#  Result     : see -browsecmd flag of hierarchy command
#
##############################################################################

proc calltree_hyper {treew textw x y} {

  set nodepath [lindex [$treew get [$treew curselection]] 0]

  set index [$textw index @$x,$y]
  set range [$textw tag prevrange matched_text $index]
  set function [$textw get [lindex $range 0] [lindex $range 1]]

  set newpath [concat $nodepath $function]
  calltree_select $treew [concat $nodepath $function]
}

##############################################################################
#
#  Purpose    : Act upon the expansion of a node of a calltree hierarchy.
#               Return a list of functions called by the given function.
#
#  Parameters : see -browsecmd flag of hierarchy command
#
#  Result     : see -browsecmd flag of hierarchy command
#
##############################################################################

proc calltree_expand {widget nodepath} {
  global query_backend database_file called_by

  set root [winfo toplevel $widget]
  set_root_base $root

  set function [lindex $nodepath end]

  # Check the cache for the functions called by this function
  if ![info exists called_by($function)] {

    # Start the query activity indicator
    activity_start 500 $base.activity

    # Call query_execute, but make sure to handle any errors and reraise later.
    set failed 0
    if [catch {query_execute CalledBy $function} results] {
      global errorInfo errorCode

      tk_messageBox -parent $widget -type ok -title Error -message $results

      activity_finish 500 $base.activity

      return ""
    }

    # Set the callee and line number lists
    set_callees $function $results

    set numcalls [expr [llength $called_by($function)] / 2]
    set_message "Function $function makes $numcalls function calls"
  }

  # Extract the function names from the called_by structure
  foreach {func linenum} $called_by($function) {
    set dummy($func) ""
  }

  activity_finish 500 $base.activity

  return [lsort [array names dummy]]
}

##############################################################################
#
#  Purpose    : Open a dialog to prompt for the new root to the calltree.
#
#  Parameters : base - the root of the calltree dialog
#
#  Result     : NONE
#
##############################################################################

proc calltree_root_dialog {base} {

  set toplevel $base.root

  if { ![winfo exists $toplevel] } {
    toplevel $toplevel
    wm title $toplevel "Set Root"

    frame $toplevel.toprow
    pack $toplevel.toprow -side top -expand yes -fill x  -pady 5

    label $toplevel.l -text "New Root:"
    entry $toplevel.entry

    pack $toplevel.l -side left -in $toplevel.toprow -padx 5
    pack $toplevel.entry -side left -in $toplevel.toprow -expand yes -fill x

    frame $toplevel.buttonbar
    pack $toplevel.buttonbar -side top -expand yes -pady 3

    button $toplevel.ok  -text "OK" \
        -command "calltree_set_root $base \[$toplevel.entry get\]
                  wm withdraw $toplevel"
    button $toplevel.cancel -text "Cancel" \
        -command "wm withdraw $toplevel"
    global button_defaults
    if {$button_defaults} {
      $toplevel.ok configure -default active
      $toplevel.cancel configure -default normal
    }

    pack $toplevel.ok $toplevel.cancel \
        -in $toplevel.buttonbar -side left -padx 10

    bind $toplevel <Return> "$toplevel.ok invoke"
    bind $toplevel <Escape> "$toplevel.cancel invoke"

    tkwait visibility $toplevel
  } else {
    wm deiconify $toplevel
  }  
  catch {raise $toplevel}
  focus -force $toplevel.entry
}

##############################################################################
#
#  Purpose    : Set the root node of the calltree
#
#  Parameters : toplevel - the calltree toplevel window
#               function - the new root function
#
#  Result     : NONE
#
##############################################################################

proc calltree_set_root {toplevel function} {
  $toplevel.hier configure -root $function
  calltree_select $toplevel.hier $function
}

##############################################################################
#
#  Purpose    : Clean up when the calltree dialog is destroyed.
#
#  Parameters : toplevel - the dialog box being destroyed
#
#  Result     : NONE
#
##############################################################################

proc calltree_destroy {toplevel} {
  global called_by function_loc

  catch {unset called_by}; catch {unset function_loc}

  global current_file current_line
  set current_file($toplevel.file_viewer) ""
  set current_line($toplevel.file_viewer) ""

  destroy $toplevel
}

set node_open [image create photo node_open -file $sourcedir/node_open.gif]
set node_closed [image create photo node_closed -file $sourcedir/node_closed.gif]

