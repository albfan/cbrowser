#!/usr/local/bin/wish

# $Id: ftcllib.tcl,v 1.2 2000/07/30 07:38:53 cfelaco Exp $

# Ftcllib is a collection of useful procedures for Tcl/Tk programs.
# Copyright (C) 1999-2000  B. Christopher Felaco

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

# For more information about ftcllib and its author see:
#    URL:http://cbrowser.sourceforge.net/
#
# Feel free to contact me at URL:mailto:cfelaco@users.sourceforge.net
# with enhancements or suggestions.

# $Log: ftcllib.tcl,v $
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
# Revision 1.1  1999/03/15 01:06:19  chris
# Initial revision
#

# This file contains assorted Tcl/Tk utility functions.  There are various
# short convenience functions for string/list manipulation, and many Tk
# utilities:
#   - Paned windows
#   - file viewer based on edit widget (does not allow editing)
#   - general purpose edit menu
#   - text finder for file viewer
#   - goto line dialog for file viewer
#   - "magic" scrollbars, appear only when needed
#   - scrollable history in entry fields
#   - C source code syntax highlighting
#   - blinking activity light
#----------------------------------------------------------------------------

# Setup some aliases for those pesky string commands
interp alias {} strcmp {} string compare
interp alias {} strlen {} string length
interp alias {} substring {} string range

#----------------------------------------------------------------------------
# The following is from "Practical Programming in Tcl and Tk" Second Edition
# by Brent B. Welch Copyright 1997 Prentice Hall.
#
# Enhancements:
#    - When the cursor leaves the window, reversing direction will not take
#      effect until the cursor reenters the window.
#    - A new option -background has been added to control the color of the
#      separator.
#    - Uses appropriate cursor for resizing instead of crosshair.
#----------------------------------------------------------------------------

proc Pane_Create {f1 f2 args} {

  # Map optional arguments into array values
  set t(-orient) vertical
  set t(-percent) 0.5
  set t(-in) [winfo parent $f1]
  set t(-background) black
  set t(-minpercent) 0.0
  set t(-maxpercent) 1.0
  array set t $args

  # Keep state in an array associated with the master frame
  set master $t(-in)
  upvar \#0 Pane$master pane
  array set pane [array get t]

  # Create the grip and set placement attributes that
  # will not change. A thin divider line is achieved by
  # making the two frames one pixel smaller in the
  # adjustable dimension and making the main frame black.

  set pane(1) $f1
  set pane(2) $f2
  set cursor sb_[string range $pane(-orient) 0 0]_double_arrow
  set pane(grip) [frame $master.grip -background gray50 \
                      -width 10 -height 10 -bd 2 -relief raised \
                      -cursor $cursor]
  if {[string match vert* $pane(-orient)]} {
    set pane(D) Y		;# Adjust boundary in Y direction
    place $pane(1) -in $master -x 0 -rely 0.0 -anchor nw \
        -relwidth 1.0 -height -1
    place $pane(2) -in $master -x 0 -rely 1.0 -anchor sw \
        -relwidth 1.0 -height -1
    place $pane(grip) -in $master -anchor c -relx 0.9
  } else {
    set pane(D) X 		;# Adjust boundary in X direction
    place $pane(1) -in $master -relx 0.0 -y 0 -anchor nw \
        -relheight 1.0 -width -1
    place $pane(2) -in $master -relx 1.0 -y 0 -anchor ne \
        -relheight 1.0 -width -1
    place $pane(grip) -in $master -anchor c -rely 0.8
  }
  $master configure -background $pane(-background)

  set pane(toplevel) [winfo toplevel $master]

  # Set up bindings for resize, <Configure>, and
  # for dragging the grip.

  bind $master <Configure> [list PaneGeometry $master %w %h]
  bind $pane(grip) <ButtonPress-1> \
      [list PaneDrag $master %$pane(D)]
  bind $pane(grip) <B1-Motion> \
      [list PaneDrag $master %$pane(D)]
  bind $pane(grip) <ButtonRelease-1> \
      [list PaneStop $master]

  # Do the initial layout
  PaneGeometry $master
}

proc PaneDrag {master D} {
  upvar \#0 Pane$master pane

  set d [string tolower $pane(D)]
  if {! [info exists pane(base)]} {
    # Get the screen coordinate (either x or y) of the top of the pane.
    # The vroot$d is used to correct for virtual screen managers.  The
    # root$d command on the gets the screen coordinate as opposed to the
    # coordinate relative to the parent.
    set pane(base) [expr [winfo vroot$d $pane(toplevel)] + \
        [winfo root$d $master]]
  }

  set pane(-percent) [expr double($D - $pane(base)) / $pane(size)]
  if {$pane(-percent) < $pane(-minpercent)} {
    set pane(-percent) $pane(-minpercent)
  } elseif {$pane(-percent) > $pane(-maxpercent)} {
    set pane(-percent) $pane(-maxpercent)
  }

  PaneGeometry $master
}

proc PaneStop {master} {
  upvar \#0 Pane$master pane
  catch {unset pane(base)}
}

proc PaneGeometry {master {width -1} {height -1}} {

  upvar \#0 Pane$master pane
  if {[strcmp $pane(D) "X"] == 0} {
    place $pane(1) -relwidth $pane(-percent)
    place $pane(2) -relwidth [expr 1.0 - $pane(-percent)]
    place $pane(grip) -relx $pane(-percent)

    if {$width < 0} {
      set pane(size) [winfo width $master]
    } else {
      set pane(size) $width
    }
  } else {
    place $pane(1) -relheight $pane(-percent)
    place $pane(2) -relheight [expr 1.0 - $pane(-percent)]
    place $pane(grip) -rely $pane(-percent)

    if {$height < 0} {
      set pane(size) [winfo height $master]
    } else {
      set pane(size) $height
    }
  }
}

##############################################################################
#
#  Purpose    : Setup a file viewer window with scrollbars
#
#  Parameters : root - the root widget path
#               frame - the frame to pack the file_viewer in
#
#  Result     : NONE
#
##############################################################################

proc setup_file_viewer {root frame} {

  set_root_base $root

  if {[strcmp [bind Viewer] ""] == 0} {
    setup_viewer_bindings
  }

#   # Determine the width of the first tab by calculating the width of the
#   # text_mark
#   global text_mark
#   set tabwidth [expr ([image width $text_mark] + 4) / (72 * [tk scaling])]
#   puts "tabwidth = $tabwidth"
  set tabwidth 0.5

  # Create the text widget for the viewer.
  # The automagic scrollbar system is installed for this widget.
  text $base.file_viewer \
      -height 25 -width 1 -state disabled \
      -wrap none \
      -background black -foreground grey \
      -selectbackground grey -selectforeground black \
      -insertbackground green 
  #-tabs "${tabwidth}i left 1i left 1i left 1i left" 

  magic_scroll $base.file_viewer $frame

  # Replace the Text group with the Viewer group
  set tags [bindtags $base.file_viewer]
  set index [lsearch $tags Text]
  set tags [lreplace $tags $index $index Viewer]
  bindtags $base.file_viewer $tags

  global highlight_tags
  # Preconfigure the code highlight tags (avoids redraws)
  $base.file_viewer tag configure quote -foreground green3
  $base.file_viewer tag configure keyword -foreground red
  $base.file_viewer tag configure cpp -foreground khaki4
  $base.file_viewer tag configure typename -foreground orange
  $base.file_viewer tag configure comment -foreground tan
  # Make sure comment tags override any others
  $base.file_viewer tag raise comment

  # Create a tag to underline matches
  $base.file_viewer tag configure matched_text -underline yes

  # Setup the search tag
  $base.file_viewer tag configure find -foreground black -background grey

  # Setup the binding for the finder and goto
  bind $base.file_viewer <Control-s> "browser_find_dialog $base.file_viewer"
  bind $base.file_viewer <Control-r> "browser_find_dialog $base.file_viewer"
  bind $base.file_viewer <Meta-g> "browser_goto $base.file_viewer"

  # Setup the popup edit menu
  setup_edit_menu $base.file_viewer.menu $base.file_viewer
  bind $base.file_viewer <Button-3> "tk_popup $base.file_viewer.menu %X %Y"
}

##############################################################################
#
#  Purpose    : Setup the Viewer binding tag.  It is identical to the Text
#               binding tag except without insertion.
#
#  Parameters : NONE
#
#  Result     : NONE
#
##############################################################################

proc setup_viewer_bindings {} {
  # Copy the event bindings from Text to Viewer
  foreach event [bind Text] {
    bind Viewer $event [bind Text $event]
  }

  # Shut any events that can modify the text.  Hopefully this is all of them.
  # It would have been nice if all of these were already grouped into an
  # "Editor" tags group!
  foreach event { <Key> 
    <Key-Delete> <Key-BackSpace> <Control-Key-h> 
    <Meta-Key-Delete> <Meta-Key-BackSpace> <Meta-Key-d>
    <Control-Key-t> <Control-Key-o> <Control-Key-k> <Control-Key-d>
    <Key-Insert> <Key-Return> <Control-Key-i>
    <<Clear>> <<Paste>> <<Cut>> \
    <Button-2> <ButtonPress-2> <ButtonRelease-2>} {
    bind Viewer $event {}
  }


  # Make Tab act like normal widgets
  bind Viewer <Key-Tab> [bind all <Tab>]
  bind Viewer <Shift-Key-Tab> [bind all <Shift-Key-Tab>]

  # Allow use of scroll mouse in viewer
  setup_scroll_bindings Viewer
}

##############################################################################
#
#  Purpose    : Setup an Edit menu
#
#  Parameters : menu - the menu widget to create
#               viewer - the viewer widget it operates on
#
#  Result     : NONE
#
##############################################################################

proc setup_edit_menu {menu viewer} {

  # Set up the Edit menu
  menu $menu -postcommand "edit_menu_filter $menu $viewer"
  set accel [lindex [event info <<Cut>>] 0]; regsub "Key-" $accel "" accel
  $menu add command -label "Cut" -underline 1 -accel $accel \
      -command {clipboard_cut}
  set accel [lindex [event info <<Copy>>] 0]; regsub "Key-" $accel "" accel
  $menu add command -label "Copy" -underline 0 -accel $accel \
      -command {clipboard_copy}
  set accel [lindex [event info <<Paste>>] 0]; regsub "Key-" $accel "" accel
  $menu add command -label "Paste" -underline 0 -accel $accel \
      -command "set_query_selection $menu"

  $menu add separator
  $menu add command -label "Find..." -underline 0 -accel <Control-s> \
      -command "browser_find_dialog $viewer"
  $menu add command -label "Goto line..." -underline 0 \
      -accel <Meta-g> -command "browser_goto $viewer"
}

##############################################################################
#
#  Purpose    : Filter the entries in the edit menu before posting.
#
#  Parameters : menu - the menu it was invoked on
#               viewer - the viewer widget it checks
#
#  Result     : NONE
#
##############################################################################

proc edit_menu_filter {menu viewer} {
  global current_file
  if {[info exists current_file($viewer)] &&
      [strlen $current_file($viewer)] > 0} {
    $menu entryconfigure "Find..." -state normal
    $menu entryconfigure "Goto line..." -state normal
  } else {
    $menu entryconfigure "Find..." -state disabled
    $menu entryconfigure "Goto line..." -state disabled
  }

  set widget [selection own]
  if {[strlen $widget] > 0} {
    $menu entryconfigure "Copy" -state normal
    if {[strcmp [winfo class $widget] "Entry"] == 0} {
      if {[strcmp [$widget cget -state] "normal"] == 0} {
        $menu entryconfigure "Cut"  -state normal
      } else {
        $menu entryconfigure "Cut"  -state disabled
      }
    } else {
      $menu entryconfigure "Cut"  -state disabled
    }
  } else {
    $menu entryconfigure "Cut"  -state disabled
    $menu entryconfigure "Copy" -state disabled
  }
}

#----------------------------------------------------------------------------
# The following functions for automagic scrollbars deserve some comments.
# The basic idea is to leave the scrollbars unpacked until they are needed.
# Once they are needed, leave them up, even if the window is scrolled to the
# point where no lines extend out of the window.  This avoids the possibility
# of an infinite loop.  If the size of the window is increased, it is safe to
# unpack the scrollbars, because they will be repacked by the resulting
# scrollcommand if they are needed.
#----------------------------------------------------------------------------

##############################################################################
#
#  Purpose    : Install magic scrollbars around the given widget.
#
#  Parameters : widget - the widget to put scrollbars on
#               frame - the frame to pack it in
#
#  Result     : NONE
#
##############################################################################

set magic_scroll [expr [info tclversion] >= 8.0]

proc magic_scroll {widget frame} {

  global magic_scroll

  # Create the scrollbars
  scrollbar ${widget}_xscroll -orient h -takefocus 0 \
      -command [list ${widget} xview]
  scrollbar ${widget}_yscroll -orient v -takefocus 0 \
      -command [list ${widget} yview]

  # Use these commands to pack the scrollbars
  set pack_xscroll [list grid ${widget}_xscroll \
                        -in $frame -row 1 -column 0 -sticky we]
  set pack_yscroll [list grid ${widget}_yscroll \
                        -in $frame -row 0 -column 1 -sticky ns]

  # Pack the scrollable widget
  grid $widget -row 0 -column 0 -sticky nesw -in $frame
  
  if {$magic_scroll} {
    ${widget} configure \
        -xscrollcommand [list scroll_set ${widget}_xscroll $pack_xscroll] \
        -yscrollcommand [list scroll_set ${widget}_yscroll $pack_yscroll]

    # Handle automagic scrollbars removal when the widget resizes
    bind $widget <Configure> \
      "scroll_reconfigure %W %w %h ${widget}_xscroll ${widget}_yscroll"
  } else {
    ${widget} configure \
      -xscrollcommand [list ${widget}_xscroll set] \
      -yscrollcommand [list ${widget}_yscroll set]

    eval $pack_xscroll
    eval $pack_yscroll
  }

  # Configure the rows and columns
  grid rowconfigure    $frame 0 -weight 1
  grid columnconfigure $frame 0 -weight 1
  grid columnconfigure $frame 1 -weight 0
}

##############################################################################
#
#  Purpose    : Set the scrollbar to match the window.  Pack it if it is not
#               already packed.  For use as [xy]scrollcommand.
#
#  Parameters : scrollbar - the scrollbar to modify
#               geoCmd - the command to pack the scrollbar
#               offset - the offset within the window
#               size - the overall size of the window
#
#  Result     : NONE
#
##############################################################################

proc scroll_set {scrollbar geoCmd offset size} {

  set ispacked [string length [winfo manager $scrollbar]]

  if {$offset != 0.0 || $size != 1.0} {
    if {!$ispacked} {
      eval $geoCmd
    }
  }
  $scrollbar set $offset $size
}

##############################################################################
#
#  Purpose    : Remove scrollbars when widget size increases.
#
#  Parameters : widget - the widget being reconfigured
#               width - the new width
#               height - the new height
#               xscroll - the corresponding x scrollbar
#               yscroll - the corresponding y scrollbar
#
#  Result     : NONE
#
##############################################################################

proc scroll_reconfigure {widget width height xscroll yscroll} {
  global widget_width widget_height

  if {[info exists widget_width($widget)] && 
      $width > $widget_width($widget)} {

    set xview [$widget xview]
    if { [lindex $xview 0] == 0.0 && [lindex $xview 1] == 1.0} {
      unmap $xscroll
    }
  } 

  if {[info exists widget_height($widget)] && 
      $height > $widget_height($widget)} {  

    set yview [$widget yview]
    if { [lindex $yview 0] == 0.0 && [lindex $yview 1] == 1.0} {
      unmap $yscroll
    }
  }

  # Record the new width and height
  set widget_width($widget) $width
  set widget_height($widget) $height
}

###############################################################################
#
#  Purpose    : Unmap a widget using whatever geometry manager was used to
#               map it.
#
#  Parameters : widget - the widget to unmap
#
#  Result     : NONE
#
##############################################################################

proc unmap {widget} {
  set manager [winfo manager $widget]
  if {[string length $manager] > 0} {
    $manager forget $widget
  }
}


##############################################################################
#
#  Purpose    : Create a dialog box for finding text in the textwidget.
#
#  Parameters : textw - the text widget to act upon
#
#  Result     : NONE
#
##############################################################################

proc browser_find_dialog {textw} {
  global history_button find_hlist find_dialog current_file

  set toplevel $textw.finder

  if {[strcmp [$textw cget -state] "disabled"] == 0} {
    return
  }

  if { ![winfo exists $toplevel] } {
    toplevel $toplevel

    frame $toplevel.toprow
    pack $toplevel.toprow -side top -expand yes -fill x  -pady 5

    label $toplevel.l -text "Find:"
    entry $toplevel.entry -textvariable find_text($textw)
    menubutton $toplevel.recall -image $history_button \
        -menu $toplevel.recall.menu
    menu $toplevel.recall.menu
    pack $toplevel.l -side left -in $toplevel.toprow -padx 5
    pack $toplevel.entry -side left -in $toplevel.toprow -expand yes -fill x
    pack $toplevel.recall -side left -in $toplevel.toprow -padx 3

    frame $toplevel.checkrow
    pack $toplevel.checkrow -side top -pady 5

    checkbutton $toplevel.regexp -variable find_regexp($textw) -text "Regexp"
    checkbutton $toplevel.case -variable find_case($textw) -text "Case Sensitive"
    pack $toplevel.regexp $toplevel.case -side left -in $toplevel.checkrow

    frame $toplevel.buttonbar
    pack $toplevel.buttonbar -side top -expand yes -pady 3

    button $toplevel.forward  -text "Forward" -underline 0 \
        -command "browser_find $textw -forward; focus $toplevel.forward"
    button $toplevel.backward -text "Backward" -underline 0 \
        -command "browser_find $textw -backward; focus $toplevel.backward"
    button $toplevel.close    -text "Close" -underline 0 \
        -command "$textw tag remove find 1.0 end
                  wm withdraw $toplevel"

    bind $toplevel <Control-f> "$toplevel.forward invoke"
    bind $toplevel <Meta-f>    "$toplevel.forward invoke"
    bind $toplevel <Control-b> "$toplevel.backward invoke"
    bind $toplevel <Meta-b>    "$toplevel.backward invoke"
    bind $toplevel <Control-s> "$toplevel.forward invoke"
    bind $toplevel <Control-r> "$toplevel.backward invoke"
    bind $toplevel <<Cancel>>  "$toplevel.close invoke"
    bind $toplevel <Meta-c>    "$toplevel.close invoke"

    bind $toplevel.entry    <Return> "focus $toplevel.forward"
    bind $toplevel.entry    <Key-Up> \
        "field_history %W %K find_hindex find_hlist"
    bind $toplevel.entry    <Key-Down> \
        "field_history %W %K find_hindex find_hlist"
    bind $toplevel.forward  <Return> "%W invoke"
    bind $toplevel.backward <Return> "%W invoke"
    bind $toplevel.close    <Return> "%W invoke"

    # Ensure that the array elements are unset when the dialog is destroyed
    bind $toplevel          <Destroy> "browser_find_close"

    pack $toplevel.forward $toplevel.backward $toplevel.close \
        -in $toplevel.buttonbar -side left -padx 10

    # Initialize the start and end marks to the first position
    $textw mark set find_start 1.0
    $textw mark set find_end   1.0

    # Initialize the recall history if unset
    if ![info exists find_hlist] {
      set find_hlist ""
      $toplevel.recall configure -state disabled
    }

    # Track the dialog for each widget
    set find_dialog($textw) $toplevel

    # Populate the recall menu
    foreach item $find_hlist {
      $toplevel.recall.menu add command -label $item \
        -command [list set find_text($textw) $item]
    }

    wm title $toplevel "Find: $current_file($textw)"
    wm minsize $toplevel 300 110

    tkwait visibility $toplevel

    bind $toplevel <Destroy> "browser_find_close $textw"
  } else {
    wm deiconify $toplevel
  }
  catch {raise $toplevel}
  catch {focus -force $toplevel.entry}
}

##############################################################################
#
#  Purpose    : Cleanup when browser_find dialog is closed.
#
#  Parameters : toplevel - the text widget associated with the dialog being
#               destroyed
#
#  Result     : NONE
#
##############################################################################

proc browser_find_close {textw} {
  foreach array { \
    find_text find_regexp find_case find_dialog} {

    global $array
    # NOTE: the backslash is needed to suppress the array substitution.
    if [info exists $array\($textw\)] {
      unset $array\($textw\)
    }
  }
}

##############################################################################
#
#  Purpose    : Find the selected text in the given text widget.
#
#  Parameters : widget - the text widget to search in
#               direction - direction to search in
#
#  Result     : NONE
#
##############################################################################

proc browser_find {widget {direction -forward}} {
  global find_regexp find_case find_text find_hlist find_hindex find_dialog

  $widget tag remove find 1.0 end

  if {[strlen $find_text($widget)] == 0} {
    return
  }

  # Add text to history.
  if {[lsearch $find_hlist $find_text($widget)] < 0} {
    set find_hlist [concat [list $find_text($widget)] $find_hlist]
    if [info exists find_hindex] {unset find_hindex}

    foreach widget [array names find_text] {
      set finder "$widget.finder"
      if [winfo exists $finder] {
        $finder.recall.menu add command -label $find_text($widget) \
            -command [list set find_text($widget) $find_text($widget)]
        $widget.finder.recall configure -state normal
      }
    }
  }

  set args ""

  if {$find_regexp($widget)} {
    lappend args "-regexp"
  }
  if {!$find_case($widget)} {
    lappend args "-nocase"
  }

  # By default, use the current insertion point as the start of the search
  set index insert

  # If switching directions, make sure not to just find the same point
  if {[string match {-b*} $direction] && 
      [$widget compare insert == find_end]} {
     set index find_start
  } elseif {[string match {-f*} $direction] &&
            [$widget compare insert == find_start]} {
    set index find_end
  }

  set match [eval [concat $widget search $direction $args -count count -- \
                       [list $find_text($widget)] $index]]

  if {[strlen $match] > 0} {
    $widget see $match

    set match_end "$match +$count char"
    $widget tag add find $match $match_end

    # Set the new start and end marks
    $widget mark set find_start $match
    $widget mark set find_end   $match_end

    if {[string match {-b*} $direction]} {
      $widget mark set insert $match
    } elseif {[string match {-f*} $direction]} {
      $widget mark set insert $match_end
    } else {
      error "bad direction, expected -backward or -forward"
    }
  } else {
    tk_messageBox -type ok -parent $widget \
        -title "Not Found" -message "$find_text($widget) not found"
    # Reraise and focus the find dialog.
    if ![catch {raise $find_dialog($widget)}] {
      focus $find_dialog($widget).entry
    }
  }
}

##############################################################################
#
#  Purpose    : Popup a dialog box to allow the user to jump to a particular
#               line.
#
#  Parameters : textw - the text widget to act upon
#
#  Result     : NONE
#
##############################################################################

proc browser_goto {textw} {
  global goto_line

  set toplevel $textw.goto

  if {[strcmp [$textw cget -state] "disabled"] == 0} {
    return
  }

  if {![winfo exists $textw.goto]} {
    toplevel $toplevel

    frame $toplevel.top_row
    label $toplevel.label -text "Goto line:"
    entry $toplevel.entry -textvariable goto_line($textw) -width 5
    pack $toplevel.label -side left -in $toplevel.top_row
    pack $toplevel.entry -side left -fill x -in $toplevel.top_row
    pack $toplevel.top_row -side top -fill x

    scale $toplevel.slider -from 1 -variable goto_line($textw) \
        -orient horizontal -showvalue true -bigincrement 50

    pack $toplevel.slider -side top -fill x -expand yes -pady 3 -padx 3

    frame $toplevel.buttonbar
    button $toplevel.ok -text OK -width 8 \
        -command "$textw mark set insert \$goto_line($textw).1
                  $textw see insert
                  focus $textw
                  wm withdraw $toplevel"
    button $toplevel.cancel -text Cancel -width 8 \
        -command "wm withdraw $toplevel"
    pack $toplevel.ok $toplevel.cancel -side left -in $toplevel.buttonbar \
        -padx 30

    pack $toplevel.buttonbar -side top

    bind $toplevel <Return> "$toplevel.ok invoke"
    bind $toplevel <<Cancel>> "$toplevel.cancel invoke"

    wm geometry $toplevel 500x130
    wm minsize $toplevel 300 135
    wm title $toplevel "Goto Line"
  } else {
    wm deiconify $toplevel
  }

  set numlines [expr int([$textw index end])]
  set tickinterval [expr $numlines / 7]
  $toplevel.slider configure -to $numlines -tickinterval $tickinterval

  set goto_line($textw) [expr int([$textw index insert])]

  catch {raise $toplevel}
  catch {focus -force $toplevel.entry}
  # Select the line number so immediate typing will override the value.
  $toplevel.entry select range 0 end
}


##############################################################################
#
#  Purpose    : Trim a history list to a certain number of entries.
#
#  Parameters : varname - the name of the list to modify
#               maxlength - the maximum length of the list
#
#  Result     : the new value of the list
#
##############################################################################

proc history_trim {varname {maxlength 20}} {
  upvar $varname history

  set len [llength $history]
  if {$len > $maxlength} {
    set history [lrange $history [expr $len - $maxlength] end]
  }
  return $history
}

##############################################################################
#
#  Purpose    : Utility function to determine the "base" window path from the
#               root window path.  Basically, base is the prefix for other
#               widgets, while root can stand alone.
#
#  Parameters : root - the root window parameter
#
#  Result     : NONE
#
##############################################################################

proc set_root_base {root} {
  # This treats "." as a special case
  if {[strcmp $root "."] == 0} {
    uplevel [list set base {}]
  } else {
    uplevel [list set base $root]
  }
}

##############################################################################
#
#  Purpose    : Unconditionally execute a block of code after another even if
#               the first returns an error.
#
#  Parameters : bodyform - the main text to evaluate
#               unwindform - the form to evaluate when the body returns
#               resultvar - variable to store the result in
#
#  Result     : return value of bodyform
#
##############################################################################

proc unwind_protect {bodyform unwindform {resultvar ""}} {

  set failed 0
  if [catch {uplevel $bodyform} results] {
    global errorInfo errorCode
    set failed 1
    set info $errorInfo
    set code $errorCode
    set message $results
  }

  uplevel $unwindform

  if {$failed} {
    error $message $info $code
  } else {
    if {[strlen $resultvar] > 0} {
      upvar $resultvar var
      set var $results
    }
    return $results
  }
}


##############################################################################
#
#  Purpose    : Set the status message.
#
#  Parameters : msg - the message to set
#
#  Result     : NONE
#
##############################################################################

proc set_message {msg} {
  global status_msg
  set status_msg $msg
}

##############################################################################
#
#  Purpose    : Clear the file viewer in the given window.
#
#  Parameters : root - the root of the window to operate on
#
#  Result     : NONE
#
##############################################################################

proc display_clear {root} {
  global current_file current_line

  set_root_base $root

  set textw $base.file_viewer

  set current_file($textw) ""
  set current_line($textw) ""

  # Clear the display viewer and disable it
  $textw delete 1.0 end
  $textw configure -state disabled

  # If we're doing magic scrollbars, unmap them
  global magic_scroll
  if {$magic_scroll} {
    # Unmap the scrollbars if they are mapped
    unmap $base.file_viewer_xscroll
    unmap $base.file_viewer_yscroll
  }

  set current_results ""
}

##############################################################################
#
#  Purpose    : Create a dialog box for finding text in the textwidget.
#
#  Parameters : textw - the text widget to act upon
#
#  Result     : NONE
#
##############################################################################

proc browser_find_dialog {textw} {
  global history_button find_hlist find_dialog current_file

  set toplevel $textw.finder

  if {[strcmp [$textw cget -state] "disabled"] == 0} {
    return
  }

  if { ![winfo exists $toplevel] } {
    toplevel $toplevel

    frame $toplevel.toprow
    pack $toplevel.toprow -side top -expand yes -fill x  -pady 5

    label $toplevel.l -text "Find:"
    entry $toplevel.entry -textvariable find_text($textw)
    menubutton $toplevel.recall -image $history_button \
        -menu $toplevel.recall.menu
    menu $toplevel.recall.menu
    pack $toplevel.l -side left -in $toplevel.toprow -padx 5
    pack $toplevel.entry -side left -in $toplevel.toprow -expand yes -fill x
    pack $toplevel.recall -side left -in $toplevel.toprow -padx 3

    frame $toplevel.checkrow
    pack $toplevel.checkrow -side top -pady 5

    checkbutton $toplevel.regexp -variable find_regexp($textw) -text "Regexp"
    checkbutton $toplevel.case -variable find_case($textw) -text "Case Sensitive"
    pack $toplevel.regexp $toplevel.case -side left -in $toplevel.checkrow

    frame $toplevel.buttonbar
    pack $toplevel.buttonbar -side top -expand yes -pady 3

    button $toplevel.forward  -text "Forward" -underline 0 \
        -command "browser_find $textw -forward; focus $toplevel.forward"
    button $toplevel.backward -text "Backward" -underline 0 \
        -command "browser_find $textw -backward; focus $toplevel.backward"
    button $toplevel.close    -text "Close" -underline 0 \
        -command "$textw tag remove find 1.0 end
                  wm withdraw $toplevel"

    bind $toplevel <Control-f> "$toplevel.forward invoke"
    bind $toplevel <Meta-f>    "$toplevel.forward invoke"
    bind $toplevel <Control-b> "$toplevel.backward invoke"
    bind $toplevel <Meta-b>    "$toplevel.backward invoke"
    bind $toplevel <Control-s> "$toplevel.forward invoke"
    bind $toplevel <Control-r> "$toplevel.backward invoke"
    bind $toplevel <<Cancel>>  "$toplevel.close invoke"
    bind $toplevel <Meta-c>    "$toplevel.close invoke"

    bind $toplevel.entry    <Return> "focus $toplevel.forward"
    bind $toplevel.entry    <Key-Up> \
        "field_history %W %K find_hindex find_hlist"
    bind $toplevel.entry    <Key-Down> \
        "field_history %W %K find_hindex find_hlist"
    bind $toplevel.forward  <Return> "%W invoke"
    bind $toplevel.backward <Return> "%W invoke"
    bind $toplevel.close    <Return> "%W invoke"

    # Ensure that the array elements are unset when the dialog is destroyed
    bind $toplevel          <Destroy> "browser_find_close"

    pack $toplevel.forward $toplevel.backward $toplevel.close \
        -in $toplevel.buttonbar -side left -padx 10

    # Initialize the start and end marks to the first position
    $textw mark set find_start 1.0
    $textw mark set find_end   1.0

    # Initialize the recall history if unset
    if ![info exists find_hlist] {
      set find_hlist ""
      $toplevel.recall configure -state disabled
    }

    # Track the dialog for each widget
    set find_dialog($textw) $toplevel

    # Populate the recall menu
    foreach item $find_hlist {
      $toplevel.recall.menu add command -label $item \
        -command [list set find_text($textw) $item]
    }

    wm title $toplevel "Find: $current_file($textw)"
    wm minsize $toplevel 300 110

    tkwait visibility $toplevel

    bind $toplevel <Destroy> "browser_find_close $textw"
  } else {
    wm deiconify $toplevel
  }
  catch {raise $toplevel}
  catch {focus -force $toplevel.entry}
}

##############################################################################
#
#  Purpose    : Cleanup when browser_find dialog is closed.
#
#  Parameters : toplevel - the text widget associated with the dialog being
#               destroyed
#
#  Result     : NONE
#
##############################################################################

proc browser_find_close {textw} {
  foreach array { \
    find_text find_regexp find_case find_dialog} {

    global $array
    # NOTE: the backslash is needed to suppress the array substitution.
    if [info exists $array\($textw\)] {
      unset $array\($textw\)
    }
  }
}

##############################################################################
#
#  Purpose    : Find the selected text in the given text widget.
#
#  Parameters : widget - the text widget to search in
#               direction - direction to search in
#
#  Result     : NONE
#
##############################################################################

proc browser_find {widget {direction -forward}} {
  global find_regexp find_case find_text find_hlist find_hindex find_dialog

  $widget tag remove find 1.0 end

  if {[strlen $find_text($widget)] == 0} {
    return
  }

  # Add text to history.
  if {[lsearch $find_hlist $find_text($widget)] < 0} {
    set find_hlist [concat [list $find_text($widget)] $find_hlist]
    if [info exists find_hindex] {unset find_hindex}

    foreach widget [array names find_text] {
      set finder "$widget.finder"
      if [winfo exists $finder] {
        $finder.recall.menu add command -label $find_text($widget) \
            -command [list set find_text($widget) $find_text($widget)]
        $widget.finder.recall configure -state normal
      }
    }
  }

  set args ""

  if {$find_regexp($widget)} {
    lappend args "-regexp"
  }
  if {!$find_case($widget)} {
    lappend args "-nocase"
  }

  # By default, use the current insertion point as the start of the search
  set index insert

  # If switching directions, make sure not to just find the same point
  if {[string match {-b*} $direction] && 
      [$widget compare insert == find_end]} {
     set index find_start
  } elseif {[string match {-f*} $direction] &&
            [$widget compare insert == find_start]} {
    set index find_end
  }

  set match [eval [concat $widget search $direction $args -count count -- \
                       [list $find_text($widget)] $index]]

  if {[strlen $match] > 0} {
    $widget see $match

    set match_end "$match +$count char"
    $widget tag add find $match $match_end

    # Set the new start and end marks
    $widget mark set find_start $match
    $widget mark set find_end   $match_end

    if {[string match {-b*} $direction]} {
      $widget mark set insert $match
    } elseif {[string match {-f*} $direction]} {
      $widget mark set insert $match_end
    } else {
      error "bad direction, expected -backward or -forward"
    }
  } else {
    tk_messageBox -type ok -parent $widget \
        -title "Not Found" -message "$find_text($widget) not found"
    # Reraise and focus the find dialog.
    if ![catch {raise $find_dialog($widget)}] {
      focus $find_dialog($widget).entry
    }
  }
}

# *** Thanks to Brian Meifert for the original basis of this highlighting code.
# *** It has been completely rewritten to use features of Tk7.6 and for improved
# *** support of C++.

##############################################################################
#
#  Purpose    : Highlight C/C++ code to clarify syntax.
#
#  Parameters : widget - the widget to be acted upon
#
#  Result     : NONE
#
##############################################################################

set highlight_tags {
  comment keyword typename cpp quote
}

set c_keywords "if while for return else typedef struct const static enum
switch case break default extern class inline protected private public virtual
operator using namespace template throw try catch sizeof"

set c_typenames "void char int float double long short unsigned signed wchar_t
bool"

proc c_highlights { widget } {

  # Clear all highlight tags if the code_highlight option is off.
  global code_highlight
  if { ! $code_highlight } {
    global highlight_tags
    foreach tag $highlight_tags {
      $widget tag remove $tag 1.0 end
    }
    return
  }

  # Highlight comments and strings all at once
  c_syntax_highlight $widget
  update idletasks

  global keyword_highlight c_keywords_regexp c_typenames_regexp
  if ![info exists c_keywords_regexp] {
    set c_keywords_regexp "namespace|using|operator|virtual|p(r(ivate|otected)|ublic)|default|break|c(onst|lass|a(tch|se))|s(t(atic|ruct)|witch|izeof)|t(ypedef|emplate|hrow|ry)|e(lse|num|xtern)|return|for|while|i(f|nline)"

    set c_typenames_regexp "void|char|int|float|double|long|short|(un)?signed|wchar_t|bool"
  }

  if {$keyword_highlight} {
    # Highlight C keywords
    highlight_word $c_keywords_regexp keyword $widget
    update idletasks

    highlight_word $c_typenames_regexp typename $widget
    update idletasks

    cpp_highlight $widget
    update idletasks
  } else {
    foreach tag {keyword typename cpp} {
      $widget tag remove $tag 1.0 end
    }
  }
}

##############################################################################
#
#  Purpose    : Trace modifications to the code_highlight and
#               keyword_highlight toggles to ensure the displays are
#               consistent with the settings
#
#  Parameters : variable - the variable to trace
#               index - the index if variable is an array
#               op - the operation performed
#
#  Result     : NONE
#
##############################################################################

proc highlight_trace {variable index op} {
  global current_file current_line
  foreach textw [array names current_file] {
    if {[strlen $current_file($textw)] > 0} {
      c_highlights $textw
    }
  }
}

##############################################################################
#
#  Purpose    : Highlight C/C++ syntactic elements - comments and quotes.
#
#  Parameters : widget - the widget to be acted upon
#
#  Result     : NONE
#
##############################################################################

proc c_syntax_highlight {widget} {
  # Search for both style comments, and a quote that is not within a character
  # constant.
  set start_pattern {/\*|//|[^\\']\"}
  set tagtype "comment"

  set temp [$widget search -regexp $start_pattern 1.0]
  while { [strlen $temp] > 0 &&
          [$widget compare $temp < end] } {
    set match [$widget get $temp "$temp + 2chars"]

    if {[strcmp $match "//"] == 0} {
      set endrange [$widget index "$temp lineend"]
    } elseif {[strcmp $match "/*"] == 0} {
      set tagtype "comment"
      # Start searching for the end comment after the start of the comment
      set endrange [$widget search -regexp {\*/} "$temp + 2chars"]
      if {[strlen $endrange] == 0} {
        break
      } else {
        # Make the range include the end comment mark
        set endrange "$endrange + 2chars"
      }
    } else { # Must be a quote...
      set tagtype "quote"
      # The search for the quote will stop at the character preceding the quote
      # character, because the search pattern excludes character constants.
      set temp "$temp + 1chars"
      # By starting at the first quote, this will automatically handle the
      # empty string case.
      set endrange [$widget search -regexp -- {[^\\]\"} "$temp"]
      if {[strlen $endrange] == 0} {
        break
      } else {
        set endrange "$endrange + 2chars"
      }
    }
    if {[strlen $endrange] != 0} {
      $widget tag add $tagtype $temp "$endrange"
      set temp [$widget search -regexp $start_pattern "$endrange + 1chars" end]
    } else {
      break
    }
  }
  
}

##############################################################################
#
#  Purpose    : Highlight C/C++ style comments.
#
#  Parameters : widget - the widget to be acted upon
#
#  Result     : NONE
#
##############################################################################

proc comment_highlight {widget} {

  set temp [$widget search -regexp {/\*|//} 1.0]
  while { [strlen $temp] > 0 &&
          [$widget compare $temp < end] } {
    set match [$widget get $temp "$temp + 2chars"]
    if {[strcmp $match "//"] == 0} {
      set endcomment [$widget index "$temp lineend"]
    } else {
      set endcomment [$widget search -regexp {\*/} "$temp + 2chars"]
      if {[strlen $endcomment] == 0} {
        break
      } else {
        set endcomment "$endcomment + 2chars"
      }
    }
    if {[strlen $endcomment] != 0} {
      $widget tag add comment $temp "$endcomment"
      set temp [$widget search -regexp {/\*|//} "$endcomment" end]
    } else {
      break
    }
  }
}

##############################################################################
#
#  Purpose    : To highlight double quoted phrases
#
#  Parameters : widget - the widget to be acted upon
#
#  Result     : NONE
#
#  Note       : This function attempted to skirt around comment regions, so
#               as not to double highlight things.  However, it would fail if
#               a comment start appeared within a string.  The only correct
#               way to handle these cases is to perform comment and string
#               parsing in a single pass, which is now done in the function
#               c_syntax_highlight.
#
##############################################################################

proc quote_highlight { widget } {

  set pattern {[^\\']\"}

  # Look in between commented regions for quotes
  foreach {start end} [concat 1.0 [$widget tag ranges comment] end] {
    puts "start: $start, end: $end"

    while {[set temp [$widget search -regexp -- $pattern $start $end]] != ""} {
      puts "temp: $temp"

      set endquote [$widget search -regexp -- {[^\\]\"} "$temp + 1chars" $end]
      puts "endquote: $temp"

      if {[strlen $endquote] > 0} {
        set start [$widget index "$endquote + 2chars"]

        $widget tag add quote "$temp + 1chars" $start
      } else {
        # If there's no endquote, something's wrong.  Skip this region.
        break
      }
    }
  }
}

##############################################################################
#
#  Purpose    : To highlight the given words in the given text widget
#
#  Parameters : word - regexp pattern to search for
#               color - the text tag to mark the word with
#               widget - the text widget to act on
#
#  Result     : NONE
#
##############################################################################

proc highlight_word {word color widget {range_start 1.0} {range_end end}} {

  set start $range_start
  set end $range_end

  # Safety check, otherwise infinite loop could result...
  if {[strlen $word] <= 0} {
    error "Can't highlight blank pattern"
  }

  while {[$widget compare $start < $end] &&
         [set temp [$widget search -count count -regexp -- \
                        $word $start $end]] != ""} {

    # Check if in a comment
    set crange [$widget tag prevrange comment $temp]
    if {[llength $crange] == 2 &&
        [$widget compare $temp < [lindex $crange 1]]} {
      set start [lindex $crange 1]
      continue
    }

    # Check if in quotes
    set qrange [$widget tag prevrange quote $temp]
    if {[llength $qrange] == 2 &&
        [$widget compare $temp < [lindex $qrange 1]]} {
      set start [lindex $qrange 1]
      continue
    }

    # Make sure that a whole word was found.
    if {[$widget compare [$widget index "$temp wordstart"] == "$temp"] &&
        [$widget compare [$widget index "$temp wordend"] == \
             [$widget index "$temp + ${count}chars"]]} {

      $widget tag add "$color" $temp "$temp + ${count}chars"
    }

    set start [$widget index "$temp +  ${count}chars"]
  }

  return

  # Look in between commented regions for words to mark
  foreach {start end} [concat [list $range_start] \
                           [$widget tag ranges comment] \
                           [list $range_end]] {
    while {[set temp [$widget search -count count -regexp -- \
                          "$word" $start $end]] != ""} {

      # Make sure that a whole word was found.
      if {[$widget compare [$widget index "$temp wordstart"] == "$temp"] &&
          [$widget compare [$widget index "$temp wordend"] == \
               [$widget index "$temp + ${count}chars"]]} {

        $widget tag add "$color" $temp "$temp + ${count}chars"
      }
      set start [$widget index "$temp +  ${count}chars"]
    }
  }

  return
}

##############################################################################
#
#  Purpose    : To highlight preprocessor statements
#
#  Parameters : widget - the widget to be acted upon
#
#  Result     : NONE
#
##############################################################################

proc cpp_highlight {widget} {
  puts ">>> cpp_highlight"
  set pattern \
      [subst -nocommands -novariables \
           {^[ \t\n]*\#[ \t]*(ifdef|ifndef|if|define|undef|include|endif|else)}]

  # Look in between commented regions for words to mark
  foreach {start end} [concat 1.0 [$widget tag ranges comment] end] {

    while {[$widget compare $start < $end]} {

      set temp [$widget search -count count -regexp -- $pattern $start $end]
      if {[strlen $temp] > 0} {
        $widget tag add cpp $temp "$temp +${count} chars"
      }
      set start [$widget index "$start lineend +1 char"]
    }
  }
  puts "<<< cpp_highlight"
  return
}

#----------------------------------------------------------------------------

##############################################################################
#
#  Purpose    : Utility function to be used as a trace.
#               The simple boolean variable controls the state of
#               the given entry in the given menu,
#
#  Parameters : menu - the menu widget where the toggle lives
#               entry - the label of the toggle widget
#               varname - the name of the toggle controlling variable
#               index - required parameter when used as trace, ignored
#               op -  required parameter when used as trace, ignored
#
#  Result     : NONE
#
#############################################################################

proc toggle_trace {menu entry varname index op} {
  upvar $varname var
  if {$var} {
    $menu entryconfigure $entry -state normal
  } else {
    $menu entryconfigure $entry -state disabled
  }
}

##############################################################################
#
#  Purpose    : Allow only a single tearoff for a menu.
#               Use as -tearoffcommand
#
#  Parameters : menu - the menu being tornoff
#               torn - the new tearoff menu
#
#  Result     : NONE
#
##############################################################################

proc single_tearoff {menu torn} {
  global tearoff

  # Record the correspondence with the tearoff
  set tearoff($menu) $torn

  # Disable future tearoffs
  $menu configure -tearoff 0

  # Restore when the tearoff is deleted
  bind $torn <Destroy> \
      "$menu configure -tearoff 1;
       unset tearoff($menu)"
}

##############################################################################
#
#  Purpose    : Add an item to a field's scrollable history
#
#  Parameters : field - the entry field to scroll
#               direction - Up or Down
#               indexvar - the index of the current value
#               hlistvar - the history list variable
#
#  Result     : NONE
#
##############################################################################

proc field_history_add {hlistvar indexvar value} {
  upvar \#0 $indexvar hindex
  upvar \#0 $hlistvar hlist

  # Reset the history index mechanism, and add this to the list
  if [info exists hindex] {unset hindex}

  if [info exists hlist] {
    # If already in the list, remove it
    set index [lsearch $hlist $value]
    if {$index >= 0} {
      set hlist [lreplace $hlist $index $index]
    }
    set hlist [concat [list $value] $hlist]
  } else {
    set hlist $value
  }
}

##############################################################################
#
#  Purpose    : Scroll through field history with keyboard
#
#  Parameters : field - the entry field to scroll
#               direction - Up or Down
#               indexvar - the index of the current value
#               hlistvar - the history list variable
#
#  Result     : NONE
#
##############################################################################

proc field_history {field direction indexvar hlistvar} {
  upvar \#0 $indexvar hindex
  upvar \#0 $hlistvar hlist

  # If there's no history list, just return
  if ![info exists hlist] {
    return
  }

  set last [expr [llength $hlist] - 1]

  if [info exists hindex] {
    switch -- $direction {
      "Up" {incr hindex}
      "Down" {incr hindex -1}
    }
    if {$hindex > $last} {
      set hindex $last
    }
    if {$hindex < 0} {
      set hindex 0
    }
  } else {
    set hindex 0
  }

  # Set the fields value
  $field delete 0 end
  $field insert 0 [lindex $hlist $hindex]
}

##############################################################################
#
#  Purpose    : Blink an activity indicator every interval.
#
#  Parameters : time - the interval to blink at
#               light - the widget to use as the blinking light
#
#  Result     : NONE
#
##############################################################################

proc activity_start {time light} {
  if {[strcmp [$light cget -background] "grey80"] == 0} {
    $light configure -background green
  } else {
    $light configure -background grey80
  }
  update idletasks

  after $time "activity_start $time $light"
}

##############################################################################
#
#  Purpose    : Stop the blinking activity indicator when the 
#
#  Parameters : time - the interval to blink at
#               light - the widget to use as the blinking light
#
#  Result     : NONE
#
##############################################################################

proc activity_finish {time light} {
  after cancel "activity_start $time $light"
  $light configure -background grey80
}

##############################################################################
#
#  Purpose    : Setup the event bindings for a scroll mouse.
#
#  Parameters : bindtag - the bindtag to setup
#
#  Result     : NONE
#
##############################################################################

proc setup_scroll_bindings {bindtag} {
    bind $bindtag <Button-5> [list %W yview scroll 5 units]
    bind $bindtag <Button-4> [list %W yview scroll -5 units]
    bind $bindtag <Shift-Button-5> [list %W yview scroll 1 units]
    bind $bindtag <Shift-Button-4> [list %W yview scroll -1 units]
    bind $bindtag <Control-Button-5> [list %W yview scroll 1 pages]
    bind $bindtag <Control-Button-4> [list %W yview scroll -1 pages]
}

