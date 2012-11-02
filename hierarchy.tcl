##
## hierarchy.tcl
## Hierarchical Display Widget
##
## Layout routines taken from oooold code, author unkown.
## Copyright 1995-1997 Jeffrey Hobbs
##
## jhobbs@cs.uoregon.edu, http://www.cs.uoregon.edu/~jhobbs/
##
## source standard_disclaimer.tcl
## source beer_ware.tcl
##
## Last Update: 28 June 1997

##-----------------------------------------------------------------------
## PROCEDURE(S)
##	hierarchy, hierarchy_dir, hierarchy_widget
##
## ARGUMENTS && DESCRIPTION
##
## hierarchy <window pathname> <options>
##	Implements a hierarchical listbox
## hierarchy_dir <window pathname> <options>
##	Implements a hierarchical listbox using a directory view structure
##	for the default methods
## hierarchy_widget <window pathname> <options>
##	Implements a hierarchical listbox using a widget view structure
##	for the default methods
##
## OPTIONS
##	(Any canvas option may be used with a hierarchy)
##
## -autoscrollbar TCL_BOOLEAN			DEFAULT: 1
##	Determines whether scrollbars automagically pop-up or
##	are permanently there.
##
## -browsecmd procedure				DEFAULT: noop
##	A command which the widget will execute when the node is expanded
##	to retrieve the children of a node.  The widget and node path are
##	appended to the command as a list of node names which
##	form a path to the node from the root.  Thus the first
##	element of this list will always be the root node.
##
## -command procedure				DEFAULT: noop
##	A command which the widget will execute when the node is toggled.
##	The name of the widget, the node path, and whether the children of
##	the node are showing (0/1) is appended to the procedure args.
##
## -common TCL_BOOLEAN				DEFAULT: 0
##	If this is true, when a node gets added to the selection,
##	all other nodes with the same *name* (regardless of
##	the path to the node) get selected as well.  The selection
##	is reported only as a set of node names, not a set of node 
##	paths.  Thus selection acts like an equivalence over nodes
##	of the same name.  Useful only in hierarchies where some
##	nodes in the hierarchy really refer to the same logical object.
##	NOTE: FEATURE NOT YET IMPLEMENTED
##
## -decoration TCL_BOOLEAN			DEFAULT: 1
##	If this is true, the "tree" lines are drawn.
##
## -expand #					DEFAULT: 1
##	an integer value for an initial depth to expand to.
##
## -font fontname				DEFAULT: fixed
##	The default font used for the text.
##
## -foreground color				DEFAULT: black
##	The default foreground color used for text of unselected nodes.
##
## -ipad #					DEFAULT: 3
##	The internal space added between the image and the text for a
##	given node.
##
## -nodelook procedure				DEFAULT: noop
##	A command the widget will execute to get the look of a node.
##	The node is appended to the command as a list of
##	node-names which form a path to the node from the root.
##	Thus the first element of this list will always be the
##	root node.  Also appended is a 
##	boolean value which indicates whether the node's children
##	are currently displayed.  This allows the node's
##	look to change if it is "opened" or "closed".
##
##	This command must return a 4-tuple list containing:
##		0. the text to display at the node
##		1. the font to use for the text
##		2. an image to display
##		3. the foreground color to use for the node
##	If no font (ie. {}) is specified then
##	the value from -font is used.  If no image is specified
##	then no image is displayed.
##	The default is a command to which produces a nice look
##	for a file manager.  
##
## -paddepth #					DEFAULT: 12
##	The indent space added for child branches.
##
## -padstack #					DEFAULT: 2
##	The space added between two rows
##
## -root rootname				DEFAULT: {}
##  	The name of the root node of the tree.  Each node
##	name must be unique amongst the children of each node.
##
## -selectbackground color			DEFAULT: red
##	The default background color used for the text of selected nodes.
##
## -selectmode (single|browse|multiple)		DEFAULT: browse
##	Like listbox modes, "multiple" is a mix of multiple && extended.
##
## -showall TCL_BOOLEAN				DEFAULT: 0
##	For directory nodelook, also show Unix '.' (hidden) files/dirs.
##
## -showfiles TCL_BOOLEAN			DEFAULT: 0
##	Show files as well as directories.
##
## -showparent string				DEFAULT: {}
##	For hierarchy_dir nodelook, if string != {}, then it will show that
##	string which will reset the root node to its parent.
##
## RETURNS: The window pathname
##
## METHODS
##	These are the methods that the hierachical listbox object recognizes.
##	(ie - hierachy .h ; .h <method> <args>)
##	Any unique substring is acceptable
##
## configure ?option? ?value option value ...?
## cget option
##	Standard tk widget routines.
##
## close index
##	Closes the specified index (will trigger -command).
##
## curselection
##	Returns the indices of the selected items.  This differs from the
##	listbox method because indices here have no implied order.
##
## get index ?index ...?
##	Returns the node paths of the items referenced.  Ranges are not
##	allowed.  Index specification is like that allowed by the index
##	method.
##
## qget index ?index ...?
##	As above, but the indices must be that of the item (as returned
##	by the index or curselection method).
##
## index index
##	Returns the hierarchy numerical index of the item (the numerical
##	index has no implied order relative to the list items).  index
##	may be of the form:
##
##	number - Specifies the element as a numerical index.
##	root   - specifies the root item.
##	string - Specifis an item that has that text in it's node.
##	@x,y   - Indicates the element that covers the point in
##		the listbox window specified by x and y (in pixel
##		coordinates).  If no element covers that point,
##		then the closest element to that point is used.
##
## open index
##	Opens the specified index (will trigger -command).
##
## see index
##	Ensures that the item specified by the index is viewable.
##
## selection option arg
##	This works like the listbox selection method with the following
##	exceptions:
##
##	The selection clear option can take multiple indices, but not a range.
##	No arguments to clear means clear all the selected elements.
##
##	The selection set option can take multiple indices, but not a range.
##	The key word 'all' sets the selection for all elements.
##
## size
##	Returns the number of items in the hierarchical listbox.
##
## toggle index
##	Toggles (open or closed) the item specified by index
##	(triggers -command).
##
## BINDINGS
##	Most Button-1 bindings on the hierarchy work in the same manner
##	as those for the listbox widget, as defined by the selectmode.
##	Those that vary are listed below:
##
## <Double-Button-1>
##	Toggles a node in the hierarchy
##
## NAMESPACE & STATE
##	The megawidget creates a global array with the classname, and a
## global array which is the name of each megawidget created.  The latter
## array is deleted when the megawidget is destroyed.
##	The procedure hierarchy and those beginning with Hierarchy are
## used.  Also, when a widget is created, commands named .$widgetname
## and Hierarchy$widgetname are created.
##
## Cryptic source code arguments explained:
## np   == node path
## cnp  == changed np
## knp  == kids np
## xcnp == extra cnp
##-----------------------------------------------------------------------

source "widget.tcl"

#package require Widget 1.0
package provide Hierarchy 1.11

## Create the Hiearchy megawidget class definition
##
## In general, we cannot use $data(basecmd) in the construction, but the
## scrollbar commands won't be called until after it really exists as a
## proper command
array set Hierarchy {
    type		frame
    base		{canvas canvas canvas {-relief sunken -bd 1 \
	    -highlightthickness 1 \
	    -yscrollcommand [list $data(yscrollbar) set] \
	    -xscrollcommand [list $data(xscrollbar) set]}}
    components		{
	{scrollbar xscrollbar sx {-orient h -bd 1 -highlightthickness 1\
		-command [list $data(basecmd) xview]}}
	{scrollbar yscrollbar sy {-orient v -bd 1 -highlightthickness 1\
		-command [list $data(basecmd) yview]}}
    }

    -autoscrollbar	{autoScrollbar	AutoScrollbar	1}
    -browsecmd		{browseCmd	BrowseCmd	{}}
    -command		{command	Command		{}}
    -decoration		{decoration	Decoration	1}
    -expand		{expand		Expand		1}
    -font		{font		Font		fixed}
    -foreground		{foreground	Foreground	black}
    -ipad		{ipad		Ipad		3}
    -nodelook		{nodeLook	NodeLook	{}}
    -paddepth		{padDepth	PadDepth	12}
    -padstack		{padStack	PadStack	2}
    -root		{root		Root		{}}
    -selectmode		{selectMode	SelectMode	browse}
    -selectbackground	{selectBackground SelectBackground red}
    -state		{state		State		normal}

    -showall		{showAll	ShowAll		0}
    -showparent		{showParent	ShowParent	{}}
    -showfiles		{showFiles	ShowFiles	0}
}
# Create this to make sure there are registered in auto_mkindex
# these must come before the [widget create ...]
proc Hierarchy args {}
proc hierarchy args {}
widget create Hierarchy

proc hierarchy_dir {w args} {
    uplevel hierarchy $w -root [list [pwd]] \
	    -nodelook	Hierarchy:FileLook \
	    -command	Hierarchy:FileActivate \
	    -browsecmd	Hierarchy:FileList \
	    $args
}

proc hierarchy_widget {w args} {
    uplevel hierarchy $w -root . \
	    -nodelook	Hierarchy:WidgetLook \
	    -command	Hierarchy:WidgetActivate \
	    -browsecmd	Hierarchy:WidgetList \
	    $args
}

;proc Hierarchy:construct { w } {
    upvar \#0 $w data

    ## Private variables
    array set data [list \
	    hasnodelook	0 \
	    halfpstk	[expr $data(-padstack)/2] \
	    width	400 \
	    ]

    grid $data(canvas) $data(yscrollbar) -sticky news
    grid $data(xscrollbar) -sticky ew
    grid columnconfig $w 0 -weight 1
    grid rowconfig $w 0 -weight 1
    bind $data(canvas) <Configure> [list Hierarchy:Resize $w %w %h]
}

;proc Hierarchy:init { w } {
    upvar \#0 $w data

    set data(:$data(-root),showkids) 0
    Hierarchy:ExpandNodeN $w $data(-root) $data(-expand)
    if {[catch {$w see $data(-root)}]} {
	$data(basecmd) configure -scrollregion "0 0 1 1"
    }
}

;proc Hierarchy:configure {w args} {
    upvar \#0 $w data

    set truth {^(1|yes|true|on)$}
    set resize 0
    foreach {key val} $args {
	switch -- $key {
	    -autoscrollbar {
		set val [regexp -nocase $truth $val]
		if {$val} {
		    set resize 1
		} else {
		    grid $data(xscrollbar)
		    grid $data(yscrollbar)
		}
	    }
	    -decoration	{ set val [regexp -nocase $truth $val] }
	    -padstack	{ set data(halfpstk) [expr $val/2] }
	    -nodelook	{
		## We set this special bool val because it saves some
		## computation in ExpandNode, a deeply nested proc
		set data(hasnodelook) [string compare $val {}]
	    }
	    -root		{
		if {[info exists data(:$data(-root),showkids)]} {
		    ## All data about items and selection should be
		    ## cleared and the items deleted
		    foreach name [concat [array names data :*] \
			    [array names data S,*]] {unset data($name)}
		    $data(basecmd) delete all
		    set data(-root) $val
		    set data(:$val,showkids) 0
		    Hierarchy:ExpandNodeN $w $val $data(-expand)
		    ## We can reset resize because ExpandNodeN forces it
		    set resize 0
		    continue
		}
	    }
	    -selectbackground {
		foreach i [array names data S,*] {
		    $data(basecmd) itemconfigure [string range $i 2 end] \
			    -fill $val
		}
	    }
	    -state	{
		if {![regexp {^(normal|disabled)$} $val junk val]} {
		    return -code error "bad state value \"$val\":\
			    must be normal or disabled"
		}
	    }
	    -showall	-
	    -showfiles	{
		set val [regexp -nocase $truth $val]
		if {$val == $data($key)} continue
		if {[info exists data(:$data(-root),showkids)]} {
		    foreach i [array names data :*,kids] { unset data($i) }
		    foreach i [array names data :*,showkids] {
			## FIX - this doesn't really work dynamically
			if {$data($i)} {
			    ## probably requires a call to Hierarchy:Redraw
			}
		    }
		}
	    }
	}
	set data($key) $val
    }
    if {$resize} {
	Hierarchy:Resize $w [winfo width $data(canvas)] \
		[winfo height $data(canvas)]
    }
}

;proc Hierarchy_index { w idx } {
    upvar \#0 $w data
    set c $data(basecmd)
    if {[string match all $idx]} {
	return [$c find withtag box]
    } elseif {[regexp {^(root|anchor)$} $idx]} {
	return [$c find withtag box:$data(-root)]
    }
    foreach i [$c find withtag $idx] {
	if {[string match rec* [$c type $i]]} { return $i }
    }
    if {[regexp {@(-?[0-9]+),(-?[0-9]+)} $idx z x y]} {
	return [$c find closest [$w canvasx $x] [$w canvasy $y] 1 text]
    }
    foreach i [$c find withtag box:[lindex $idx 0]] { return $i }
    return -code error "bad hierarchy index \"$idx\":\
	    must be current, @x,y, a number, or a node name"
}

;proc Hierarchy_selection { w args } {
    if {[string match {} $args]} {
	return -code error \
		"wrong \# args: should be \"$w selection option args\""
    }
    upvar \#0 $w data
    set err [catch {Hierarchy_index $w [lindex $args 1]} idx]
    switch -glob -- [lindex $args 0] {
	an* {
	    ## anchor
	    ## stubbed out - too complicated to support
	}
	cl* {
	    ## clear
	    set c $data(basecmd)
	    if {$err} {
		foreach arg [array names data S,*] { unset data($arg) }
		$c itemconfig box -fill {}
	    } else {
		catch {unset data(S,$idx)}
		$c itemconfig $idx -fill {}
		foreach idx [lrange $args 2 end] {
		    if {[catch {Hierarchy_index $w $idx} idx]} {
			catch {unset data(S,$idx)}
			$c itemconfig $idx -fill {}
		    }
		}
	    }
	}
	in* {
	    ## includes
	    if {$err} {
		if {[llength $args]==2} {
		    return -code error $idx
		} else {
		    return -code error "wrong \# args:\
			    should be \"$w selection includes index\""
		}
	    }
	    return [info exists data(S,$idx)]
	}
	se* {
	    ## set
	    if {$err} {
		if {[string compare {} $args]} return
		return -code error "wrong \# args:\
			should be \"$w selection set index ?index ...?\""
	    } else {
		set c $data(basecmd); set col $data(-selectbackground)
		if {[string match all [lindex $args 1]]} {
		    foreach i $idx { set data(S,$i) 1 }
		    $c itemconfig box -fill $col
		} else {
		    set data(S,$idx) 1
		    $c itemconfig $idx -fill $col
		    foreach idx [lrange $args 2 end] {
			if {![catch {Hierarchy_index $w $idx} idx]} {
			    set data(S,$idx) 1
			    $c itemconfig $idx -fill $col
			}
		    }
		}
	    }
	}
	default {
	    return -code error "bad selection option \"[lindex $args 0]\":\
		    must be clear, includes, set"
	}
    }
}

;proc Hierarchy_curselection {w} {
    upvar \#0 $w data

    set res {}
    foreach i [array names data S,*] { lappend res [string range $i 2 end] }
    return $res
}

;proc Hierarchy_get {w args} {
    upvar \#0 $w data

    set nps {}
    foreach arg $args {
	if {![catch {Hierarchy_index $w $arg} idx] && \
		[string compare {} $idx]} {
	    set tags [$data(basecmd) gettags $idx]
	    if {[set i [lsearch -glob $tags box:*]]>-1} {
		lappend nps [string range [lindex $tags $i] 4 end]
	    }
	}
    }
    return $nps
}

;proc Hierarchy_qget {w args} {
    upvar \#0 $w data

    ## Quick get.  Avoids expensive Hierarchy_index call
    set nps {}
    foreach arg $args {
	set tags [$data(basecmd) itemcget $arg -tags]
	if {[set i [lsearch -glob $tags box:*]]>-1} {
	    lappend nps [string range [lindex $tags $i] 4 end]
	}
    }
    return $nps
}

;proc Hierarchy_see {w args} {
    upvar \#0 $w data

    if {[catch {Hierarchy_index $w $args} idx]} {
	return -code error $idx
    } elseif {[string compare {} $idx]} {
	set c $data(basecmd)
	foreach {x y x1 y1} [$c bbox $idx] {top btm} [$c yview] {
	    set stk [lindex [$c cget -scrollregion] 3]
	    set pos [expr (($y1+$y)/2.0)/$stk - ($btm-$top)/2.0]
	}
	$c yview moveto $pos
    }
}

;proc Hierarchy_size {w} {
    upvar \#0 $w data
    return [llength [$data(basecmd) find withtag box]]
}

## This will be the one called by <Double-Button-1> on the canvas,
## if -state is normal, so we have to make sure that $w is correct.
##
;proc Hierarchy_toggle { w index } {
    Hierarchy:toggle $w $index toggle
}

;proc Hierarchy_close { w index } {
    Hierarchy:toggle $w $index close
}

;proc Hierarchy_open { w index } {
    Hierarchy:toggle $w $index open
}

;proc Hierarchy:toggle { w index which } {
    if {[string compare Hierarchy [winfo class $w]]} {
	set w [winfo parent $w]
    }
    upvar \#0 $w data

    if {[string match {} [set np [$w get $index]]]} return
    set np [lindex $np 0]

    set old [$data(basecmd) cget -cursor]
    $data(basecmd) config -cursor watch
    update
    switch $which {
	close	{ Hierarchy:CollapseNodeAll $w $np }
	open	{ Hierarchy:ExpandNodeN $w $np 1 }
	toggle	{
	    if {$data(:$np,showkids)} {
		Hierarchy:CollapseNodeAll $w $np
	    } else {
		Hierarchy:ExpandNodeN $w $np 1
	    }
	}
    }
    if {[string compare {} $data(-command)]} {
	uplevel \#0 $data(-command) [list $w $np $data(:$np,showkids)]
    }
    $data(basecmd) config -cursor $old
    return
}

;proc Hierarchy:Resize { w wid hgt } {
    upvar \#0 $w data
    set c $data(basecmd)
    if {[string compare {} [set box [$c bbox image text]]]} {
	set X [lindex $box 2]
	if {$data(-autoscrollbar)} {
	    set Y [lindex $box 3]
	    if {$wid>$X} {
		set X $wid
		grid remove $data(xscrollbar)
	    } else {
		grid $data(xscrollbar)
	    }
	    if {$hgt>$Y} {
		set Y $hgt
		grid remove $data(yscrollbar)
	    } else {
		grid $data(yscrollbar)
	    }
	    $c config -scrollregion "0 0 $X $Y"
	}
	## This makes full width highlight boxes
	## data(width) is the default width of boxes
	if {$X>$data(width)} {
	    set data(width) $X
	    foreach b [$c find withtag box] {
		foreach {x y x1 y1} [$c coords $b] { $c coords $b 0 $y $X $y1 }
	    }
	}
    } elseif {$data(-autoscrollbar)} {
	grid remove $data(xscrollbar) $data(yscrollbar)
    }
}

;proc Hierarchy:CollapseNodeAll { w np } {
    if {[Hierarchy:CollapseNode $w $np]} {
	upvar \#0 $w data
	Hierarchy:Redraw $w $np
	Hierarchy:DiscardChildren $w $np
	Hierarchy:Resize $w [winfo width $data(canvas)] \
		[winfo height $data(canvas)]
    }
}

;proc Hierarchy:ExpandNodeN { w np n } {
    upvar \#0 $w data
    if {[Hierarchy:ExpandNodeN_aux $w $np $n] || \
	    ([string compare $data(-root) {}] && \
	    ![string compare $data(-root) $np])} {
	Hierarchy:Redraw $w $np
	Hierarchy:Resize $w [winfo width $data(canvas)] \
		[winfo height $data(canvas)]
    }
}

;proc Hierarchy:ExpandNodeN_aux { w np n } {
    if {![Hierarchy:ExpandNode $w $np]} { return 0 }
    if {$n==1} { return 1 }
    incr n -1
    upvar \#0 $w data
    foreach k $data(:$np,kids) {
	Hierarchy:ExpandNodeN_aux $w "$np [list $k]" $n
    }
    return 1
}

########################################################################
##
## Private routines to collapse and expand a single node w/o redrawing
## Most routines return 0/1 to indicate if any change has occurred
##
########################################################################

;proc Hierarchy:ExpandNode { w np } {
    upvar \#0 $w data

    if {$data(:$np,showkids)} { return 0 }
    set data(:$np,showkids) 1
    if {![info exists data(:$np,kids)]} {
	if {[string compare $data(-browsecmd) {}]} {
	    set data(:$np,kids) [uplevel \#0 $data(-browsecmd) [list $w $np]]
	} else {
	    set data(:$np,kids) {}
	}
    }
    if $data(hasnodelook) {
	set data(:$np,look) [uplevel \#0 $data(-nodelook) [list $w $np 1]]
    } else {
	set data(:$np,look) {}
    }
    if {[string match {} $data(:$np,kids)]} {
	foreach {txt font img fg} $data(:$np,look) {
	    lappend tags box:$np box $np
	    set c $data(basecmd)
	    if {[string compare $img {}]} {
		## Catch just in case the image doesn't exist
		catch {
		    $c itemconfigure img:$np -image $img
		    lappend tags $img
		}
	    }
	    if {[string compare $txt {}]} {
		if {[string match {} $font]} { set font $data(-font) }
		if {[string match {} $fg]}   { set fg $data(-foreground) }
		$c itemconfigure txt:$np -fill $fg -text $txt -font $font
		if {[string compare $np $txt]} { lappend tags $txt }
	    }
	    $c itemconfigure box:$np -tags $tags
	    ## We only want to go through once
	    break
	}
	return 0
    }
    foreach k $data(:$np,kids) {
	set knp "$np [list $k]"
	set data(:$knp,showkids) 0
	if $data(hasnodelook) {
	    set data(:$knp,look) [uplevel \#0 $data(-nodelook) [list $w $knp 0]]
	} else {
	    set data(:$knp,look) {}
	}
    }
    return 1
}

;proc Hierarchy:CollapseNode { w np } {
    upvar \#0 $w data
    if {!$data(:$np,showkids)} { return 0 }
    set data(:$np,showkids) 0
    if {[string match {} $data(:$np,kids)]} { return 0 }
    if {[string compare $data(-nodelook) {}]} {
	set data(:$np,look) [uplevel \#0 $data(-nodelook) [list $w $np 0]]
    } else {
	set data(:$np,look) {}
    }
    foreach k $data(:$np,kids) { Hierarchy:CollapseNode $w "$np [list $k]" }
    return 1
}

;proc Hierarchy:DiscardChildren { w np } {
    upvar \#0 $w data
    if {[info exists data(:$np,kids)]} {
	foreach k $data(:$np,kids) {
	    set knp "$np [list $k]"
	    $data(basecmd) delete img:$knp txt:$knp box:$knp
	    foreach i {showkids look stkusg stack iwidth offset} {
		catch {unset data(:$knp,$i)}
	    }
	    Hierarchy:DiscardChildren $w $knp
	}
	unset data(:$np,kids)
    }
}

## REDRAW mechanism
## 2 parts:	recompute offsets of all children from changed node path
##		then redraw children based on their offsets and look
##
;proc Hierarchy:Redraw { w cnp } {
    upvar \#0 $w data

    set c $data(basecmd)
    # When a node changes, the positions of a whole lot of things
    # change.  The size of the scroll region also changes.
    $c delete decor

    # Calculate the new offset locations of everything
    Hierarchy:Recompute $w $data(-root) [lrange $cnp 1 end]

    # Next recursively move all the bits around to their correct positions.
    # We choose an initial point (4,4) to begin at.
    Hierarchy:Redraw_aux $w $data(-root) 4 4

    # Necessary to make sure find closest gets the right item
    # ordering: image > text > box
    after idle "catch { $c raise image text; $c lower box text }"
}

## RECOMPUTE recurses through the tree working out the relative offsets
## of children from their parents in terms of stack values.  
##
## "cnp" is either empty or a node name which indicates where the only
## changes have occured in the hierarchy since the last call to Recompute.
## This is used because when a node is toggled on/off deep in the
## hierarchy then not all the positions of items need to be recomputed.
## The only ones that do are everything below the changed node (of
## course), and also everything which might depend on the stack usage of
## that node (i.e. everything above it).  Specifically the usages of the
## changed node's siblings do *not* need to be recomputed.
##
;proc Hierarchy:Recompute { w np cnp } {
    upvar \#0 $w data
    # If the cnp now has only one element then
    # it must be one of the children of the current node.
    # We do not need to Recompute the usages of its siblings if it is.
    set cnode_is_child [expr [llength $cnp]==1]
    if {$cnode_is_child} {
	set cnode [lindex $cnp 0]
    } else {
	set xcnp [lrange $cnp 1 end]
    }
    
    # Run through the children, recursively calculating their usage of
    # stack real-estate, and allocating an intial placement for each child
    #
    # Values do not need to be recomputed for siblings of the changed
    # node and their descendants.  For the cnode itself, in the
    # recursive call we set the value of cnode to {} to prevent
    # any further cnode checks.

    set children_stack 0
    if {$data(:$np,showkids)} { 
	foreach k $data(:$np,kids) {
	    set knp "$np [list $k]"
	    set data(:$knp,offset) $children_stack
	    if {$cnode_is_child && [string match $cnode $k]} {
		set data(:$knp,stkusg) [Hierarchy:Recompute $w $knp {}]
	    } elseif {!$cnode_is_child} {
		set data(:$knp,stkusg) [Hierarchy:Recompute $w $knp $xcnp]
	    }
	    incr children_stack $data(:$knp,stkusg)
	    incr children_stack $data(-padstack)
	}
    }

    ## Make the image/text if they don't exist.
    ## Positioning occurs in Hierarchy:Redraw_aux.
    ## And calculate the stack usage of our little piece of the world.
    set img_height 0; set img_width 0; set txt_width 0; set txt_height 0

    foreach {txt font img fg} $data(:$np,look) {
	lappend tags box:$np box $np
	set c $data(basecmd)
	if {[string compare $img {}]} {
	    if {[string match {} [$c find withtag img:$np]]} {
		$c create image 0 0 -anchor nw -tags [list img:$np image]
	    }
	    ## Catch just in case the image doesn't exist
	    catch {
		$c itemconfigure img:$np -image $img
		lappend tags $img
		foreach {x y img_width img_height} [$c bbox img:$np] {
         incr img_width [expr {-1*$x}]
         incr img_height [expr {-1*$y}]
		}
	    }
	}
	if {[string compare $txt {}]} {
	    if {[string match {} [$c find withtag txt:$np]]} {
		$c create text 0 0 -anchor nw -tags [list txt:$np text]
	    }
	    if {[string match {} $font]} { set font $data(-font) }
	    if {[string match {} $fg]}   { set fg $data(-foreground) }
	    $c itemconfigure txt:$np -fill $fg -text $txt -font $font
	    if {[string compare $np $txt]} { lappend tags $txt }
	    foreach {x y txt_width txt_height} [$c bbox txt:$np] {
          incr txt_width [expr {-1*$x}]
          incr txt_height [expr {-1*$y}]
	    }
	}
	if {[string match {} [$c find withtag box:$np]]} {
	    $c create rect 0 0 1 1 -tags [list box:$np box] -outline {}
	}
	$c itemconfigure box:$np -tags $tags
	## We only want to go through this once
	break
    }

    if {$txt_height>$img_height} {
	set stack $txt_height
    } else {
	set stack $img_height
    }

    # Now reposition the children downward by "stack"
    set overall_stack [expr $children_stack+$stack]

    if {$data(:$np,showkids)} { 
	set off [expr $stack+$data(-padstack)]
	foreach k $data(:$np,kids) {
	    set knp "$np [list $k]"
	    incr data(:$knp,offset) $off
	}
    }
    # remember some facts for locating the image and drawing decor
    array set data [list :$np,stack $stack :$np,iwidth $img_width]

    return $overall_stack
}

;proc Hierarchy:Redraw_aux { w np deppos stkpos} {
    upvar \#0 $w data

    set c $data(basecmd)
    $c coords img:$np $deppos $stkpos
    $c coords txt:$np [expr {$deppos+$data(:$np,iwidth)+$data(-ipad)}] $stkpos
    $c coords box:$np 0 [expr $stkpos-$data(halfpstk)] \
	    $data(width) [expr $stkpos+$data(:$np,stack)+$data(halfpstk)]

    if {!$data(:$np,showkids) || [string match {} $data(:$np,kids)]} return

    set minkid_stkpos 100000
    set maxkid_stkpos 0
    set bar_deppos [expr $deppos+$data(-paddepth)/2]
    set kid_deppos [expr $deppos+$data(-paddepth)]

    foreach k $data(:$np,kids) {
	set knp "$np [list $k]"
	set kid_stkpos [expr $stkpos+$data(:$knp,offset)]
	Hierarchy:Redraw_aux $w $knp $kid_deppos $kid_stkpos
	
	if {$data(-decoration)} {
	    if {$kid_stkpos<$minkid_stkpos} {set minkid_stkpos $kid_stkpos}
	    set kid_stkpos [expr $kid_stkpos+$data(:$knp,stack)/2]
	    if {$kid_stkpos>$maxkid_stkpos} {set maxkid_stkpos $kid_stkpos}
	    
	    $c create line $bar_deppos $kid_stkpos $kid_deppos $kid_stkpos \
		    -width 1 -tags decor
	}
    }
    if {$data(-decoration)} {
	$c create line $bar_deppos $minkid_stkpos $bar_deppos $maxkid_stkpos \
		-width 1 -tags decor
    }
}


##
## DEFAULT BINDINGS FOR HIERARCHY
##
## Since we give no border to the frame, all Hierarchy bindings
## will always register on the canvas widget
##
bind Hierarchy <Double-Button-1> {
    set w [winfo parent %W]
    if {[string match normal [$w cget -state]]} {
	$w toggle @%x,%y
    }
}
bind Hierarchy <ButtonPress-1> {
    if {[winfo exists %W]} { Hierarchy:BeginSelect [winfo parent %W] @%x,%y }
}
bind Hierarchy <B1-Motion> {
    set tkPriv(x) %x
    set tkPriv(y) %y
    Hierarchy:Motion [winfo parent %W] @%x,%y
}
# bind Hierarchy <ButtonRelease-1> { tkCancelRepeat }
bind Hierarchy <Shift-1>   { Hierarchy:BeginExtend [winfo parent %W] @%x,%y }
bind Hierarchy <Control-1> { Hierarchy:BeginToggle [winfo parent %W] @%x,%y }
bind Hierarchy <B1-Leave> {
    set tkPriv(x) %x
    set tkPriv(y) %y
    Hierarchy:AutoScan [winfo parent %W]
}
# bind Hierarchy <B1-Enter>	{ tkCancelRepeat }

## Should reserve L/R U/D for traversing nodes
bind Hierarchy <Up>		{ %W yview scroll -1 units }
bind Hierarchy <Down>		{ %W yview scroll  1 units }
bind Hierarchy <Left>		{ %W xview scroll -1 units }
bind Hierarchy <Right>		{ %W xview scroll  1 units }

bind Hierarchy <Control-Up>	{ %W yview scroll -1 pages }
bind Hierarchy <Control-Down>	{ %W yview scroll  1 pages }
bind Hierarchy <Control-Left>	{ %W xview scroll -1 pages }
bind Hierarchy <Control-Right>	{ %W xview scroll  1 pages }
bind Hierarchy <Prior>		{ %W yview scroll -1 pages }
bind Hierarchy <Next>		{ %W yview scroll  1 pages }
bind Hierarchy <Control-Prior>	{ %W xview scroll -1 pages }
bind Hierarchy <Control-Next>	{ %W xview scroll  1 pages }
bind Hierarchy <Home>		{ %W xview moveto 0 }
bind Hierarchy <End>		{ %W xview moveto 1 }
bind Hierarchy <Control-slash>	{ Hierarchy:SelectAll [winfo parent %W] }
bind Hierarchy <Control-backslash> { [winfo parent %W] selection clear }

bind Hierarchy <Button-5>       { %W yview scroll 5 units }
bind Hierarchy <Button-4>       { %W yview scroll -5 units }
bind Hierarchy <Shift-Button-5> { %W yview scroll 1 units }
bind Hierarchy <Shift-Button-4> { %W yview scroll -1 units }
bind Hierarchy <Control-Button-5> { %W yview scroll 1 pages }
bind Hierarchy <Control-Button-4> { %W yview scroll -1 pages }

bind Hierarchy <2> {
    set tkPriv(x) %x
    set tkPriv(y) %y
    %W scan mark %x %y
}
bind Hierarchy <B2-Motion> {
    %W scan dragto $tkPriv(x) %y
}

# Hierarchy:BeginSelect --
#
# This procedure is typically invoked on button-1 presses.  It begins
# the process of making a selection in the hierarchy.  Its exact behavior
# depends on the selection mode currently in effect for the hierarchy;
# see the Motif documentation for details.
#
# Arguments:
# w -		The hierarchy widget.
# el -		The element for the selection operation (typically the
#		one under the pointer).  Must be in numerical form.

;proc Hierarchy:BeginSelect {w el} {
    global tkPriv
    if {[catch {Hierarchy_index $w $el} el]} return
    $w selection clear
    $w selection set $el
    set tkPriv(hierarchyPrev) $el
}

# Hierarchy:Motion --
#
# This procedure is called to process mouse motion events while
# button 1 is down.  It may move or extend the selection, depending
# on the hierarchy's selection mode.
#
# Arguments:
# w -		The hierarchy widget.
# el -		The element under the pointer (must be a number).

;proc Hierarchy:Motion {w el} {
    global tkPriv
    if {[catch {Hierarchy_index $w $el} el] || \
	    [string match $el $tkPriv(hierarchyPrev)]} return
    switch [Hierarchy_cget $w -selectmode] {
	browse {
	    Hierarchy_selection $w clear 0 end
	    if {![catch {Hierarchy_selection $w set $el}]} {
		set tkPriv(hierarchyPrev) $el
	    }
	}
	multiple {
	    ## This happens when a double-1 occurs and all the index boxes
	    ## have changed
	    if {[catch {Hierarchy_selection $w includes \
		    $tkPriv(hierarchyPrev)} inc]} {
		set tkPriv(hierarchyPrev) [Hierarchy_index $w $el]
		return
	    }
	    if {$inc} {
		Hierarchy_selection $w set $el
	    } else {
		Hierarchy_selection $w clear $el
	    }
	    set tkPriv(hierarchyPrev) $el
	}
    }
}

# Hierarchy:BeginExtend --
#
# This procedure is typically invoked on shift-button-1 presses.  It
# begins the process of extending a selection in the hierarchy.  Its
# exact behavior depends on the selection mode currently in effect
# for the hierarchy;
#
# Arguments:
# w -		The hierarchy widget.
# el -		The element for the selection operation (typically the
#		one under the pointer).  Must be in numerical form.

;proc Hierarchy:BeginExtend {w el} {
    if {[catch {Hierarchy_index $w $el} el]} return
    if {[string match multiple [$w cget -selectmode]]} {
	Hierarchy:Motion $w $el
    }
}

# Hierarchy:BeginToggle --
#
# This procedure is typically invoked on control-button-1 presses.  It
# begins the process of toggling a selection in the hierarchy.  Its
# exact behavior depends on the selection mode currently in effect
# for the hierarchy;  see the Motif documentation for details.
#
# Arguments:
# w -		The hierarchy widget.
# el -		The element for the selection operation (typically the
#		one under the pointer).  Must be in numerical form.

;proc Hierarchy:BeginToggle {w el} {
    global tkPriv
    if {[catch {Hierarchy_index $w $el} el]} return
    if {[string match multiple [$w cget -selectmode]]} {
	$w selection anchor $el
	if {[$w selection includes $el]} {
	    $w selection clear $el
	} else {
	    $w selection set $el
	}
	set tkPriv(hierarchyPrev) $el
    }
}

# Hierarchy:AutoScan --
# This procedure is invoked when the mouse leaves an entry window
# with button 1 down.  It scrolls the window up, down, left, or
# right, depending on where the mouse left the window, and reschedules
# itself as an "after" command so that the window continues to scroll until
# the mouse moves back into the window or the mouse button is released.
#
# Arguments:
# w -		The hierarchy widget.

;proc Hierarchy:AutoScan {w} {
    global tkPriv
    if {![winfo exists $w]} return
    set x $tkPriv(x)
    set y $tkPriv(y)
    if {$y>=[winfo height $w]} {
	$w yview scroll 1 units
    } elseif {$y<0} {
	$w yview scroll -1 units
    } elseif {$x>=[winfo width $w]} {
	$w xview scroll 2 units
    } elseif {$x<0} {
	$w xview scroll -2 units
    } else {
	return
    }
    #Hierarchy:Motion $w [$w index @$x,$y]
    set tkPriv(afterId) [after 50 Hierarchy:AutoScan $w]
}

# Hierarchy:SelectAll
#
# This procedure is invoked to handle the "select all" operation.
# For single and browse mode, it just selects the root element.
# Otherwise it selects everything in the widget.
#
# Arguments:
# w -		The hierarchy widget.

;proc Hierarchy:SelectAll w {
    if {[regexp (browse|single) [$w cget -selectmode]]} {
	$w selection clear
	$w selection set root
    } else {
	$w selection set all
    }
}

#------------------------------------------------------------
# Default nodelook methods
#------------------------------------------------------------

;proc Hierarchy:FileLook { w np isopen } {
    upvar \#0 $w data
    set path [eval file join $np]
    set file [lindex $np end]
    set bmp  {}
    if {[file readable $path]} {
	if {[file isdirectory $path]} {
	    if {$isopen} {
		## We know that kids will always be set by the time
		## the isopen is set to 1
		if {[string compare $data(:$np,kids) {}]} {
		    set bmp bmp:dir_minus
		} else {
		    set bmp bmp:dir
		}
	    } else {
		set bmp bmp:dir_plus
	    }
	    if 0 {
		## NOTE: accurate, but very expensive
		if {[string compare [Hierarchy:FileList $w $np] {}]} {
		    if {$isopen} {set bmp bmp:dir_minus} {set bmp bmp:dir_plus}
		} else {
		    set bmp bmp:dir
		}
	    }
	}
	set fg \#000000
    } elseif {[string compare $data(-showparent) {}] && \
	    [string match $data(-showparent) $file]} {
	set fg \#0000FF
	set bmp bmp:up
    } else {
	set fg \#a9a9a9
	if {[file isdirectory $path]} {set bmp bmp:dir}
    }
    return [list $file $data(-font) $bmp $fg] 
}

## Hierarchy:FileList
# ARGS:	w	hierarchy widget
#	np	node path	
# Returns:	directory listing
##
;proc Hierarchy:FileList { w np } {
    set pwd [pwd]
    if {[catch "cd \[file join $np\]"]} {
	set list {}
    } else {
	global tcl_platform
	upvar \#0 $w data
	set str *
	if {!$data(-showfiles)} { append str / }
	if {$data(-showall) && [string match unix $tcl_platform(platform)]} {
	    ## NOTE: Use of non-core lremove
	    if {[catch {lsort [concat [glob -nocomplain $str] \
		    [lremove [glob -nocomplain .$str] {. ..}]]} list]} {
		return {}
	    }
	} else {
	    ## The extra catch is necessary for unusual error conditions
	    if {[catch {lsort [glob -nocomplain $str]} list]} {
		return {}
	    }
	}
	set root $data(-root)
	if {[string compare {} $data(-showparent)] && \
		[string match $root $np]} {
	    if {![regexp {^(.:)?/+$} $root] && \
		    [string compare [file dir $root] $root]} {
		set list [linsert $list 0 $data(-showparent)]
	    }
	}
    }
    cd $pwd
    return $list
}

;proc Hierarchy:FileActivate { w np isopen } {
    upvar \#0 $w data
    set path [eval file join $np]
    if {[file isdirectory $path]} return
    if {[string compare $data(-showparent) {}] && \
	    [string match $data(-showparent) [lindex $np end]]} {
	$w config -root [file dir $data(-root)]
    }
}

;proc Hierarchy:WidgetLook { W np isopen } {
    upvar \#0 $W data
    if {$data(-showall)} {
	set w [lindex $np end]
    } else {
	set w [join $np {}]
	regsub {\.\.} $w {.} w
    }
    if {[string compare [winfo children $w] {}]} {set fg blue} {set fg black}
    return [list "\[[winfo class $w]\] [lindex $np end]" {} {} $fg]
}

;proc Hierarchy:WidgetList { W np } {
    upvar \#0 $W data
    if {$data(-showall)} {
	set w [lindex $np end]
    } else {
	set w [join $np {}]
	regsub {\.\.} $w {.} w
    }
    set kids {}
    foreach i [lsort [winfo children $w]] {
	if {$data(-showall)} {
	    lappend kids $i
	} else {
	    lappend kids [file extension $i]
	}
    }
    return $kids
}

;proc Hierarchy:WidgetActivate { w np isopen } {}


## BITMAPS
##
image create bitmap bmp:dir -data {#define folder_width 16
#define folder_height 12
static char folder_bits[] = {
  0x00, 0x1f, 0x80, 0x20, 0x40, 0x20, 0xfc, 0x7f, 0x02, 0x40, 0x02, 0x40,
  0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0xfe, 0x7f};}
image create bitmap bmp:dir_plus -data {#define folder_plus_width 16
  #define folder_plus_height 12
static char folder_plus_bits[] = {
  0x00, 0x1f, 0x80, 0x20, 0x40, 0x20, 0xfc, 0x7f, 0x02, 0x40, 0x82, 0x40,
  0x82, 0x40, 0xe2, 0x43, 0x82, 0x40, 0x82, 0x40, 0x02, 0x40, 0xfe, 0x7f};}
image create bitmap bmp:dir_minus -data {#define folder_minus_width 16
#define folder_minus_height 12
static char folder_minus_bits[] = {
  0x00, 0x1f, 0x80, 0x20, 0x40, 0x20, 0xfc, 0x7f, 0x02, 0x40, 0x02, 0x40,
  0x02, 0x40, 0xe2, 0x43, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0xfe, 0x7f};}
image create bitmap bmp:up -data {#define up.xbm_width 16
#define up.xbm_height 12
static unsigned char up.xbm_bits[] = {
  0x00, 0x00, 0x10, 0x00, 0x38, 0x00, 0x7c, 0x00, 0xfe, 0x00, 0x38, 0x00,
  0x38, 0x00, 0x38, 0x00, 0xf8, 0x7f, 0xf0, 0x7f, 0xe0, 0x7f, 0x00, 0x00};}
image create bitmap bmp:text -data {#define text_width 15
#define text_height 14
static char text_bits[] = {
  0xff,0x07,0x01,0x0c,0x01,0x04,0x01,0x24,0xf9,0x7d,0x01,0x78,0x01,0x40,0xf1,
  0x41,0x01,0x40,0x01,0x40,0xf1,0x41,0x01,0x40,0x01,0x40,0xff,0x7f};}

return
