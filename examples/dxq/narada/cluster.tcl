# TCL code for starting up multixterm for the cluster example
# Invoke with
#
#     multixterm -xf cluster.tcl

set yPos 0
set xPos 0
#foreach name {18 19 20 21 22 23 24 25 26 27 28 29 30 31 32} {
# cactus221 is chronically ill, so skip it.
foreach name {18 19 20 21 23 24 25 26 27 28 29} {
    set ::xtermArgs "-sb 200 -geometry 80x6+$xPos+$yPos -font 6x10"
    set ::xtermNames "cactus${name}1"
    set ::xtermCmd "ssh cactus${name}1"
    xtermStartAll
    incr xPos 100
    if {$xPos > 1000} {set xPos 0; incr yPos 100}
}
