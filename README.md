# sway-cycle-workspaces
A tool to **switch** sway **workspaces** in a particular way, namely **either up or down considering only the monitor/output containing the focused window**.
It was built to accomodate my specific use case where Mod-Key+Scroll-Up is supposed to go to the next workspace on the focused monitor and Mod-Key+Scroll-Down to do the reverse.

**By the time you read this it may be possible to produce the same behaviour by configuring sway directly. When I needed the feature I could not figure out a way to do it that way and I was eager to write a bit of Haskell, so here we are. Read the sway documentation more carefully than I did before installing additional software.**

## usage

## what it does
Imagine a configuration of two monitors with 3 workspaces each:
<br/> `(1 2* 3) (4 5 6)`<br/>
where workspace 2 is focused, which is indicated by an asterisk.
Mod-Key+**Scroll-Up** would take the workspaces to the configuration
<br/>`(1 2 3*) (4 5 6)`<br/>
with repeating Mod-Key+Scroll-Up presses leaving the focus unchanged.

Repeated Mod-Key+**Scroll-Down** presses would, initially, return to the starting point
<br/>`1 2* 3) (4 5 6)`,<br/>
then to
<br/>`(1* 2 3) (4 5 6)`<br/>
and then remain there.

## how it does it
It queries the entire workspace configuration as exposed by the command line interface<br/>
`swaymsg -t get_workspaces` see https://github.com/swaywm/sway/blob/master/swaymsg/swaymsg.1.scd <br/>
Its JSON-formatted output is transformed using [jq](https://stedolan.github.io/jq/) into lines of text where each line corresponds to one of the active workspaces.
```
eDP-1 1 true
eDP-1 2 false
HDMI-A-1 4 false
HDMI-A-1 5 false
HDMI-A-1 6 false
```
A line indicates, in the order of appearance: The name of the output/monitor, the name of the workspace and either 'true' or 'false' indicating wether the workspace contains the currently focused window or not. The elements are separated by spaces.<br/><br/>
These lines are fed to a haskell program wich, depending on its command line arguments - 'next' or 'previous' - either returns the name of the workspace we wish to focus or an error if the input was malformed or there is no next/previous workspace to focus. If an error occurs the pipe discontinues and no harm is done..............yet.