# sway-cycle-workspaces
A tool to **switch** sway **workspaces** a particular way, namely **either up or down considering only the monitor/output containing the focused window**.
It was built to accomodate my specific use case where Mod-Key+Scroll-Up is supposed to go to the next workspace on the focused monitor and Mod-Key+Scroll-Down to do the reverse.

**By the time you read this it may be possible to produce the same behaviour by configuring sway directly. When I needed the feature I could not figure out a way to do it that way and I was eager to write a bit of haskell, so here we are. Read the sway documentation more carefully than I did before installing additional software.**

## usage

## what it does
Imagine a configuration of two monitors with 3 workspaces each: (1 2* 3) (4 5 6) where workspace 2 is focused, which is indicated by an asterisk.
Mod-Key+Scroll-Up would take the workspaces to the configuration (1 2 3*) (4 5 6) with repeating Mod-Key+Scroll-Up presses leaving the focus unchanged.

Repeated Mod-Key+Scroll-Down presses would first return to the starting point (1 2* 3) (4 5 6), then to (1* 2 3) (4 5 6) and then remain there.
