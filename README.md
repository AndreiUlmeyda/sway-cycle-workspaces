![example workflow](https://github.com/AndreiUlmeyda/sway-cycle-workspaces/actions/workflows/ci.yml/badge.svg)

# sway-cycle-workspaces
A tool to **switch** [sway](https://swaywm.org/) **workspaces** in a particular way, namely **either up or down considering only the monitor/output containing the focused window**.
It was built to accomodate my specific use case where Mod-Key+Scroll-Up is supposed to go to the next workspace on the focused monitor and Mod-Key+Scroll-Down to do the reverse.

**By the time you read this it may be possible to produce the same behaviour by configuring sway directly. When I needed the feature I could not figure out a way to do it that way and I was eager to write a bit of Haskell, so here we are. Read the sway documentation more carefully than I did before installing additional software.**

## what it does
Imagine a configuration of two monitors with 3 workspaces each:
<br/> `(1 2* 3) (4 5 6)`<br/>
where workspace 2 is focused, which is indicated by an asterisk.
Mod-Key+**Scroll-Up** would take the workspaces to the configuration
<br/>`(1 2 3*) (4 5 6)`<br/>
with repeating Mod-Key+Scroll-Up presses leaving the focus unchanged.

Repeated Mod-Key+**Scroll-Down** presses would, initially, return to the starting point
<br/>`(1 2* 3) (4 5 6)`,<br/>
then to
<br/>`(1* 2 3) (4 5 6)`<br/>
and then remain there.

## usage
Most likely you will want to call this tool using some sort of hotkey. The most convenient setup for
me is using the the mod key together with the mouse scroll wheel and, simultaneously, using mod+shift+left/right
as well as mod+shift+up/down, because redundancy saves lives in an engineering context or something ¯\\_(^^)_/¯.
A corresponding sway configuration could look like
<br/> `bindsym --whole-window --border $mod+button4 exec 'cycle-workspaces next'`
<br/> `bindsym --whole-window --border $mod+button5 exec 'cycle-workspaces previous'`
<br/> `bindsym $mod+Shift+Right exec 'sway-cycle-workspaces next'`
<br/> `bindsym $mod+Shift+Left exec 'sway-cycle-workspaces previous'`
and so on and so forth.

## installation
In case this package here did not come preinstalled on your linux distribution - and it would be highly concerning if it did - you can choose one of two methods to install this tool. For both you will need to download the source files first and make sure at least one dependency ([stack](https://docs.haskellstack.org/en/stable/README/)) is installed manually.
Install stack, later used to build the [Haskell](https://www.haskell.org/) portion of this project, either from the [AUR](https://wiki.archlinux.org/title/Arch_User_Repository) or using the method described on their website. Then download the source files for this project
<br/> `git clone https://github.com/AndreiUlmeyda/sway-cycle-workspaces.git`
<br/> `cd sway-cycle-workspaces`
### If you are running Arch-Linux
...then the best method would be to **build and install a proper package**. To this end a file called PKGBUILD resides inside of the directory 'distribution'. It can be assembled using the command 'makepkg' and the resulting file can be installed using your favourite package manager.
<br/> `cd distribution`
<br/> `makepkg`
<br/> `sudo pacman -U sway-cycle-workspaces-0.0.1-1-x86_64.pkg.tar.zst`

### **If not, though**
...~~even more hard and demeaning manual labour is required.~~
Make sure all dependencies - bash, findutils, jq, sway (and stack, if you want to compile the haskell binary yourself) - are installed beforehand. 
You could, then, try to clone this repository, as described above, and copy the 3 executables - a shell script, a jq script and a haskell binary - to suitable locations.
Copy the file bin/sway-cycle-workspaces into a folder that is in your shells PATH.
Copy the files bin/new-workspace-focus and bin/json-to-workspace-lines.jq into the folder /usr/lib/sway-cycle-workspaces/.
Make sure everything is executable, as well.
All things considered, this is ugly as all heck. If you find yourself doing that, please author an issue describing your distribution and I will try to provide a
proper package for it. And if you want to supply one yourself, well, what are you waiting for? For climate change to reverse? Hmm?

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
