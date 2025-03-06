# GAUNTLET EDITOR v1.0 #


### About this program ###

'Gauntlet Editor' is a Crossplatform editor for the game Gauntlet for MSX computers

### How do I get set up? ###

Gauntlet Editor has been written in Lazarus IDE and FreePascal with the purpose of being multiplatform (Windows, Mac, Linux).

To build Gauntlet Editor, Freepascal v3.2.2 or upper must be used, as well as Lazarus IDE (V3.8 or later).

Gauntlet Editor needs a range of packages installed on Lazarus. They will be required as needed. Just install them by following the instructions.
Additionally, Linux building requires QT6 files to be distribuiteda as part of the build.

On Linux, the libsqlite3-dev package is a prerequisite.


### ToDo feature list ###

* Place character start point
* Fix size calculation
* Fix array bound violation while determining the best direction of traces
+ Space counting isn't taking into acount trap bound walls
- Add an "autofix" feature for too-complex maps (e.g. removing a stroke randomly)
- Start position not correctly recalled from disk
- Fix starting of "draw gate" command
- Support for the ZX Spectrum version
- Database persistence
- Preview based on WebMSX
- Undo/Redo
- Fix memory leaks
- Copy and Paste regions between mazes or in a maze
- Apply styles on the zoom
- Fix font resolution issues on Linux
- Check compilation on Mac
- Use TPatternMap to implement the index-pattern map.
- Fix "modified" glyph of TATTabs
- Change TMemoryBuffer copy for a direct method
- Add a "BOOM" button to clear the whole maze
- The top border is overwritten when a random maze is generated and wrapping is enabled
