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
- Fix memory leaks
- Support for the ZX Spectrum version
    +export block
    -export TZX
    +import block
- Support for the Amstrad CPC version
    +export block
    -export CDT
    +import block    
- Apply styles on the zoom
- Preview based on WebMSX
- Undo/Redo
- Database persistence
- Copy and Paste regions between mazes or in a maze
- Fix font resolution issues on Linux
- Check compilation on Mac
- Use TFPGMap to implement the index-pattern map.
- Change TMemoryBuffer copy for a direct method
- Add SAVE AS or DUPLICATE (I prefer the latter)
- Use local config to avoid writing in protected areas
    