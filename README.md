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
* Space counting isn't taking into acount trap bound walls
* The top border is overwritten when a random maze is generated and wrapping is enabled
* Add a "BOOM" button to clear the whole maze
* Fix starting of "draw gate" command
* Add an "autofix" feature for too-complex maps (e.g. removing a stroke randomly)
* Start position not correctly recalled from disk
* Add file extensions for native saving of maze and block
    * Maze file (native): .gmf
    * Maze block file (native): .gmb
    * Export file: .*
    * Export block: .*
* Add "Overwrite file?"
* Add CTRL+S to save current file if has already a filename
    * Add path to the Maze properties
* Check max block size (#d000-#de7f). The MSX DSK version copies things at #de80 and it can break the last block. MSX ROM/TZX and ZX/CPC have different limits. Take the biggest known block as reference.
* Investigate the unknown flag
* Check load/import dialogs (they have SAVE button)
* Add the missing flag: choose one EXIT only 
* Draw gates better (use the right pattern depending on the direction)
* Fix "modified" glyph of TATTabs (not possible, changed to an old school *)
    * Add "modified" handling
    * List of actions that cause a TGauntMaze to modify:
        * Change Style
        * Change H Wrap
        * Change V Wrap
        * Change Damage
        * Change Keep One Exit
        * Change Name
        * Write cell
        * Place character
        * Trap
        * Use maze tools
* Check ATCombobox as an alternative (check styling in the examples provided) (NOT VALID)
* Add a default name when a maze is imported
* Add a shortcut to close tabs
* Trap block tool must toggle trap-notrap
* Try to close all tabs when closing application
* Analyse vertical wrap. Stock levels do write on the top border.
    * Analyse 8 and 9 from MAZE03 (something looks wrong)
        * 9 in MAZE10. Last byte is #5C, should be #36 (due to the MSX block bug).
        * 8 in MAZE10. Has exits on top border, but hasn't wrap flag enabled. Check in the game.
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
    