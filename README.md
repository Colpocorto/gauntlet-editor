# GAUNTLET EDITOR v1.0


## About this program ##

'Gauntlet Editor' is a Crossplatform editor for the game Gauntlet for MSX computers

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.
Also, this project uses open-source code from the following libraries:

AtFlatControls:         https://github.com/Alexey-T/ATFlatControls
BGRAControls:           https://github.com/bgrabitmap/bgracontrols
KControls:              https://github.com/kryslt/KControls
FreePascal and Lazarus: https://github.com/fpc

MSX, ZX Spectrum and Amstrad CPC (as well as their logos) mentioned in this project are the property of their respective owners. These names are used for identification purposes only and do not imply endorsement or affiliation with the "Gauntlet Editor" project.

## How do I get set up? ##

Gauntlet Editor has been written in Lazarus IDE and FreePascal with the purpose of being multiplatform (Windows, Mac, Linux).

To build Gauntlet Editor, Freepascal v3.2.2 or upper must be used, as well as Lazarus IDE (V3.8 or later).

Gauntlet Editor needs a range of packages installed on Lazarus. They will be required as needed. Just install them by following the instructions provided by de IDE, or use the Online Package Manager tool.
Additionally, Linux building requires QT6 files to be distribuited as part of the build.

	KControlsLaz >= 1.7.3
	pl_ExControls >= 7.2.1
	atflagcontrols_package >= 2.3 (latest commit from Github recommended) 
	BGRAControls >= 9.0.1.6
	pl_Cindy >= 6.8.1

## Using Gauntlet Editor

The Z80-based versions of Gauntlet (MSX, ZX, CPC) load packages of 10 levels (being 9 and 10 the treasure rooms), although 4 levels (and optinally a treasure room) are randomly chosen.
This editor allows to save single mazes or one 10-level block as described formerly. The file format can be:

* Native (use "load" or "save" features), the Editor's own format.
* MSX / raw (use "import" or "export" features).
* MSX Disk version (use "import" or "export" features).
* ZX Spectrum / Raw
* Amstrad CPC / Raw

The raw files can be inserted into a TSX/CDT/TZX tape image file by using the TZX Tools or ZX-Block Editor tool.

In the future, creation of ready-to-play tape image files will be supported.

There are a full maze library (taken from the original and Deeper Dungeons tapes available for MSX, ZX Spectrum and CPC) in the "maze-library" folder inside the application directory.

## Contributing

Pull requests are welcome. For major changes, please open an issue first
to discuss what you would like to change.
    