unit uData;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LazLoggerBase,
  SQLite3Conn, sqldb;

type
  TGauntMap = array[0..31, 0..31] of byte;
  TLayer = (background, objects, positions);
  TGauntTraceDir = (up, up_left, right, down_right, down, down_left, left, up_right);
  TSearchTraceType = (sttWall, sttTrapWall, sttGate, sttExit);
  TGauntVersion = (gvDSK, gvROM, gvCAS, gvTSX);

  TGauntStyle = record
    id: integer;
    Name: string;
    desc: string;
  end;
  TCustomGauntTraceCmd = class abstract (TObject);

  TGauntTrace = class(TCustomGauntTraceCmd)
    dir: TGauntTraceDir;
    steps: smallint;
  end;

  TGauntMaze = class(TComponent)
  private
    FSize: integer;  //0-511
    FHorzWrap: boolean;
    FVertWrap: boolean;
    FStunPlayers: boolean;
    FHurtPlayers: boolean;
    FName: string;
    FVisitedData: TGauntMap;
    FStyle: TGauntStyle;
    FBuffer: TMemoryStream;
    Fid: string;
    FCreationDate: TDateTime;
    FLastSavedDate: TDateTime;
    FPlayerPos: TPoint;
    procedure InitMap(var map: TGauntMap);
    function ProcessTraceLayer: integer;
    function ProcessObjectLayer: integer;
    function FindTraces(Atype: TSearchTraceType): integer;
    function IsAWall(cell: byte): boolean;
    function IsAGate(cell: byte): boolean;
    function IsATrapWall(cell: byte): boolean;
    function CountExits: integer;
    function IsLookedType(cell: byte; Atype: TSearchTraceType): boolean;
    function GetTraceOriginCode(AType: TSearchTraceType): byte;
    function PeekMapData(offset: integer): byte;
    procedure PokeMapData(offset: integer; Value: byte);
    function CountConsecutiveSpaces(offset: integer): integer;
    function CountConsecutiveObjects(offset: integer; Data: byte): integer;
    function AnyObjectsLeft(offset: integer): boolean;
  public
    MapData: TGauntMap;
    ItemData: TGauntMap;

    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function getItemsLayersize(): integer;
    property Name: string read FName write FName;
    property StunPlayers: boolean read FStunPlayers write FStunPlayers;
    property HurtPlayers: boolean read FHurtPlayers write FHurtPlayers;
    property Style: TGauntStyle read FStyle write FStyle;
    property id: string read Fid write Fid;
    property CreationDate: TDateTime read FCreationDate write FCreationDate;
    property LastSavedDate: TDateTime read FLastSavedDate write FLastSavedDate;
    function getSize: integer;
    procedure SetHorzWrap(w: boolean);
    procedure SetVertWrap(w: boolean);
    function GetHorzWrap(): boolean;
    function GetVertWrap(): boolean;
    procedure InitMapData;
    procedure InitItemData;
    procedure InitVisitedData;
    procedure ToFileStream(fs: TFileStream);
    procedure FromFileStream(fs: TFileStream);

    procedure ExportToFileStream(fs: TFileStream);

    function ProcessMap: integer;
    procedure SetPlayerPos(col: byte; row: byte);
    function GetPlayerPos: TPoint;

  end;

  TPictureIndex = record
    id: integer;
    fileName: string;
    desc: string;
  end;
  TGauntBlock = array[0..9] of TGauntMaze;

const
  MAX_TRACE_SIZE = 255;
  MAX_ITEM_SIZE = (512 - 4 - MAX_TRACE_SIZE);
  RESOURCES_DIR = 'resources/';
  DATABASE_FILENAME = 'gauntedit.sqlite3';
  APPDATA_DIR = '.gauntlet_editor/';
  STYLES_OFFSET = $100;    //because of the trap-bind bit a set can reach $B6
  TRAP_OFFSET = $80;
  gauntStyles: array[0..7] of TGauntStyle = (
    (id: 0; Name: 'Style 1'; desc: 'Red/Yellow, chess pattern'),
    (id: 1; Name: 'Style 2'; desc: 'Dark Green/Green, brick pattern'),
    (id: 2; Name: 'Style 3'; desc: 'Blue/Light Blue, diamond pattern'),
    (id: 3; Name: 'Style 4'; desc: 'Blue/Magenta, chess pattern'),
    (id: 4; Name: 'Style 5'; desc: 'Light Red/Yellow, brick pattern'),
    (id: 5; Name: 'Style 6'; desc: 'Grey/White, diamond pattern'),
    (id: 6; Name: 'Style 7'; desc: 'Red/Yellow, diamond pattern'),
    (id: 7; Name: 'Style 8'; desc: 'Light Red/Yellow, diamond pattern')
    );
  Widths: array[0..6] of integer = (16, 24, 32, 48, 64, 96, 128);
  patternIndex: array of TPictureIndex = (
    (id: $00; fileName: 'blank.png'; desc: 'blank'),
    (id: $01 + 0 * STYLES_OFFSET; fileName: 'wall00_01.png'; desc: 'Down end wall'),
    (id: $02 + 0 * STYLES_OFFSET; fileName: 'wall00_02.png'; desc: 'Left end wall'),
    (id: $03 + 0 * STYLES_OFFSET; fileName: 'wall00_03.png';
    desc: 'Left down corner wall'),
    (id: $04 + 0 * STYLES_OFFSET; fileName: 'wall00_04.png'; desc: 'Top end wall'),
    (id: $05 + 0 * STYLES_OFFSET; fileName: 'wall00_05.png'; desc: 'Vertical wall'),
    (id: $06 + 0 * STYLES_OFFSET; fileName: 'wall00_06.png';
    desc: 'Left top corner wall'),
    (id: $07 + 0 * STYLES_OFFSET; fileName: 'wall00_07.png';
    desc: 'Bifurcation to the right wall'),
    (id: $08 + 0 * STYLES_OFFSET; fileName: 'wall00_08.png'; desc: 'Right end wall'),
    (id: $09 + 0 * STYLES_OFFSET; fileName: 'wall00_09.png';
    desc: 'Right down corner wall'),
    (id: $0a + 0 * STYLES_OFFSET; fileName: 'wall00_0a.png'; desc: 'Horizontal wall'),
    (id: $0b + 0 * STYLES_OFFSET; fileName: 'wall00_0b.png';
    desc: 'Bifurcation to up wall'),
    (id: $0c + 0 * STYLES_OFFSET; fileName: 'wall00_0c.png';
    desc: 'Right top corner wall'),
    (id: $0d + 0 * STYLES_OFFSET; fileName: 'wall00_0d.png';
    desc: 'Bifurcation to the left wall'),
    (id: $0e + 0 * STYLES_OFFSET; fileName: 'wall00_0e.png';
    desc: 'Bifurcation to down wall'),
    (id: $0f + 0 * STYLES_OFFSET; fileName: 'wall00_0f.png'; desc: 'Cross wall'),
    (id: $10 + 0 * STYLES_OFFSET; fileName: 'wall00_10.png'; desc: 'Single block wall'),
    (id: $11; fileName: 'gate_h.png'; desc: 'Horizontal gate'),
    (id: $12; fileName: 'gate_v.png'; desc: 'Vertical gate'),
    (id: $13; fileName: 'treasure.png'; desc: 'Treasure'),
    (id: $14; fileName: 'cider.png'; desc: 'Cider'),
    (id: $15; fileName: 'ham.png'; desc: 'Food'),
    (id: $16; fileName: 'potion.png'; desc: 'Potion'),
    (id: $17; fileName: 'unbreak_potion.png'; desc: 'Non-destructible potion'),
    (id: $18; fileName: 'amulet.png'; desc: 'Amulet'),
    (id: $19; fileName: 'x_armour_potion.png'; desc: 'Extra Armour potion'),
    (id: $1a; fileName: 'x_pickup_potion.png'; desc: 'Extra Pickup Power potion'),
    (id: $1b; fileName: 'x_magic_potion.png'; desc: 'Extra Magic Power potion'),
    (id: $1c; fileName: 'x_shot_pwr_potion.png'; desc: 'Extra Shot Power Potion'),
    (id: $1d; fileName: 'x_shot_spd_potion.png'; desc: 'Extra Shot Speed Potion'),
    (id: $1e; fileName: 'x_fight_potion.png'; desc: 'Extra Fight Potion'),
    (id: $1f; fileName: 'key.png'; desc: 'Key'),
    (id: $20; fileName: 'gen_ghost_1.png'; desc: 'Ghost Generator Level 1'),
    (id: $21; fileName: 'gen_ghost_2.png'; desc: 'Ghost Generator Level 2'),
    (id: $22; fileName: 'gen_ghost_3.png'; desc: 'Ghost Generator Level 3'),
    (id: $23; fileName: 'gen_grunt_1.png'; desc: 'Grunt Generator Level 1'),
    (id: $24; fileName: 'gen_grunt_1.png'; desc: 'Grunt Generator Level 2'),
    (id: $25; fileName: 'gen_grunt_1.png'; desc: 'Grunt Generator Level 3'),
    (id: $26; fileName: 'gen_grunt_1.png'; desc: 'Demon Generator Level 1'),
    (id: $27; fileName: 'gen_grunt_1.png'; desc: 'Demon Generator Level 2'),
    (id: $28; fileName: 'gen_grunt_1.png'; desc: 'Demon Generator Level 3'),
    (id: $29; fileName: 'gen_grunt_1.png'; desc: 'Lobber Generator Level 1'),
    (id: $2a; fileName: 'gen_grunt_1.png'; desc: 'Lobber Generator Level 2'),
    (id: $2b; fileName: 'gen_grunt_1.png'; desc: 'Lobber Generator Level 3'),
    (id: $2c; fileName: 'gen_grunt_1.png'; desc: 'Sorcerer Generator Level 1'),
    (id: $2d; fileName: 'gen_grunt_1.png'; desc: 'Sorcerer Generator Level 2'),
    (id: $2e; fileName: 'gen_grunt_1.png'; desc: 'Sorcerer Generator Level 3'),
    (id: $2f; fileName: 'trap.png'; desc: 'Trap'),
    (id: $30; fileName: 'transporter.png'; desc: 'Transporter'),
    (id: $31; fileName: 'bad_cider.png'; desc: 'Poison Cider'),
    (id: $32; fileName: 'keyring.png'; desc: 'Keyring'),
    (id: $33 + 0 * STYLES_OFFSET; fileName: 'breakable_wall_0_3.png';
    desc: 'Destructible Wall Level 3'),
    (id: $34 + 0 * STYLES_OFFSET; fileName: 'breakable_wall_0_2.png';
    desc: 'Destructible Wall Level 2'),
    (id: $35 + 0 * STYLES_OFFSET; fileName: 'breakable_wall_0_1.png';
    desc: 'Destructible Wall Level 1'),
    (id: $36; fileName: 'exit.png'; desc: 'Exit door'),
    (id: $3f; fileName: 'warrior.png'; desc: 'Player start position'),

    (id: $40; fileName: 'ghost.png'; desc: 'Ghost Level 1'),
    (id: $41; fileName: 'ghost.png'; desc: 'Ghost Level 2'),
    (id: $42; fileName: 'ghost.png'; desc: 'Ghost Level 3'),
    (id: $48; fileName: 'grunt.png'; desc: 'Grunt Level 1'),
    (id: $49; fileName: 'grunt.png'; desc: 'Grunt Level 2'),
    (id: $4a; fileName: 'grunt.png'; desc: 'Grunt Level 3'),
    (id: $50; fileName: 'demon.png'; desc: 'Demon Level 1'),
    (id: $51; fileName: 'demon.png'; desc: 'Demon Level 2'),
    (id: $52; fileName: 'demon.png'; desc: 'Demon Level 3'),
    (id: $58; fileName: 'lobber.png'; desc: 'Lobber Level 1'),
    (id: $59; fileName: 'lobber.png'; desc: 'Lobber Level 2'),
    (id: $5a; fileName: 'lobber.png'; desc: 'Lobber Level 3'),
    (id: $60; fileName: 'sorcerer.png'; desc: 'Sorcerer Level 1'),
    (id: $61; fileName: 'sorcerer.png'; desc: 'Sorcerer Level 2'),
    (id: $62; fileName: 'sorcerer.png'; desc: 'Sorcerer Level 3'),
    (id: $68; fileName: 'death.png'; desc: 'Death'),
    (id: $01 + 1 * STYLES_OFFSET; fileName: 'wall01_01.png'; desc: ''),
    (id: $02 + 1 * STYLES_OFFSET; fileName: 'wall01_02.png'; desc: ''),
    (id: $03 + 1 * STYLES_OFFSET; fileName: 'wall01_03.png'; desc: ''),
    (id: $04 + 1 * STYLES_OFFSET; fileName: 'wall01_04.png'; desc: ''),
    (id: $05 + 1 * STYLES_OFFSET; fileName: 'wall01_05.png'; desc: ''),
    (id: $06 + 1 * STYLES_OFFSET; fileName: 'wall01_06.png'; desc: ''),
    (id: $07 + 1 * STYLES_OFFSET; fileName: 'wall01_07.png'; desc: ''),
    (id: $08 + 1 * STYLES_OFFSET; fileName: 'wall01_08.png'; desc: ''),
    (id: $09 + 1 * STYLES_OFFSET; fileName: 'wall01_09.png'; desc: ''),
    (id: $0a + 1 * STYLES_OFFSET; fileName: 'wall01_0a.png'; desc: ''),
    (id: $0b + 1 * STYLES_OFFSET; fileName: 'wall01_0b.png'; desc: ''),
    (id: $0c + 1 * STYLES_OFFSET; fileName: 'wall01_0c.png'; desc: ''),
    (id: $0d + 1 * STYLES_OFFSET; fileName: 'wall01_0d.png'; desc: ''),
    (id: $0e + 1 * STYLES_OFFSET; fileName: 'wall01_0e.png'; desc: ''),
    (id: $0f + 1 * STYLES_OFFSET; fileName: 'wall01_0f.png'; desc: ''),
    (id: $10 + 1 * STYLES_OFFSET; fileName: 'wall01_10.png'; desc: ''),
    (id: $33 + 1 * STYLES_OFFSET; fileName: 'breakable_wall_1_3.png'; desc: ''),
    (id: $34 + 1 * STYLES_OFFSET; fileName: 'breakable_wall_1_2.png'; desc: ''),
    (id: $35 + 1 * STYLES_OFFSET; fileName: 'breakable_wall_1_1.png'; desc: ''),
    (id: $01 + 2 * STYLES_OFFSET; fileName: 'wall02_01.png'; desc: ''),
    (id: $02 + 2 * STYLES_OFFSET; fileName: 'wall02_02.png'; desc: ''),
    (id: $03 + 2 * STYLES_OFFSET; fileName: 'wall02_03.png'; desc: ''),
    (id: $04 + 2 * STYLES_OFFSET; fileName: 'wall02_04.png'; desc: ''),
    (id: $05 + 2 * STYLES_OFFSET; fileName: 'wall02_05.png'; desc: ''),
    (id: $06 + 2 * STYLES_OFFSET; fileName: 'wall02_06.png'; desc: ''),
    (id: $07 + 2 * STYLES_OFFSET; fileName: 'wall02_07.png'; desc: ''),
    (id: $08 + 2 * STYLES_OFFSET; fileName: 'wall02_08.png'; desc: ''),
    (id: $09 + 2 * STYLES_OFFSET; fileName: 'wall02_09.png'; desc: ''),
    (id: $0a + 2 * STYLES_OFFSET; fileName: 'wall02_0a.png'; desc: ''),
    (id: $0b + 2 * STYLES_OFFSET; fileName: 'wall02_0b.png'; desc: ''),
    (id: $0c + 2 * STYLES_OFFSET; fileName: 'wall02_0c.png'; desc: ''),
    (id: $0d + 2 * STYLES_OFFSET; fileName: 'wall02_0d.png'; desc: ''),
    (id: $0e + 2 * STYLES_OFFSET; fileName: 'wall02_0e.png'; desc: ''),
    (id: $0f + 2 * STYLES_OFFSET; fileName: 'wall02_0f.png'; desc: ''),
    (id: $10 + 2 * STYLES_OFFSET; fileName: 'wall02_10.png'; desc: ''),
    (id: $33 + 2 * STYLES_OFFSET; fileName: 'breakable_wall_2_3.png'; desc: ''),
    (id: $34 + 2 * STYLES_OFFSET; fileName: 'breakable_wall_2_2.png'; desc: ''),
    (id: $35 + 2 * STYLES_OFFSET; fileName: 'breakable_wall_2_1.png'; desc: ''),
    (id: $01 + 3 * STYLES_OFFSET; fileName: 'wall03_01.png'; desc: ''),
    (id: $02 + 3 * STYLES_OFFSET; fileName: 'wall03_02.png'; desc: ''),
    (id: $03 + 3 * STYLES_OFFSET; fileName: 'wall03_03.png'; desc: ''),
    (id: $04 + 3 * STYLES_OFFSET; fileName: 'wall03_04.png'; desc: ''),
    (id: $05 + 3 * STYLES_OFFSET; fileName: 'wall03_05.png'; desc: ''),
    (id: $06 + 3 * STYLES_OFFSET; fileName: 'wall03_06.png'; desc: ''),
    (id: $07 + 3 * STYLES_OFFSET; fileName: 'wall03_07.png'; desc: ''),
    (id: $08 + 3 * STYLES_OFFSET; fileName: 'wall03_08.png'; desc: ''),
    (id: $09 + 3 * STYLES_OFFSET; fileName: 'wall03_09.png'; desc: ''),
    (id: $0a + 3 * STYLES_OFFSET; fileName: 'wall03_0a.png'; desc: ''),
    (id: $0b + 3 * STYLES_OFFSET; fileName: 'wall03_0b.png'; desc: ''),
    (id: $0c + 3 * STYLES_OFFSET; fileName: 'wall03_0c.png'; desc: ''),
    (id: $0d + 3 * STYLES_OFFSET; fileName: 'wall03_0d.png'; desc: ''),
    (id: $0e + 3 * STYLES_OFFSET; fileName: 'wall03_0e.png'; desc: ''),
    (id: $0f + 3 * STYLES_OFFSET; fileName: 'wall03_0f.png'; desc: ''),
    (id: $10 + 3 * STYLES_OFFSET; fileName: 'wall03_10.png'; desc: ''),
    (id: $33 + 3 * STYLES_OFFSET; fileName: 'breakable_wall_3_3.png'; desc: ''),
    (id: $34 + 3 * STYLES_OFFSET; fileName: 'breakable_wall_3_2.png'; desc: ''),
    (id: $35 + 3 * STYLES_OFFSET; fileName: 'breakable_wall_3_1.png'; desc: ''),
    (id: $01 + 4 * STYLES_OFFSET; fileName: 'wall04_01.png'; desc: ''),
    (id: $02 + 4 * STYLES_OFFSET; fileName: 'wall04_02.png'; desc: ''),
    (id: $03 + 4 * STYLES_OFFSET; fileName: 'wall04_03.png'; desc: ''),
    (id: $04 + 4 * STYLES_OFFSET; fileName: 'wall04_04.png'; desc: ''),
    (id: $05 + 4 * STYLES_OFFSET; fileName: 'wall04_05.png'; desc: ''),
    (id: $06 + 4 * STYLES_OFFSET; fileName: 'wall04_06.png'; desc: ''),
    (id: $07 + 4 * STYLES_OFFSET; fileName: 'wall04_07.png'; desc: ''),
    (id: $08 + 4 * STYLES_OFFSET; fileName: 'wall04_08.png'; desc: ''),
    (id: $09 + 4 * STYLES_OFFSET; fileName: 'wall04_09.png'; desc: ''),
    (id: $0a + 4 * STYLES_OFFSET; fileName: 'wall04_0a.png'; desc: ''),
    (id: $0b + 4 * STYLES_OFFSET; fileName: 'wall04_0b.png'; desc: ''),
    (id: $0c + 4 * STYLES_OFFSET; fileName: 'wall04_0c.png'; desc: ''),
    (id: $0d + 4 * STYLES_OFFSET; fileName: 'wall04_0d.png'; desc: ''),
    (id: $0e + 4 * STYLES_OFFSET; fileName: 'wall04_0e.png'; desc: ''),
    (id: $0f + 4 * STYLES_OFFSET; fileName: 'wall04_0f.png'; desc: ''),
    (id: $10 + 4 * STYLES_OFFSET; fileName: 'wall04_10.png'; desc: ''),
    (id: $33 + 4 * STYLES_OFFSET; fileName: 'breakable_wall_4_3.png'; desc: ''),
    (id: $34 + 4 * STYLES_OFFSET; fileName: 'breakable_wall_4_2.png'; desc: ''),
    (id: $35 + 4 * STYLES_OFFSET; fileName: 'breakable_wall_4_1.png'; desc: ''),
    (id: $01 + 5 * STYLES_OFFSET; fileName: 'wall05_01.png'; desc: ''),
    (id: $02 + 5 * STYLES_OFFSET; fileName: 'wall05_02.png'; desc: ''),
    (id: $03 + 5 * STYLES_OFFSET; fileName: 'wall05_03.png'; desc: ''),
    (id: $04 + 5 * STYLES_OFFSET; fileName: 'wall05_04.png'; desc: ''),
    (id: $05 + 5 * STYLES_OFFSET; fileName: 'wall05_05.png'; desc: ''),
    (id: $06 + 5 * STYLES_OFFSET; fileName: 'wall05_06.png'; desc: ''),
    (id: $07 + 5 * STYLES_OFFSET; fileName: 'wall05_07.png'; desc: ''),
    (id: $08 + 5 * STYLES_OFFSET; fileName: 'wall05_08.png'; desc: ''),
    (id: $09 + 5 * STYLES_OFFSET; fileName: 'wall05_09.png'; desc: ''),
    (id: $0a + 5 * STYLES_OFFSET; fileName: 'wall05_0a.png'; desc: ''),
    (id: $0b + 5 * STYLES_OFFSET; fileName: 'wall05_0b.png'; desc: ''),
    (id: $0c + 5 * STYLES_OFFSET; fileName: 'wall05_0c.png'; desc: ''),
    (id: $0d + 5 * STYLES_OFFSET; fileName: 'wall05_0d.png'; desc: ''),
    (id: $0e + 5 * STYLES_OFFSET; fileName: 'wall05_0e.png'; desc: ''),
    (id: $0f + 5 * STYLES_OFFSET; fileName: 'wall05_0f.png'; desc: ''),
    (id: $10 + 5 * STYLES_OFFSET; fileName: 'wall05_10.png'; desc: ''),
    (id: $33 + 5 * STYLES_OFFSET; fileName: 'breakable_wall_5_3.png'; desc: ''),
    (id: $34 + 5 * STYLES_OFFSET; fileName: 'breakable_wall_5_2.png'; desc: ''),
    (id: $35 + 5 * STYLES_OFFSET; fileName: 'breakable_wall_5_1.png'; desc: ''),
    (id: $01 + 6 * STYLES_OFFSET; fileName: 'wall06_01.png'; desc: ''),
    (id: $02 + 6 * STYLES_OFFSET; fileName: 'wall06_02.png'; desc: ''),
    (id: $03 + 6 * STYLES_OFFSET; fileName: 'wall06_03.png'; desc: ''),
    (id: $04 + 6 * STYLES_OFFSET; fileName: 'wall06_04.png'; desc: ''),
    (id: $05 + 6 * STYLES_OFFSET; fileName: 'wall06_05.png'; desc: ''),
    (id: $06 + 6 * STYLES_OFFSET; fileName: 'wall06_06.png'; desc: ''),
    (id: $07 + 6 * STYLES_OFFSET; fileName: 'wall06_07.png'; desc: ''),
    (id: $08 + 6 * STYLES_OFFSET; fileName: 'wall06_08.png'; desc: ''),
    (id: $09 + 6 * STYLES_OFFSET; fileName: 'wall06_09.png'; desc: ''),
    (id: $0a + 6 * STYLES_OFFSET; fileName: 'wall06_0a.png'; desc: ''),
    (id: $0b + 6 * STYLES_OFFSET; fileName: 'wall06_0b.png'; desc: ''),
    (id: $0c + 6 * STYLES_OFFSET; fileName: 'wall06_0c.png'; desc: ''),
    (id: $0d + 6 * STYLES_OFFSET; fileName: 'wall06_0d.png'; desc: ''),
    (id: $0e + 6 * STYLES_OFFSET; fileName: 'wall06_0e.png'; desc: ''),
    (id: $0f + 6 * STYLES_OFFSET; fileName: 'wall06_0f.png'; desc: ''),
    (id: $10 + 6 * STYLES_OFFSET; fileName: 'wall06_10.png'; desc: ''),
    (id: $33 + 6 * STYLES_OFFSET; fileName: 'breakable_wall_6_3.png'; desc: ''),
    (id: $34 + 6 * STYLES_OFFSET; fileName: 'breakable_wall_6_2.png'; desc: ''),
    (id: $35 + 6 * STYLES_OFFSET; fileName: 'breakable_wall_6_1.png'; desc: ''),
    (id: $01 + 7 * STYLES_OFFSET; fileName: 'wall07_01.png'; desc: ''),
    (id: $02 + 7 * STYLES_OFFSET; fileName: 'wall07_02.png'; desc: ''),
    (id: $03 + 7 * STYLES_OFFSET; fileName: 'wall07_03.png'; desc: ''),
    (id: $04 + 7 * STYLES_OFFSET; fileName: 'wall07_04.png'; desc: ''),
    (id: $05 + 7 * STYLES_OFFSET; fileName: 'wall07_05.png'; desc: ''),
    (id: $06 + 7 * STYLES_OFFSET; fileName: 'wall07_06.png'; desc: ''),
    (id: $07 + 7 * STYLES_OFFSET; fileName: 'wall07_07.png'; desc: ''),
    (id: $08 + 7 * STYLES_OFFSET; fileName: 'wall07_08.png'; desc: ''),
    (id: $09 + 7 * STYLES_OFFSET; fileName: 'wall07_09.png'; desc: ''),
    (id: $0a + 7 * STYLES_OFFSET; fileName: 'wall07_0a.png'; desc: ''),
    (id: $0b + 7 * STYLES_OFFSET; fileName: 'wall07_0b.png'; desc: ''),
    (id: $0c + 7 * STYLES_OFFSET; fileName: 'wall07_0c.png'; desc: ''),
    (id: $0d + 7 * STYLES_OFFSET; fileName: 'wall07_0d.png'; desc: ''),
    (id: $0e + 7 * STYLES_OFFSET; fileName: 'wall07_0e.png'; desc: ''),
    (id: $0f + 7 * STYLES_OFFSET; fileName: 'wall07_0f.png'; desc: ''),
    (id: $10 + 7 * STYLES_OFFSET; fileName: 'wall07_10.png'; desc: ''),
    (id: $33 + 7 * STYLES_OFFSET; fileName: 'breakable_wall_7_3.png'; desc: ''),
    (id: $34 + 7 * STYLES_OFFSET; fileName: 'breakable_wall_7_2.png'; desc: ''),
    (id: $35 + 7 * STYLES_OFFSET; fileName: 'breakable_wall_7_1.png'; desc: '')
    );

  HEAD_DSK: array[0..160] of byte = (
    $FD, $7E, $4E, $FE, $80, $20, $13, $FD, $77, $4D, $FD, $36, $4E, $C0, $21, $B8,
    $D0, $11, $00, $C8, $01, $00, $08, $ED, $B0, $C9, $0E, $FF, $11, $FF, $FF, $CD,
    $78, $D0, $4F, $FD, $CB, $FF, $76, $28, $06, $CD, $87, $D0, $4F, $18, $0C, $FD,
    $7E, $3B, $FE, $04, $30, $05, $CD, $87, $D0, $18, $03, $CD, $78, $D0, $57, $CD,
    $78, $D0, $5F, $CD, $78, $D0, $ED, $53, $B6, $D0, $32, $B5, $D0, $79, $32, $6C,
    $D0, $11, $00, $C8, $DD, $21, $B5, $D0, $06, $03, $DD, $7E, $00, $DD, $23, $C5,
    $D5, $CD, $8F, $D0, $EB, $D1, $ED, $B0, $C1, $10, $EF, $3E, $00, $CD, $8F, $D0,
    $D5, $DD, $E1, $FD, $36, $4D, $07, $C9, $CD, $00, $85, $E6, $07, $BB, $28, $F8,
    $BA, $28, $F5, $B9, $28, $F2, $C9, $CD, $00, $85, $E6, $01, $C6, $08, $C9, $11,
    $B8, $D0, $21, $A1, $D0, $3C, $4E, $23, $46, $23, $3D, $C8, $EB, $09, $EB, $18,
    $F5
    );

var
  ilMap: TImageList;
  ilTools: TImageList;
  resourcesDir: string;
  patternIndexMap: array of integer;
  transaction: TSQLTransaction;
  dbConn: TSQLite3Connection;
  HomeDir: string;

procedure loadGraphics(AOwner: TComponent; AWidth: integer);
procedure InitData;
procedure CleanData;
procedure GauntDebugLn(ATextLine: string);
function GetUUID: string;
function FindPatternDataById(APatternID: integer): TPictureIndex;
function ImportBlock(fs: TFileStream; AType: TGauntVersion): TGauntBlock;
procedure ExportBlock(fs: TFileStream; ABlock: TGauntBlock; AType: TGauntVersion);

implementation

constructor TGauntMaze.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FTraceLayer := TList.Create;
  self.FBuffer := TMemoryStream.Create;
  self.InitVisitedData;
  self.InitMapData;
  self.FStyle := gauntStyles[0];
  self.FHorzWrap := False;
  self.FVertWrap := False;
  self.FStunPlayers := False;
  self.FHurtPlayers := False;
  self.Fid := GetUUID;
  self.FCreationDate := now;
  self.FLastSavedDate := now;
  self.FPlayerPos.X := 1;
  self.FPlayerPos.Y := 1;
  self.SetPlayerPos(1, 1);
end;

destructor TGauntMaze.Destroy;
begin
  //FTraceLayer.Free;
  self.FBuffer.Free;
  inherited Destroy;
end;

procedure TGauntMaze.SetPlayerPos(col: byte; row: byte);
begin
  //check if the new position is empty
  if self.MapData[col, row] = 0 then
  begin
    //remove old position from the map
    self.MapData[FPlayerPos.X, FPlayerPos.Y] := 0;

    //update position as a property
    self.FPlayerPos.X := col;
    self.FPlayerPos.Y := row;

    //update the map
    self.MapData[col, row] := $3f;
  end;
end;

function TGauntMaze.GetPlayerPos: TPoint;
begin
  Result.X := self.FPlayerPos.X;
  Result.Y := 0;
end;

function TGauntMaze.ProcessMap: integer;
var
  TraceLayerSize: integer;
  ObjectLayerSize: integer;
  tmpbyte: byte;
  block: TGauntBlock;      //TEMP DELETE!!!
  fsExport: TFileStream;
  fsSave: TFileStream;
  fsExportBlock: TFileStream;
begin
  if CountExits < 1 then
  begin
    Result := -3;
    Exit;
  end;
  TraceLayerSize := ProcessTraceLayer;
  if TraceLayerSize > 255 then
  begin
    Result := -1;    //trace layer size not allowed!!
    Exit;
  end;
  ObjectLayerSize := ProcessObjectLayer;
  if (4 + ObjectLayersize + TraceLayerSize) > 511 then
  begin
    Result := -2;    //object layer size not allowed!!
    Exit;
  end;

  Result := ObjectLayerSize + TraceLayerSize + 4;
  //Set header bytes
  FBuffer.Seek(0, TSeekOrigin.soBeginning);
  FBuffer.WriteByte(Result and $ff);
  //bits 0-7 of size go here
  //process flags
  tmpbyte := 0;
  if self.FHorzWrap then tmpbyte := tmpbyte + $80;
  if self.FVertWrap then tmpbyte := tmpbyte + $40;
  if self.FStunPlayers then tmpbyte := tmpbyte + $02;
  if self.FHurtPlayers then tmpbyte := tmpbyte + $01;
  FBuffer.WriteByte(tmpbyte);
  //process Style and MSB of size
  tmpbyte := (Result and $100) shr 1;
  tmpbyte := tmpbyte + (self.Style.id shl 3);
  FBuffer.WriteByte(tmpbyte);
  //process trace layer size
  FBuffer.WriteByte(TraceLayerSize);
  self.FSize := Result;

  //TEMP, DELETE !!!!
  fsExport := TFileStream.Create('test.dat', fmCreate);
  self.ExportToFileStream(fsExport);
  fsSave := TFileStream.Create('test.obj', fmCreate);
  self.ToFileStream(fsSave);
  block[0] := self;
  block[1] := self;
  block[2] := self;
  block[3] := self;
  block[4] := self;
  block[5] := self;
  block[6] := self;
  block[7] := self;
  block[8] := self;
  block[9] := self;
  fsExportBlock := TFileStream.Create('MAZE01', fmCreate);
  uData.ExportBlock(fsExportBlock, block, gvDSK);
  fsExport.Free;
  fsSave.Free;
  //fsExportBlock.Free;

end;

function TGauntMaze.AnyObjectsLeft(offset: integer): boolean;
var
  d: byte;
begin
  while offset < (32 * 32) do
  begin
    d := PeekMapData(offset);
    if not ((d = 0) or IsAWall(d) or IsATrapWall(d) or IsAGate(d)) then
    begin
      Result := True;
      break;
    end
    else
    begin
      Result := False;
      Inc(offset);
    end;
  end;
end;

function TGauntMaze.ProcessObjectLayer: integer;
var
  addr: integer;
  v: byte;
  Data: byte;
begin
  Result := 0;  //size = 0 at start

  addr := 0;
  repeat
    Data := self.PeekMapData(addr);
    case Data of
      0..$12, $80..$92:
      begin
        //first check if there are no objects left, in such case we can exit
        if not self.AnyObjectsLeft(addr) then Exit;

        //count spaces;
        v := CountConsecutiveSpaces(addr);
        self.FBuffer.WriteByte($80 + v - 1);
        Result := Result + 1;
        //-1 is to adjust range to from 0-127 to 0-128 when it's parsed by the game
        addr := addr + v;
      end;
      $13..$7f, $93..$ff:
      begin
        //there is at least one object here, and that's sure, so let's write it to the file
        self.FBuffer.WriteByte(Data);
        Result := Result + 1;
        //now let's count how many if its kind there are ahead
        v := CountConsecutiveObjects(addr, Data);
        if v > 0 then
        begin
          self.FBuffer.WriteByte(v - 1);
          Result := Result + 1;
        end;
        addr := addr + v + 1;
      end;
    end;

  until addr >= (32 * 32);

end;

function TGauntMaze.CountConsecutiveObjects(offset: integer; Data: byte): integer;
var
  Value: byte;
const
  MAX_OBJECTS = $13;
begin
  Value := Data;
  Result := 0;
  offset := offset + 1;
  while (Result < MAX_OBJECTS) and (offset < (32 * 32)) and (Value = Data) do
  begin
    Value := PeekMapData(offset);
    if Value = Data then Result := Result + 1;
    offset := offset + 1;
  end;

end;

function TGauntMaze.CountConsecutiveSpaces(offset: integer): integer;
var
  Value: byte;
const
  MAX_SPACES = 128;
begin
  Value := 0;
  Result := 1;
  offset := offset + 1;
  while (Result < MAX_SPACES) and (offset < (32 * 32)) and (Value < $12) do
  begin
    Value := PeekMapData(offset);
    if (Value <= $12) or ((Value >= $80) and (Value <= $92)) then Result := Result + 1;
    offset := offset + 1;
  end;

  // >$12 means an object
end;

function TGauntMaze.ProcessTraceLayer: integer;
begin
  Result := 0;
  //The trace layer starts at pos +4 of the buffer
  self.FBuffer.Seek(4, TSeekOrigin.soBeginning);
  Result := Result + FindTraces(TSearchTraceType.sttWall);
  Result := Result + FindTraces(TSearchTraceType.sttTrapWall);
  Result := Result + FindTraces(TSearchTraceType.sttGate);
  if CountExits > 2 then
    //if there are only one or two exits it's better to use the object RLE encoding
    Result := Result + FindTraces(TSearchTraceType.sttExit);
end;

function TGauntMaze.FindTraces(Atype: TSearchTraceType): integer;
type
  TSearchDir = record
    colInc: smallint;
    rowInc: smallint;
    byteCode: byte;
  end;

  TTraceStroke = record
    dir: TSearchDir;
    steps: byte;
  end;

  TTraceSeq = record
    Xorigin: byte;
    Yorigin: byte;
    strokes: array of TTraceStroke;
  end;
const
  SearchDirs: array [0..7] of TSearchDir = (
    (colInc: 0; rowInc: -1; byteCode: $00),              //UP
    (colInc: -1; rowInc: -1; byteCode: $20),             //UP-LEFT
    (colInc: 1; rowInc: 0; byteCode: $40),               //RIGHT
    (colInc: 1; rowInc: 1; byteCode: $60),               //DOWN-RIGHT
    (colInc: 0; rowInc: 1; byteCode: $80),               //DOWN
    (colInc: -1; rowInc: 1; byteCode: $a0),              //DOWN-LEFT
    (colInc: -1; rowInc: 0; byteCode: $c0),              //LEFT
    (colInc: 1; rowInc: -1; byteCode: $e0)               //UP-RIGHT
    );
var
  row, col: integer;
  searchingRow, searchingCol: integer;
  CurrentRow, CurrentCol: integer;
  i, j: integer;
  firstColumn: integer;

  Seq: TTraceSeq;
  CurrentLength, BestLength: integer;
  BestDir: integer;
begin
  InitVisitedData;
  Result := 0;
  if self.FHorzWrap then firstColumn := 0
  else
    firstColumn := 1;

  for row := 1 to 31 do      //first row always has a wall even if vert wrap is true
    for col := firstColumn to 31 do
    begin
      if IsLookedType(self.MapData[col, row], Atype) and
        (self.FVisitedData[col, row] = 0) then
      begin
        //initialize variables for a new sequence of strokes
        Seq.strokes := [];
        Seq.Xorigin := col;
        Seq.Yorigin := row;
        CurrentRow := row;
        CurrentCol := col;
        FVisitedData[col, row] := 1;
        while True do
        begin
          BestDir := -1;
          BestLength := 0;
          for j := 0 to length(SearchDirs) - 1 do
          begin
            CurrentLength := 0;
            for i := 1 to 31 do
            begin
              searchingCol := CurrentCol + SearchDirs[j].colInc * i;
              searchingRow := CurrentRow + SearchDirs[j].rowInc * i;
              if (searchingCol >= firstColumn) and (searchingCol < 32) and
                (searchingRow >= 1) and (searchingRow < 32) and
                (FVisitedData[searchingCol, searchingRow] = 0) and
                IsLookedType(self.MapData[searchingCol, searchingRow], Atype) then
              begin
                CurrentLength := CurrentLength + 1;
              end
              else
                break;
            end;
            if CurrentLength > BestLength then
            begin
              BestLength := CurrentLength;
              BestDir := j;
            end;
          end;
          if BestDir > -1 then
          begin
            //mark all cells of that stroke as already visited
            for i := 1 to BestLength + 1 do
            begin
              searchingCol := CurrentCol + SearchDirs[BestDir].colInc * i;
              searchingRow := CurrentRow + SearchDirs[BestDir].rowInc * i;
              if (searchingCol < 32) and (searchingRow < 32) then
                FVisitedData[searchingCol, searchingRow] := 1;
              //CHECK this might write outbounds the array   (checked)
            end;
            //add it to the sequence
            setLength(Seq.strokes, length(Seq.strokes) + 1);
            Seq.strokes[length(Seq.strokes) - 1].dir := SearchDirs[BestDir];
            Seq.strokes[length(Seq.strokes) - 1].steps := BestLength;

            CurrentCol := CurrentCol + SearchDirs[BestDir].colInc * BestLength;
            CurrentRow := CurrentRow + SearchDirs[BestDir].rowInc * BestLength;
          end
          else
          begin
            break;
          end;
        end;
        //Write the sequence to the buffer
        self.FBuffer.WriteByte(GetTraceOriginCode(AType) + Seq.Xorigin);
        self.FBuffer.WriteByte(GetTraceOriginCode(AType) + Seq.Yorigin);
        //update size of this trace type
        Result := Result + 2;

        for i := 0 to length(Seq.strokes) - 1 do
        begin
          self.FBuffer.WriteByte(Seq.strokes[i].dir.byteCode + Seq.strokes[i].steps
            );
          Result := Result + 1;
        end;
      end;
    end;
end;

function TGauntMaze.GetTraceOriginCode(AType: TSearchTraceType): byte;
begin
  case AType of
    sttWall:
      Result := $e0;
    sttTrapWall:
      Result := $c0;
    sttGate:
      Result := $40;
    //needs to be convered to $80 somewhere if the first gate of the sequence is verticalk
    sttExit:
      Result := $20;
  end;
end;

function TGauntMaze.IsAWall(cell: byte): boolean;
begin
  if (cell >= 1) and (cell <= $10) then
    Result := True
  else
    Result := False;
end;

function TGauntMaze.IsAGate(cell: byte): boolean;
begin
  if cell in [$11, $12] then
    Result := True
  else
    Result := False;
end;

function TGauntMaze.IsATrapWall(cell: byte): boolean;
begin
  if (cell >= $81) and (cell <= $90) then
    Result := True
  else
    Result := False;
end;

function TGauntMaze.IsLookedType(cell: byte; Atype: TSearchTraceType): boolean;
begin
  case Atype of
    TSearchTraceType.sttWall:
      Result := IsAWall(cell);
    TSearchTraceType.sttTrapWall:
      Result := IsATrapWall(cell);
    TSearchTraceType.sttGate:
      Result := IsAGate(cell);
    TSearchTraceType.sttExit:
      if cell = $36 then Result := True
      else
        Result := False;
  end;
end;

function TGauntMaze.PeekMapData(offset: integer): byte;
begin
  Result := self.MapData[offset mod 32, offset div 32];
end;

procedure TGauntMaze.PokeMapData(offset: integer; Value: byte);
begin
  self.MapData[offset mod 32, offset div 32] := Value;
end;

function TGauntMaze.CountExits: integer;
var
  col, row: integer;
begin
  Result := 0;
  for row := 0 to 31 do
    for col := 0 to 31 do
      if self.MapData[col, row] = $36 then Result := Result + 1;

end;

procedure TGauntMaze.SetHorzWrap(w: boolean);
var
  i: integer;
  f: integer;
begin
  if w then f := 0   //no wall
  else
    f := $05;      //vertical wall

  for i := 0 to 31 do
  begin
    self.MapData[0, i] := f;
  end;

  if not w then
  begin
    //add also the corner
    self.MapData[0, 0] := $06;
  end
  else
  begin
    //add regular horz block
    self.MapData[0, 0] := $0a;
  end;
  self.FHorzWrap := w;

end;

procedure TGauntMaze.SetVertWrap(w: boolean);
begin
  self.FVertWrap := w;
end;

function TGauntMaze.GetHorzWrap(): boolean;
begin
  Result := self.FHorzWrap;
end;

function TGauntMaze.GetVertWrap(): boolean;
begin
  Result := self.FVertWrap;
end;

function TGauntMaze.getSize: integer;
begin
  Result := FSize;
end;

function TGauntMaze.getItemsLayersize: integer;
begin
  Result := 0;
end;

procedure InitData;
begin
  //Get directories
  HomeDir := IncludeTrailingPathDelimiter(GetUserdir);
  if not DirectoryExists(HomeDir + APPDATA_DIR, True) then
    CreateDir(HomeDir + APPDATA_DIR);

  resourcesDir := ExtractFilePath(ParamStr(0)) + RESOURCES_DIR;

  //Initialize database stuff
  transaction := TSQLTransaction.Create(nil);
  dbConn := TSQLite3Connection.Create(nil);
  dbConn.Transaction := transaction;
  dbConn.DatabaseName := HomeDir + APPDATA_DIR + DATABASE_FILENAME;
  dbConn.CreateDB;
  dbConn.Open;
  //Create tables if don't exist
  if dbConn.Connected then
  begin
    debugln('Creating table MAZES.');
    try
      dbConn.ExecuteDirect(
        'CREATE TABLE IF NOT EXISTS MAZES (UUID TEXT PRIMARY KEY, Name TEXT, Style INTEGER, H_wrap INTEGER, V_wrap INTEGER, can_stun INTEGER, can_hurt INTEGER, created TEXT, modified TEXT, map_data TEXT)');
      dbConn.ExecuteDirect(
        'CREATE TABLE IF NOT EXISTS BLOCKS (ID INTEGER PRIMARY KEY AUTOINCREMENT, Name TEXT, created TEXT, modified TEXT, slot_0 TEXT, slot_1 TEXT, slot_2 TEXT, slot_3 TEXT, slot_4 TEXT, slot_5 TEXT, slot_6 TEXT, slot_7 TEXT)');
      dbConn.ExecuteDirect(
        'CREATE TABLE IF NOT EXISTS COLLECTIONS (ID INTEGER PRIMARY KEY AUTOINCREMENT, Name TEXT, created TEXT, modified TEXT)');
      dbConn.ExecuteDirect(
        'CREATE TABLE IF NOT EXISTS COL_BLOCKS (ID INTEGER PRIMARY KEY AUTOINCREMENT, block_id INTEGER, collection_id INTEGER, FOREIGN KEY(block_id) REFERENCES BLOCKS(ID), FOREIGN KEY(collection_id) REFERENCES COLLECTIONS(ID))');
      transaction.Commit;
    except
      on E: Exception do
        debugln('Error: ', E.message);
    end;
  end
  else
  begin
    debugln('There was a problem creating or opening the persistence layer.');
  end;

end;

procedure CleanData;
begin
  patternIndex := [];
  dbConn.Close(True);
  dbConn.Free;
  transaction.Free;
end;

procedure loadGraphics(AOwner: TComponent; AWidth: integer);
var
  tmpPic: TPicture;
  bmps: array [0..0] of TCustomBitmap;
  i: integer;
begin
  setLength(patternIndexMap, STYLES_OFFSET * length(gauntStyles));

  //Create objects
  ilMap := TImageList.Create(AOwner);

  ilMap.Width := AWidth;
  ilMap.Height := ilMap.Width;
  ilMap.RegisterResolutions(Widths);

  ilTools := TImageList.Create(AOwner);
  tmpPic := TPicture.Create;

  //Load map objects
  for i := 0 to length(patternIndex) - 1 do
  begin
    try
      tmpPic.LoadFromFile(resourcesDir + patternIndex[i].fileName);
      bmps[0] := tmpPic.Bitmap;
      patternIndexMap[patternIndex[i].id] := ilMap.AddMultipleResolutions(bmps);
      patternIndexMap[patternIndex[i].id + TRAP_OFFSET] :=
        patternIndexMap[patternIndex[i].id];

    except
      on E: Exception do DebugLn('Error loading ' + patternIndex[i].fileName +
          ': ' + E.Message);
    end;
  end;
  tmpPic.Free;
end;

procedure TGauntMaze.InitMap(var map: TGauntMap);
var
  x, y: integer;
begin
  for x := 0 to 31 do
    for y := 0 to 31 do
      map[x, y] := 0;
end;

procedure TGauntMaze.InitMapData;
var
  i: integer;
begin
  self.InitMap(self.MapData);
  //add top border of the dungeon
  for i := 0 to 31 do
    self.MapData[i, 0] := $0a;

  //add left border if not wrapping H
  if not self.FHorzWrap then
  begin
    for i := 0 to 31 do
    begin
      self.MapData[0, i] := $05;
    end;
    //corner
    self.MapData[0, 0] := $06;
  end;
end;

procedure TGauntMaze.InitItemData;
begin
  self.InitMap(self.ItemData);
end;

procedure TGauntMaze.InitVisitedData;
begin
  self.InitMap(self.FVisitedData);
end;

procedure TGauntMaze.ExportToFileStream(fs: TFileStream);
var
  i: integer;
begin
  try
    //guarantee the TMemoryStream is at pos 0
    self.FBuffer.Seek(0, soBeginning);

    //set the target file to append data
    fs.Seek(0, TSeekOrigin.soEnd);
    for i := 0 to self.FBuffer.Size - 1 do
    begin
      fs.WriteByte(FBuffer.ReadByte);
    end;

  except
    on E: Exception do GauntDebugLn('Error exporting maze ' + self.Name +
        ' to file ' + fs.FileName + ': ' + E.Message);
  end;
  //fs.Free;
end;

procedure TGauntMaze.ToFileStream(fs: TFileStream);
var
  i: integer;
begin

  //set the target file to append data
  fs.Seek(0, TSeekOrigin.soEnd);
  try
    fs.WriteAnsiString(self.Name);
    fs.WriteByte(self.GetPlayerPos.X);
    fs.WriteByte(self.GetPlayerPos.Y);
    fs.WriteByte(Ord(self.FHorzWrap));
    fs.WriteByte(Ord(self.FVertWrap));
    fs.WriteByte(Ord(self.FHurtPlayers));
    fs.WriteByte(Ord(self.FStunPlayers));
    fs.WriteByte(self.FStyle.id);
    for i := 0 to 32 * 32 - 1 do
    begin
      fs.WriteByte(self.PeekMapData(i));
    end;
  except
    on E: Exception do GauntDebugLn('Error saving maze ' + self.Name +
        ' to file ' + fs.FileName + ': ' + E.Message);
  end;
  //fs.Free;
end;

procedure TGauntMaze.FromFileStream(fs: TFileStream);
var
  i: integer;
begin

  try
    self.Name := fs.ReadAnsiString;
    self.FPlayerPos.X := fs.ReadByte;
    self.FPlayerPos.Y := fs.ReadByte;
    self.FHorzWrap := fs.ReadByte <> 0;
    self.FVertWrap := fs.ReadByte <> 0;
    self.FHurtPlayers := fs.ReadByte <> 0;
    self.FStunPlayers := fs.ReadByte <> 0;
    self.FStyle := gauntStyles[fs.ReadByte];
    for i := 0 to 32 * 32 - 1 do
    begin
      self.PokeMapData(i, fs.ReadByte);
    end;
  except
    on E: Exception do GauntDebugLn('Error loading maze ' + self.Name +
        ' to file ' + fs.FileName + ': ' + E.Message);
  end;
  fs.Free;
end;

procedure GauntDebugLn(ATextLine: string);
begin
  DebugLn(ATextLine);
end;

function GetUUID: string;
var
  GUID: TGUID;
begin
  if CreateGUID(GUID) = 0 then
  begin
    Result := GUIDToString(GUID);
  end
  else
    Result := '';
end;

function FindPatternDataById(APatternID: integer): TPictureIndex;
var
  i: integer;
begin
  for i := 0 to length(patternIndex) do
    if patternIndex[i].id = APatternID then
    begin
      Result := patternIndex[i];
      break;
    end;
end;

function ImportBlock(fs: TFileStream; AType: TGauntVersion): TGauntBlock;
begin
  Result[0] := nil;
end;

procedure ExportBlock(fs: TFileStream; ABlock: TGauntBlock; AType: TGauntVersion);

  procedure writeBinHeader(fs: TFileStream);
  begin
    //write BSAVE header
    fs.WriteByte($fe);
    fs.WriteWord($d000);
    fs.WriteWord($de80);
    fs.WriteWord($d000);
  end;

  procedure writeBlockHeader(AHeader: array of byte; fs: TFileStream);
  var
    i: integer;
  begin
    for i := 0 to length(AHeader) - 1 do
    begin
      fs.WriteByte(AHeader[i]);
    end;
  end;

var
  i: integer;
begin
  try
    case AType of
      gvDSK:
      begin
        //write header
        writeBinHeader(fs);
        writeBlockHeader(HEAD_DSK, fs);
      end;
      else
      begin

      end;
    end;
    //common structure goes here

    //table of sizes
    for i := 0 to length(ABlock) - 1 do
    begin
      fs.WriteWord(word(ABlock[i].FSize));
    end;
    // random selection variables (3 dummy bytes)

    fs.WriteByte(0);
    fs.WriteByte(0);
    fs.WriteByte(0);


    for i := 0 to length(ABlock) - 1 do
    begin
      ABlock[i].ExportToFileStream(fs);
    end;
  except
    on E: Exception do
    begin
      GauntDebugLn('Error writing maze to file ' + fs.FileName + ': ' + E.Message);
    end;
  end;
  fs.Free;
end;

end.
