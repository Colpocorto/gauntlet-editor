unit uData;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LazLoggerBase;

type
  TGauntMap = array[0..31, 0..31] of byte;
  TLayer = (background, objects, positions);
  TGauntOrgType = (wall, trap_bound_wall, gate_h, gate_v, exit);
  TGauntTraceDir = (up, up_left, right, down_right, down, down_left, left, up_right);

  TGauntStyle = record
    id: integer;
    Name: string;
    desc: string;
  end;
  TCustomGauntTraceCmd = class abstract(TObject);
  //TCustomGauntItemCmd = class abstract(TObject);
  TGauntTraceOrigin = class(TCustomGauntTraceCmd)
    col, row: smallint;
    orgType: TGauntOrgType;
  end;

  TGauntTrace = class(TCustomGauntTraceCmd)
    dir: TGauntTraceDir;
    steps: smallint;
  end;

  TGauntMaze = class (TPersistent)
  private
    FSize: integer;  //0-511
    FHorzWrap: boolean;
    FVertWrap: boolean;
    FFindThePotion: boolean;
    FStunPlayers: boolean;
    FHurtPlayers: boolean;
    //FTraceLayer: TList;
    //FItemsLayer: TList;
    FName: string;
    //FMapData: TGauntMap;
    FVisitedData: TGauntMap;
    FStyle: TGauntStyle;
    FBuffer: TMemoryStream;
    procedure InitMap(var map: TGauntMap);
  public
    MapData: TGauntMap;
    ItemData: TGauntMap;
    VisitedData: TGauntMap;

    constructor Create;
    destructor Destroy; override;
    function getTraceLayerSize(): integer;
    function getItemsLayersize(): integer;
    property Name: string read FName write FName;
    //property HorzWrap: boolean read FHorzWrap write FHorzWrap;
    //property VertWrap: boolean read FVertWrap write FVertWrap;
    property FindThePotion: boolean read FFindThePotion write FFindThePotion;
    property StunPlayers: boolean read FStunPlayers write FStunPlayers;
    property HurtPlayers: boolean read FHurtPlayers write FHurtPlayers;
    property Style: TGauntStyle read FStyle write FStyle;

    function getSize: integer;
    procedure SetHorzWrap(w: boolean);
    procedure SetVertWrap(w: boolean);
    function GetHorzWrap(): boolean;
    function GetVertWrap(): boolean;
    procedure InitMapData;
    procedure InitItemData;
    procedure InitVisitedData;
    procedure ToFileStream(fs: TFileStream);
  end;

  TPictureIndex = record
    id: integer;
    fileName: string;
  end;

const
  MAX_TRACE_SIZE = 255;
  MAX_ITEM_SIZE = (512 - 4 - MAX_TRACE_SIZE);
  RESOURCES_DIR = 'resources/';
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
    (id: $00; fileName: 'blank.png'),
    (id: $01 + 0 * STYLES_OFFSET; fileName: 'wall00_01.png'),
    (id: $02 + 0 * STYLES_OFFSET; fileName: 'wall00_02.png'),
    (id: $03 + 0 * STYLES_OFFSET; fileName: 'wall00_03.png'),
    (id: $04 + 0 * STYLES_OFFSET; fileName: 'wall00_04.png'),
    (id: $05 + 0 * STYLES_OFFSET; fileName: 'wall00_05.png'),
    (id: $06 + 0 * STYLES_OFFSET; fileName: 'wall00_06.png'),
    (id: $07 + 0 * STYLES_OFFSET; fileName: 'wall00_07.png'),
    (id: $08 + 0 * STYLES_OFFSET; fileName: 'wall00_08.png'),
    (id: $09 + 0 * STYLES_OFFSET; fileName: 'wall00_09.png'),
    (id: $0a + 0 * STYLES_OFFSET; fileName: 'wall00_0a.png'),
    (id: $0b + 0 * STYLES_OFFSET; fileName: 'wall00_0b.png'),
    (id: $0c + 0 * STYLES_OFFSET; fileName: 'wall00_0c.png'),
    (id: $0d + 0 * STYLES_OFFSET; fileName: 'wall00_0d.png'),
    (id: $0e + 0 * STYLES_OFFSET; fileName: 'wall00_0e.png'),
    (id: $0f + 0 * STYLES_OFFSET; fileName: 'wall00_0f.png'),
    (id: $10 + 0 * STYLES_OFFSET; fileName: 'wall00_10.png'),
    (id: $11; fileName: 'gate_h.png'),
    (id: $12; fileName: 'gate_v.png'),
    (id: $13; fileName: 'treasure.png'),
    (id: $14; fileName: 'cider.png'),
    (id: $15; fileName: 'ham.png'),
    (id: $16; fileName: 'potion.png'),
    (id: $17; fileName: 'unbreak_potion.png'),
    (id: $18; fileName: 'amulet.png'),
    (id: $19; fileName: 'x_armour_potion.png'),
    (id: $1a; fileName: 'x_pickup_potion.png'),
    (id: $1b; fileName: 'x_magic_potion.png'),
    (id: $1c; fileName: 'x_shot_pwr_potion.png'),
    (id: $1d; fileName: 'x_shot_spd_potion.png'),
    (id: $1e; fileName: 'x_fight_potion.png'),
    (id: $1f; fileName: 'key.png'),
    (id: $20; fileName: 'gen_ghost_1.png'),
    (id: $21; fileName: 'gen_ghost_2.png'),
    (id: $22; fileName: 'gen_ghost_3.png'),
    (id: $23; fileName: 'gen_grunt_1.png'),
    (id: $24; fileName: 'gen_grunt_1.png'),
    (id: $25; fileName: 'gen_grunt_1.png'),
    (id: $26; fileName: 'gen_grunt_1.png'),
    (id: $27; fileName: 'gen_grunt_1.png'),
    (id: $28; fileName: 'gen_grunt_1.png'),
    (id: $29; fileName: 'gen_grunt_1.png'),
    (id: $2a; fileName: 'gen_grunt_1.png'),
    (id: $2c; fileName: 'gen_grunt_1.png'),
    (id: $2d; fileName: 'gen_grunt_1.png'),
    (id: $2d; fileName: 'gen_grunt_1.png'),
    (id: $2e; fileName: 'gen_grunt_1.png'),
    (id: $2f; fileName: 'trap.png'),
    (id: $30; fileName: 'transporter.png'),
    (id: $31; fileName: 'bad_cider.png'),
    (id: $32; fileName: 'keyring.png'),
    (id: $33; fileName: 'breakable_wall_3.png'),
    (id: $34; fileName: 'breakable_wall_2.png'),
    (id: $35; fileName: 'breakable_wall_1.png'),
    (id: $36; fileName: 'exit.png'),
    (id: $40; fileName: 'ghost.png'),
    (id: $41; fileName: 'grunt.png'),
    (id: $42; fileName: 'demon.png'),
    (id: $43; fileName: 'lobber.png'),
    (id: $44; fileName: 'sorcerer.png'),
    (id: $45; fileName: 'death.png'),
    (id: $01 + 1 * STYLES_OFFSET; fileName: 'wall01_01.png'),
    (id: $02 + 1 * STYLES_OFFSET; fileName: 'wall01_02.png'),
    (id: $03 + 1 * STYLES_OFFSET; fileName: 'wall01_03.png'),
    (id: $04 + 1 * STYLES_OFFSET; fileName: 'wall01_04.png'),
    (id: $05 + 1 * STYLES_OFFSET; fileName: 'wall01_05.png'),
    (id: $06 + 1 * STYLES_OFFSET; fileName: 'wall01_06.png'),
    (id: $07 + 1 * STYLES_OFFSET; fileName: 'wall01_07.png'),
    (id: $08 + 1 * STYLES_OFFSET; fileName: 'wall01_08.png'),
    (id: $09 + 1 * STYLES_OFFSET; fileName: 'wall01_09.png'),
    (id: $0a + 1 * STYLES_OFFSET; fileName: 'wall01_0a.png'),
    (id: $0b + 1 * STYLES_OFFSET; fileName: 'wall01_0b.png'),
    (id: $0c + 1 * STYLES_OFFSET; fileName: 'wall01_0c.png'),
    (id: $0d + 1 * STYLES_OFFSET; fileName: 'wall01_0d.png'),
    (id: $0e + 1 * STYLES_OFFSET; fileName: 'wall01_0e.png'),
    (id: $0f + 1 * STYLES_OFFSET; fileName: 'wall01_0f.png'),
    (id: $10 + 1 * STYLES_OFFSET; fileName: 'wall01_10.png'),
    (id: $01 + 2 * STYLES_OFFSET; fileName: 'wall02_01.png'),
    (id: $02 + 2 * STYLES_OFFSET; fileName: 'wall02_02.png'),
    (id: $03 + 2 * STYLES_OFFSET; fileName: 'wall02_03.png'),
    (id: $04 + 2 * STYLES_OFFSET; fileName: 'wall02_04.png'),
    (id: $05 + 2 * STYLES_OFFSET; fileName: 'wall02_05.png'),
    (id: $06 + 2 * STYLES_OFFSET; fileName: 'wall02_06.png'),
    (id: $07 + 2 * STYLES_OFFSET; fileName: 'wall02_07.png'),
    (id: $08 + 2 * STYLES_OFFSET; fileName: 'wall02_08.png'),
    (id: $09 + 2 * STYLES_OFFSET; fileName: 'wall02_09.png'),
    (id: $0a + 2 * STYLES_OFFSET; fileName: 'wall02_0a.png'),
    (id: $0b + 2 * STYLES_OFFSET; fileName: 'wall02_0b.png'),
    (id: $0c + 2 * STYLES_OFFSET; fileName: 'wall02_0c.png'),
    (id: $0d + 2 * STYLES_OFFSET; fileName: 'wall02_0d.png'),
    (id: $0e + 2 * STYLES_OFFSET; fileName: 'wall02_0e.png'),
    (id: $0f + 2 * STYLES_OFFSET; fileName: 'wall02_0f.png'),
    (id: $10 + 2 * STYLES_OFFSET; fileName: 'wall02_10.png'),
    (id: $01 + 3 * STYLES_OFFSET; fileName: 'wall03_01.png'),
    (id: $02 + 3 * STYLES_OFFSET; fileName: 'wall03_02.png'),
    (id: $03 + 3 * STYLES_OFFSET; fileName: 'wall03_03.png'),
    (id: $04 + 3 * STYLES_OFFSET; fileName: 'wall03_04.png'),
    (id: $05 + 3 * STYLES_OFFSET; fileName: 'wall03_05.png'),
    (id: $06 + 3 * STYLES_OFFSET; fileName: 'wall03_06.png'),
    (id: $07 + 3 * STYLES_OFFSET; fileName: 'wall03_07.png'),
    (id: $08 + 3 * STYLES_OFFSET; fileName: 'wall03_08.png'),
    (id: $09 + 3 * STYLES_OFFSET; fileName: 'wall03_09.png'),
    (id: $0a + 3 * STYLES_OFFSET; fileName: 'wall03_0a.png'),
    (id: $0b + 3 * STYLES_OFFSET; fileName: 'wall03_0b.png'),
    (id: $0c + 3 * STYLES_OFFSET; fileName: 'wall03_0c.png'),
    (id: $0d + 3 * STYLES_OFFSET; fileName: 'wall03_0d.png'),
    (id: $0e + 3 * STYLES_OFFSET; fileName: 'wall03_0e.png'),
    (id: $0f + 3 * STYLES_OFFSET; fileName: 'wall03_0f.png'),
    (id: $10 + 3 * STYLES_OFFSET; fileName: 'wall03_10.png'),
    (id: $01 + 4 * STYLES_OFFSET; fileName: 'wall04_01.png'),
    (id: $02 + 4 * STYLES_OFFSET; fileName: 'wall04_02.png'),
    (id: $03 + 4 * STYLES_OFFSET; fileName: 'wall04_03.png'),
    (id: $04 + 4 * STYLES_OFFSET; fileName: 'wall04_04.png'),
    (id: $05 + 4 * STYLES_OFFSET; fileName: 'wall04_05.png'),
    (id: $06 + 4 * STYLES_OFFSET; fileName: 'wall04_06.png'),
    (id: $07 + 4 * STYLES_OFFSET; fileName: 'wall04_07.png'),
    (id: $08 + 4 * STYLES_OFFSET; fileName: 'wall04_08.png'),
    (id: $09 + 4 * STYLES_OFFSET; fileName: 'wall04_09.png'),
    (id: $0a + 4 * STYLES_OFFSET; fileName: 'wall04_0a.png'),
    (id: $0b + 4 * STYLES_OFFSET; fileName: 'wall04_0b.png'),
    (id: $0c + 4 * STYLES_OFFSET; fileName: 'wall04_0c.png'),
    (id: $0d + 4 * STYLES_OFFSET; fileName: 'wall04_0d.png'),
    (id: $0e + 4 * STYLES_OFFSET; fileName: 'wall04_0e.png'),
    (id: $0f + 4 * STYLES_OFFSET; fileName: 'wall04_0f.png'),
    (id: $10 + 4 * STYLES_OFFSET; fileName: 'wall04_10.png'),
    (id: $01 + 5 * STYLES_OFFSET; fileName: 'wall05_01.png'),
    (id: $02 + 5 * STYLES_OFFSET; fileName: 'wall05_02.png'),
    (id: $03 + 5 * STYLES_OFFSET; fileName: 'wall05_03.png'),
    (id: $04 + 5 * STYLES_OFFSET; fileName: 'wall05_04.png'),
    (id: $05 + 5 * STYLES_OFFSET; fileName: 'wall05_05.png'),
    (id: $06 + 5 * STYLES_OFFSET; fileName: 'wall05_06.png'),
    (id: $07 + 5 * STYLES_OFFSET; fileName: 'wall05_07.png'),
    (id: $08 + 5 * STYLES_OFFSET; fileName: 'wall05_08.png'),
    (id: $09 + 5 * STYLES_OFFSET; fileName: 'wall05_09.png'),
    (id: $0a + 5 * STYLES_OFFSET; fileName: 'wall05_0a.png'),
    (id: $0b + 5 * STYLES_OFFSET; fileName: 'wall05_0b.png'),
    (id: $0c + 5 * STYLES_OFFSET; fileName: 'wall05_0c.png'),
    (id: $0d + 5 * STYLES_OFFSET; fileName: 'wall05_0d.png'),
    (id: $0e + 5 * STYLES_OFFSET; fileName: 'wall05_0e.png'),
    (id: $0f + 5 * STYLES_OFFSET; fileName: 'wall05_0f.png'),
    (id: $10 + 5 * STYLES_OFFSET; fileName: 'wall05_10.png'),
    (id: $01 + 6 * STYLES_OFFSET; fileName: 'wall06_01.png'),
    (id: $02 + 6 * STYLES_OFFSET; fileName: 'wall06_02.png'),
    (id: $03 + 6 * STYLES_OFFSET; fileName: 'wall06_03.png'),
    (id: $04 + 6 * STYLES_OFFSET; fileName: 'wall06_04.png'),
    (id: $05 + 6 * STYLES_OFFSET; fileName: 'wall06_05.png'),
    (id: $06 + 6 * STYLES_OFFSET; fileName: 'wall06_06.png'),
    (id: $07 + 6 * STYLES_OFFSET; fileName: 'wall06_07.png'),
    (id: $08 + 6 * STYLES_OFFSET; fileName: 'wall06_08.png'),
    (id: $09 + 6 * STYLES_OFFSET; fileName: 'wall06_09.png'),
    (id: $0a + 6 * STYLES_OFFSET; fileName: 'wall06_0a.png'),
    (id: $0b + 6 * STYLES_OFFSET; fileName: 'wall06_0b.png'),
    (id: $0c + 6 * STYLES_OFFSET; fileName: 'wall06_0c.png'),
    (id: $0d + 6 * STYLES_OFFSET; fileName: 'wall06_0d.png'),
    (id: $0e + 6 * STYLES_OFFSET; fileName: 'wall06_0e.png'),
    (id: $0f + 6 * STYLES_OFFSET; fileName: 'wall06_0f.png'),
    (id: $10 + 6 * STYLES_OFFSET; fileName: 'wall06_10.png'),
    (id: $01 + 7 * STYLES_OFFSET; fileName: 'wall07_01.png'),
    (id: $02 + 7 * STYLES_OFFSET; fileName: 'wall07_02.png'),
    (id: $03 + 7 * STYLES_OFFSET; fileName: 'wall07_03.png'),
    (id: $04 + 7 * STYLES_OFFSET; fileName: 'wall07_04.png'),
    (id: $05 + 7 * STYLES_OFFSET; fileName: 'wall07_05.png'),
    (id: $06 + 7 * STYLES_OFFSET; fileName: 'wall07_06.png'),
    (id: $07 + 7 * STYLES_OFFSET; fileName: 'wall07_07.png'),
    (id: $08 + 7 * STYLES_OFFSET; fileName: 'wall07_08.png'),
    (id: $09 + 7 * STYLES_OFFSET; fileName: 'wall07_09.png'),
    (id: $0a + 7 * STYLES_OFFSET; fileName: 'wall07_0a.png'),
    (id: $0b + 7 * STYLES_OFFSET; fileName: 'wall07_0b.png'),
    (id: $0c + 7 * STYLES_OFFSET; fileName: 'wall07_0c.png'),
    (id: $0d + 7 * STYLES_OFFSET; fileName: 'wall07_0d.png'),
    (id: $0e + 7 * STYLES_OFFSET; fileName: 'wall07_0e.png'),
    (id: $0f + 7 * STYLES_OFFSET; fileName: 'wall07_0f.png'),
    (id: $10 + 7 * STYLES_OFFSET; fileName: 'wall07_10.png')
    );

var
  ilMap: TImageList;
  ilTools: TImageList;
  resourcesDir: string;
  patternIndexMap: array of integer;



procedure loadGraphics(AOwner: TComponent; AWidth: integer);
procedure InitData;

implementation

constructor TGauntMaze.Create;
begin
  inherited Create;
  //FTraceLayer := TList.Create;
  self.FBuffer := TMemoryStream.Create;
  self.InitVisitedData;
  self.InitMapData;
  self.FStyle := gauntStyles[0];
  self.FHorzWrap := False;
  self.FVertWrap := False;
  self.FStunPlayers := False;
  self.FHurtPlayers := False;
end;

destructor TGauntMaze.Destroy;
begin
  //FTraceLayer.Free;
  self.FBuffer.Destroy;
  inherited Destroy;
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
end;

function TGauntMaze.getTraceLayerSize: integer;
var
  i: integer;
begin
  Result := 0;
 { if FTraceLayer.Count > 0 then
  begin
    for i := 0 to FTraceLayer.Count - 1 do
    begin
      if TCustomGauntTraceCmd(FTraceLayer.Items[i]).ClassType = TGauntTrace then
      begin
        Result := Result + 1;
      end
      else
      begin
        //it's a new origin, so size is 2-byte
        Result := Result + 2;
      end;
    end;
  end;}
end;

function TGauntMaze.getItemsLayersize: integer;
begin
  Result := 0;
end;

procedure InitData;
begin

  resourcesDir := ExtractFilePath(ParamStr(0)) + RESOURCES_DIR;

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

procedure TGauntMaze.ToFileStream(fs: TFileStream);
var
  i: integer;
begin
  try
    //guarantee the TMemoryStream is at pos 0
    self.FBuffer.Seek(0, soBeginning);

    //set the target file to append data
    fs.Seek(0, TSeekOrigin.soEnd);
    for i := 0 to fs.Size - 1 do
    begin
      fs.WriteByte(FBuffer.ReadByte);
    end;

  except
    on E: Exception do DebugLn('Error saving maze ' + self.Name +
        ' to file ' + fs.FileName + ': ' + E.Message);
  end;

end;

end.
