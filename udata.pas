unit uData;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LazLoggerBase;

type
  TGauntMap = array[0..31, 0..31] of integer;
  TLayer = (background, objects, positions);
  TGauntOrgType = (wall, trap_bound_wall, gate_h, gate_v, exit);
  TGauntTraceDir = (up, up_left, right, down_right, down, down_left, left, up_right);

  TGauntStyle = record
    id: integer;
    Name: string;
    desc: string;
  end;
  TCustomGauntTraceCmd = class abstract(TObject);

  TGauntTraceOrigin = class(TCustomGauntTraceCmd)
    col, row: smallint;
    orgType: TGauntOrgType;
  end;

  TGauntTrace = class(TCustomGauntTraceCmd)
    dir: TGauntTraceDir;
    steps: smallint;
  end;

  TGauntMaze = class
  private
    FTraceLayer: TList;
    FItemsLayer: TList;
    FName: string;
    FMapData: TGauntMap;
    FVisitedData: TGauntMap;
    procedure InitMap(var map: TGauntMap);
  public
    MapData: TGauntMap;
    VisitedData: TGauntMap;
    constructor Create;
    destructor Destroy; override;
    function getTraceLayerSize(): integer;
    function getItemsLayersize(): integer;
    property Name: string read FName write FName;
    procedure InitMapData;
    procedure InitVisitedData;
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
    (id: $01; fileName: 'wall00_01.png'),
    (id: $02; fileName: 'wall00_02.png'),
    (id: $03; fileName: 'wall00_03.png'),
    (id: $04; fileName: 'wall00_04.png'),
    (id: $05; fileName: 'wall00_05.png'),
    (id: $06; fileName: 'wall00_06.png'),
    (id: $07; fileName: 'wall00_07.png'),
    (id: $08; fileName: 'wall00_08.png'),
    (id: $09; fileName: 'wall00_09.png'),
    (id: $0a; fileName: 'wall00_0a.png'),
    (id: $0b; fileName: 'wall00_0b.png'),
    (id: $0c; fileName: 'wall00_0c.png'),
    (id: $0d; fileName: 'wall00_0d.png'),
    (id: $0e; fileName: 'wall00_0e.png'),
    (id: $0f; fileName: 'wall00_0f.png'),
    (id: $10; fileName: 'wall00_10.png'),
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
    (id: $45; fileName: 'death.png')
    );

var
  gauntStyle: TGauntStyle;
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
  FTraceLayer := TList.Create;
  self.InitVisitedData;
  self.InitMapData;
end;

destructor TGauntMaze.Destroy;
begin
  FTraceLayer.Free;
  inherited Destroy;
end;

function TGauntMaze.getTraceLayerSize: integer;
var
  i: integer;
begin
  Result := 0;
  if FTraceLayer.Count > 0 then
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
  end;
end;

function TGauntMaze.getItemsLayersize: integer;
begin
  Result := 0;
end;

procedure InitData;
begin
  gauntStyle := gauntStyles[0];
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
begin
  self.InitMap(self.FMapData);
end;

procedure TGauntMaze.InitVisitedData;
begin
  self.InitMap(self.FVisitedData);
end;

end.
