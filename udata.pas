unit uData;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LazLoggerBase, editbtn, fgl
  //  ,SQLite3Conn, sqldb
  ;

type
  TGauntMap = array[0..31, 0..31] of byte;
  TLayer = (background, objects, positions);
  TSearchTraceType = (sttWall, sttTrapWall, sttGateD, sttGateV, sttExit);
  TGauntVersion = (gvMSX_DSK, gvMSX, gvMSX_TSX, gvZX_TZX, gvCPC_TZX, gvZX, gvCPC
    );

  TGauntStyle = record
    id: integer;
    Name: string;
    desc: string;
  end;
  TCustomGauntTraceCmd = class abstract (TObject);

  TProcessResult = specialize TFPGMap<integer, string>;
  TMazeFileList = specialize TFPGMap<integer, TControl>;

  TSearchDir = record
    colInc: smallint;
    rowInc: smallint;
    byteCode: byte;
  end;

  TGauntMaze = class(TComponent)
  private
    FSize: integer;  //0-511
    FHorzWrap: boolean;
    FVertWrap: boolean;
    FOneExit: boolean;
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
    FFileName: string;
    procedure InitMap(var map: TGauntMap);
    function ProcessTraceLayer: integer;
    function ProcessObjectLayer: integer;
    function FindTraces(Atype: TSearchTraceType): integer;
    function IsAWall(cell: byte): boolean;
    function IsAGate(cell: byte): boolean;
    function IsAGateV(cell: byte): boolean;
    function IsATrapWall(cell: byte): boolean;
    function CountExits: integer;
    function LocatePlayer(var Xpos, Ypos: integer): boolean;
    function IsLookedType(cell: byte; Atype: TSearchTraceType): boolean;
    function GetTraceOriginCode(AType: TSearchTraceType): byte;
    function PeekMapData(offset: integer): byte;
    procedure PokeMapData(offset: integer; Value: byte);
    function CountConsecutiveSpaces(offset: integer): integer;
    function CountConsecutiveObjects(offset: integer; Data: byte): integer;
    function AnyObjectsLeft(offset: integer): boolean;
    procedure DecodeMaze;
  public
    MapData: TGauntMap;
    ItemData: TGauntMap;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function getItemsLayersize(): integer;
    property Name: string read FName write FName;
    property FileName: string read FFileName write FFileName;
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
    function GetOneExit(): boolean;
    procedure SetOneExit(AValue: boolean);
    procedure InitMapData;
    procedure InitItemData;
    procedure InitVisitedData;
    procedure ToFileStream(fs: TFileStream);
    procedure FromFileStream(fs: TFileStream);
    procedure ImportFromFileStream(fs: TFileStream);
    procedure ExportToFileStream(fs: TFileStream);
    function ProcessMap: integer;
    procedure SetPlayerPos(col: byte; row: byte);
    function GetPlayerPos: TPoint;
    procedure FindRoomForPlayer(var startX, startY: integer);
    procedure FindRoomForExit(var startX, startY: integer);
    procedure HorzMirror;
    procedure VertMirror;
    procedure UpdateChecksum(var CheckSum: integer);


  end;

  TPictureIndex = record
    id: integer;
    fileName: string;
    desc: string;
  end;
  TGauntBlock = array[0..9] of TGauntMaze;

const
  {$I 'uData_consts.inc'}

var
  ilMap: TImageList;
  ilTools: TImageList;
  resourcesDir: string;
  patternIndexMap: array of integer;
  block: TGauntBlock;
  {transaction: TSQLTransaction;
  dbConn: TSQLite3Connection;}
  HomeDir: string;
  DFSdirections: array [0..3] of integer = (0, 1, 2, 3); //, 4, 5, 6, 7);
  ProcessResults: TProcessResult;
  ActionStatusText: string;
  ActionStatus: integer;

procedure loadGraphics(AOwner: TComponent; AWidth: integer);
procedure InitData;
procedure CleanData;
procedure GauntDebugLn(ATextLine: string);
function GetUUID: string;
function FindPatternDataById(APatternID: integer): TPictureIndex;
procedure LoadBlock(fs: TFileStream; var ABlock: TGauntBlock);
procedure ImportBlock(fs: TFileStream; var ABlock: TGauntBlock; AType: TGauntVersion);
procedure LoadIntoBlock(FileList: TMazeFileList; var Block: TGauntBlock);
procedure ExportBlock(fs: TFileStream; var ABlock: TGauntBlock; AType: TGauntVersion);
procedure SaveBlock(fs: TFileStream; var ABlock: TGauntBlock);
procedure InitializeDFSMaze(var Maze: TGauntMap; startX, startY: integer);
procedure GenerateDFSMaze(var Maze: TGauntMap; startX, startY, x, y: integer;
  BiasCoefficient: integer);
procedure InitializePrimMaze(var Maze: TGauntMap; startX, startY: integer);
procedure GeneratePrimMaze(var Maze: TGauntMap; startX, startY: integer);
procedure ReduceWalls(var Maze: TGauntMap; startX, startY: integer);
procedure BackupMap(var Source: TGauntMap; var destination: TGauntMap);
function CheckAllFilesExist(FileList: TMazeFileList): integer;
function VerifyBlock(var ABlock: TGauntBlock; AType: TGauntVersion;
  var Count: integer): integer;

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
  self.FOneExit := False;
  self.FStunPlayers := False;
  self.FHurtPlayers := False;
  self.Fid := GetUUID;
  self.FCreationDate := now;
  self.FLastSavedDate := now;
  self.FPlayerPos.X := 1;
  self.FPlayerPos.Y := 1;
  self.SetPlayerPos(1, 1);
  self.FileName := '';
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
  Result.Y := self.FPlayerPos.Y;
end;

function TGauntMaze.ProcessMap: integer;
var
  TraceLayerSize: integer;
  ObjectLayerSize: integer;
  tmpbyte: byte;

  fsExport: TFileStream;
  fsSave: TFileStream;
  fsExportBlock: TFileStream;
begin
  //Reset the TMemoryStream
  FBuffer.Clear;

  //Check constraints
  if CountExits < 1 then
  begin
    Result := -3;
    Exit;
  end;
  TraceLayerSize := ProcessTraceLayer;
  if TraceLayerSize > MAX_TRACE_SIZE then
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
  if self.FOneExit then tmpbyte := tmpbyte + $04;
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
  Result := Result + FindTraces(TSearchTraceType.sttGateD);
  if CountExits > 2 then
    //if there are only one or two exits it's better to use the object RLE encoding
    Result := Result + FindTraces(TSearchTraceType.sttExit);
end;

function TGauntMaze.FindTraces(Atype: TSearchTraceType): integer;
type

  TTraceStroke = record
    dir: TSearchDir;
    steps: byte;
  end;

  TTraceSeq = record
    Xorigin: byte;
    Yorigin: byte;
    strokes: array of TTraceStroke;
  end;
var
  row, col: integer;
  searchingRow, searchingCol: integer;
  CurrentRow, CurrentCol: integer;
  i, j: integer;
  firstColumn: integer;

  Seq: TTraceSeq;
  CurrentLength, BestLength: integer;
  BestDir: integer;
  GateSelect: integer = 0;
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
              if (searchingCol < 32) and (searchingRow < 32) and
                (searchingCol >= 0) and (searchingRow >= 0) then
                FVisitedData[searchingCol, searchingRow] := 1;
              //CHECK this might write outbounds the array   (checked)
              //CHECK adjust this to the actual margins considering wrapping
            end;

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


        //gate stroke is $40 unless the first stroke is RIGHT (also $40)

        if (length(Seq.strokes) > 0) and (AType = sttGateD) then
          if Seq.strokes[0].dir.byteCode = $40 then GateSelect := $40;

        self.FBuffer.WriteByte(GetTraceOriginCode(AType) + GateSelect + Seq.Xorigin);
        self.FBuffer.WriteByte(GetTraceOriginCode(AType) + GateSelect + Seq.Yorigin);

        //update size of this trace type
        Result := Result + 2;

        for i := 0 to length(Seq.strokes) - 1 do
        begin
          self.FBuffer.WriteByte(Seq.strokes[i].dir.byteCode + Seq.strokes[i].steps - 1
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
    sttGateD:
      Result := $40;
    sttGateV:
      Result := $80;
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

function TGauntMaze.IsAGateV(cell: byte): boolean;
begin
  if cell in [$12] then
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
    TSearchTraceType.sttGateD:
      Result := IsAGate(cell);
    TSearchTraceType.sttGateV:
      Result := IsAGateV(cell);
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
  if w <> self.FHorzWrap then
  begin
    if w then f := 0   //no wall
    else
      f := $05;      //vertical wall
    for i := 1 to 31 do
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
  end;

  self.FHorzWrap := w;

end;

procedure TGauntMaze.SetVertWrap(w: boolean);
begin
  self.FVertWrap := w;
end;

function TGauntMaze.GetOneExit(): boolean;
begin
  Result := self.FOneExit;
end;

procedure TGauntMaze.SetOneExit(AValue: boolean);
begin
  self.FOneExit := AValue;
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
 {
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
  }
  //Create key value lists
  ProcessResults := TProcessResult.Create;
  ProcessResults.Add(-1, 'The walls layer is too big!');
  ProcessResults.Add(-2, 'The RLE layer is too big');
  ProcessResults.Add(-3, 'There is no EXIT');

end;

procedure CleanData;
begin
  patternIndex := [];
  {dbConn.Close(True);
  dbConn.Free;
  transaction.Free;}
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
    fs.WriteByte(Ord(self.FOneExit));
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
end;

procedure TGauntMaze.ImportFromFileStream(fs: TFileStream);
begin
  //read the file
  try
    try
      self.FBuffer.Seek(0, TSeekOrigin.soBeginning);
      fs.Seek(0, TSeekOrigin.soBeginning);
      self.FBuffer.CopyFrom(fs, fs.Size);
    except
      on E: Exception do raise;
    end;
  finally
  end;
  //decode the buffer
  self.DecodeMaze;
end;

procedure TGauntMaze.DecodeMaze;
var
  i: integer;
  TempByte: byte;
  WallSize: byte;
  ObjectLayerSize: integer;
  Buffer: pbyte;


  procedure DecodeWalls;
  var
    x: byte = 0;
    y: byte = 0;
    WallCount: byte;
    CurrentPat: byte = $10;
    CurrentCmd: byte = $e0;

    function SameCmdInARow(index: integer): boolean;
    begin
      if (index - 4) > WallSize - 1 then
        Result := False
      else
      if (Buffer[index] and $e0) = (Buffer[index + 1] and $e0) then
        Result := True
      else
        Result := False;
    end;

    procedure SetOrigin(Ax, Ay: byte; Acmd: byte);
    begin
      CurrentCmd := Acmd;
      x := Ax;
      y := Ay;
      case Acmd of
        $e0:
        begin
          self.MapData[x, y] := $10;
          CurrentPat := $10;
        end;
        $c0:
        begin
          self.MapData[x, y] := $90;
          CurrentPat := $90;
        end;
        $80:
        begin
          self.MapData[x, y] := $11;
          CurrentPat := $11;
        end;
        $40:
        begin
          self.MapData[x, y] := $12;
          CurrentPat := $12;
        end;
        $20:
        begin
          self.MapData[x, y] := $36;
          CurrentPat := $36;
        end;
      end;
      i := i + 2;
    end;

    procedure DrawCmd(Acmd: byte; AOrgCmd: byte);
    var
      c: byte;
      f: byte;
      SD: TSearchDir;
      pattern: byte;
    begin
      //select drawing pattern
      case AOrgCmd and $e0 of
        $e0:
          pattern := $10;
        $c0:
          pattern := $80 + $10;
        $20:
          pattern := $36;
        $40, $80:
          case Acmd and $e0 of
            $00, $80:
              pattern := $12;
            else
              pattern := $11;
          end;
      end;

      for f := 0 to length(SearchDirs) - 1 do
      begin
        if SearchDirs[f].byteCode = Acmd and $e0 then
        begin
          SD := SearchDirs[f];
          break;
        end;
      end;
      for c := 0 to Acmd and $1f do
      begin
        x := x + SD.colInc;
        y := y + SD.rowInc;
        self.MapData[x, y] := pattern;
      end;

      Inc(i);            //advance the pointer
    end;

  begin
    //i := 0;    //first byte of wall layer
    while (i - 4) < WallSize do
    begin
      if SameCmdInARow(i) then
      begin
        //check if only 2 (in such case they're a "set origin" command
        if SameCmdInARow(i + 1) then
        begin
          //at least 3 in a row
          //check the 4th
          if SameCmdInARow(i + 2) then
          begin
            //there were 4, so do they are 2 Set Origin
            //CONFIRMED a set origin here
            SetOrigin(Buffer[i] and $1f, buffer[i + 1] and $1f, Buffer[i] and $e0);
          end
          else
          begin
            //ok, it was definitely 3... so first one is DRAW, the next ones are "set origin"
            //CONFIRMED, do a DRAW
            DrawCmd(Buffer[i], CurrentCmd);
          end;
        end
        else
        begin
          //ok, only 2... do a set origin
          //CONFIRMED a set origin here
          SetOrigin(Buffer[i] and $1f, buffer[i + 1] and $1f, Buffer[i] and $e0);
        end;
      end
      else
      begin
        //no 2 in a row, so it's a "draw command"
        //do the draw cmd
        //CONFIRMED
        DrawCmd(Buffer[i], CurrentCmd);
      end;
    end;
  end;

  procedure DecodeObjects;
  var
    MazePtr: integer = 0;
    LastObj: integer = 0;
    c: integer;
  begin
    while ObjectLayerSize > 0 do
    begin
      case Buffer[i] of
        $80..$ff:  //count spaces
        begin
          Inc(MazePtr, (Buffer[i] and $7f) + 1);
          Inc(i);
          Dec(ObjectLayerSize);
        end;
        $40..$7f, $13..$3e:  //object or enemy
        begin
          self.PokeMapData(MazePtr, Buffer[i]);
          LastObj := Buffer[i];
          Inc(MazePtr);
          Inc(i);
          Dec(ObjectLayerSize);
        end;
        $3f:           //set player position
        begin
          //self.PokeMapData(MazePtr, Buffer[i]);
          self.SetPlayerPos(MazePtr mod 32, MazePtr div 32);
          Inc(MazePtr);
          Inc(i);
          Dec(ObjectLayerSize);
        end;
        $00..$12:              //repeat previous object
        begin
          for c := 0 to Buffer[i] do
          begin
            self.PokeMapData(MazePtr, LastObj);
            Inc(MazePtr);
          end;
          Inc(i);
        end;
      end;
    end;
  end;

begin
  //read LSB of size
  self.FBuffer.Seek(0, TSeekOrigin.soBeginning);
  self.FSize := FBuffer.ReadByte;                   // +0
  TempByte := FBuffer.ReadByte;                     // +1

  self.FHorzWrap := ((TempByte shr 7) and 1) = 1;
  self.FVertWrap := ((TempByte shr 6) and 1) = 1;
  self.FOneExit := ((TempByte shr 2) and 1) = 1;
  self.FStunPlayers := ((TempByte shr 1) and 1) = 1;
  self.FHurtPlayers := ((TempByte shr 0) and 1) = 1;

  //update map border according to wrap
  self.InitMapData;

  TempByte := FBuffer.ReadByte;                     // +2

  self.FSize := self.FSize + 256 * ((TempByte shr 7) and 1);
  self.FStyle := gauntStyles[(TempByte shr 3) and 7];
  WallSize := FBuffer.ReadByte;                     // +3
  ObjectLayerSize := self.FSize - 4 - WallSize;

  Buffer := FBuffer.Memory;
  i := 4;   //start of wall decoding
  DecodeWalls;
  //"i" is supposed to be the index pointing to the object layer start
  DecodeObjects;
end;

procedure TGauntMaze.FromFileStream(fs: TFileStream);
var
  i: integer;
  xpos, ypos: integer;
begin
  try
    self.Name := fs.ReadAnsiString;
    xpos := fs.ReadByte;
    ypos := fs.ReadByte;
    self.FHorzWrap := fs.ReadByte <> 0;
    self.FVertWrap := fs.ReadByte <> 0;
    self.FOneExit := fs.ReadByte <> 0;
    self.FHurtPlayers := fs.ReadByte <> 0;
    self.FStunPlayers := fs.ReadByte <> 0;
    self.FStyle := gauntStyles[fs.ReadByte];
    for i := 0 to 32 * 32 - 1 do
    begin
      self.PokeMapData(i, fs.ReadByte);
    end;
    self.SetPlayerPos(xpos, ypos);
  except
    on E: Exception do
    begin
      GauntDebugLn('Error loading maze ' + self.Name + ' to file ' +
        fs.FileName + ': ' + E.Message);
      raise;
    end;
  end;
  //fs.Free; //must be freed from the outside to allow loading a sequence of maps
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

procedure LoadBlock(fs: TFileStream; var ABlock: TGauntBlock);
var
  Maze: TGauntMaze = nil;
  i: integer;
begin
  try
    try
      fs.Seek(0, TSeekOrigin.soBeginning);
      for i := 0 to length(block) - 1 do
      begin
        Maze := TGauntMaze.Create(nil);
        Maze.FromFileStream(fs);
        ABlock[i] := Maze;
      end;
    except
      on E: Exception do
      begin
        raise;
      end;
    end;
  finally
  end;
end;

procedure ImportBlock(fs: TFileStream; var ABlock: TGauntBlock; AType: TGauntVersion);
var
  Maze: TGauntMaze = nil;
  i: integer;
  s: word;
  sizes: array [0..9] of word;
begin
  //Result[0] := nil;
  try
    try
      //skip loader according to version
      case AType of
        gvMSX_DSK:
          fs.Seek(7 + length(HEAD_DSK), TSeekOrigin.soBeginning);
        gvMSX:
          fs.Seek(length(HEAD_DD_TSX), TSeekOrigin.soBeginning);
        gvZX:
          fs.Seek(2 + length(HEAD_DD_TZX), TSeekOrigin.soBeginning);
        gvCPC:
          fs.Seek(length(HEAD_DD_CDT), TSeekOrigin.soBeginning);
      end;
      //read the table of sizes
      for i := 0 to 9 do
      begin
        sizes[i] := fs.ReadByte + 256 * fs.ReadByte;
      end;
      //read the 3 dummy bytes
      fs.ReadByte;
      fs.ReadByte;
      fs.ReadByte;

      //read the mazes and store them into the Block
      for i := 0 to 9 do
      begin
        Maze := TGauntMaze.Create(nil);
        Maze.FBuffer.CopyFrom(fs, sizes[i]);
        Maze.FSize := sizes[i];
        Maze.DecodeMaze;
        ABlock[i] := Maze;
      end;


    except
      on E: Exception do
      begin
        raise;
      end;
    end;
  finally
  end;

end;

procedure SaveBlock(fs: TFileStream; var ABlock: TGauntBlock);
var
  i: integer;
begin
  fs.Seek(0, TSeekOrigin.soBeginning);

  try
    for i := 0 to 9 do
    begin
      ABlock[i].ToFileStream(fs);
    end;
  except
    raise;
  end;
end;

function CheckAllFilesExist(FileList: TMazeFileList): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to 9 do
  begin
    if not FileExists(TFileNameEdit(FileList.KeyData[i]).FileName, True) then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function VerifyBlock(var ABlock: TGauntBlock; AType: TGauntVersion;
  var Count: integer): integer;
var
  i: integer;
  VerifyResult: integer;
  MaxSize: integer;
begin
  //Return -1 if all blocks have been compiled successfully
  //Return -2 if size is too big
  //Return i_maze if maze can't be verified

  case AType of
    gvMSX_DSK: Count := length(HEAD_DSK);
    gvMSX: Count := length(HEAD_DD_TSX);
    gvCPC: Count := length(HEAD_DD_CDT);
    gvZX: Count := length(HEAD_DD_TZX);
  end;
  Result := -1;
  for i := 0 to 9 do
  begin
    if assigned(ABlock[i]) then
    begin
      VerifyResult := ABlock[i].ProcessMap;
      if (VerifyResult = -1) or (VerifyResult = -2) or (VerifyResult = -3) then
      begin
        Result := i;
        exit;
      end;
    end
    else
    begin
      Result := i;
      exit;
    end;
    Count := Count + VerifyResult;
  end;
  Count := Count + 10 * 2 + 3;    //size of table plus 3 dummy bytes

  case AType of
    gvMSX_DSK: MaxSize := $de80 - $d000;
    gvMSX: MaxSize := 3759;
    gvCPC: MaxSize := 3785;
    gvZX: MaxSize := 3944;
  end;
  if Count > MaxSize then Result := -2; //CHECK must be fixed for each version

end;

procedure ExportBlock(fs: TFileStream; var ABlock: TGauntBlock; AType: TGauntVersion);

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
  BlockSize: integer = 0;
  CheckSum: integer = 0;
begin
  {
  write checksum and size (MSX cassette version only)
  +0 [1]      #80 (0-7) #C0 (8+ standard) #C1 (8+ deeper)
  +1 [2]      block size
  [block]
  checksum[1] 16bit  THE MSB IS PART OF THE CHECKSUM BUT SHOULD BE PART OF THE MAZE!!
    THIS IS THE ACTUAL BUG!!  THE BUG IS ACTUALLY DURING THE MSX BUILD OF THE BLOCKS
    (THEY LACK ONE BYTE THAT MUST BE RECOVERED FROM THE OTHER SYSTEMS). THE LOADING
    ROUTINE ITSELF IS CORRECT.
  }
  //  TGauntVersion = (gvMSX_DSK, gvMSX, **gvMSX_TSX, **gvZX_TZX, **gvCPC_TZX, gvZX, gvCPC );
  try
    //Write header for each version
    case AType of
      gvMSX_DSK:
      begin
        //write header
        writeBinHeader(fs);
        //write the loading header itself
        writeBlockHeader(HEAD_DSK, fs);
      end;
      gvMSX:
      begin
        fs.WriteByte($c1);
        //levels 1-7 set #c0 or #c1 accordingly (a way to prevent mixing both tapes, only on MSX)
        VerifyBlock(uData.block, AType, BlockSize);
        //BlockSize := BlockSize + length(HEAD_DD_TSX);
        fs.WriteByte(BlockSize and $0ff);
        fs.WriteByte(BlockSize shr 8);
        writeBlockHeader(HEAD_DD_TSX, fs);
        //update checksum
        for i := 0 to length(HEAD_DD_TSX) - 1 do
        begin
          CheckSum := CheckSum + HEAD_DD_TSX[i];
        end;
      end;
      gvCPC:
      begin
        writeBlockHeader(HEAD_DD_CDT, fs);
      end;
      gvZX:
      begin
        VerifyBlock(uData.block, AType, BlockSize);
        fs.WriteByte(BlockSize and $0ff);
        fs.WriteByte(BlockSize shr 8);
        writeBlockHeader(HEAD_DD_TZX, fs);
      end
      else
      begin
      end;
    end;
    //common structure goes here

    //table of sizes
    for i := 0 to length(ABlock) - 1 do
    begin
      fs.WriteWord(word(ABlock[i].FSize));

      //update checksum
      if AType = gvMSX then
      begin
        CheckSum := CheckSum + (ABlock[i].FSize and $ff);
        CheckSum := CheckSum + ((ABlock[i].FSize shr 8) and $ff);
      end;
    end;
    // random selection variables (3 dummy bytes)

    fs.WriteByte(0);
    fs.WriteByte(0);
    fs.WriteByte(0);

    //write actual maze data
    for i := 0 to length(ABlock) - 1 do
    begin
      ABlock[i].ExportToFileStream(fs);
      if AType = gvMSX then
      begin
        ABlock[i].UpdateChecksum(CheckSum);
      end;
    end;

    //write block end
    case AType of
      gvMSX_DSK:
      begin
        //do nothing
      end;
      gvMSX:
      begin
        //add 16bit checksum
        fs.WriteByte(CheckSum and $ff);
        fs.WriteByte((CheckSum shr 8) and $ff);
      end;
      gvCPC:
      begin
        fs.WriteByte($ff);
      end;
      gvZX:
      begin
        fs.WriteByte($ff);
      end;
    end;

  except
    on E: Exception do
    begin
      GauntDebugLn('Error writing maze to file ' + fs.FileName + ': ' + E.Message);
    end;
  end;
  //fs.Free;
end;

procedure TGauntMaze.UpdateChecksum(var CheckSum: integer);
var
  i: integer;
begin
  //guarantee the TMemoryStream is at pos 0
  self.FBuffer.Seek(0, soBeginning);
  for i := 0 to self.FBuffer.Size - 1 do
  begin
    CheckSum := CheckSum + self.FBuffer.ReadByte;
  end;
end;

procedure LoadIntoBlock(FileList: TMazeFileList; var Block: TGauntBlock);
var
  i: integer;
  maze: TGauntMaze;
  fs: TFileStream = nil;
begin
  try
    for i := 0 to 9 do
    begin
      maze := TGauntMaze.Create(nil);
      fs := TFileStream.Create(TFileNameEdit(FileList.KeyData[i]).FileName, fmOpenRead);
      maze.FromFileStream(fs);
      Block[i] := maze;
      fs.Free;
      fs := nil;
    end;
  finally
    if assigned(fs) then fs.Free;
  end;
end;

procedure InitializeDFSMaze(var Maze: TGauntMap; startX, startY: integer);
var
  x, y, temp: integer;
begin
  for y := startY to 31 do
    for x := startX to 31 do
      Maze[x][y] := WALL_GEN_ID;  //single block

  //initialize direction vector
  // Shuffle the directions array to randomize the order
  for x := 0 to 3 do
  begin
    y := Random(4);
    temp := DFSdirections[x];
    DFSdirections[x] := DFSdirections[y];
    DFSdirections[y] := temp;
  end;
end;

procedure GenerateDFSMaze(var Maze: TGauntMap; startX, startY, x, y: integer;
  BiasCoefficient: integer);
var
  //directions: array[0..3] of integer = (0, 1, 2, 3);
  dx, dy, nx, ny, i, j, temp: integer;
begin
  //adjust the constraints
  if x < startX then x := startX;
  if y < startY then y := startY;


  Maze[x][y] := 0; // Mark the current cell as walkable

  // Shuffle again with a random bias
  for i := 0 to BiasCoefficient - 1 do
  begin
    j := Random(BiasCoefficient);
    temp := DFSdirections[i];
    DFSdirections[i] := DFSdirections[j];
    DFSdirections[j] := temp;
  end;

  // Explore each direction
  for i := 0 to 3 do
  begin
    case DFSdirections[i] of
      0: begin
        dx := 0;
        dy := -1;
      end; // Up
      1: begin
        dx := 1;
        dy := 0;
      end;  // Right
      2: begin
        dx := 0;
        dy := 1;
      end;  // Down
      3: begin
        dx := -1;
        dy := 0;
      end; // Left
      {
      4: begin
        dx := 1;
        dy := -1;
      end;
      5: begin
        dx := 1;
        dy := 1;
      end;
      6: begin
        dx := -1;
        dy := 1;
      end;
      7: begin
        dx := -1;
        dy := -1;
      end;
      }
    end;

    nx := x + dx * 2;
    ny := y + dy * 2;

    if (nx >= startX) and (nx < 32) and (ny >= startY) and (ny < 32) and
      (Maze[nx][ny] = WALL_GEN_ID) then
    begin
      Maze[x + dx][y + dy] := 0; // Carve a path
      GenerateDFSMaze(Maze, startX, startY, nx, ny, BiasCoefficient);
      // Recursively generate the maze

    end;
  end;
end;

procedure InitializePrimMaze(var Maze: TGauntMap; startX, startY: integer);
var
  x, y: integer;
begin
  for y := startY to 31 do
    for x := startX to 31 do
      Maze[x][y] := WALL_GEN_ID;  //single block

end;

{procedure InitializeKruskalMaze(var Maze: TGauntMap; startX, startY: integer);
var
  x, y: integer;
begin
  for y := startY to 31 do
    for x := startX to 31 do
      Maze[x][y] := WALL_GEN_ID;  //single block

end;
procedure GenerateKruskalMaze(var Maze: TGauntMap; startX, startY: integer);
type
  TEdge = record
    x1, y1, x2, y2: integer;
  end;
const
  SIZE = 32; // Size of the maze (32x32)
var
  edges: array of TEdge;
  i, j, x, y, nx, ny, index: integer;
  dirs: array[0..1] of TPoint = ((x: 1; y: 0), (x: 0; y: 1));
  Parent: array[0..SIZE * SIZE - 1] of integer;

  function FindSet(x: integer): integer;
  begin
    if Parent[x] <> x then
      Parent[x] := FindSet(Parent[x]);
    Result := Parent[x];
  end;

  procedure UnionSets(x, y: integer);
  begin
    Parent[FindSet(x)] := FindSet(y);
  end;

begin
  // Initialize disjoint sets
  for i := 0 to SIZE * SIZE - 1 do
    Parent[i] := i;

  // Generate all possible edges
  for y := 0 to SIZE - 1 do
    for x := 0 to SIZE - 1 do
      for j := 0 to 1 do
      begin
        nx := x + dirs[j].x;
        ny := y + dirs[j].y;
        if (nx < SIZE) and (ny < SIZE) then
        begin
          SetLength(edges, Length(edges) + 1);
          edges[High(edges)].x1 := x;
          edges[High(edges)].y1 := y;
          edges[High(edges)].x2 := nx;
          edges[High(edges)].y2 := ny;
        end;
      end;

  // Shuffle the edges
  for i := 0 to Length(edges) - 1 do
  begin
    j := Random(Length(edges));
    if i <> j then
    begin
      edges[i] := edges[j];
      edges[j] := edges[i];
    end;
  end;

  // Process edges
  for i := 0 to Length(edges) - 1 do
  begin
    x := edges[i].x1;
    y := edges[i].y1;
    nx := edges[i].x2;
    ny := edges[i].y2;

    if FindSet(y * SIZE + x) <> FindSet(ny * SIZE + nx) then
    begin
      Maze[x][y] := 0;
      Maze[nx][ny] := 0;
      Maze[(x + nx) div 2][(y + ny) div 2] := 0;
      UnionSets(y * SIZE + x, ny * SIZE + nx);
    end;
  end;
end;
}
procedure GeneratePrimMaze(var Maze: TGauntMap; startX, startY: integer);
var
  frontier: array of TPoint;
  dirs: array[0..3] of TPoint = ((x: 0; y: -2), (x: 2; y: 0), (x: 0;
    y: 2), (x: -2; y: 0));
  i, nx, ny, index: integer;
  current: TPoint;
begin
  startY := startY - 1;
  startX := startX - 1;
  // Start with a random cell
  current.x := 1 + Random((32) div 2) * 2;
  current.y := 1 + Random((32) div 2) * 2;
  Maze[current.x][current.y] := 0;

  // Add neighboring walls to the frontier
  for i := 0 to 3 do
  begin
    nx := current.x + dirs[i].x;
    ny := current.y + dirs[i].y;
    if (nx > startX) and (nx < 32) and (ny > startY) and (ny < 32) and
      (Maze[nx][ny] = WALL_GEN_ID) then
    begin
      SetLength(frontier, Length(frontier) + 1);
      frontier[High(frontier)] := Point(nx, ny);
    end;
  end;

  while Length(frontier) > 0 do
  begin
    // Pick a random frontier cell
    index := Random(Length(frontier));
    current := frontier[index];
    frontier[index] := frontier[High(frontier)];
    SetLength(frontier, Length(frontier) - 1);

    // Connect it to the maze
    for i := 0 to 3 do
    begin
      nx := current.x + dirs[i].x;
      ny := current.y + dirs[i].y;
      if (nx > startX) and (nx < 32) and (ny > startY) and (ny < 32) and
        (Maze[nx][ny] = 0) then
      begin
        Maze[current.x][current.y] := 0;
        Maze[(current.x + nx) div 2][(current.y + ny) div 2] := 0;
        Break;
      end;
    end;

    // Add new frontier cells
    for i := 0 to 3 do
    begin
      nx := current.x + dirs[i].x;
      ny := current.y + dirs[i].y;
      if (nx > startX) and (nx < 32) and (ny > startY) and (ny < 32) and
        (Maze[nx][ny] = WALL_GEN_ID) then
      begin
        SetLength(frontier, Length(frontier) + 1);
        frontier[High(frontier)] := Point(nx, ny);
      end;
    end;
  end;
end;

procedure TGauntMaze.FindRoomForPlayer(var startX, startY: integer);
var
  col, row: integer;
  firstCol: integer = 1;
begin
  if self.LocatePlayer(startX, startY) then Exit; //return the position in vars

  if self.FHorzWrap then firstCol := 0;

  for col := firstCol to 31 do
    for row := 1 to 31 do
      if self.MapData[col, row] = 0 then
      begin
        self.MapData[col, row] := $3f;
        startX := col;
        startY := row;
        exit;
      end;
  //not found (unlikely but might be, for the first corner to hold the character
  self.MapData[firstCol, 1] := $3f;
  startX := firstCol;
  startY := 1;
end;

procedure TGauntMaze.FindRoomForExit(var startX, startY: integer);
var
  col, row: integer;
  firstCol: integer = 1;
begin
  if self.FHorzWrap then firstCol := 0;

  for col := 31 downto firstCol do
    for row := 31 downto 1 do
      if self.MapData[col, row] = 0 then
      begin
        self.MapData[col, row] := $36;
        startX := col;
        startY := row;
        exit;
      end;
  //not found (unlikely but might be, for the first corner to hold the character
  self.MapData[31, 31] := $36;
  startX := 31;
  startY := 31;
end;

procedure TGauntMaze.HorzMirror;
var
  firstCol: integer = 1;
  col, row: integer;
begin
  if self.FHorzWrap then firstCol := 0;

  for row := 17 to 31 do
  begin
    for col := firstCol to 31 do
    begin
      if self.MapData[col, row] <> $3f then
        self.MapData[col, 31 - row + 1] := self.MapData[col, row];
    end;
  end;

end;

procedure TGauntMaze.VertMirror;
var
  firstCol: integer = 1;
  col, row: integer;
begin
  if self.FHorzWrap then firstCol := 0;

  for col := 16 + firstCol to 31 do
  begin
    for row := 1 to 31 do
    begin
      if self.MapData[col, row] <> $3f then
        self.MapData[31 - col + 1 * firstCol, row] := self.MapData[col, row];
    end;
  end;

end;

function TGauntMaze.LocatePlayer(var Xpos, Ypos: integer): boolean;
var
  x, y: integer;
begin
  Result := False;

  for x := 0 to 31 do
    for y := 1 to 31 do
      if self.MapData[x, y] = $3f then
      begin
        Xpos := x;
        Ypos := y;
        Result := True;
        break;
      end;

end;

procedure BackupMap(var Source: TGauntMap; var destination: TGauntMap);
var
  col, row: integer;
begin

  for col := 0 to 32 - 1 do
    for row := 0 to 32 - 1 do
      destination[col, row] := Source[col, row];

end;

procedure ReduceWalls(var Maze: TGauntMap; startX, startY: integer);
var
  x, y: integer;

  function CountLiveNeighbours(x, y: integer): integer;
  begin
    Result := 0;
    if x > 0 then
    begin
      Result := Result + Maze[x - 1, y];
      if y > 0 then Result := Result + Maze[x - 1, y - 1];
      if y < 31 then Result := Result + Maze[x - 1, y + 1];
    end;
    if x < 31 then
    begin
      Result := Result + Maze[x + 1, y];
      if y > 0 then Result := Result + Maze[x + 1, y - 1];
      if y < 31 then Result := Result + Maze[x + 1, y + 1];
    end;
    if y > 0 then Result := Result + Maze[x, y - 1];
    if y < 31 then Result := Result + Maze[x, y + 1];
    Result := Result div WALL_GEN_ID;
  end;

begin
  for x := startX to 31 do
    for y := startY to 31 do
    begin
      if CountLiveNeighbours(x, y) < 2 then Maze[x, y] := 0;
    end;
end;

end.
