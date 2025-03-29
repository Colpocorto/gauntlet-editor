unit uKruskal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, udata;

type
  TEdge = record
    u, v, weight: integer;
  end;
  PEdge = ^TEdge; // Pointer to TEdge

  TDisjointSet = class
  private
    FParent: array[0..1023] of integer;
    FRank: array[0..1023] of integer;
    FSize: integer;

  public
    constructor Create(size: integer);
    destructor Destroy; override;
    function Find(i: integer): integer;
    procedure Union(i, j: integer);
    function GetSize: integer;
  end;

const
  MAZE_SIZE = 16;
  MAX_CELLS = MAZE_SIZE * MAZE_SIZE;

var
  FMaze: array[0..MAZE_SIZE * 2, 0..MAZE_SIZE * 2] of byte;
  Edges: TList;
  DS: TDisjointSet;

procedure GenerateKruskalMaze(var AMaze: TGauntMap; startX, startY,weightX, weightY: integer);

implementation

constructor TDisjointSet.Create(size: integer);
var
  i: integer;
begin
  inherited Create;
  FSize := size;
  for i := 0 to size - 1 do
  begin
    FParent[i] := i;
    FRank[i] := 0;
  end;
end;

destructor TDisjointSet.Destroy;
begin
  inherited Destroy;
end;

function TDisjointSet.Find(i: integer): integer;
begin
  if FParent[i] <> i then
    FParent[i] := Find(FParent[i]);
  Result := FParent[i];
end;

procedure TDisjointSet.Union(i, j: integer);
var
  rootI, rootJ: integer;
begin
  rootI := Find(i);
  rootJ := Find(j);
  if rootI <> rootJ then
  begin
    if FRank[rootI] < FRank[rootJ] then
      FParent[rootI] := rootJ
    else if FRank[rootI] > FRank[rootJ] then
      FParent[rootJ] := rootI
    else
    begin
      FParent[rootJ] := rootI;
      Inc(FRank[rootI]);
    end;
  end;
end;

function TDisjointSet.GetSize: integer;
begin
  Result := FSize;
end;

procedure GenerateKruskalMaze(var AMaze: TGauntMap; startX, startY, weightX, weightY: integer);
var
  x, y, i, j, u, v, weight, index: integer;
  edge: PEdge;
  tempEdge: TEdge;
begin

  Randomize;

  // Initialize maze with walls
  for y := 0 to MAZE_SIZE * 2 do
    for x := 0 to MAZE_SIZE * 2 do
      FMaze[x, y] := WALL_GEN_ID;

  // Create edges
  Edges := TList.Create;
  for y := 0 to MAZE_SIZE - 1 do
    for x := 0 to MAZE_SIZE - 1 do
    begin
      New(edge); // Allocate memory for a new edge
      edge^.u := y * MAZE_SIZE + x;
      if x < MAZE_SIZE - 1 then
      begin
        edge^.v := edge^.u + 1;
        edge^.weight := Random(weightX);
        Edges.Add(edge); // Add the allocated edge pointer
      end;
      if y < MAZE_SIZE - 1 then
      begin
        New(edge); // Allocate memory for a new edge
        edge^.u := y * MAZE_SIZE + x;
        edge^.v := edge^.u + MAZE_SIZE;
        edge^.weight := Random(weightY);
        Edges.Add(edge); // Add the allocated edge pointer
      end;
    end;

  // Sort edges by weight (Bubble Sort)
  for i := 0 to Edges.Count - 2 do
    for j := i + 1 to Edges.Count - 1 do
    begin
      if TEdge(Edges.Items[i]^).weight > TEdge(Edges.Items[j]^).weight then
      begin
        tempEdge := TEdge(Edges.Items[i]^);
        TEdge(Edges.Items[i]^) := TEdge(Edges.Items[j]^);
        TEdge(Edges.Items[j]^) := tempEdge;
      end;
    end;

  // Kruskal's algorithm
  DS := TDisjointSet.Create(MAX_CELLS);
  try
    for i := 0 to Edges.Count - 1 do
    begin
      edge := PEdge(Edges.Items[i]);
      if DS.Find(edge^.u) <> DS.Find(edge^.v) then
      begin
        DS.Union(edge^.u, edge^.v);
        // Create passage in the maze
        u := edge^.u;
        v := edge^.v;
        x := u mod MAZE_SIZE;
        y := u div MAZE_SIZE;
        FMaze[x * 2 + 1, y * 2 + 1] := 0;
        x := v mod MAZE_SIZE;
        y := v div MAZE_SIZE;
        FMaze[x * 2 + 1, y * 2 + 1] := 0;
        if edge^.u + 1 = edge^.v then
          FMaze[Min(edge^.u mod MAZE_SIZE, edge^.v mod MAZE_SIZE) *
            2 + 2, edge^.u div MAZE_SIZE * 2 + 1] := 0
        else
          FMaze[edge^.u mod MAZE_SIZE * 2 + 1,
            Min(edge^.u div MAZE_SIZE, edge^.v div MAZE_SIZE) * 2 + 2] := 0;
      end;
    end;
  finally
    FreeAndNil(DS);
    for i := 0 to Edges.Count - 1 do
    begin
      Dispose(PEdge(Edges.Items[i])); // Free allocated memory
    end;
    FreeAndNil(Edges);
  end;
  //copy the temporary array to the actual maze
  for x := startX to 31 do
    for y := startY to 31 do
    begin
      AMaze[x, y] := FMaze[x, y];
    end;
end;

end.
