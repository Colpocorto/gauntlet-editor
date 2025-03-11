unit uMazeTools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, ExtCtrls,
  Buttons, StdCtrls, SpinEx,
  RTTICtrls, BCButton, BCComboBox, BCFluentSlider, ECSpinCtrls, uData, uKruskal;

type

  { TfMazeTools }

  TfMazeTools = class(TForm)
    aCancel: TAction;
    aDemolish: TAction;
    aReduceWalls: TAction;
    aGenerateMaze: TAction;
    aOk: TAction;
    alMazeTools: TActionList;
    btnReduceWalls: TSpeedButton;
    btnDemolishMaze: TSpeedButton;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    slideWeight: TBCFluentSlider;
    btnOk: TBCButton;
    btnCancel: TBCButton;
    cbBias: TBCComboBox;
    cbAlgorithm: TBCComboBox;
    cbOrgX: TBCComboBox;
    cbOrgY: TBCComboBox;
    ilMazeTools: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    panGenerate: TPanel;
    panEffects: TPanel;
    SpeedButton1: TSpeedButton;
    btnHorzMirror: TSpeedButton;
    brnVertMirror: TSpeedButton;
    procedure aCancelExecute(Sender: TObject);
    procedure aDemolishExecute(Sender: TObject);
    procedure aGenerateMazeExecute(Sender: TObject);
    procedure aOkExecute(Sender: TObject);
    procedure aReduceWallsExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure btnHorzMirrorClick(Sender: TObject);
    procedure brnVertMirrorClick(Sender: TObject);
  private
    FCurrentMaze: TGauntMaze;
    FPreviewObject: TControl;

  public
    MapBackup: TGauntMap;

    procedure SetCurrentMaze(AMaze: TGauntMaze);
    procedure SetPreviewObject(obj: TControl);
    procedure UpdatePreview;
    procedure BackupMaze;
    procedure RestoreMaze;

  end;

var
  fMazeTools: TfMazeTools;


implementation

uses uMain;
  {$R *.lfm}

procedure TfMazeTools.aCancelExecute(Sender: TObject);
begin
  self.Close;
end;

procedure TfMazeTools.aDemolishExecute(Sender: TObject);
var
  startX: integer;
  startY: integer = 1;
  x, y: integer;
begin
  if self.FCurrentMaze.GetHorzWrap then
    startX := 0
  else
    startX := 1;
  for x := startX to 31 do
    for y := startY to 31 do
      if (FCurrentMaze.MapData[x, y] <= $12) and (FCurrentMaze.MapData[x, y] > 0) then
        FCurrentMaze.MapData[x, y] := 0;

  self.FPreviewObject.Repaint;

end;

procedure TfMazeTools.aGenerateMazeExecute(Sender: TObject);
var
  startX: integer;
  startY: integer = 1;
  PosX: integer = 0;
  PosY: integer = 0;
begin
  if self.FCurrentMaze.GetHorzWrap then
    startX := 0
  else
    startX := 1;

  case cbAlgorithm.ItemIndex of
    0:
    begin
      //DFS algorithm
      InitializeDFSMaze(self.FCurrentMaze.MapData, startX, startY);
      GenerateDFSMaze(self.FCurrentMaze.MapData, startX, startY,
        cbOrgX.ItemIndex + 1,
        cbOrgY.ItemIndex + 1, cbBias.ItemIndex + 1);
    end;
    1:
    begin
      //Prim's algorithm
      InitializePrimMaze(self.FCurrentMaze.MapData, startX, startY);
      GeneratePrimMaze(self.FCurrentMaze.MapData, startX, startY);
    end;
    2:
    begin
      //Kruskal's algorithm
      //InitializeKruskalMaze(self.FCurrentMaze.MapData, startX, startY);
      GenerateKruskalMaze(self.FCurrentMaze.MapData, startX, startY,
        slideWeight.Value, 100 - slideWeight.Value);
    end;
  end;

  self.FCurrentMaze.FindRoomForPlayer(PosX, PosY);  //PosX, PosY passed as reference
  self.FCurrentMaze.SetPlayerPos(PosX, PosY);

  self.FCurrentMaze.FindRoomForExit(PosX, PosY);
  //PosX, PosY passed as reference, not use here

  self.FPreviewObject.Repaint;
end;

procedure TfMazeTools.aOkExecute(Sender: TObject);
begin
  self.BackupMaze; //store the new modification
  self.Close;
end;

procedure TfMazeTools.aReduceWallsExecute(Sender: TObject);
var
  startX: integer;
  startY: integer = 1;
begin
  if self.FCurrentMaze.GetHorzWrap then
    startX := 0
  else
    startX := 1;
  ReduceWalls(self.FCurrentMaze.MapData, startX, startY);
  FPreviewObject.Repaint;
end;

procedure TfMazeTools.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  self.RestoreMaze;
  fMain.Show;
end;

procedure TfMazeTools.FormShow(Sender: TObject);
begin
  //copy the active map to the backup
  self.BackupMaze;
end;

procedure TfMazeTools.btnHorzMirrorClick(Sender: TObject);
var
  PosX: integer = 0;
  PosY: integer = 0;
begin
  FCurrentMaze.HorzMirror;
  FCurrentMaze.FindRoomForPlayer(PosX, PosY);
  FCurrentMaze.SetPlayerPos(PosX, PosY);

  FPreviewObject.Repaint;
end;

procedure TfMazeTools.brnVertMirrorClick(Sender: TObject);
var
  PosX: integer = 0;
  PosY: integer = 0;
begin
  FCurrentMaze.VertMirror;
  FCurrentMaze.FindRoomForPlayer(PosX, PosY);

  FPreviewObject.Repaint;
end;

procedure TfMazeTools.SetCurrentMaze(AMaze: TGauntMaze);
begin
  FCurrentMaze := AMaze;
end;

procedure TfMazeTools.SetPreviewObject(obj: TControl);
begin
  FPreviewObject := obj;
end;

procedure TfMazeTools.UpdatePreview;
begin
  if assigned(FPreviewObject) then FPreviewObject.Repaint;
end;

procedure TfMazeTools.BackupMaze;
begin
  //copy the active map to the backup map
  uData.BackupMap(self.FCurrentMaze.MapData, self.MapBackup);
end;

procedure TfMazeTools.RestoreMaze;
begin
  //copy the active map to the backup map
  uData.BackupMap(self.MapBackup, self.FCurrentMaze.MapData);
  self.FPreviewObject.Repaint;
end;

end.
