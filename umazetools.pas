unit uMazeTools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, ExtCtrls,
  Buttons, StdCtrls, Spin, SpinEx, BCButtonFocus, BCButton, BGRASpeedButton,
  BCPanel, BCComboBox, ECSpinCtrls, uData;

type

  { TfMazeTools }

  TfMazeTools = class(TForm)
    aCancel: TAction;
    aOk: TAction;
    aGenerateDFSMaze: TAction;
    alMazeTools: TActionList;
    btnOk: TBCButton;
    btnCancel: TBCButton;
    cbBias: TBCComboBox;
    cbOrgX: TBCComboBox;
    cbOrgY: TBCComboBox;
    ilMazeTools: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    panGenerate: TPanel;
    panEffects: TPanel;
    SpeedButton1: TSpeedButton;
    procedure aCancelExecute(Sender: TObject);
    procedure aGenerateDFSMazeExecute(Sender: TObject);
    procedure aOkExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
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

{$R *.lfm}

procedure TfMazeTools.aCancelExecute(Sender: TObject);
begin
  self.Close;
end;

procedure TfMazeTools.aGenerateDFSMazeExecute(Sender: TObject);
var
  startX: integer;
  startY: integer = 1;
begin
  //first, initialize the maze taking into account the boundaries
  if self.FCurrentMaze.GetHorzWrap then
    startX := 0
  else
    startX := 1;

  InitializeDFSMaze(self.FCurrentMaze.MapData, startX, startY);
  GenerateDFSMaze(self.FCurrentMaze.MapData, startX, startY,
    cbOrgX.ItemIndex + 1,
    cbOrgY.ItemIndex + 1, cbBias.ItemIndex + 1);

  self.FPreviewObject.Repaint;

end;

procedure TfMazeTools.aOkExecute(Sender: TObject);
begin
  self.BackupMaze; //store the new modification
  self.Close;
end;

procedure TfMazeTools.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  self.RestoreMaze;
end;

procedure TfMazeTools.FormShow(Sender: TObject);
begin
  //copy the active map to the backup
  self.BackupMaze;
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
