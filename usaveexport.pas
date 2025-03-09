unit uSaveExport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  EditBtn, StdCtrls, ActnList, kedits, BCButton, cyEditFilename, uData;

type

  { TfSaveExport }

  TfSaveExport = class(TForm)
    aCancel: TAction;
    aExportBlock: TAction;
    aSaveMaze: TAction;
    aExportMaze: TAction;
    aSaveBlock: TAction;
    alSaveExport: TActionList;
    btnManyMazes: TSpeedButton;
    btnSave: TBCButton;
    btnExport: TBCButton;
    btnCancel: TBCButton;
    dlgExportBlock: TSaveDialog;
    dlgSaveMaze: TSaveDialog;
    dlgExportMaze: TSaveDialog;
    dlgSaveBlock: TSaveDialog;
    editMazeFile1: TFileNameEdit;
    editMazeFile10: TFileNameEdit;
    editMazeFile2: TFileNameEdit;
    editMazeFile3: TFileNameEdit;
    editMazeFile4: TFileNameEdit;
    editMazeFile5: TFileNameEdit;
    editMazeFile6: TFileNameEdit;
    editMazeFile7: TFileNameEdit;
    editMazeFile8: TFileNameEdit;
    editMazeFile9: TFileNameEdit;
    ilExport: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    lblOneMaze: TLabel;
    lblMazeBlock: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    panCollection: TPanel;
    panSelection: TPanel;
    btnOneMaze: TSpeedButton;
    procedure aCancelExecute(Sender: TObject);
    procedure aSaveMazeExecute(Sender: TObject);
    procedure btnManyMazesClick(Sender: TObject);
    procedure btnOneMazeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCurrentMaze: TGauntMaze;
  public
    procedure SetCurrentMaze(AMaze: TGauntMaze);
  end;

var
  fSaveExport: TfSaveExport;

implementation

{$R *.lfm}

procedure TfSaveExport.FormCreate(Sender: TObject);
begin
  btnOneMaze.Down := True;
  btnOneMaze.Click;
end;

procedure TfSaveExport.btnOneMazeClick(Sender: TObject);
begin
  panCollection.Enabled := False;
  btnSave.Action := aSaveMaze;
  btnExport.Action := aExportMaze;
end;

procedure TfSaveExport.btnManyMazesClick(Sender: TObject);
begin
  panCollection.Enabled := True;
  btnSave.Action := aSaveBlock;
  btnExport.Action := aExportBlock;
end;

procedure TfSaveExport.aSaveMazeExecute(Sender: TObject);
var
  processResult: integer;
  fsSave: TFileStream;
begin
  processResult := FCurrentMaze.ProcessMap;

  case processResult of
    -1,-2,-3: ShowMessage(ProcessResults.KeyData[processResult]);
    else
    begin
      try
        if dlgSaveMaze.Execute then
        begin
          fsSave := TFileStream.Create(dlgSaveMaze.FileName, fmCreate);
          self.FCurrentMaze.ToFileStream(fsSave);
        end;
      finally
        fsSave.Free;
      end;
      ShowMessage('Map compiled successfully. Size: ' + IntToStr(processResult));
    end;
  end;
  self.ModalResult := mrOk;
end;

procedure TfSaveExport.aCancelExecute(Sender: TObject);
begin
  self.ModalResult:=mrCancel;
  self.Close;
end;

procedure TfSaveExport.SetCurrentMaze(AMaze: TGauntMaze);
begin
  FCurrentMaze := AMaze;
end;

end.
