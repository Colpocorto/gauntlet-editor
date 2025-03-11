unit uLoadImport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls, ActnList, BCButton, uData;

type

  { TfLoadImport }

  TfLoadImport = class(TForm)
    aCancel: TAction;
    aImportBlock: TAction;
    aImportMaze: TAction;
    alLoadImport: TActionList;
    aLoadBlock: TAction;
    aLoadMaze: TAction;
    btnCancel: TBCButton;
    btnExport: TBCButton;
    btnManyMazes: TSpeedButton;
    btnOneMaze: TSpeedButton;
    btnSave: TBCButton;
    dlgImportBlock: TSaveDialog;
    dlgImportMaze: TSaveDialog;
    dlgLoadBlock: TSaveDialog;
    dlgLoadMaze: TSaveDialog;
    ilImport: TImageList;
    lblMazeBlock: TLabel;
    lblOneMaze: TLabel;
    panSelection: TPanel;
    procedure aCancelExecute(Sender: TObject);
    procedure aLoadMazeExecute(Sender: TObject);
    procedure btnManyMazesClick(Sender: TObject);
    procedure btnOneMazeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCurrentMaze: TGauntMaze;
    FMazeFileList: TMazeFileList;
  public
    procedure SetCurrentMaze(AMaze: TGauntMaze);

  end;

var
  fLoadImport: TfLoadImport;

implementation

{$R *.lfm}

{ TfLoadImport }

procedure TfLoadImport.aCancelExecute(Sender: TObject);
begin
  self.ModalResult := mrCancel;
  self.Close;
end;

procedure TfLoadImport.aLoadMazeExecute(Sender: TObject);
var
  Maze: TGauntMaze;
  fs: TFileStream;
begin
  if dlgLoadMaze.Execute then
  begin
    Maze := TGauntMaze.Create(self);
    try
      try
        fs := TFileStream.Create(dlgLoadMaze.FileName, fmOpenRead);
        Maze.FromFileStream(fs);
        Maze.FileName:=fs.FileName;
        uData.block[0] := Maze;
        self.ModalResult := mrOk;
      except
        on E: Exception do
        begin
          ShowMessage('Error loading maze ' + dlgLoadMaze.FileName + ': ' + E.Message);
          self.ModalResult := mrNo;
        end;
      end;
    finally
      fs.Free;
    end;

  end;

end;

procedure TfLoadImport.btnManyMazesClick(Sender: TObject);
begin
  btnSave.Action := aImportMaze;
  btnExport.Action := aImportBlock;
end;

procedure TfLoadImport.btnOneMazeClick(Sender: TObject);
begin
  //panCollection.Enabled := False;
  btnSave.Action := aLoadMaze;
  btnExport.Action := aImportMaze;
end;

procedure TfLoadImport.FormCreate(Sender: TObject);
begin
  btnOneMaze.Down := True;
  btnOneMaze.Click;
end;

procedure TfLoadImport.SetCurrentMaze(AMaze: TGauntMaze);
begin
  FCurrentMaze := AMaze;
end;

end.
