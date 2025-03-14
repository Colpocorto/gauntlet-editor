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
    aImportCollection: TAction;
    aImportBlock: TAction;
    aImportMaze: TAction;
    alLoadImport: TActionList;
    aLoadBlock: TAction;
    aLoadMaze: TAction;
    btnAmstrad: TSpeedButton;
    btnAmstrad_CDT: TSpeedButton;
    btnCancel: TBCButton;
    btnCollection: TSpeedButton;
    btnImport: TBCButton;
    btnManyMazes: TSpeedButton;
    btnMSX: TSpeedButton;
    btnMSX_DSK: TSpeedButton;
    btnMSX_TSX: TSpeedButton;
    btnOneMaze: TSpeedButton;
    btnLoad: TBCButton;
    btnZX: TSpeedButton;
    btnZX_TZX: TSpeedButton;
    dlgImportBlock: TSaveDialog;
    dlgImportMaze: TSaveDialog;
    dlgLoadBlock: TSaveDialog;
    dlgLoadMaze: TSaveDialog;
    ilImport: TImageList;
    lblCollection: TLabel;
    lblMazeBlock: TLabel;
    lblOneMaze: TLabel;
    lblPanFormat: TLabel;
    panFormat: TPanel;
    panSelection: TPanel;
    procedure aCancelExecute(Sender: TObject);
    procedure aImportBlockExecute(Sender: TObject);
    procedure aImportCollectionExecute(Sender: TObject);
    procedure aImportMazeExecute(Sender: TObject);
    procedure aLoadBlockExecute(Sender: TObject);
    procedure aLoadMazeExecute(Sender: TObject);
    procedure btnCollectionClick(Sender: TObject);
    procedure btnManyMazesClick(Sender: TObject);
    procedure btnOneMazeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMazeFileList: TMazeFileList;
  public
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

procedure TfLoadImport.aImportBlockExecute(Sender: TObject);
begin
  ShowMessage(
    'Not implemented yet. Coming soon...  In the meanwhile, it''s a good idea to use ZX-BlockEditor to rip blocks manually');
end;

procedure TfLoadImport.aImportCollectionExecute(Sender: TObject);
begin
  ShowMessage('Not implemented yet. Coming soon...');
end;

procedure TfLoadImport.aImportMazeExecute(Sender: TObject);
begin
  ShowMessage('Not implemented yet. Coming soon...');
end;

procedure TfLoadImport.aLoadBlockExecute(Sender: TObject);
var
  fs: TFileStream = nil;
begin
  if dlgLoadBlock.Execute then
  begin
    try
      try
        fs := TFileStream.Create(dlgLoadBlock.FileName, fmOpenRead);
        uData.LoadBlock(fs, uData.block);
        self.ModalResult := mrAll;
      except
        on E: Exception do
        begin
          ShowMessage('Error loading maze block' + dlgLoadBlock.FileName +
            ': ' + E.Message);
          self.ModalResult := mrNo;
        end;
      end;
    finally
    end;
  end;
end;

procedure TfLoadImport.aLoadMazeExecute(Sender: TObject);
var
  Maze: TGauntMaze;
  fs: TFileStream = nil;
begin
  if dlgLoadMaze.Execute then
  begin
    Maze := TGauntMaze.Create(self);
    try
      try
        fs := TFileStream.Create(dlgLoadMaze.FileName, fmOpenRead);
        Maze.FromFileStream(fs);
        Maze.FileName := dlgLoadMaze.FileName;
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
      if assigned(fs) then fs.Free;
    end;
  end;
end;

procedure TfLoadImport.btnCollectionClick(Sender: TObject);
begin
  btnLoad.Enabled := False;
  btnLoad.StateNormal.FontEx.Color := $808080;

  btnImport.Action := aImportCollection;
  panFormat.Enabled := True;
  btnMSX_DSK.Enabled := False;
  btnMSX.Enabled := False;
  btnZX.Enabled := False;
  btnAmstrad.Enabled := False;
  btnZX_TZX.Enabled := True;
  btnAmstrad_CDT.Enabled := True;
  btnMSX_TSX.Enabled := True;

  btnZX_TZX.Down := True;

  btnCancel.Enabled := True;
  aCancel.Enabled := True;

end;

procedure TfLoadImport.btnManyMazesClick(Sender: TObject);
begin
  btnLoad.Action := aLoadBlock;
  btnImport.Action := aImportBlock;
  btnLoad.Enabled := True;
  btnLoad.StateNormal.FontEx.Color := clWhite;

  aLoadBlock.Enabled := True;
  aImportBlock.Enabled := True;
  panFormat.Enabled := True;

  btnMSX_DSK.Enabled := True;
  btnMSX.Enabled := True;
  btnZX.Enabled := True;
  btnAmstrad.Enabled := True;

  btnMSX_DSK.Down := True;

  btnZX_TZX.Enabled := False;
  btnAmstrad_CDT.Enabled := False;
  btnMSX_TSX.Enabled := False;

  btnCancel.Enabled := True;
  aCancel.Enabled := True;

end;

procedure TfLoadImport.btnOneMazeClick(Sender: TObject);
begin
  btnLoad.Enabled := True;
  btnLoad.StateNormal.FontEx.Color := clWhite;

  btnLoad.Action := aLoadMaze;
  btnImport.Action := aImportMaze;

  aLoadMaze.Enabled := True;
  aImportMaze.Enabled := True;

  panFormat.Enabled := False;

  btnCancel.Enabled := True;
  aCancel.Enabled := True;
end;

procedure TfLoadImport.FormCreate(Sender: TObject);
begin
  btnOneMaze.Down := True;
  btnOneMaze.Click;
end;

end.
