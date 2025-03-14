unit uSaveExport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  EditBtn, StdCtrls, ActnList, kedits, BCButton, cyEditFilename, uData, fgl;

type

  { TfSaveExport }

  TfSaveExport = class(TForm)
    aCancel: TAction;
    aExportCollection: TAction;
    aExportBlock: TAction;
    aSaveMaze: TAction;
    aExportMaze: TAction;
    aSaveBlock: TAction;
    alSaveExport: TActionList;
    btnAmstrad_CDT: TSpeedButton;
    btnManyMazes: TSpeedButton;
    btnCollection: TSpeedButton;
    btnMSX_DSK: TSpeedButton;
    btnMSX_TSX: TSpeedButton;
    btnZX: TSpeedButton;
    btnSave: TBCButton;
    btnExport: TBCButton;
    btnCancel: TBCButton;
    btnAmstrad: TSpeedButton;
    btnZX_TZX: TSpeedButton;
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
    lblCollection: TLabel;
    lblPanFormat: TLabel;
    lblSaveExportHint: TLabel;
    lblBlockComposition: TLabel;
    lblTreasureLegend: TLabel;
    lblSlot05: TLabel;
    lblSlot06: TLabel;
    lblSlot07: TLabel;
    lblSlot08: TLabel;
    lblSlot09: TLabel;
    lblSlot10: TLabel;
    lblOneMaze: TLabel;
    lblMazeBlock: TLabel;
    lblSlot01: TLabel;
    lblSlot02: TLabel;
    lblSlot03: TLabel;
    lblSlot04: TLabel;
    panCollection: TPanel;
    panFormat: TPanel;
    panSelection: TPanel;
    btnOneMaze: TSpeedButton;
    btnMSX: TSpeedButton;
    procedure aCancelExecute(Sender: TObject);
    procedure aExportBlockExecute(Sender: TObject);
    procedure aExportCollectionExecute(Sender: TObject);
    procedure aExportMazeExecute(Sender: TObject);
    procedure aSaveBlockExecute(Sender: TObject);
    procedure aSaveMazeExecute(Sender: TObject);
    procedure btnCollectionClick(Sender: TObject);
    procedure btnManyMazesClick(Sender: TObject);
    procedure btnOneMazeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CreateFileList;
    function GetSelectedVersion(): TGauntVersion;
  private
    FCurrentMaze: TGauntMaze;
    FMazeFileList: TMazeFileList;
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
  CreateFileList;
  editMazeFile1.Filter := 'Gauntlet Maze File|*.gmf';
  editMazeFile2.Filter := 'Gauntlet Maze File|*.gmf';
  editMazeFile3.Filter := 'Gauntlet Maze File|*.gmf';
  editMazeFile4.Filter := 'Gauntlet Maze File|*.gmf';
  editMazeFile5.Filter := 'Gauntlet Maze File|*.gmf';
  editMazeFile6.Filter := 'Gauntlet Maze File|*.gmf';
  editMazeFile7.Filter := 'Gauntlet Maze File|*.gmf';
  editMazeFile8.Filter := 'Gauntlet Maze File|*.gmf';
  editMazeFile9.Filter := 'Gauntlet Maze File|*.gmf';
  editMazeFile10.Filter := 'Gauntlet Maze File|*.gmf';
end;

procedure TfSaveExport.CreateFileList;
begin
  //create list of TFileNameEdit controls
  FMazeFileList := TMazeFileList.Create;
  FMazeFileList.Add(0, self.editMazeFile1);
  FMazeFileList.Add(1, self.editMazeFile2);
  FMazeFileList.Add(2, self.editMazeFile3);
  FMazeFileList.Add(3, self.editMazeFile4);
  FMazeFileList.Add(4, self.editMazeFile5);
  FMazeFileList.Add(5, self.editMazeFile6);
  FMazeFileList.Add(6, self.editMazeFile7);
  FMazeFileList.Add(7, self.editMazeFile8);
  FMazeFileList.Add(8, self.editMazeFile9);
  FMazeFileList.Add(9, self.editMazeFile10);
end;

procedure TfSaveExport.btnOneMazeClick(Sender: TObject);
begin
  panCollection.Enabled := False;
  self.btnSave.Enabled := True;
  btnSave.Action := aSaveMaze;
  btnExport.Action := aExportMaze;

  aSaveMaze.Enabled := True;
  aExportMaze.Enabled := True;

  panFormat.Enabled := False;

  btnCancel.Enabled := True;
  aCancel.Enabled := True;
end;

procedure TfSaveExport.btnManyMazesClick(Sender: TObject);
begin
  panCollection.Enabled := True;
  panFormat.Enabled := True;
  self.btnSave.Enabled := True;
  btnSave.Action := aSaveBlock;
  btnExport.Action := aExportBlock;

  aSaveBlock.Enabled := True;
  aExportBlock.Enabled := True;
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

procedure TfSaveExport.btnCollectionClick(Sender: TObject);
begin
  panCollection.Enabled := False;
  btnSave.Enabled := False;
  btnExport.Action := aExportCollection;
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

procedure TfSaveExport.aSaveMazeExecute(Sender: TObject);
var
  fsSave: TFileStream = nil;
begin
  //first of all, check if a MAZE has been selected on the main form
  //If not, do not allow the user do this because it is useless
  if not assigned(self.FCurrentMaze) then
  begin
    ShowMessage('Please, select a maze on the editor first.');
    self.ModalResult := mrCancel;
  end
  else
  begin
    dlgSaveMaze.Options := [TOpenOption.ofOverwritePrompt];
    if dlgSaveMaze.Execute then
    begin
      try
        try
          fsSave := TFileStream.Create(dlgSaveMaze.FileName, fmCreate);
          self.FCurrentMaze.ToFileStream(fsSave);
          self.FCurrentMaze.FileName := dlgSaveMaze.FileName;
          ShowMessage('Map saved successfully.');
          self.ModalResult := mrOk;
        except
          on E: Exception do
          begin
            ShowMessage('Error saving maze ' + dlgSaveMaze.FileName + ': ' + E.Message);
            self.ModalResult := mrCancel;
          end;
        end;
      finally
        if assigned(fsSave) then  fsSave.Free;
      end;
    end;

  end;
end;



procedure TfSaveExport.aCancelExecute(Sender: TObject);
begin
  self.ModalResult := mrCancel;
  self.Close;
end;

procedure TfSaveExport.aExportBlockExecute(Sender: TObject);
var
  missingFile, verifyResult: integer;
  fsSave: TFileStream = nil;
  BlockSize: integer = 0;
begin
  missingFile := CheckAllFilesExist(FMazeFileList);
  if missingFile <> -1 then
  begin
    ShowMessage('Please, check that file selected at Slot ' +
      IntToStr(missingFile + 1) + ' exists.');
  end
  else
  begin
    LoadIntoBlock(FMazeFileList, uData.block);
    verifyResult := VerifyBlock(uData.block, GetSelectedVersion, BlockSize);
    case verifyResult of
      -1:   //compile all mazes and verify errors
      begin

        dlgExportBlock.Options := [TOpenOption.ofOverwritePrompt];
        if dlgExportBlock.Execute then
        begin
          try
            try
              fsSave := TFileStream.Create(dlgExportBlock.FileName, fmCreate);
              ExportBlock(fsSave, uData.block, GetSelectedVersion());
            except
              on E: Exception do
                ShowMessage('Error exporting block ' + dlgExportBlock.FileName +
                  ': ' + E.Message);
            end;
          finally
            if assigned(fsSave) then fsSave.Free;
          end;
        end;
        self.ModalResult := mrCancel;
        //mrOk is reserved to flag single maze as unmodified

        ShowMessage('Block saved successfully.');
      end;
      -2:
      begin
        //The block is too big
        ShowMessage('The block is too big to be loaded on memory. Please, replace some maze with a smaller one.');
      end
      else
      begin
        //there is a compilation error
        ShowMessage('Maze at Slot ' + IntToStr(verifyResult) +
          ' has errors. Please, open it on the editor and correct any issue, then try again.');
      end;
    end;
  end;
end;

procedure TfSaveExport.aExportCollectionExecute(Sender: TObject);
begin
  ShowMessage(
    'Not implemented yet. Coming soon...  In the meanwhile, it''s a good idea to use ZX-BlockEditor to create the loadable file manually');

end;

procedure TfSaveExport.aExportMazeExecute(Sender: TObject);
var
  processResult: integer;
  fsSave: TFileStream = nil;
begin
  self.ModalResult := mrCancel;
  processResult := FCurrentMaze.ProcessMap;
  case processResult of
    -1, -2, -3: ShowMessage(ProcessResults.KeyData[processResult]);
    else
    begin
      dlgExportMaze.Options := [TOpenOption.ofOverwritePrompt];
      if dlgExportMaze.Execute then
      begin
        try
          try
            fsSave := TFileStream.Create(dlgExportMaze.FileName, fmCreate);
            self.FCurrentMaze.ExportToFileStream(fsSave);
          except
            on E: Exception do
              ShowMessage('Error exporting maze ' + dlgExportMaze.FileName +
                ': ' + E.Message);
          end;
        finally
          if assigned(fsSave) then fsSave.Free;
        end;
      end;
      self.ModalResult := mrOk;
      ShowMessage('Map compiled successfully. Size: ' + IntToStr(processResult));
    end;
  end;

end;

procedure TfSaveExport.aSaveBlockExecute(Sender: TObject);
var
  missingFile: integer;
  fsSave: TFileStream = nil;
begin
  missingFile := CheckAllFilesExist(FMazeFileList);
  if missingFile <> -1 then
  begin
    ShowMessage('Please, check that file selected at Slot ' +
      IntToStr(missingFile + 1) + ' exists.');
  end
  else
  begin
    try
      try
        LoadIntoBlock(FMazeFileList, uData.block);
        dlgSaveBlock.Options := [TOpenOption.ofOverwritePrompt];
        if dlgSaveBlock.Execute then
        begin
          fsSave := TFileStream.Create(dlgSaveBlock.FileName, fmCreate);
          SaveBlock(fsSave, uData.block);
          ShowMessage('Block saved successfully.');
          //mrOk is reserved to flag single maze as unmodified
          self.ModalResult := mrCancel;
        end;

      except
        on E: Exception do
          ShowMessage('Error saving block ' + dlgSaveBlock.FileName + ': ' + E.Message);
      end;
    finally
      if assigned(fsSave) then fsSave.Free;
    end;
  end;
end;

procedure TfSaveExport.SetCurrentMaze(AMaze: TGauntMaze);
begin
  FCurrentMaze := AMaze;
end;

function TfSaveExport.GetSelectedVersion(): TGauntVersion;
begin
  //gvMSX_DSK, gvMSX, gvMSX_TSX, gvZX_TZX, gvCPC_TZX, gvZX, gvCPC
  if btnMSX.Down then
    Result := gvMSX
  else
  if btnMSX_DSK.Down then
    Result := gvMSX_DSK
  else
  if btnZX.Down then
    Result := gvZX
  else
  if btnAmstrad.Down then
    Result := gvCPC
  else
  if btnZX_TZX.Down then
    Result := gvZX_TZX
  else
  if btnAmstrad_CDT.Down then
    Result := gvCPC_TZX
  else
    Result := gvMSX_TSX;
end;

end.
