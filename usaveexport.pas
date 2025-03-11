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
    panSelection: TPanel;
    btnOneMaze: TSpeedButton;
    procedure aCancelExecute(Sender: TObject);
    procedure aExportBlockExecute(Sender: TObject);
    procedure aExportMazeExecute(Sender: TObject);
    procedure aSaveBlockExecute(Sender: TObject);
    procedure aSaveMazeExecute(Sender: TObject);
    procedure btnManyMazesClick(Sender: TObject);
    procedure btnOneMazeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CreateFileList;
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
  fsSave: TFileStream;
begin
  self.ModalResult := mrCancel;
  try
    try
      if dlgSaveMaze.Execute then
      begin
        fsSave := TFileStream.Create(dlgSaveMaze.FileName, fmCreate);
        self.FCurrentMaze.ToFileStream(fsSave);
        ShowMessage('Map saved successfully.');
        self.ModalResult := mrOk;
      end;
    except
      on E: Exception do
      begin ShowMessage('Error saving maze ' +
          fsSave.FileName + ': ' + E.Message);
          self.ModalResult := mrCancel;
      end;
    end;
  finally
    fsSave.Free;
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
  fsSave: TFileStream;
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
    verifyResult := VerifyBlock(uData.block);
    case verifyResult of
      -1:   //compile all mazes and verify errors
      begin
        try
          try
            if dlgExportBlock.Execute then
            begin
              fsSave := TFileStream.Create(dlgExportBlock.FileName, fmCreate);
              ExportBlock(fsSave, uData.block, TGauntVersion.gvMSX_DSK);
            end;
            self.ModalResult := mrCancel;
            //mrOk is reserved to flag single maze as unmodified
            ShowMessage('Block saved successfully.');
          except
            on E: Exception do
              ShowMessage('Error exporting block ' + fsSave.FileName + ': ' + E.Message);
          end;
        finally
          fsSave.Free;
        end;
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

procedure TfSaveExport.aExportMazeExecute(Sender: TObject);
var
  processResult: integer;
  fsSave: TFileStream;
begin
  self.ModalResult := mrCancel;
  processResult := FCurrentMaze.ProcessMap;
  case processResult of
    -1, -2, -3: ShowMessage(ProcessResults.KeyData[processResult]);
    else
    begin
      try
        try
          if dlgExportMaze.Execute then
          begin
            fsSave := TFileStream.Create(dlgExportMaze.FileName, fmCreate);
            self.FCurrentMaze.ExportToFileStream(fsSave);
          end;
          self.ModalResult := mrOk;
          ShowMessage('Map compiled successfully. Size: ' + IntToStr(processResult));

        except
          on E: Exception do
            ShowMessage('Error exporting maze ' + fsSave.FileName + ': ' + E.Message);
        end;
      finally
        fsSave.Free;
      end;
    end;
  end;

end;

procedure TfSaveExport.aSaveBlockExecute(Sender: TObject);
var
  missingFile: integer;
  fsSave: TFileStream;
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
        if dlgSaveBlock.Execute then
        begin
          fsSave := TFileStream.Create(dlgSaveBlock.FileName, fmCreate);
          SaveBlock(fsSave, uData.block);
        end;
        self.ModalResult := mrCancel; //mrOk is reserved to flag single maze as unmodified
        ShowMessage('Block saved successfully.');

      except
        on E: Exception do
          ShowMessage('Error saving block ' + fsSave.FileName + ': ' + E.Message);
      end;
    finally
      fsSave.Free;
    end;
  end;
end;

procedure TfSaveExport.SetCurrentMaze(AMaze: TGauntMaze);
begin
  FCurrentMaze := AMaze;
end;

end.
