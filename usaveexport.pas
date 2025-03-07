unit uSaveExport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, uData;

type

  { TfSaveExport }

  TfSaveExport = class(TForm)
    btnCanel: TBitBtn;
    btnExport: TBitBtn;
    btnSave: TBitBtn;
    dialExportBlock: TSaveDialog;
    dialSaveMaze: TSaveDialog;
    dialExportMaze: TSaveDialog;
    dialSaveBlock: TSaveDialog;
  private
    FCurrentMaze: TGauntMaze;
  public
    procedure SetCurrentMaze(AMaze: TGauntMaze);
  end;

var
  fSaveExport: TfSaveExport;

implementation

{$R *.lfm}

procedure TfSaveExport.SetCurrentMaze(AMaze: TGauntMaze);
begin
  FCurrentMaze := AMaze;
end;

end.

