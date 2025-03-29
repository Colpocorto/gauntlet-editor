program gauntletEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  LazLogger,
  lazcontrols,
  uMain,
  uData,
  uSaveExport,
  umazetools,
  ukruskal,
  uloadimport { you can add units after this };

  {$R *.res}


begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  {$PUSH}
  {$WARN 5044 OFF}
  Application.MainFormOnTaskbar := True;
  {$POP}
  Application.Initialize;
  uData.InitData;
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TfSaveExport, fSaveExport);
  Application.CreateForm(TfMazeTools, fMazetools);
  Application.CreateForm(TfLoadImport, fLoadImport);
  Application.Run;
  uData.CleanData;
end.
