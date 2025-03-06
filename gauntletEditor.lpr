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
  runtimetypeinfocontrols,
  LazLogger,
  lazcontrols,
  uMain,
  uData,
  uSaveExport { you can add units after this };

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  {$PUSH}
  {$WARN 5044 OFF}
  Application.MainFormOnTaskbar := True;
  {$POP}
  Application.Initialize;
  uData.InitData;
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TfSaveExport, fSaveExport);
  //fSaveExport.Parent := fMain;
  Application.Run;
  uData.CleanData;
end.
