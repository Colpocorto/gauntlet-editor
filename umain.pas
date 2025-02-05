unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  PairSplitter, RTTICtrls, RTTIGrids, laz.VirtualTrees, DividerBevel, Types,
  uData, ActnList, ComCtrls, LResources, StdCtrls, LCLIntf, LCLtype,
  BCExpandPanels;

type

  { TfMain }

  TfMain = class(TForm)
    alMain: TActionList;
    appProp: TApplicationProperties;
    BCEPanelsOpt: TBCExpandPanels;
    dgMap: TDrawGrid;
    hSplitter: TPairSplitter;
    hSplitterLeft: TPairSplitterSide;
    hSplitterRight: TPairSplitterSide;
    ScrollBox1: TScrollBox;
    scrollOptions: TScrollBox;
    timerMain: TTimer;
    procedure appPropActionExecute(AAction: TBasicAction; var Handled: boolean);
    procedure BCEPanelsOptArrangePanels(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure dgMapClick(Sender: TObject);
    procedure dgMapDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure dgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure hSplitterChangeBounds(Sender: TObject);
    procedure panOptionsClick(Sender: TObject);
    procedure timerMainTimer(Sender: TObject);
    procedure CreateOptionPanels(Container: TControl);
    procedure CreateInfoCtrls(Container: TControl);
  private

    index_repaint_needed: boolean;
  public

  end;

procedure TextRectOut(customControl: TCustomControl; rect: TRect;
  x, y: integer; Text: string);
procedure WriteCell(grid: TCustomGrid; index: integer);
function GetShiftState(): TShiftState;

var
  fMain: TfMain;

implementation

{$R *.lfm}

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
begin

  //Initialize maps
  uData.initMap(mapData);
  uData.initMap(visitedData);

  self.index_repaint_needed := True;

  //Load graphics
  uData.loadGraphics(self.scrollOptions);

  //Create controls dynamically

  //Create expandable panels
  CreateOptionPanels(self.hSplitterRight);

  //Panel maze info
  CreateInfoCtrls(BCEPanelsOpt.Panel(BCEPanelsOpt.IdxOfPanel('expMazeInfo')));

  //Adjust dimensions
  dgMap.Width := dgMap.ColCount * dgMap.DefaultColWidth;
  dgMap.Height := dgMap.RowCount * dgMap.DefaultRowHeight;
  self.Height := 16 + dgMap.Height;
  hSplitter.Position := dgMap.Width;

  //start timer for graphics refreshing
  timerMain.Enabled := True;

end;
procedure TfMain.CreateInfoCtrls(Container: TControl);
begin


end;

procedure TfMain.CreateOptionPanels(Container: TControl);
var
  tempObj: TBCExpandPanel;
begin
  //create maze info panel
  tempObj := TBCExpandPanel.Create(Container);
  tempObj.Name := 'expMazeInfo';
  tempObj.Visible := True;
  BCEPanelsOpt.AddPanel(tempObj);

  tempObj := TBCExpandPanel.Create(Container);
  tempObj.Name := 'expMazeStyle';
  BCEPanelsOpt.AddPanel(tempObj);

  tempObj := TBCExpandPanel.Create(Container);
  tempObj.Name := 'expTools';
  BCEPanelsOpt.AddPanel(tempObj);

  BCEPanelsOpt.ArrangePanels;

end;

procedure TfMain.FormResize(Sender: TObject);
begin

  hSplitter.Width := self.ClientWidth;

  // hSplitterLeft.Width:=self.ClientWidth-hSplitterLeft.Left;
end;

procedure TfMain.hSplitterChangeBounds(Sender: TObject);
begin

end;

procedure TfMain.panOptionsClick(Sender: TObject);
begin

end;

procedure TfMain.timerMainTimer(Sender: TObject);
begin
  dgMap.Repaint;
end;

procedure TfMain.dgMapDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  MapCol, MapRow: integer;
begin

  //check if cell belongs to frame

  dgMap.Canvas.Brush.Color := $d08010;
  dgMap.Canvas.Font.Color := clWhite;

  if ((aCol = 0) or (aCol = dgMap.ColCount - 1)) and (aRow <> 0) and
    (aRow <> dgMap.RowCount - 1) then
  begin
    //print row number - 1
    TextRectOut(dgMap, aRect, 0, 0, IntToStr(aRow - 1));
  end
  else
  if ((aRow = 0) or (aRow = dgMap.RowCount - 1)) and (aCol <> 0) and
    (aCol <> dgMap.ColCount - 1) then
  begin
    //print col number - 1
    TextRectOut(dgMap, aRect, 0, 0, IntToStr(aCol - 1));
  end
  else
  begin
    if not ((aCol = 0) or (aCol = 33) or (aRow = 0) or (aRow = 33)) then
    begin
      //draw map

      //normalize col/row first to match matrix indices
      MapCol := aCol - 1;
      MapRow := aRow - 1;

      case uData.mapData[MapCol, MapRow] of
        0:
        begin
          dgMap.Canvas.Brush.Color := dgMap.Color;
          dgMap.Canvas.FillRect(aRect);      //delete block
        end;
        1..31: ilMap.Draw(dgMap.Canvas,
            aRect.TopLeft.X, aRect.TopLeft.Y, 0, True);
      end;

    end;
  end;

end;

procedure TfMain.dgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if ssLeft in Shift then
  begin
    if ssCtrl in Shift then
    begin
      WriteCell(dgMap, 0);
    end
    else
    begin
      WriteCell(dgMap, 1);
    end;
  end;
end;

procedure TfMain.appPropActionExecute(AAction: TBasicAction; var Handled: boolean);
begin

end;

procedure TfMain.BCEPanelsOptArrangePanels(Sender: TObject);
begin

end;

procedure TfMain.Button2Click(Sender: TObject);
begin

end;

procedure TfMain.dgMapClick(Sender: TObject);
begin
  if ssCtrl in GetShiftState then
  begin
    WriteCell(dgMap, 0);
  end
  else
  begin
    WriteCell(dgMap, 1);
  end;
  dgMap.Repaint;
end;

procedure TextRectOut(customControl: TCustomControl; rect: TRect;
  x, y: integer; Text: string);
var
  ts: TTextStyle;
begin
  ts := customControl.Canvas.TextStyle;
  ts.Alignment := taCenter;
  ts.Layout := Graphics.TTextLayout.tlCenter;
  customControl.Canvas.FillRect(rect);
  customControl.Canvas.TextRect(rect, x, y, Text, ts);

end;

procedure WriteCell(grid: TCustomGrid; index: integer);
var
  Col, Row: integer;
begin
  // Get the cell coordinates from the mouse click
  grid.MouseToCell(grid.ScreenToClient(Mouse.CursorPos).X,
    grid.ScreenToClient(Mouse.CursorPos).Y, Col, Row);
  if (Col > 0) and (Col < 33) and (Row > 0) and (Row < 33) then
  begin
    //update map matrix
    uData.mapData[Col - 1, Row - 1] := index;

  end;

end;

function GetShiftState(): TShiftState;
begin
  // Initialize ShiftState as an empty set
  Result := [];

  // Check if Shift key is pressed
  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);

  // Check if Ctrl key is pressed
  if GetKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);

  // Check if Alt key is pressed
  if GetKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);

  // Check if Left button is pressed
  if GetKeyState(VK_LBUTTON) < 0 then
    Include(Result, ssLeft);

end;

end.
