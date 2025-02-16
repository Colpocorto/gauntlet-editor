unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  PairSplitter, RTTICtrls, RTTIGrids, laz.VirtualTrees, Types,
  uData, ActnList, ComCtrls, LResources, LCLIntf, LCLtype, StdActns, Buttons,
  StdCtrls, TplCheckBoxUnit, cyPanel, cyFlyingContainer, cyBevel,
  BCExpandPanels, BGRATheme, attabs, BGRABitmap,
  BGRACustomDrawn, BCComboBox, BGRAGradientScanner,
  BGRABitmapTypes, GraphType, ImgList;

type

  { TfMain }

  TfMain = class(TForm)
    aProcessMaze: TAction;
    aSaveExport: TAction;
    aGoEditName: TAction;
    aStyle4: TAction;
    aStyle5: TAction;
    aStyle6: TAction;
    aStyle7: TAction;
    aStyle8: TAction;
    aStyle3: TAction;
    aStyle2: TAction;
    aStyle1: TAction;
    alMain: TActionList;
    appProp: TApplicationProperties;
    btnStyle1: TSpeedButton;
    bvSpacer3: TcyBevel;
    cbDamage: TBCComboBox;
    btnExit: TSpeedButton;
    btnSave: TSpeedButton;
    btnStyle2: TSpeedButton;
    btnStyle3: TSpeedButton;
    btnStyle4: TSpeedButton;
    btnStyle5: TSpeedButton;
    btnStyle6: TSpeedButton;
    btnStyle7: TSpeedButton;
    btnStyle8: TSpeedButton;
    bvSpacer1: TcyBevel;
    bvSpacer2: TcyBevel;
    cbWrapH: TplCheckBox;
    cypanelMain: TCyPanel;
    cyFlyingContainer1: TcyFlyingContainer;
    aExit: TFileExit;
    aSaveMaze: TFileSaveAs;
    ilStyles: TImageList;
    ilTools: TImageList;
    leName: TLabeledEdit;
    panStyles: TPanel;
    cbWrapV: TplCheckBox;
    btEditName: TSpeedButton;
    tabsMain: TATTabs;
    BCEPanelsOpt: TBCExpandPanels;
    dgMap: TDrawGrid;
    hSplitter: TPairSplitter;
    hSplitterLeft: TPairSplitterSide;
    hSplitterRight: TPairSplitterSide;
    ilMenu: TImageList;
    ScrollBox1: TScrollBox;
    scrollOptions: TScrollBox;
    timerMain: TTimer;
    procedure aGoEditNameExecute(Sender: TObject);
    procedure aProcessMazeExecute(Sender: TObject);
    procedure cbDamageChange(Sender: TObject);
    procedure cbWrapHClick(Sender: TObject);
    procedure cbWrapVClick(Sender: TObject);
    procedure dgMapClick(Sender: TObject);
    procedure dgMapDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure dgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure aStyleExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure PatternButtonClick(Sender: TObject);
    procedure leNameDblClick(Sender: TObject);
    procedure leNameExit(Sender: TObject);
    procedure leNameKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure tabsMainTabChanged(Sender: TObject);
    procedure tabsMainTabPlusClick(Sender: TObject);
    procedure timerMainTimer(Sender: TObject);
    procedure CreateOptionPanels(AContainer: TWinControl);
    procedure SelectButtonByStyle(styleId: integer);
    procedure WriteCell(grid: TCustomGrid; index: integer);
    procedure ExpandPanelResize(Sender: TObject);
    function AddNewMaze: integer;
    function PatternButtonFactory(AContainer: TWinControl; AImageList: TImageList;
      AImageIndex: integer; AGroupIndex: integer; AName: string;
      ACaption: string; Id: integer; AOnClick: TNotifyEvent): TSpeedButton;
    procedure enablePatternButtons(low: integer; high: integer);
    procedure disablePatternButtons(low: integer; high: integer);
    procedure ConstructOnMap(Sender: TObject; shift: TShiftState);
    procedure PlaceObject(Sender: TObject; shift: TShiftState);
    procedure BindToTrap(Sender: TObject; shift: TShiftState);
  private
    procedure NameEdit;
    procedure CreateToolsCtrls(AParent: TCustomControl);

    procedure CreatePatternCtrls(AParent: TCustomControl);
    function ExpandPanelFactory(AContainer: TWinControl; AName: string;
      ACaption: string; AHeight: integer): TBCExpandPAnel;
  public

  end;

  TPatternButton = class(TSpeedButton)
  private
    FPatternId: integer;
  public
    property PatternId: integer read FPatternId write FPatternId;
  end;

  TGauntTool = (gtPointer, gtPlaceObj, gtEraser, gtBindTrap, gtGlass, gtConstruct,
    gtPlacePly, gtTestMaze, gtQuestion);

  TGauntToolBox = class(TComponent)
  private
    FTool: TGauntTool;
    FWallId: integer;
    FObjectId: integer;
    FTrapBound: boolean;
  public
    constructor Create(AOwner: TComponent);
    procedure SetTool(ATool: TGauntTool);
    function GetTool: TGauntTool;
    function SetWall(AWallId: integer): boolean;
    function GetWall: integer;
    function SetObject(AObjectId: integer): boolean;
    function GetObject: integer;
    property TrapBound: boolean read FTrapBound write FTrapBound;
  end;

procedure TextRectOut(customControl: TCustomControl; rect: TRect;
  x, y: integer; Text: string);

function GetShiftState(): TShiftState;

var
  fMain: TfMain;
  ToolBox: TGauntToolBox;

implementation

{$R *.lfm}

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
begin

  self.Caption := Application.Title;
  ToolBox := TGauntToolBox.Create(self);

  //Load graphics
  uData.loadGraphics(self, dgMap.DefaultColWidth);

  //Create controls dynamically

  //Create tabs
  AddNewMaze;

  //Create expandable panels
  CreateOptionPanels(self.scrollOptions);

  //Tool controls
  CreateToolsCtrls(BCEPanelsOpt.Panel(BCEPanelsOpt.IdxOfPanel('expTools')));
  //temporarily remove the pointer button since it has no function yet
  TSpeedButton(BCEPanelsOpt.Panel(BCEPanelsOpt.IdxOfPanel('expTools')).Controls[1]).Visible := False;

  //Pattern controls
  CreatePatternCtrls(BCEPanelsOpt.Panel(BCEPanelsOpt.IdxOfPanel('expObjects')));

  //click the pencil by default. Take into account the Controls[0] is the own expand/collapse button of the panel
  TSpeedButton(BCEPanelsOpt.Panel(BCEPanelsOpt.IdxOfPanel('expTools')).Controls[2]).Down
  := True;
  TSpeedButton(BCEPanelsOpt.Panel(BCEPanelsOpt.IdxOfPanel('expTools')).Controls[2]).Click;

  //Adjust dimensions
  dgMap.Width := dgMap.ColCount * dgMap.DefaultColWidth;
  dgMap.Height := dgMap.RowCount * dgMap.DefaultRowHeight;
  self.Height := 16 + dgMap.Height;
  hSplitter.Position := dgMap.Width + 32;

  //start timer for graphics refreshing
  //timerMain.Interval := 33;
  //timerMain.Enabled := True;

end;

function TfMain.AddNewMaze: integer;
var
  tab1: TATTabData;
begin
  tab1 := TATTabData.Create(nil);
  tab1.TabCaption := 'New Maze ' + IntToStr(tabsMain.TabCount + 1);
  tab1.TabObject := TGauntMaze.Create;
  TGauntMaze(tab1.TabObject).Name := tab1.TabCaption;
  tabsMain.AddTab(tabsMain.TabCount, tab1);
  tabsMain.ShowTab(tabsMain.TabCount - 1);
  tabsMain.TabIndex := tabsMain.TabCount - 1;
  Result := tabsMain.TabCount;
end;

procedure TfMain.CreateOptionPanels(AContainer: TWinControl);
var
  tempObj: TBCExpandPanel;
begin
  //create maze info panel
  tempObj := ExpandPanelFactory(AContainer, 'expMazeInfo', 'Info panel', 300);
  BCEPanelsOpt.AddPanel(tempObj);

  tempObj := ExpandPanelFactory(AContainer, 'expTools', 'Edition tools', 300);
  BCEPanelsOpt.AddPanel(tempObj);

  tempObj := ExpandPanelFactory(AContainer, 'expObjects', 'Objects', 1200);
  BCEPanelsOpt.AddPanel(tempObj);

  BCEPanelsOpt.ArrangePanels;

end;

procedure TfMain.FormResize(Sender: TObject);
begin

  hSplitter.Width := self.ClientWidth;

  // hSplitterLeft.Width:=self.ClientWidth-hSplitterLeft.Left;
end;

procedure TfMain.leNameDblClick(Sender: TObject);
begin
  aGoEditName.Execute;
end;

procedure TfMain.NameEdit;
begin
  if trim(leName.Text) = '' then
    ShowMessage('Please, enter a valid name')
  else
  begin
    leName.Color := $2f2f2f;
    leName.ReadOnly := True;
    //update object property
    TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).Name :=
      leName.Text;
    tabsMain.GetTabData(tabsMain.TabIndex).TabCaption := leName.Text;
    leName.SelLength := 0;
  end;
end;

procedure TfMain.leNameExit(Sender: TObject);
begin
  NameEdit;
end;

procedure TfMain.leNameKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    NameEdit;
  end;
end;

procedure TfMain.tabsMainTabChanged(Sender: TObject);
begin
  //sync style button
  SelectButtonByStyle(TGauntMaze(tabsMain.GetTabData(
    TATTabs(Sender).TabIndex).TabObject).Style.id);

  //sync dungeon flags
  cbWrapV.Checked := TGauntMaze(tabsMain.GetTabData(
    TATTabs(Sender).TabIndex).TabObject).GetVertWrap();
  cbWrapH.Checked := TGauntMaze(tabsMain.GetTabData(
    TATTabs(Sender).TabIndex).TabObject).GetHorzWrap();

  //sync damage others mode
  if (TGauntMaze(tabsMain.GetTabData(TATTabs(Sender).TabIndex).TabObject).HurtPlayers
    =
    False) and (TGauntMaze(tabsMain.GetTabData(
    TATTabs(Sender).TabIndex).TabObject).StunPlayers = False) then
  begin
    cbDamage.ItemIndex := 0;
  end
  else
  if TGauntMaze(tabsMain.GetTabData(TATTabs(Sender).TabIndex).TabObject).HurtPlayers =
    True then
  begin
    cbDamage.ItemIndex := 1;
  end
  else
    cbDamage.ItemIndex := 2;

  //sync maze name
  leName.Text := TGauntMaze(tabsMain.GetTabData(
    TATTabs(Sender).TabIndex).TabObject).Name;

  dgMap.Repaint;
end;

procedure TfMain.SelectButtonByStyle(styleId: integer);
var
  i: integer;
begin
  for i := 0 to panStyles.ControlCount - 1 do
  begin
    if TSpeedButton(panStyles.Controls[i]).ImageIndex = styleId then
    begin
      TSpeedButton(panStyles.Controls[i]).Down := True;
      break;
    end;
  end;
end;

procedure TfMain.tabsMainTabPlusClick(Sender: TObject);
begin
  self.AddNewMaze;
end;

procedure TfMain.timerMainTimer(Sender: TObject);
begin
  dgMap.Repaint;
end;

procedure TfMain.dgMapDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  MapCol, MapRow: integer;
  cell: integer;
  trapBound: boolean = False;
begin

  //check if cell belongs to frame
  dgMap.Canvas.Brush.Style := bsSolid;
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
      cell := TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).MapData
        [MapCol, MapRow];
      if (cell and $80) <> 0 then
      begin
        trapBound := True;
        cell := cell and $7f;
      end;
      case cell of
        0:
        begin
          dgMap.Canvas.Brush.Color := dgMap.Color;
          dgMap.Canvas.Brush.Style := bsSolid;
          dgMap.Canvas.FillRect(aRect);      //delete block
        end;
        1..$45:
        begin
          if (cell <= $10) or ((cell >= $33) and (cell <= $35)) then
          begin
            //apply style
            cell := cell + STYLES_OFFSET * TGauntMaze(
              tabsMain.GetTabData(tabsMain.tabIndex).TabObject).Style.id;
          end;

          //add the visual effect to indicate the block will disappear when a
          //trap is hit
          if not (trapBound) then
          begin
            ilMap.Draw(dgMap.Canvas,
              aRect.TopLeft.X, aRect.TopLeft.Y, patternIndexMap[cell], True);
          end
          else
          begin
            ilMap.Draw(dgMap.Canvas,
              aRect.TopLeft.X, aRect.TopLeft.Y,
              patternIndexMap[cell], TDrawingStyle.dsNormal, itImage,
              TGraphicsDrawEffect.gdeHighlighted);
          end;
        end;

      end;

    end;
  end;
end;

procedure TfMain.dgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if ssLeft in Shift then
  begin
    ConstructOnMap(Sender, Shift);
  end;
end;

procedure TfMain.aStyleExecute(Sender: TObject);
begin
  TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).Style :=
    gauntStyles[TAction(Sender).ImageIndex];
  dgMap.Repaint;

end;

procedure TfMain.dgMapClick(Sender: TObject);
begin
  case toolBox.GetTool of
    gtEraser:
      ConstructOnMap(Sender, GetShiftState);
    gtConstruct:
      ConstructOnMap(Sender, GetShiftState);
    gtPlaceObj:
      PlaceObject(Sender, GetShiftState);
    gtBindTrap:
      BindToTrap(Sender, GetShiftState);
  end;
  dgMap.Repaint;
end;

procedure TfMain.BindToTrap(Sender: TObject; shift: TShiftState);
var
  Col, Row: integer;
  minCol: integer = 1;
  currentValue: integer;
  grid: TCustomGrid;
begin
  grid := TCustomGrid(Sender);
  // Get the cell coordinates from the mouse click
  grid.MouseToCell(grid.ScreenToClient(Mouse.CursorPos).X,
    grid.ScreenToClient(Mouse.CursorPos).Y, Col, Row);
  if TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).GetHorzWrap() then
    minCol := 0
  else
    minCol := 1;

  if (Col > minCol) and (Col < 33) and (Row > 1) and (Row < 33) then
  begin
    //update map matrix
    currentValue := TGauntMaze(tabsMain.GetTabData(
      tabsMain.TabIndex).TabObject).MapData[Col - 1, Row - 1];

    if (currentValue < $80) then
      TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).MapData[Col -
        1, Row - 1] := currentValue or $80
    else
      TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).MapData[Col -
        1, Row - 1] := currentValue and $7f;
  end;

end;

procedure TfMain.PlaceObject(Sender: TObject; shift: TShiftState);
begin
  if ssCtrl in shift then
  begin
    WriteCell(dgMap, 0);
  end
  else
  begin
    WriteCell(dgMap, toolBox.GetObject);
  end;
end;

procedure TfMain.ConstructOnMap(Sender: TObject; shift: TShiftState);
begin
  case toolBox.GetTool() of
    gtEraser:
    begin
      WriteCell(dgMap, 0);
    end;
    gtConstruct:
      if ssCtrl in shift then
      begin
        WriteCell(dgMap, 0);
      end
      else
      begin
        WriteCell(dgMap, toolBox.GetWall);
      end;
  end;
end;

procedure TfMain.cbWrapVClick(Sender: TObject);
begin
  TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).SetVertWrap(
    cbWrapV.Checked);
end;

procedure TfMain.cbWrapHClick(Sender: TObject);
begin
  TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).SetHorzWrap(
    cbWrapH.Checked);
  dgMap.Repaint;
end;

procedure TfMain.cbDamageChange(Sender: TObject);
begin
  case TBCComboBox(Sender).ItemIndex of
    0:
    begin
      TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).HurtPlayers := False;
      TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).StunPlayers := False;
    end;
    1:
    begin
      TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).HurtPlayers := True;
      TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).StunPlayers := False;
    end;
    2:
    begin
      TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).HurtPlayers := False;
      TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).StunPlayers := True;
    end;
  end;
end;

procedure TfMain.aGoEditNameExecute(Sender: TObject);
begin
  self.leName.ReadOnly := False;
  self.leName.Color := $5f5f5f;
end;

procedure TfMain.aProcessMazeExecute(Sender: TObject);
var
  processResult: integer;
begin
  processResult := TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).ProcessMap;
  case processResult of
    -1: ShowMessage('The walls layer is too big!');
    -2: ShowMessage ('The RLE layer is too big');
    else
        ShowMessage ('Map compiled successfully. Size: ' + intToStr(processResult));
  end;
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

procedure TfMain.WriteCell(grid: TCustomGrid; index: integer);
var
  Col, Row: integer;
  minCol: integer = 1;
begin
  // Get the cell coordinates from the mouse click
  grid.MouseToCell(grid.ScreenToClient(Mouse.CursorPos).X,
    grid.ScreenToClient(Mouse.CursorPos).Y, Col, Row);
  if TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).GetHorzWrap() then
    minCol := 0
  else
    minCol := 1;

  if (Col > minCol) and (Col < 33) and (Row > 1) and (Row < 33) then
  begin
    //update map matrix
    TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).MapData[Col -
      1, Row - 1] := index;
  end;

end;

procedure TfMain.CreateToolsCtrls(AParent: TCustomControl);
begin
  PatternButtonFactory(AParent, ilTools, 0, 2, 'btnPointer', 'Selection tool',
    0, @ToolButtonClick);
  PatternButtonFactory(AParent, ilTools, 1, 2, 'btnConstruct', 'Build walls and gates',
    1, @ToolButtonClick);
  PatternButtonFactory(AParent, ilTools, 2, 2, 'btnObject', 'Place object',
    2, @ToolButtonClick);
  PatternButtonFactory(AParent, ilTools, 3, 2, 'btnEraser', 'Erase block', 3,
    @ToolButtonClick);
  PatternButtonFactory(AParent, ilTools, 4, 2, 'btnBindTrap',
    'Make block disappear', 4, @ToolButtonClick);
  PatternButtonFactory(AParent, ilTools, 5, 2, 'btnGlass', 'Analyse block',
    5, @ToolButtonClick);
  PatternButtonFactory(AParent, ilTools, 6, 2, 'btnPlacePly',
    'Place Player', 6, @ToolButtonClick);
  PatternButtonFactory(AParent, ilTools, 7, 0, 'btnVerify', 'Verify the Maze',
    7, @aProcessMazeExecute);

end;

procedure TfMain.CreatePatternCtrls(AParent: TCustomControl);
var
  i: integer;
begin
  //Add walls and objects
  for i := 1 to $36 do
  begin
    PatternButtonFactory(AParent, uData.ilMap, PatternIndexMap[i], 3,
      'btnPattern' + IntToStr(i), FindPatternDataById(i).desc, i, @PatternButtonClick);
  end;

  //Add enemies
  PatternButtonFactory(AParent, uData.ilMap, PatternIndexMap[$40],
    3, 'btnPattern' + IntToStr($40), FindPatternDataById($40).desc,
    $40, @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap, PatternIndexMap[$41],
    3, 'btnPattern' + IntToStr($41), FindPatternDataById($41).desc,
    $41, @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap, PatternIndexMap[$42],
    3, 'btnPattern' + IntToStr($42), FindPatternDataById($42).desc,
    $42, @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap, PatternIndexMap[$48], 3,
    'btnPattern' + IntToStr($48), FindPatternDataById($48).desc, $48,
    @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap, PatternIndexMap[$49], 3,
    'btnPattern' + IntToStr($49), FindPatternDataById($49).desc, $49,
    @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap, PatternIndexMap[$4a],
    3, 'btnPattern' + IntToStr($4a), FindPatternDataById($4a).desc,
    $4a, @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap, PatternIndexMap[$50],
    3, 'btnPattern' + IntToStr($50), FindPatternDataById($50).desc,
    $50, @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap, PatternIndexMap[$51],
    3, 'btnPattern' + IntToStr($51), FindPatternDataById($51).desc,
    $51, @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap,
    PatternIndexMap[$52], 3, 'btnPattern' + IntToStr($52),
    FindPatternDataById($52).desc, $52, @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap,
    PatternIndexMap[$58], 3, 'btnPattern' + IntToStr($58),
    FindPatternDataById($58).desc, $58, @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap,
    PatternIndexMap[$59], 3, 'btnPattern' + IntToStr($59),
    FindPatternDataById($59).desc, $59, @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap,
    PatternIndexMap[$5a], 3, 'btnPattern' + IntToStr($5a),
    FindPatternDataById($5a).desc, $5a, @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap,
    PatternIndexMap[$60], 3, 'btnPattern' + IntToStr($60),
    FindPatternDataById($60).desc, $60, @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap,
    PatternIndexMap[$61], 3, 'btnPattern' + IntToStr($61),
    FindPatternDataById($61).desc, $61, @PatternButtonClick);
  PatternButtonFactory(AParent, uData.ilMap,
    PatternIndexMap[$62], 3, 'btnPattern' + IntToStr($62),
    FindPatternDataById($62).desc, $62, @PatternButtonClick);
  PatternButtonFactory(AParent,
    uData.ilMap, PatternIndexMap[$68], 3, 'btnPattern' + IntToStr($68),
    FindPatternDataById($68).desc, $68, @PatternButtonClick);

end;

procedure TfMain.ToolButtonClick(Sender: TObject);
begin

  case TPatternButton(Sender).PatternId of
    0: toolBox.SetTool(gtPointer);
    1: toolBox.SetTool(gtConstruct);
    2: toolBox.SetTool(gtPlaceObj);
    3: toolBox.SetTool(gtEraser);
    4: toolBox.SetTool(gtBindTrap);
    5: toolBox.SetTool(gtGlass);
    6: toolBox.SetTool(gtPlacePly);
    7: toolBox.SetTool(gtTestMaze);
  end;
end;

procedure TfMain.PatternButtonClick(Sender: TObject);
begin
  case toolBox.GetTool of
    gtConstruct:
      toolBox.SetWall(TPatternButton(Sender).PatternId);
    gtPlaceObj:
      toolBox.SetObject(TPatternButton(Sender).PatternId);
  end;
end;

function TfMain.PatternButtonFactory(AContainer: TWinControl;
  AImageList: TImageList; AImageIndex: integer; AGroupIndex: integer;
  AName: string; ACaption: string; Id: integer; AOnClick: TNotifyEvent): TSpeedButton;
begin
  Result := TPatternButton.Create(AContainer);

  Result.Align := alNone;
  Result.Anchors := [akLeft, akTop];
  Result.AnchorSide[akLeft].Control := nil;
  Result.AnchorSide[akTop].Control := nil;
  Result.Caption := '';
  Result.Hint := ACaption;
  Result.Name := AName;
  Result.ShowHint := True;
  Result.AutoSize := False;
  Result.ImageWidth := 32;
  Result.Images := AImageList;
  Result.ImageIndex := AImageIndex;
  Result.flat := True;
  Result.Visible := True;
  Result.Enabled := True;
  Result.BorderSpacing.CellAlignHorizontal := ccaFill;
  Result.OnClick := AOnClick;
  Result.Parent := AContainer;
  Result.ControlStyle := Result.ControlStyle + [csFixedWidth, csFixedHeight];
  TPatternButton(Result).PatternId := Id;
  Result.GroupIndex := AGroupIndex;

end;

function TfMain.ExpandPanelFactory(AContainer: TWinControl; AName: string;
  ACaption: string; AHeight: integer): TBCExpandPAnel;
var
  tmppic: TPicture;
begin
  tmppic := TPicture.Create;

  Result := TBCExpandPanel.Create(AContainer);
  with Result do
  begin
    Align := alCustom;
    Anchors := [akTop, akLeft, akRight];
    Animated := True;
    AnimationSpeed := 60;
    AutoSize := False;
    BevelWidth := 16;
    BevelInner := bvSpace;
    BevelOuter := bvNone;
    Button.Caption := ACaption;
    Button.Color := $005F5F5F;
    Button.ColorExpanded := $005F5F5F;
    Button.ColorHighlight := $005F5F5F;
    Button.ColorShadow := $005F5F5F;
    Button.Flat := True;
    Button.Font.Color := $e0e0e0;
    Button.Font.Quality := fqCleartypeNatural;
    try
      tmppic.LoadFromFile(resourcesDir + 'exp-arrow-right.png');
      Button.GlyphCollapsed := tmppic.Bitmap;
      tmppic.LoadFromFile(resourcesDir + 'exp-arrow-down.png');
      Button.GlyphExpanded := tmppic.Bitmap;
    except
      on E: Exception do GauntDebugLn('Error loading ExpandPanel glyphs: ' + E.Message);
    end;
    Button.GlyphLayout := glLeft;
    Button.Height := 32;
    Button.Left := 0;
    Button.Style := bbsTab;
    Button.TabWidth := -102;
    Button.TextLayout := BCEXPANDPANELS.TTextLayout.tlCenter;
    ButtonPosition := akTop;
    ButtonSize := 32;
    Name := AName;
    Caption := '';
    ChildSizing.EnlargeHorizontal := crsAnchorAligning;
    ChildSizing.EnlargeVertical := crsAnchorAligning;
    ChildSizing.ShrinkHorizontal := crsSameSize;
    ChildSizing.ShrinkVertical := crsSameSize;
    ChildSizing.ControlsPerLine :=
      trunc((ClientWidth - BevelWidth - ChildSizing.HorizontalSpacing) /
      (32 + ChildSizing.HorizontalSpacing));

    ChildSizing.HorizontalSpacing := 16;
    ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    ChildSizing.VerticalSpacing := 16;
    CollapseKind := akTop;
    Color := $1f1f1f;
    Height := AHeight;
    ParentBackground := False;
    Parent := AContainer;
    Rounding.RoundX := 0;
    Rounding.RoundY := 0;
    Visible := True;
    Width := AContainer.ClientWidth;
    OnResize := @ExpandPanelResize;
  end;
end;

procedure TfMain.ExpandPanelResize(Sender: TObject);
begin
  TBCExpandPanel(Sender).ChildSizing.ControlsPerLine :=
    trunc((TBCExpandPanel(Sender).ClientWidth - TBCExpandPanel(Sender).BevelWidth -
    TBCExpandPanel(Sender).ChildSizing.HorizontalSpacing) /
    (32 + TBCExpandPanel(Sender).ChildSizing.HorizontalSpacing));

end;

procedure TfMain.enablePatternButtons(low: integer; high: integer);
var
  i: integer;
  panel: TBCExpandPanel;
  tmpobj: TObject;
begin
  panel := BCEPanelsOpt.Panel(BCEPanelsOpt.IdxOfPanel('expObjects'));
  for i := 0 to panel.ControlCount - 1 do
  begin
    tmpobj := panel.Controls[i];
    if tmpobj is TSpeedButton then
    begin
      if (TPatternButton(panel.Controls[i]).PatternId >= low) and
        (TPatternButton(panel.Controls[i]).PatternId <= high) then
        TPatternButton(panel.Controls[i]).Enabled := True;
    end;
  end;
end;

procedure TfMain.disablePatternButtons(low: integer; high: integer);
var
  i: integer;
  panel: TBCExpandPanel;
  tmpobj: TObject;
begin
  panel := BCEPanelsOpt.Panel(BCEPanelsOpt.IdxOfPanel('expObjects'));
  for i := 0 to panel.ControlCount - 1 do
  begin
    tmpobj := panel.Controls[i];
    if tmpobj is TSpeedButton then
    begin
      if (TPatternButton(panel.Controls[i]).PatternId >= low) and
        (TPatternButton(panel.Controls[i]).PatternId <= high) then
        TPatternButton(panel.Controls[i]).Enabled := False;
    end;
  end;
end;

constructor TGauntToolBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.FTool := gtConstruct;
  self.FObjectId := $12;
  self.FWallId := $10;
  self.FTrapBound := False;
end;

procedure TGauntToolBox.SetTool(ATool: TGauntTool);
var
  i: integer;
  panel: TBCExpandPanel;
begin
  //this coupling is quite ugly, should be changed by a different approach in the future
  panel := fMain.BCEPanelsOpt.Panel(fMain.BCEPanelsOpt.IdxOfPanel('expObjects'));

  FTool := ATool;
  case FTool of
    gtConstruct:
    begin
      TfMain(self.Owner).enablePatternButtons($01, $12);
      TfMain(self.Owner).disablePatternButtons($13, $6f);
      //push down the pattern button
      for i := 1 to panel.ControlCount - 1 do
        //0 is the header, so first user added control should be 1
      begin
        if (TPatternButton(panel.Controls[i]).PatternId = self.GetWall) then
        begin
          TPatternButton(panel.Controls[i]).Down := True;
        end;
      end;
    end;
    gtPlaceObj:
    begin
      TfMain(self.Owner).disablePatternButtons($01, $12);
      TfMain(self.Owner).enablePatternButtons($13, $6f);
      //push down the pattern button
      for i := 1 to panel.ControlCount - 1 do
      begin
        if (TPatternButton(panel.Controls[i]).PatternId = self.GetObject) then
        begin
          TPatternButton(panel.Controls[i]).Down := True;
        end;
      end;
    end;
    else
    begin
      TfMain(self.Owner).disablePatternButtons($01, $6f);
    end;
  end;
end;

function TGauntToolBox.GetTool(): TGauntTool;
begin
  Result := FTool;
end;

function TGauntToolBox.SetWall(AWallId: integer): boolean;
begin
  FWallId := AWallId;
  Result := True;
end;

function TGauntToolBox.GetWall: integer;
begin
  Result := FWallId;
end;

function TGauntToolBox.SetObject(AObjectId: integer): boolean;
begin
  FObjectId := AObjectId;
  Result := True;
end;

function TGauntToolBox.GetObject(): integer;
begin
  Result := FObjectId;
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
