unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  PairSplitter, RTTICtrls, RTTIGrids, laz.VirtualTrees, CheckBoxThemed, Types,
  uData, ActnList, ComCtrls, LResources, LCLIntf, LCLtype, StdActns, Buttons,
  StdCtrls, TplCheckBoxUnit, cyPanel, cyFlyingContainer, cyBevel, cyCheckbox,
  BCExpandPanels, BGRATheme, attabs, BGRABitmap, BGRASpeedButton,
  BGRACustomDrawn, BCComboBox, BGRAThemeCheckBox, BGRAGradientScanner,
  BGRABitmapTypes;

type

  { TfMain }

  TfMain = class(TForm)
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
    procedure leNameDblClick(Sender: TObject);
    procedure leNameExit(Sender: TObject);
    procedure leNameKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure tabsMainTabChanged(Sender: TObject);
    procedure tabsMainTabPlusClick(Sender: TObject);
    procedure timerMainTimer(Sender: TObject);
    procedure CreateOptionPanels(AContainer: TWinControl);
    procedure SelectButtonByStyle(styleId: integer);
    procedure WriteCell(grid: TCustomGrid; index: integer);

    function AddNewMaze: integer;
  private
    procedure NameEdit;
    procedure CreateInfoCtrls(AParent: TCustomControl);
    function ExpandPanelFactory(AContainer: TWinControl; AName: string;
      ACaption: string): TBCExpandPAnel;
  public

  end;

procedure TextRectOut(customControl: TCustomControl; rect: TRect;
  x, y: integer; Text: string);

function GetShiftState(): TShiftState;

var
  fMain: TfMain;

implementation

{$R *.lfm}

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
begin

  self.Caption := Application.Title;

  //Load graphics
  uData.loadGraphics(self, dgMap.DefaultColWidth);

  //Create controls dynamically

  //Create tabs
  AddNewMaze;

  //Create expandable panels
  CreateOptionPanels(self.scrollOptions);

  //Panel maze info
  CreateInfoCtrls(BCEPanelsOpt.Panel(BCEPanelsOpt.IdxOfPanel('expMazeInfo')));

  //Adjust dimensions
  dgMap.Width := dgMap.ColCount * dgMap.DefaultColWidth;
  dgMap.Height := dgMap.RowCount * dgMap.DefaultRowHeight;
  self.Height := 16 + dgMap.Height;
  hSplitter.Position := dgMap.Width + 32;

  //start timer for graphics refreshing
  timerMain.Interval := 33;
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
  tempObj := ExpandPanelFactory(AContainer, 'expMazeInfo', 'Info panel');
  BCEPanelsOpt.AddPanel(tempObj);

  tempObj := ExpandPanelFactory(AContainer, 'expTools', 'Edition tools');
  BCEPanelsOpt.AddPanel(tempObj);

  tempObj := ExpandPanelFactory(AContainer, 'expObjects', 'Objects');
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
      cell := TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).MapData
        [MapCol, MapRow];
      case cell of
        0:
        begin
          dgMap.Canvas.Brush.Color := dgMap.Color;
          dgMap.Canvas.FillRect(aRect);      //delete block
        end;
        1..$45:
        begin
          if cell <= $10 then
          begin
            //apply style
            cell := cell + STYLES_OFFSET * TGauntMaze(
              tabsMain.GetTabData(tabsMain.tabIndex).TabObject).Style.id;
          end;
          ilMap.Draw(dgMap.Canvas,
            aRect.TopLeft.X, aRect.TopLeft.Y, patternIndexMap[cell], True);
        end;

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
      WriteCell(dgMap, $0a);
    end;
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
  if ssCtrl in GetShiftState then
  begin
    WriteCell(dgMap, 0);
  end
  else
  begin
    WriteCell(dgMap, $0a);
  end;
  dgMap.Repaint;
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

procedure TfMain.CreateInfoCtrls(AParent: TCustomControl);
var
  i: integer;
  btn: TSpeedButton;
begin
  for i := 0 to 19 do
  begin
    btn := TSpeedButton.Create(AParent);
    btn.Align:=alNone;
    btn.Caption:='TST';
    btn.AutoSize:=false;
    btn.Parent := AParent;
    btn.Visible := True;
    btn.Enabled := True;
    btn.Height := 50;
    btn.Width := 50;
    //btn.Top := TBCExpandPanel(AParent).Button.Height;
    //   AParent.InsertControl(btn);

  end;
end;

function TfMain.ExpandPanelFactory(AContainer: TWinControl; AName: string;
  ACaption: string): TBCExpandPAnel;
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
    Result.BevelWidth:=16;
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
    ChildSizing.HorizontalSpacing := 16;
    ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    ChildSizing.VerticalSpacing := 16;
    CollapseKind := akTop;
    Color := $1f1f1f;
    Height := 300;
    ParentBackground := False;
    Parent := AContainer;
    Rounding.RoundX := 0;
    Rounding.RoundY := 0;
    Visible := True;
    Width := AContainer.ClientWidth;

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
