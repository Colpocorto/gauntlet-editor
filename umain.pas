unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  PairSplitter, Types, ActnList, ComCtrls, LCLIntf, LCLtype, StdActns,
  Buttons, StdCtrls, TplCheckBoxUnit, cyPanel, cyBevel,
  BCExpandPanels, attabs, BCComboBox, BGRABitmapTypes, GraphType, ImgList,
  uData, uSaveExport, uMazeTools, uLoadImport;

type

  { TfMain }

  TfMain = class(TForm)
    aCloseTab: TAction;
    aSave: TAction;
    aShowMazeTools: TAction;
    aLoadImport: TAction;
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
    btnLoadImport: TSpeedButton;
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
    cbOneExit: TplCheckBox;
    cypanelMain: TCyPanel;
    //cyFlyingContainer1: TcyFlyingContainer;
    aExit: TFileExit;
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
    //    procedure aGenerateDFSMazeExecute(Sender: TObject);
    procedure aCloseTabExecute(Sender: TObject);
    procedure aGoEditNameExecute(Sender: TObject);
    procedure aLoadImportExecute(Sender: TObject);
    procedure aProcessMazeExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveExportExecute(Sender: TObject);
    procedure aShowMazeToolsExecute(Sender: TObject);
    procedure cbDamageChange(Sender: TObject);
    procedure cbOneExitClick(Sender: TObject);
    procedure cbWrapHClick(Sender: TObject);
    procedure cbWrapVClick(Sender: TObject);
    procedure dgMapClick(Sender: TObject);
    procedure dgMapDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure dgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure aStyleExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tabsMainTabClose(Sender: TObject; ATabIndex: integer;
      var ACanClose, ACanContinue: boolean);
    procedure tabsMainTabEmpty(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure PatternButtonClick(Sender: TObject);
    procedure leNameDblClick(Sender: TObject);
    procedure leNameExit(Sender: TObject);
    procedure leNameKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure tabsMainTabChanged(Sender: TObject);
    procedure tabsMainTabPlusClick(Sender: TObject);
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
    procedure PlacePlayer(Sender: TObject);
    procedure UpdateInfo(Sender: TObject);
    procedure UpdateTabSave(ATab: TATTabData; TabModified: boolean);
  private
    procedure NameEdit;
    procedure CreateInfoCtrls(AParent: TCustomControl);
    procedure CreateToolsCtrls(AParent: TCustomControl);
    procedure CreatePatternCtrls(AParent: TCustomControl);
    function ExpandPanelFactory(AContainer: TWinControl; AName: string;
      ACaption: string; AHeight: integer): TBCExpandPAnel;
    procedure SyncTab(Sender: TObject);
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
    constructor Create(AOwner: TComponent); override;
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
  //Info controls
  CreateInfoCtrls(BCEPanelsOpt.Panel(BCEPanelsOpt.IdxOfPanel('expMazeInfo')));
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
  self.Height := dgMap.Height + tabsMain.Height + cypanelMain.Height;
  hSplitter.Position := dgMap.Width + 32;
end;

function TfMain.AddNewMaze: integer;
var
  newTab: TATTabData;
  newIndex: integer;
begin

  //method .AddTab does't actually adds the object, it adds a COPY, so any property
  //should have been set before adding (or getting the object with GetTabData)
  newIndex := tabsMain.TabCount;
  tabsMain.AddTab(newIndex, TATTabData.Create(nil));
  //it's guaranteed that AddTab first enlarges the list by 1, then adds the item to TabCount-1

  newTab := tabsMain.GetTabData(newIndex);
  newTab.TabObject := TGauntMaze.Create(self);
  tabsMain.TabIndex := newIndex;

  TGauntMaze(newTab.TabObject).Name := 'New Maze ' + IntToStr(tabsMain.TabCount);
  self.UpdateTabSave(newTab, True);
  tabsMain.ShowTab(newIndex);
  //ShowMessage(BoolToStr(newTab.TabModified));
  Result := newIndex;
  dgMap.Enabled := True;
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

procedure TfMain.tabsMainTabClose(Sender: TObject; ATabIndex: integer;
  var ACanClose, ACanContinue: boolean);
var
  UserResponse: integer;
  fs: TFileStream;
  dlg: TSaveDialog;
  FileName: string;
begin
  //ShowMessage( BoolToStr( tabsMain.GetTabData(ATabIndex).TabModified));
  if not tabsMain.GetTabData(ATabIndex).TabModified then
  begin
    ACanClose := True;
    Exit;
  end;
  UserResponse := MessageDlg('Do you want to save the file?',
    mtConfirmation, [mbYes, mbNo, mbCancel], 0);
  case UserResponse of
    mrYes:
    begin
      if TGauntMaze(tabsMain.GetTabData(ATabIndex).TabObject).FileName = '' then
      begin
        dlg := TSaveDialog.Create(self);
        dlg.Filter := 'Gauntlet Maze File|*.gmf';
        dlg.Options := [ofOverwritePrompt, ofPathMustExist, ofEnableSizing,
          ofViewDetail];
        if dlg.Execute then
          FileName := dlg.FileName
        else
        begin
          ACanClose := False;
          Exit;
        end;
        //aSaveExport.Execute;
      end
      else
        FileName := TGauntMaze(tabsMain.GetTabData(ATabIndex).TabObject).FileName;
      try
        try
          fs := TFileStream.Create(FileName, fmCreate);
          TGauntMaze(tabsMain.GetTabData(ATabIndex).TabObject).ToFileStream(fs);
          UpdateTabSave(tabsMain.GetTabData(ATabIndex), False);
        except
          on E: Exception do
          begin
            ShowMessage('Error while saving file ' + TGauntMaze(
              tabsMain.GetTabData(ATabIndex).TabObject).FileName +
              ': ' + E.Message);
          end;
        end;
      finally
        fs.Free;
      end;
    end;
    mrNo:
    begin
      ACanClose := True;
    end;
    mrCancel: begin
      ACanClose := False;
      ACanContinue := False;
    end;
  end;
end;

procedure TfMain.tabsMainTabEmpty(Sender: TObject);
begin
  dgMap.Enabled := False;
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
    UpdateTabSave(tabsMain.GetTabData(tabsMain.TabIndex), tabsMain.GetTabData(
      tabsMain.TabIndex).TabModified);
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
  cbOneExit.Checked := TGauntMaze(tabsMain.GetTabData(
    TATTabs(Sender).TabIndex).TabObject).GetOneExit();
  //sync damage others mode
  if (TGauntMaze(tabsMain.GetTabData(TATTabs(Sender).TabIndex).TabObject).HurtPlayers =
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

procedure TfMain.SyncTab(Sender: TObject);
begin
  tabsMainTabChanged(Sender);
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

{procedure TfMain.timerMainTimer(Sender: TObject);
begin
  dgMap.Repaint;
end;
 }
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

  if tabsMain.TabCount = 0 then Exit;
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
        1..$7f:
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

  UpdateTabSave(tabsMain.GetTabData(tabsMain.TabIndex), True);

  dgMap.Invalidate;

end;

procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  while (tabsMain.TabCount > 0) and CanClose do
    CanClose := tabsMain.DeleteTab(tabsMain.TabIndex, True, True,
      aocDefault, adrClickOnXButton);
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
    gtPlacePly:
      PlacePlayer(Sender);
    gtGlass:
      UpdateInfo(Sender);
  end;
  dgMap.Repaint;
end;

procedure TfMain.UpdateInfo(Sender: TObject);
var
  Col, Row: integer;
  grid: TCustomGrid;
  maze: TGauntMaze;
  pattern: byte;
  expPanel: TBCExpandPanel;
  tmpStr: string;
begin
  grid := TCustomGrid(Sender);
  // Get the cell coordinates from the mouse click
  grid.MouseToCell(grid.ScreenToClient(Mouse.CursorPos).X,
    grid.ScreenToClient(Mouse.CursorPos).Y, Col, Row);
  if (Col > 0) and (Row > 0) and (Col < 33) and (Row < 33) then
  begin
    maze := TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject);
    pattern := maze.MapData[Col - 1, Row - 1];
    expPanel := BCEPanelsOpt.Panel(BCEPanelsOpt.IdxOfPanel('expMazeInfo'));
    TLabel(TPanel(expPanel.FindComponent('panelInfo')).FindComponent(
      'lblXCoord')).Caption := IntToStr(Col - 1);
    TLabel(TPanel(expPanel.FindComponent('panelInfo')).FindComponent(
      'lblYCoord')).Caption := IntToStr(Row - 1);
    tmpStr := patternIndex[PatternIndexMap[pattern and $7f]].desc;
    if (pattern and $80) > 0 then tmpStr := tmpStr + ' (linked to traps)';
    TLabel(TPanel(expPanel.FindComponent('panelInfo')).FindComponent(
      'lblPattern')).Caption := tmpStr;

    TImage(expPanel.FindComponent('imgInfo')).ImageIndex :=
      patternIndexMap[pattern and $7f];

  end;
end;

procedure TfMain.PlacePlayer(Sender: TObject);
var
  Col, Row: integer;
  grid: TCustomGrid;
  maze: TGauntMaze;
begin
  grid := TCustomGrid(Sender);
  // Get the cell coordinates from the mouse click
  grid.MouseToCell(grid.ScreenToClient(Mouse.CursorPos).X,
    grid.ScreenToClient(Mouse.CursorPos).Y, Col, Row);
  if (Col > 0) and (Row > 0) then
  begin
    maze := TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject);
    //update modified flag

    if (maze.GetPlayerPos.x <> Col - 1) or (maze.GetPlayerPos.y <> Row - 1) then
      UpdateTabSave(tabsMain.GetTabData(tabsMain.TabIndex), True);
    maze.SetPlayerPos(Col - 1, Row - 1);

  end;
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
    if ((currentValue < $11) and (currentValue > 0)) or
      ((currentValue < $91) and (currentValue > $80)) then
      //if cell contains other than a wall, do nothing. The engine supports binding
      //any object (even an EXIT!) to traps, but the encoding format doesn't support it :-(
    begin
      TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).MapData[Col -
        1, Row - 1] := currentValue xor $80;
      UpdateTabSave(tabsMain.GetTabData(tabsMain.TabIndex), True);
    end;
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
  self.UpdateTabSave(tabsMain.GetTabData(tabsMain.TabIndex), True);
end;

procedure TfMain.cbWrapHClick(Sender: TObject);
begin
  TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).SetHorzWrap(
    cbWrapH.Checked);
  UpdateTabSave(tabsMain.GetTabData(tabsMain.TabIndex), True);
  dgMap.Invalidate;
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
  UpdateTabSave(tabsMain.GetTabData(tabsMain.TabIndex), True);
end;

procedure TfMain.cbOneExitClick(Sender: TObject);
begin
  TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).SetOneExit(
    cbOneExit.Checked);
  UpdateTabSave(tabsMain.GetTabData(tabsMain.TabIndex), True);
end;

procedure TfMain.aGoEditNameExecute(Sender: TObject);
begin
  self.leName.ReadOnly := False;
  self.leName.Color := $5f5f5f;

  UpdateTabSave(tabsMain.GetTabData(tabsMain.TabIndex), True);
end;

procedure TfMain.aCloseTabExecute(Sender: TObject);
begin
  tabsMain.DeleteTab(tabsMain.TabIndex, True, True, aocDefault, adrClickOnXButton);
end;

procedure TfMain.aLoadImportExecute(Sender: TObject);
var
  i: integer;
  tab: TATTabData;
  NumOfMazes: integer = 1;
begin
  case fLoadImport.ShowModal of
    mrOk:
    begin
      NumOfMazes := 1;
    end;
    mrAll:
    begin
      NumOfMazes := length(block);
    end;
  end;
  for i := 0 to NumOfMazes - 1 do
  begin
    tabsMain.GetTabData(self.AddNewMaze).TabObject := uData.block[i];
    tab := tabsMain.GetTabData(tabsMain.TabIndex);
    if TGauntMaze(tab.TabObject).Name <> '' then //it has been loaded, not imported
      tab.TabCaption := TGauntMaze(tab.TabObject).Name
    else
    begin
      //it has been imported, so it has no name, take it from the tab
      TGauntMaze(tab.TabObject).Name :=
        LeftStr(tab.TabCaption, length(tab.TabCaption) - 2);
    end;
    tab.TabHint := TGauntMaze(tab.TabObject).FileName;
    //SyncTab causes the modified flag to become 'true' since it updates states
    //Therefore it must be invoked before updating the Modified state
    SyncTab(tabsMain);

    //if there is just ONE maze and has been LOADED (not imported) it is expected
    //to have its FileName property set, in such case set it as non-modified.
    if TGauntMaze(tab.TabObject).FileName <> '' then
      UpdateTabSave(tab, False)
    else
      UpdateTabSave(tab, True);

    //tabsMain.Repaint;
    dgMap.Repaint;
  end;

end;

procedure TfMain.aProcessMazeExecute(Sender: TObject);
var
  processResult: integer;
begin
  processResult := TGauntMaze(tabsMain.GetTabData(
    tabsMain.TabIndex).TabObject).ProcessMap;
  if processResult < 0 then                  //negative numbers are errors
    ShowMessage(ProcessResults.KeyData[processResult])
  else
    ShowMessage('Map compiled successfully. Size: ' + IntToStr(processResult));

end;

procedure TfMain.aSaveExecute(Sender: TObject);
var
  fs: TFileStream;
begin
  if TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).FileName = '' then
  begin
    aSaveExport.Execute;
  end
  else
  begin
    try
      try
        fs := TFileStream.Create(
          TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).FileName,
          fmCreate);
        TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).ToFileStream(fs);
        UpdateTabSave(tabsMain.GetTabData(tabsMain.TabIndex), False);
      except
        on E: Exception do
        begin
          ShowMessage('Error while saving file ' + TGauntMaze(
            tabsMain.GetTabData(tabsMain.TabIndex).TabObject).FileName +
            ': ' + E.Message);
        end;
      end;
    finally
      fs.Free;
    end;
  end;
end;

procedure TfMain.aSaveExportExecute(Sender: TObject);
begin
  //check if there is at least a single maze open
  if tabsMain.TabIndex = -1 then
  begin
    fSaveExport.SetCurrentMaze(nil);
    fSaveExport.btnManyMazes.Click;
    fSaveExport.btnManyMazes.Down := True;
  end
  else
  begin
    fSaveExport.SetCurrentMaze(TGauntMaze(tabsMain.GetTabData(
      tabsMain.TabIndex).TabObject));
  end;
  if fSaveExport.ShowModal = mrOk then
  begin
    tabsMain.GetTabData(
      tabsMain.TabIndex).TabExtModified2 := False;
    tabsMain.GetTabData(
      tabsMain.TabIndex).TabHint :=
      TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).FileName;
    UpdateTabSave(tabsMain.GetTabData(tabsMain.TabIndex), False);
  end;
end;

procedure TfMain.aShowMazeToolsExecute(Sender: TObject);
begin
  fMazeTools.SetCurrentMaze(TGauntMaze(tabsMain.GetTabData(
    tabsMain.TabIndex).TabObject));
  fMazeTools.SetPreviewObject(dgMap);
  fMazeTools.SetTab(tabsMain.GetTabData(tabsMain.TabIndex));
  fMazeTools.Show;
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

  if (Col > minCol) and (Col < 33) and (Row > 0) and (Row < 33) and
    (TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).MapData[Col -
    1, Row - 1] <> $3f) then
  begin
    //writing cells on Row 0 (top border, Row = 0 on the map) is allowed but
    //only horizontal wall or objects are allowed. Empty spaces are forbidden
    if Row = 1 then
    begin
      case index of
        0, $11, $12:
          //do not allow to delete a block in top border, also gates are not allowed
          index := TGauntMaze(tabsMain.GetTabData(
            tabsMain.TabIndex).TabObject).MapData[Col - 1, Row - 1];
        $1..$10, $81..$90:
        begin
          case Col of
            1:
              //set corner
              if minCol = 1 then
                index := 6
              else
                index := 10;
            else
              index := 10; //horizontal wall
          end;
        end;
      end;
    end;

    //update map matrix
    if index <> TGauntMaze(tabsMain.GetTabData(
      tabsMain.TabIndex).TabObject).MapData[Col - 1, Row - 1] then
      UpdateTabSave(tabsMain.GetTabData(tabsMain.TabIndex), True);
    TGauntMaze(tabsMain.GetTabData(tabsMain.TabIndex).TabObject).MapData[Col -
      1, Row - 1] := index;
  end;

end;

procedure TfMain.CreateInfoCtrls(AParent: TCustomControl);
var
  tmpobj: TControl;
  tmplabel: TLabel;
begin
  //   TBCExpandPanel(AParent).
  //create properties panel
  tmpobj := TPanel.Create(AParent);
  with TPanel(tmpObj) do
  begin
    Align := alClient;
    Color := $282828;
    BevelColor := $383838;
    Height := 280;
    Parent := AParent;
    Name := 'panelInfo';
    Caption := '';
  end;

  //create labels
  tmplabel := TLabel.Create(tmpObj);
  with tmplabel do
  begin
    Autosize := False;
    Font.Color := $c9c9c9;
    Alignment := taRightJustify;
    Caption := 'X coord:';
    Parent := TPanel(tmpObj);
    Top := 50;
    Width := 70;
    Left := 0;
  end;
  //create labels
  tmplabel := TLabel.Create(tmpObj);
  with tmplabel do
  begin
    Autosize := False;
    Font.Color := $f0f0f0;
    Font.Style := [fsBold];
    Alignment := taLeftJustify;
    Name := 'lblXCoord';
    Caption := '1';
    Parent := TPanel(tmpObj);
    Top := 50;
    Width := 70;
    Left := 78;
  end;


  tmplabel := TLabel.Create(tmpObj);
  with tmplabel do
  begin
    Autosize := False;
    Font.Color := $c9c9c9;
    Alignment := taRightJustify;
    Caption := 'Y coord:';
    Parent := TPanel(tmpObj);
    Top := 70;
    Width := 70;
    Left := 0;
  end;
  //create labels
  tmplabel := TLabel.Create(tmpObj);
  with tmplabel do
  begin
    Autosize := False;
    Font.Color := $f0f0f0;
    Font.Style := [fsBold];
    Alignment := taLeftJustify;
    Name := 'lblYCoord';
    Caption := '1';
    Parent := TPanel(tmpObj);
    Top := 70;
    Width := 70;
    Left := 78;
  end;
  tmplabel := TLabel.Create(tmpObj);
  with tmplabel do
  begin
    Autosize := False;
    Font.Color := $c9c9c9;
    Alignment := taRightJustify;
    Caption := 'Pattern description:';
    Height := 60;
    Width := 70;
    WordWrap := True;
    Parent := TPanel(tmpObj);
    Top := 98;
    Left := 0;
  end;
  tmplabel := TLabel.Create(tmpObj);
  with tmplabel do
  begin
    Autosize := True;
    Align := alBottom;
    Anchors := [akBottom];
    Font.Color := $e9e9e9;
    Font.Style := [fsBold];
    Alignment := taCenter;
    Name := 'lblPattern';
    Caption := 'Player start position';
    Height := 60;
    Width := 70;
    WordWrap := True;
    Parent := TPanel(tmpObj);
    Top := 110;
    Left := 0;
  end;
  //create zoom picture
  tmpobj := TImage.Create(AParent);
  with TImage(tmpobj) do
  begin
    Color := clBlue;
    Width := 128;
    ImageWidth := 128;
    Height := tmpobj.Width;
    Align := alRight;
    Parent := AParent;
    Name := 'imgInfo';
    Images := uData.ilMap;
    ImageIndex := patternIndexMap[$3f];

  end;

  //create hint panel
  tmpobj := TPanel.Create(AParent);
  with TPanel(tmpObj) do
  begin
    Align := alBottom;
    BevelColor := clBlack;
    Height := 38;
    Autosize := False;
    Parent := AParent;
    Name := 'panelHint';
    Caption := '';
  end;
  tmplabel := TLabel.Create(tmpObj);
  with tmplabel do
  begin
    Autosize := False;
    Align := alBottom;
    Anchors := [akBottom, akLeft, akRight];
    Font.Color := $c9c9c9;
    Font.Style := [fsItalic];
    Alignment := taLeftJustify;
    Height := 36;
    //Width := 40;
    WordWrap := True;
    Parent := TPanel(tmpObj);
    Top := 0;
    Left := 0;
    Caption := 'HINT: click + CTRL when using the building or object tool to erase a cell.';
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
  PatternButtonFactory(AParent, ilTools, 9, 0, 'btnGenMaze1',
    'Generate random Maze (type 1)', 9, @aShowMazeToolsExecute);
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

  tmppic.Free;

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
  self.FObjectId := $13;
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

  // Check if mouse left button is pressed
  if GetKeyState(VK_LBUTTON) < 0 then
    Include(Result, ssLeft);

end;

procedure TfMain.UpdateTabSave(ATab: TATTabData; TabModified: boolean);
var
  StrModified: string = '';
begin
  if TabModified then StrModified := ' ' + UnSavedSym
  else
    StrModified := '';

  ATab.TabModified := TabModified;
  ATab.TabCaption := TGauntMaze(ATab.TabObject).Name + StrModified;

  tabsMain.Invalidate;

end;

end.
