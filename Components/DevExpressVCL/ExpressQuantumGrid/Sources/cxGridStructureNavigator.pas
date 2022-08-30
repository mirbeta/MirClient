{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxGridStructureNavigator; // TODO transparency

{$I cxVer.inc}

interface

uses
  DesignIntf, Windows, Messages, TypInfo, Classes, SysUtils, Controls, Graphics,
  Forms, Menus, ExtCtrls, Buttons,
  dxMessages, cxControls, cxClasses, cxGraphics, cxDesignWindows,
  cxGridCommon, cxGrid, cxGridLevel, cxGridCustomView, cxViewEditor;

type
  TcxGridStructureControl = class;
  TcxGridStructureNavigator = class;

  TcxGridColorIndex = (ciGrid, ciLevel, ciView);

  { TcxGridStructureControlViewInfo }

  TcxGridStructureControlRowViewInfo = class
    Bounds: TRect;

    LevelBounds: TRect;
    LevelContentBounds: TRect;
    LevelHighlighted: Boolean;
    LevelSelected: Boolean;
    LevelText: string;
    LevelTextArea: TRect;

    ViewBounds: TRect;
    ViewContentBounds: TRect;
    ViewHighlighted: Boolean;
    ViewSelected: Boolean;
    ViewText: string;
    ViewTextArea: TRect;

    TreeLineVertCount: Integer;
    TreeLineIsParent: Boolean;
    TreeLineIsLast: Boolean;
  end;

  TcxGridStructureControlRowsViewInfo = class(TcxObjectList)
  private
    function GetItem(Index: Integer): TcxGridStructureControlRowViewInfo;
  public
    property Items[Index: Integer]: TcxGridStructureControlRowViewInfo read GetItem; default;
  end;

  TcxGridStructureControlViewInfo = class
  private
    FCanvas: TcxCanvas;
    FStructureControl: TcxGridStructureControl;
    FTextHeight: Integer;
    FRowHeight: Integer;
    FRows: TcxGridStructureControlRowsViewInfo;
    function GetCanvas: TcxCanvas;
    function GetGrid: TcxCustomGrid;
    function GetLevelByIndex(AIndex: Integer): TcxGridLevel;
    procedure GetLevelInfo(AIndex: Integer; var ALevelText, AViewText: string; var ALevelIndex: Integer; var AIsParent, AIsLast: Boolean);
    function GetRowCount: Integer;
  public
    Bounds: TRect;
    constructor Create(AStructureControl: TcxGridStructureControl);
    destructor Destroy; override;
    procedure Calculate;

    function GetContentColor: TColor;
    function GetContentFont: TFont;
    function GetContentFontColor: TColor;
    function GetGridColor(AHighlighted: Boolean): TColor;
    function GetHighlightedFontColor: TColor;
    function GetLevelColor(AHighlighted: Boolean): TColor;
    function GetRowBorderColor(AColorIndex: TcxGridColorIndex; ASelected, AHighlighted: Boolean): TColor;
    function GetRowColor(AColorIndex: TcxGridColorIndex; ASelected, AHighlighted: Boolean): TColor;
    function GetRowTextColor(AColorIndex: TcxGridColorIndex; ASelected, AHighlighted: Boolean): TColor;
    function GetSelectedColor(AHighlighted: Boolean): TColor;
    function GetSelectedFontColor: TColor;
    function GetViewColor(AHighlighted: Boolean): TColor;

    procedure Paint;

    property Canvas: TcxCanvas read GetCanvas;
    property Grid: TcxCustomGrid read GetGrid;
    property StructureControl: TcxGridStructureControl read FStructureControl;
    property Rows: TcxGridStructureControlRowsViewInfo read FRows;
  end;

  { TcxGridStructureControl }

  TcxGridStructureControlSelectComponentEvent = procedure(AObject: TPersistent;
    AClearSelection: Boolean) of object;

  TcxGridStructureControlHitTest = (htNowhere, htLevel, htView);

  TcxGridStructureControlHitInfo = record
    HitTest: TcxGridStructureControlHitTest;
    RowIndex: Integer;
  end;

  TcxGridStructureControl = class(TcxControl)
  private
    FGrid: TcxCustomGrid;
    FHitInfo: TcxGridStructureControlHitInfo;
    FLockCount: Integer;
    FMayFocused: Boolean;
    FMouseDownHitInfo: TcxGridStructureControlHitInfo;
    FMousePressed: Boolean;
    FMultiSelect: Boolean;
    FViewInfo: TcxGridStructureControlViewInfo;
    FOnDrawBackground: TNotifyEvent;
    FOnSelectComponent: TcxGridStructureControlSelectComponentEvent;
    FOnSelectionChanged: TNotifyEvent;
    procedure ClearSelection;
    procedure SetGrid(Value: TcxCustomGrid);
    procedure SetMultiSelect(Value: Boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;

    procedure AddToSelection(AObject: TPersistent; AClearSelection: Boolean);
    procedure CancelPressed(P: TPoint);
    procedure CheckMousePos(P: TPoint);
    procedure BoundsChanged; override;
    procedure DoDrawBackground; virtual;
    procedure FontChanged; override;
    function GetComponentByHitInfo(AHitInfo: TcxGridStructureControlHitInfo): TComponent;
    function GetCurrentCursor(X, Y: Integer): TCursor; override;
    function MayFocus: Boolean; override;
    procedure MouseLeave(AControl: TControl); override;
    procedure SelectComponent(AObject: TPersistent; AClearSelection: Boolean = True);
    procedure SelectionChanged; virtual;
    procedure SetPressed(APressed: Boolean);
    procedure UpdateContent; virtual;
    procedure UpdateHighlighted;
    property LockCount: Integer read FLockCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure Changed;
    procedure EndUpdate;
    function GetHitInfo(P: TPoint): TcxGridStructureControlHitInfo;
    function GetLevelByRowIndex(ARowIndex: Integer): TcxGridLevel;
    function GetSelectedLevel: TcxGridLevel;
    procedure GetSelection(ASelectionList: TList);
    procedure GetSelectionLevels(ASelectionList: TList);
    function GetSelectionLevelCount: Integer;
//    function GetSelectedView: TcxCustomGridView;
    procedure GetSelectionViews(ASelectionList: TList);
    function GetSelectionViewCount: Integer;
    procedure SyncSelection(ASelectionList: TList);
    property Grid: TcxCustomGrid read FGrid write SetGrid;
    property Keys;
    property MayFocused: Boolean read FMayFocused write FMayFocused;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect;
    property ViewInfo: TcxGridStructureControlViewInfo read FViewInfo;
    property OnKeyPress;
    property OnDrawBackground: TNotifyEvent read FOnDrawBackground write FOnDrawBackground;
    property OnSelectComponent: TcxGridStructureControlSelectComponentEvent read FOnSelectComponent write FOnSelectComponent;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;

  { TcxGridStructureHelper }

  TcxGridLevelViewList = class(TcxGridViewList)
  private
    FLevel: TcxGridLevel;
  protected
    procedure GetViewNames; override;
  public
    constructor Create(ALevel: TcxGridLevel);
    property Level: TcxGridLevel read FLevel;
  end;

  TcxGridStructureHelper = class
  private
    FPopupMenu: TPopupMenu;
    FPopupMenuLevel: TcxGridLevel;
    FStructureControl: TcxGridStructureControl;
    FOnUpdateDesigner: TNotifyEvent;
    procedure CreateLevelClick(Sender: TObject);
    procedure CreateViewClick(Sender: TObject);
    procedure SelectViewClick(Sender: TObject);
    procedure DeleteLevelClick(Sender: TObject);
    procedure MoveLevelClick(Sender: TObject);
  protected
    FPopupMenuView: TcxCustomGridView;
    FViewMenuProvider: TcxCustomGridViewMenuProvider;
    function CanAddComponent: Boolean;
    function CanDeleteComponent(AComponent: TComponent): Boolean;
    procedure ExecuteLevelViewsMenuItem(ALevel: TcxGridLevel; AMenuItem: TMenuItem);
    procedure FillLevelViewsMenu(AMenu: TMenuItem; ALevel: TcxGridLevel; AOnClick: TNotifyEvent);
    procedure StructureControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateDesigner;
  public
    constructor Create(AStructureControl: TcxGridStructureControl); virtual;
    destructor Destroy; override;
    property StructureControl: TcxGridStructureControl read FStructureControl;
    property OnUpdateDesigner: TNotifyEvent read FOnUpdateDesigner write FOnUpdateDesigner;
  end;

  { TcxGridStructureNavigator }

  TcxGridStructureNavigator = class(TcxCustomGridStructureNavigator)
  private
    FCloseButton: TSpeedButton;
    FCustomizeButton: TSpeedButton;
    FDefaultLevel: TcxGridLevel;
    FDefaultView: TcxCustomGridView;
    FDesignHelper: TcxDesignHelper;
    FMakeSelectionVisible: Boolean;
    FSelection: TList;
    FSeparator: TBevel;
    FStructureControl: TcxGridStructureControl;
    FStructureHelper: TcxGridStructureHelper;
    procedure CloseButtonClick(Sender: TObject);
    procedure CustomizeButtonClick(Sender: TObject);
    procedure UpdateDesigner(Sender: TObject);
    procedure CMDeferUpdate(var Message: TMessage); message DXM_REFRESHCUSTOMIZATION;
  protected
    procedure Paint; override;
    procedure Calculate;
    function CalculateBoundsRect: TRect; override;
    procedure Changed; override;
    procedure CreateDefaultComponents;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    procedure DrawStructureControlBackground(Sender: TObject);
    function GetClientBounds: TRect; override;
    function GetDefaultViewClass: TcxCustomGridViewClass; virtual;
    function GetDesigner: IDesigner;
    procedure SelectComponent(AObject: TPersistent; AClearSelection: Boolean);
    property MakeSelectionVisible: Boolean read FMakeSelectionVisible write FMakeSelectionVisible;
  public
    constructor Create(AGrid: TcxCustomGrid); override;
    destructor Destroy; override;
    procedure BeforeGridLoading; override;
    function CanAddComponent: Boolean; override;
    function CanDeleteComponent(AComponent: TComponent): Boolean; override;
    procedure GetSelection(AList: TList); override;
    function IsObjectSelected(AObject: TPersistent): Boolean; override;
    procedure NotifyEditors; override;
    procedure SelectionChanged(ASelection: TList); override;
    procedure SelectObject(AObject: TPersistent; AClearSelection: Boolean); override;
    procedure SelectObjects(AObjects: TList); override;
    procedure UnselectObject(AObject: TPersistent); override;
  end;

function GenLevelName(AGrid: TcxCustomGrid; ALevel: TcxGridLevel): string;
function GenViewName(AOwnerComponent: TComponent; AView: TcxCustomGridView): string;
procedure FillRegisteredViewsMenu(AMenu: TMenuItem; AOnClick: TNotifyEvent);

implementation

uses
  Math, dxOffice11, cxLookAndFeelPainters, cxCustomData, cxGridDBTableView,
  cxGridEditor, cxLibraryConsts, dxDPIAwareUtils, dxCoreGraphics;

const
  ButtonBorderSize = 1;
  ButtonOffset = 1;
  CloseButtonHeight = 15;
  CloseButtonWidth = 15;
  FrameBorderSize = 1;
  LevelIndent = 16;
  RowBorderWidth = 1;
  RowOffset = 2;
  TextOffset = 2;
  NavigatorOffset = 12;
  // strings
  SCustomizeText = 'Customize...'; // TODO res
  SSubStr = 'TcxGrid';
  SGridDBViewPrefix = 'DB ';
  SGridServerModeViewPrefix = 'Server Mode ';

  cxGridDesignLevelColor = 15851215{16248036};
  cxGridDesignLevelHotColor = 13747387;
  cxGridDesignLevelBorderColor = 13547166;
  cxGridDesignLevelHotBorderColor = 12033927;
  cxGridDesignGridColor = 12639424;
  cxGridDesignGridHotColor = 10800292;
  cxGridDesignGridBorderColor = 10526880;
  cxGridDesignGridHotBorderColor = clDkGray;

type
  TcxCustomGridAccess = class(TcxCustomGrid);

function GenLevelName(AGrid: TcxCustomGrid; ALevel: TcxGridLevel): string;
begin
  Result := CreateUniqueName(AGrid.Owner, AGrid, ALevel, ScxGridPrefixName, '');
end;

function GenViewName(AOwnerComponent: TComponent; AView: TcxCustomGridView): string;
begin
  Result := CreateUniqueName(AOwnerComponent.Owner, AOwnerComponent, AView, ScxGridPrefixName, '');
end;

function FillUnboundRegisteredViewsMenu(AMenu: TMenuItem; AOnClick: TNotifyEvent;
  ACheckAddSeparator: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to cxGridRegisteredViews.Count - 1 do
    if (Pos(SGridDBViewPrefix, cxGridRegisteredViews.Descriptions[I]) <> 1) and
       (Pos(SGridServerModeViewPrefix, cxGridRegisteredViews.Descriptions[I]) <> 1) then
    begin
      if not Result and ACheckAddSeparator then
        AMenu.Add(CreateMenuItem(AMenu.Owner, '-'));
      AMenu.Add(CreateMenuItem(AMenu.Owner, cxGridRegisteredViews.Descriptions[I],
        AOnClick, True, I));
      Result := True;
    end;
end;

function FillDBRegisteredViewsMenu(AMenu: TMenuItem; AOnClick: TNotifyEvent;
  ACheckAddSeparator: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to cxGridRegisteredViews.Count - 1 do
    if Pos(SGridDBViewPrefix, cxGridRegisteredViews.Descriptions[I]) = 1 then
    begin
      if not Result and ACheckAddSeparator then
        AMenu.Add(CreateMenuItem(AMenu.Owner, '-'));
      AMenu.Add(CreateMenuItem(AMenu.Owner, cxGridRegisteredViews.Descriptions[I],
        AOnClick, True, I));
      Result := True;
    end;
end;

function FillServerModeRegisteredViewsMenu(AMenu: TMenuItem; AOnClick: TNotifyEvent;
  ACheckAddSeparator: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to cxGridRegisteredViews.Count - 1 do
    if Pos(SGridServerModeViewPrefix, cxGridRegisteredViews.Descriptions[I]) = 1 then
    begin
      if not Result and ACheckAddSeparator then
        AMenu.Add(CreateMenuItem(AMenu.Owner, '-'));
      AMenu.Add(CreateMenuItem(AMenu.Owner, cxGridRegisteredViews.Descriptions[I],
        AOnClick, True, I));
      Result := True;
    end;
end;

procedure FillRegisteredViewsMenu(AMenu: TMenuItem; AOnClick: TNotifyEvent);
var
  ACheckAddSeparator: Boolean;
begin
  ACheckAddSeparator := FillUnboundRegisteredViewsMenu(AMenu, AOnClick, False);
  ACheckAddSeparator := FillDBRegisteredViewsMenu(AMenu, AOnClick, ACheckAddSeparator);
  FillServerModeRegisteredViewsMenu(AMenu, AOnClick, ACheckAddSeparator);
end;

procedure CreateCloseBitmap(ABitmap: TBitmap);
var
  ACanvas: TcxCanvas;
  R: TRect;
begin
  R := Rect(0, 0, CloseButtonWidth, CloseButtonHeight);
  with ABitmap do
  begin
    Width := R.Right - R.Left;
    Height := R.Bottom - R.Top;
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(R);
    Inc(R.Top);
    Dec(R.Right);
    ACanvas := TcxCanvas.Create(Canvas);
    try
      cxLookAndFeelPaintersManager.GetPainter(lfsUltraFlat).DrawScaledButtonCross(
        ACanvas, R, clBtnText, cxbsNormal, dxSystemScaleFactor);
    finally
      ACanvas.Free;
    end;
  end;
end;

{ TDesignNotificationHandler }

type
  TDesignNotificationHandler = class(TInterfacedPersistent, IDesignNotification)
  private
    FGrids: TList;
  protected
    // IDesignNotification
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemsModified(const ADesigner: IDesigner);
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections);
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RegisterGrid(AGrid: TcxCustomGrid);
    procedure UnregisterGrid(AGrid: TcxCustomGrid);
  end;

var
  DesignNotificationHandler: TDesignNotificationHandler;

constructor TDesignNotificationHandler.Create;
begin
  inherited;
  FGrids := TList.Create;
  RegisterDesignNotification(Self);
end;

destructor TDesignNotificationHandler.Destroy;
begin
  UnregisterDesignNotification(Self);
  FGrids.Free;
  inherited;
end;

procedure TDesignNotificationHandler.ItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin
end;

procedure TDesignNotificationHandler.ItemInserted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin
end;

procedure TDesignNotificationHandler.ItemsModified(const ADesigner: IDesigner);
begin
end;

procedure TDesignNotificationHandler.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
var
  ASelectionList: TList;
  I: Integer;
begin
  ASelectionList := TList.Create;
  try
    ConvertSelectionToList(ASelection, ASelectionList);
    for I := 0 to FGrids.Count - 1 do
      TcxCustomGrid(FGrids[I]).StructureNavigator.SelectionChanged(ASelectionList);
  finally
    ASelectionList.Free;
  end;
end;

procedure TDesignNotificationHandler.DesignerOpened(const ADesigner: IDesigner;
  AResurrecting: Boolean);
begin
end;

procedure TDesignNotificationHandler.DesignerClosed(const ADesigner: IDesigner;
  AGoingDormant: Boolean);
begin
end;

procedure TDesignNotificationHandler.RegisterGrid(AGrid: TcxCustomGrid);
begin
  FGrids.Add(AGrid);
end;

procedure TDesignNotificationHandler.UnregisterGrid(AGrid: TcxCustomGrid);
begin
  FGrids.Remove(AGrid);
end;

{ TcxGridStructureControlRowsViewInfo }

function TcxGridStructureControlRowsViewInfo.GetItem(Index: Integer): TcxGridStructureControlRowViewInfo;
begin
  Result := TcxGridStructureControlRowViewInfo(inherited Items[Index]);
end;

{ TcxGridStructureControlViewInfo }

constructor TcxGridStructureControlViewInfo.Create(AStructureControl: TcxGridStructureControl);
begin
  inherited Create;
  FStructureControl := AStructureControl;
  FRows := TcxGridStructureControlRowsViewInfo.Create;
end;

destructor TcxGridStructureControlViewInfo.Destroy;
begin
  FRows.Free;
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TcxGridStructureControlViewInfo.Calculate;
var
  ASelection: TList;
  W, H: Integer;
  I, ALevelOffset, ATop: Integer;
  ARow: TcxGridStructureControlRowViewInfo;
begin
  // TODO: ref
  if Grid = nil then Exit;
  ASelection := TList.Create;
  try
    // save selection
    StructureControl.GetSelection(ASelection);
    // Init
    Canvas.Font := GetContentFont;
    FTextHeight := Canvas.TextHeight(dxMeasurePattern);
    FRowHeight := 2 * RowOffset + 2 * RowBorderWidth + 2 * TextOffset + FTextHeight;
    // Rows
    FRows.Clear;
    H := GetRowCount * FRowHeight;
    Bounds := Rect(0, 0, 0, H);
    ATop := 0;
    for I := 0 to GetRowCount - 1 do
    begin
      ARow := TcxGridStructureControlRowViewInfo.Create;
      GetLevelInfo(I, ARow.LevelText, ARow.ViewText, ARow.TreeLineVertCount,
        ARow.TreeLineIsParent, ARow.TreeLineIsLast);
      // Level
      ARow.LevelTextArea := Rect(0, ATop, Canvas.TextWidth(ARow.LevelText), ATop + FTextHeight);
      OffsetRect(ARow.LevelTextArea, TextOffset, TextOffset);
      ARow.LevelContentBounds := ARow.LevelTextArea;
      InflateRect(ARow.LevelContentBounds, TextOffset, TextOffset);
      OffsetRect(ARow.LevelTextArea, RowBorderWidth, RowBorderWidth);
      OffsetRect(ARow.LevelContentBounds, RowBorderWidth, RowBorderWidth);
      ARow.LevelBounds := ARow.LevelContentBounds;
      InflateRect(ARow.LevelBounds, RowBorderWidth, RowBorderWidth);
      ARow.Bounds := ARow.LevelBounds;
      OffsetRect(ARow.LevelBounds, RowOffset, RowOffset);
      OffsetRect(ARow.LevelContentBounds, RowOffset, RowOffset);
      OffsetRect(ARow.LevelTextArea, RowOffset, RowOffset);
      //InflateRect(ARow.Bounds, RowOffset, RowOffset);
      OffsetRect(ARow.Bounds, RowOffset, RowOffset);
      // Indent
      ALevelOffset := ARow.TreeLineVertCount * (LevelIndent + RowOffset);
      OffsetRect(ARow.LevelBounds, ALevelOffset, 0);
      OffsetRect(ARow.LevelContentBounds, ALevelOffset, 0);
      OffsetRect(ARow.LevelTextArea, ALevelOffset, 0);
      OffsetRect(ARow.Bounds, ALevelOffset, 0);
      // View
      if (I > 0) and (ARow.ViewText <> '') then
      begin
        W := 2 * RowBorderWidth + 2 * TextOffset + Canvas.TextWidth(ARow.ViewText);
        ARow.ViewBounds := Rect(ARow.LevelBounds.Right - RowBorderWidth, ARow.LevelBounds.Top,
          ARow.LevelBounds.Right - RowBorderWidth + W, ARow.LevelBounds.Bottom);
        ARow.ViewContentBounds := ARow.ViewBounds;
        InflateRect(ARow.ViewContentBounds, -RowBorderWidth, -RowBorderWidth);
        ARow.ViewTextArea := ARow.ViewContentBounds;
        InflateRect(ARow.ViewTextArea, -TextOffset, -TextOffset);
        Inc(ARow.Bounds.Right, W - RowBorderWidth);
      end;
      FRows.Add(ARow);
      Bounds.Right := Max(Bounds.Right, ARow.Bounds.Right + RowOffset);
      Inc(ATop, FRowHeight);
    end;
    Bounds.Bottom := ATop;
    // restore selection
    StructureControl.SyncSelection(ASelection);
  finally
    ASelection.Free;
  end;
end;

function TcxGridStructureControlViewInfo.GetContentColor: TColor;
begin
  Result := clWhite;
end;

function TcxGridStructureControlViewInfo.GetContentFont: TFont;
begin
  Result := StructureControl.Font;
end;

function TcxGridStructureControlViewInfo.GetContentFontColor: TColor;
begin
  Result := clBlack;
end;

function TcxGridStructureControlViewInfo.GetGridColor(AHighlighted: Boolean): TColor;
begin
  if AHighlighted then
    Result := cxGridDesignGridHotColor
  else
    Result := cxGridDesignGridColor;
end;

function TcxGridStructureControlViewInfo.GetHighlightedFontColor: TColor;
begin
  Result := GetContentFontColor;
end;

function TcxGridStructureControlViewInfo.GetLevelColor(AHighlighted: Boolean): TColor;
begin
  if AHighlighted then
    Result := cxGridDesignLevelHotColor
  else
    Result := cxGridDesignLevelColor;
end;

function TcxGridStructureControlViewInfo.GetRowBorderColor(AColorIndex: TcxGridColorIndex;
  ASelected, AHighlighted: Boolean): TColor;
begin
  if ASelected then
    if AHighlighted then
      Result := cxGridDesignSelectedHotBorderColor
    else
      Result := cxGridDesignSelectedBorderColor
  else
    case AColorIndex of
      ciLevel:
        if AHighlighted then
          Result := cxGridDesignLevelHotBorderColor
        else
          Result := cxGridDesignLevelBorderColor;
      ciView:
        Result := cxGridDesignViewBorderColor;
    else
      if AHighlighted then
        Result := cxGridDesignGridHotBorderColor
      else
        Result := cxGridDesignGridBorderColor;
    end;
end;

function TcxGridStructureControlViewInfo.GetRowColor(AColorIndex: TcxGridColorIndex;
  ASelected, AHighlighted: Boolean): TColor;
begin
  if ASelected then
    Result := GetSelectedColor(AHighlighted)
  else
    case AColorIndex of
      ciLevel:
        Result := GetLevelColor(AHighlighted);
      ciView:
        Result := GetViewColor(AHighlighted);
    else
      Result := GetGridColor(AHighlighted);
    end;
end;

function TcxGridStructureControlViewInfo.GetRowTextColor(AColorIndex: TcxGridColorIndex;
  ASelected, AHighlighted: Boolean): TColor;
begin
  if ASelected then
    Result := GetSelectedFontColor
  else
    if AHighlighted then
      Result := GetHighlightedFontColor
    else
      Result := GetContentFontColor;
end;

function TcxGridStructureControlViewInfo.GetSelectedColor(AHighlighted: Boolean): TColor;
begin
  if AHighlighted then
    Result := cxGridDesignSelectedHotColor
  else
    Result := cxGridDesignSelectedColor;
end;

function TcxGridStructureControlViewInfo.GetSelectedFontColor: TColor;
begin
  Result := GetContentFontColor;
end;

function TcxGridStructureControlViewInfo.GetViewColor(AHighlighted: Boolean): TColor;
begin
  if AHighlighted then
    Result := cxGridDesignViewHotColor
  else
    Result := cxGridDesignViewColor;
end;

procedure TcxGridStructureControlViewInfo.Paint;

  procedure SetFontAndBrush(ASelected, AHighlighted: Boolean; AColorIndex: TcxGridColorIndex);
  begin
    Canvas.Brush.Color := dxGetNearestColor(GetRowColor(AColorIndex, ASelected, AHighlighted));
    Canvas.Font.Color := dxGetNearestColor(GetRowTextColor(AColorIndex, ASelected, AHighlighted));
    {if AHighlighted then
      Canvas.Font.Style := Canvas.Font.Style + [fsUnderline]
    else
      Canvas.Font.Style := Canvas.Font.Style - [fsUnderline];}
  end;

  procedure DrawCell(ACellRect, ATextRect: TRect; const AText: string);
  begin
    Canvas.FillRect(ACellRect);
    Canvas.Brush.Style := bsClear;
    Canvas.DrawText(AText, ATextRect, cxAlignLeft or cxAlignVCenter or cxSingleLine);
    Canvas.Brush.Style := bsSolid;
  end;

  procedure DrawRows;
  var
    I: Integer;
    ARow: TcxGridStructureControlRowViewInfo;
    AColorIndex: TcxGridColorIndex;
  begin
    for I := 0 to Rows.Count - 1 do
    begin
      ARow := Rows[I];
      // Level
      if I = 0 then
        AColorIndex := ciGrid
      else
        AColorIndex := ciLevel;
      Canvas.FrameRect(ARow.LevelBounds,
        dxGetNearestColor(GetRowBorderColor(AColorIndex, ARow.LevelSelected, ARow.LevelHighlighted)),
        RowBorderWidth);
      SetFontAndBrush(ARow.LevelSelected, ARow.LevelHighlighted, AColorIndex);
      DrawCell(ARow.LevelContentBounds, ARow.LevelTextArea, ARow.LevelText);
      // View
      if not IsRectEmpty(ARow.ViewBounds) then
      begin
        Canvas.FrameRect(ARow.ViewBounds,
          dxGetNearestColor(GetRowBorderColor(ciView, ARow.ViewSelected, ARow.ViewHighlighted)),
          RowBorderWidth);
        SetFontAndBrush(ARow.ViewSelected, ARow.ViewHighlighted, ciView);
        DrawCell(ARow.ViewContentBounds, ARow.ViewTextArea, ARow.ViewText);
      end;
      Canvas.ExcludeClipRect(ARow.Bounds);
    end;
  end;

begin
  // Rows
  Canvas.Font := GetContentFont;
  DrawRows;
  // Background
  StructureControl.DoDrawBackground;
end;

function TcxGridStructureControlViewInfo.GetCanvas: TcxCanvas;
begin
  if StructureControl.HandleAllocated then
  begin
    if FCanvas <> nil then
      FreeAndNil(FCanvas);
    Result := StructureControl.Canvas;
  end
  else
  begin
    if FCanvas = nil then
      FCanvas := TcxScreenCanvas.Create;
    Result := FCanvas;
  end;
end;

function TcxGridStructureControlViewInfo.GetGrid: TcxCustomGrid;
begin
  Result := StructureControl.Grid;
end;

function TcxGridStructureControlViewInfo.GetLevelByIndex(AIndex: Integer): TcxGridLevel;
var
  ACurrent: Integer;

  procedure CalculateCount(ALevel: TcxGridLevel);
  var
    I: Integer;
  begin
    if ALevel = nil then Exit;
    if ACurrent = AIndex then
      Result := ALevel
    else
    begin
      Inc(ACurrent);
      for I := 0 to ALevel.Count - 1 do
      begin
        CalculateCount(ALevel[I]);
        if Result <> nil then
          Break;
      end;
    end;
  end;

begin
  Result := nil;
  ACurrent := 0;
  if (Grid <> nil) and (Grid.Levels <> nil) then
    CalculateCount(Grid.Levels);
end;

procedure TcxGridStructureControlViewInfo.GetLevelInfo(AIndex: Integer;
  var ALevelText, AViewText: string; var ALevelIndex: Integer;
  var AIsParent, AIsLast: Boolean);

  function GetCompName(AComponent: TComponent): string;
  begin
    Result := AComponent.Name;
    if Result = '' then
      Result := '< >';
  end;

var
  ALevel: TcxGridLevel;
begin
  ALevelText := '';
  AViewText := '';
  ALevelIndex := 0;
  AIsParent := False;
  AIsLast := False;
  ALevel := GetLevelByIndex(AIndex);
  if ALevel <> nil then
  begin
    AIsParent := ALevel.Count > 0;
    AIsLast := (ALevel.Parent <> nil) and
      (ALevel.Parent[ALevel.Parent.Count - 1] = ALevel);
    if ALevel.IsRoot then // TODO: IsRoot
      ALevelText := GetCompName(Grid)
    else
    begin
      ALevelText := GetCompName(ALevel);
      if ALevel.GridView <> nil then
        AViewText := GetCompName(ALevel.GridView);
    end;
  end;
  ALevelIndex := 0;
  while ALevel <> nil do
  begin
    if ALevel.IsRoot then // TODO: IsRoot
      Break
    else
    begin
      Inc(ALevelIndex);
      ALevel := ALevel.Parent;
    end;
  end;
end;

function TcxGridStructureControlViewInfo.GetRowCount: Integer;

  procedure CalculateCount(ALevel: TcxGridLevel);
  var
    I: Integer;
  begin
    Inc(Result, ALevel.Count);
    for I := 0 to ALevel.Count - 1 do
      CalculateCount(ALevel[I]);
  end;

begin
  if Grid = nil then
    Result := 0
  else
  begin
    Result := 1; // Root
    if Grid.Levels <> nil then
      CalculateCount(Grid.Levels);
  end;
end;

{ TcxGridStructureControl }

constructor TcxGridStructureControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViewInfo := TcxGridStructureControlViewInfo.Create(Self);
end;

destructor TcxGridStructureControl.Destroy;
begin
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

procedure TcxGridStructureControl.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TcxGridStructureControl.Changed;
begin
  if LockCount = 0 then
    UpdateContent;
end;

procedure TcxGridStructureControl.EndUpdate;
begin
  Dec(FLockCount);
  Changed;
end;

function TcxGridStructureControl.GetHitInfo(P: TPoint): TcxGridStructureControlHitInfo;
var
  I: Integer;
begin
  Result.HitTest := htNowhere;
  Result.RowIndex := -1;
  for I := 0 to ViewInfo.Rows.Count - 1 do
    if PtInRect(ViewInfo.Rows[I].Bounds, P) then
    begin
      Result.RowIndex := I;
      if PtInRect(ViewInfo.Rows[I].LevelBounds, P) then
      begin
        Result.HitTest := htLevel;
        Break;
      end
      else
        if PtInRect(ViewInfo.Rows[I].ViewBounds, P) then
        begin
          Result.HitTest := htView;
          Break;
        end;
    end;
end;

function TcxGridStructureControl.GetLevelByRowIndex(ARowIndex: Integer): TcxGridLevel;
begin
  Result := ViewInfo.GetLevelByIndex(ARowIndex);
end;

function TcxGridStructureControl.GetSelectedLevel: TcxGridLevel;
var
  ASelectionList: TList;
begin
  Result := nil;
  ASelectionList := TList.Create;
  try
    GetSelectionLevels(ASelectionList);
    if ASelectionList.Count = 1 then
      Result := TcxGridLevel(ASelectionList[0]);
  finally
    ASelectionList.Free;
  end;
end;

procedure TcxGridStructureControl.GetSelection(ASelectionList: TList);
var
  I: Integer;
  ALevel: TcxGridLevel;
begin
  ASelectionList.Clear;
  for I := 0 to ViewInfo.Rows.Count - 1 do
  begin
    ALevel := ViewInfo.GetLevelByIndex(I);
    if Assigned(ALevel) then
    begin
      if ViewInfo.Rows[I].LevelSelected then
      begin
        if ALevel.IsRoot then // TODO: IsRoot
          ASelectionList.Add(Grid)
        else
          ASelectionList.Add(ALevel);
      end;
      if ViewInfo.Rows[I].ViewSelected and (ALevel.GridView <> nil) then
        ASelectionList.Add(ALevel.GridView);
    end;
  end;
end;

procedure TcxGridStructureControl.GetSelectionLevels(ASelectionList: TList);
var
  I: Integer;
begin
  GetSelection(ASelectionList);
  for I := ASelectionList.Count - 1 downto 0 do
    if not (TObject(ASelectionList[I]) is TcxGridLevel) then
      ASelectionList.Delete(I);
end;

function TcxGridStructureControl.GetSelectionLevelCount: Integer;
var
  ASelectionList: TList;
begin
  ASelectionList := TList.Create;
  try
    GetSelectionLevels(ASelectionList);
    Result := ASelectionList.Count;
  finally
    ASelectionList.Free;
  end;
end;

procedure TcxGridStructureControl.GetSelectionViews(ASelectionList: TList);
var
  I: Integer;
begin
  GetSelection(ASelectionList);
  for I := ASelectionList.Count - 1 downto 0 do
    if not (TObject(ASelectionList[I]) is TcxCustomGridView) then
      ASelectionList.Delete(I);
end;

function TcxGridStructureControl.GetSelectionViewCount: Integer;
var
  ASelectionList: TList;
begin
  ASelectionList := TList.Create;
  try
    GetSelectionViews(ASelectionList);
    Result := ASelectionList.Count;
  finally
    ASelectionList.Free;
  end;
end;

procedure TcxGridStructureControl.SyncSelection(ASelectionList: TList);

  procedure CheckLevel(ARow: TcxGridStructureControlRowViewInfo; AComponent: TComponent);
  var
    ASelected: Boolean;
  begin
    ASelected := ASelectionList.IndexOf(AComponent) <> -1;
    if ARow.LevelSelected <> ASelected then
    begin
      ARow.LevelSelected := ASelected;
      InvalidateRect(ARow.LevelBounds, False);
    end;
  end;

  procedure CheckView(ARow: TcxGridStructureControlRowViewInfo; AComponent: TComponent);
  var
    ASelected: Boolean;
  begin
    ASelected := ASelectionList.IndexOf(AComponent) <> -1;
    if ARow.ViewSelected <> ASelected then
    begin
      ARow.ViewSelected := ASelected;
      InvalidateRect(ARow.ViewBounds, False);
    end;
  end;

var
  I: Integer;
  ALevel: TcxGridLevel;
begin
  for I := 0 to ViewInfo.Rows.Count - 1 do
  begin
    ALevel := ViewInfo.GetLevelByIndex(I);
    if Assigned(ALevel) then
      if ALevel.IsRoot then // TODO: IsRoot
        CheckLevel(ViewInfo.Rows[I], Grid)
      else
      begin
        CheckLevel(ViewInfo.Rows[I], ALevel);
        if ALevel.GridView <> nil then
          CheckView(ViewInfo.Rows[I], ALevel.GridView);
      end;
  end;
end;

procedure TcxGridStructureControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  AComponent: TComponent;
begin
  FMouseDownHitInfo := GetHitInfo(Point(X, Y));
  if Button in [mbLeft, mbRight] then
  begin
    AComponent := GetComponentByHitInfo(FMouseDownHitInfo);
    if AComponent <> nil then
      SelectComponent(AComponent, (Button = mbRight) or not MultiSelect or not (ssShift in Shift));
    SetPressed(True);
    CheckMousePos(Point(X, Y));
  end;
  inherited;
end;

procedure TcxGridStructureControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  CheckMousePos(Point(X, Y));
end;

procedure TcxGridStructureControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button in [mbLeft, mbRight] then
    CancelPressed(Point(X, Y));
end;

procedure TcxGridStructureControl.Paint;
begin
  ViewInfo.Paint;
end;

procedure TcxGridStructureControl.AddToSelection(AObject: TPersistent; AClearSelection: Boolean);

  function IsEquals(AList1, AList2: TList): Boolean;
  var
    I: Integer;
  begin
    Result := AList1.Count = AList2.Count;
    if Result then
      for I := 0 to AList1.Count - 1 do
        if AList1[I] <> AList2[I] then
        begin
          Result := False;
          Break;
        end;
  end;

var
  APrevSelection, ASelection: TList;
begin
  APrevSelection := TList.Create;
  ASelection := TList.Create;
  try
    GetSelection(APrevSelection);
    if not AClearSelection then
      GetSelection(ASelection);
    if (AObject <> nil) and (ASelection.Remove(AObject) = -1) then
      ASelection.Add(AObject);
    if not IsEquals(ASelection, APrevSelection) then
    begin
      SyncSelection(ASelection);
      SelectionChanged;
    end;
  finally
    ASelection.Free;
    APrevSelection.Free;
  end;
end;

procedure TcxGridStructureControl.CancelPressed(P: TPoint);
begin
  SetPressed(False);
  CheckMousePos(P);
end;

procedure TcxGridStructureControl.CheckMousePos(P: TPoint);
begin
  FHitInfo := GetHitInfo(P);
  UpdateHighlighted;
end;

procedure TcxGridStructureControl.BoundsChanged;
begin
  inherited;
  Changed;
end;

procedure TcxGridStructureControl.DoDrawBackground;
begin
  if Assigned(FOnDrawBackground) then
    FOnDrawBackground(Self)
  else
  begin
    Canvas.Brush.Color := ViewInfo.GetContentColor;
    Canvas.FillRect(ClientRect);
  end;
end;

procedure TcxGridStructureControl.FontChanged;
begin
  inherited;
  Changed;
end;

function TcxGridStructureControl.GetComponentByHitInfo(AHitInfo: TcxGridStructureControlHitInfo): TComponent;
var
  ALevel: TcxGridLevel;
begin
  Result := nil;
  if AHitInfo.RowIndex = -1 then Exit;
  ALevel := ViewInfo.GetLevelByIndex(AHitInfo.RowIndex);
  if ALevel = nil then Exit;
  if AHitInfo.HitTest = htLevel then
  begin
    if ALevel.IsRoot then // TODO
      Result := Grid
    else
      Result := ALevel;
  end
  else
    if AHitInfo.HitTest = htView then
    begin
      if ALevel.GridView <> nil then
        Result := ALevel.GridView;
    end;
end;

function TcxGridStructureControl.GetCurrentCursor(X, Y: Integer): TCursor;
begin
  if GetHitInfo(Point(X, Y)).HitTest in [htLevel, htView] then
    Result := crcxHandPoint
  else
    Result := inherited GetCurrentCursor(X, Y);
end;

function TcxGridStructureControl.MayFocus: Boolean;
begin
  Result := FMayFocused;
end;

procedure TcxGridStructureControl.MouseLeave(AControl: TControl);
begin
  CheckMousePos(Point(-1, -1));
end;

procedure TcxGridStructureControl.SelectComponent(AObject: TPersistent;
  AClearSelection: Boolean = True);
begin
  AddToSelection(AObject, AClearSelection);
  if Assigned(FOnSelectComponent) then
    FOnSelectComponent(AObject, AClearSelection);
end;

procedure TcxGridStructureControl.SelectionChanged;
begin
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
end;

procedure TcxGridStructureControl.SetPressed(APressed: Boolean);
begin
  if FMousePressed <> APressed then
  begin
    FMousePressed := APressed;
    // TODO
  end;
end;

procedure TcxGridStructureControl.UpdateContent;
begin
  ViewInfo.Calculate;
  Invalidate;
end;

procedure TcxGridStructureControl.UpdateHighlighted;

  procedure CheckLevel(ARow: TcxGridStructureControlRowViewInfo; AHighlighted: Boolean);
  begin
    if ARow.LevelHighlighted <> AHighlighted then
    begin
      ARow.LevelHighlighted := AHighlighted;
      InvalidateRect(ARow.LevelBounds, False);
    end;
  end;

  procedure CheckView(ARow: TcxGridStructureControlRowViewInfo; AHighlighted: Boolean);
  begin
    if ARow.ViewHighlighted <> AHighlighted then
    begin
      ARow.ViewHighlighted := AHighlighted;
      InvalidateRect(ARow.ViewBounds, False);
    end;
  end;

var
  I: Integer;
  AHitInfo: TcxGridStructureControlHitInfo;
begin
  for I := 0 to ViewInfo.Rows.Count - 1 do
  begin
    if FMousePressed then
      AHitInfo := FMouseDownHitInfo
    else
      AHitInfo := FHitInfo;
    if AHitInfo.HitTest = htLevel then
    begin
      CheckLevel(ViewInfo.Rows[I], (AHitInfo.RowIndex = I));
      CheckView(ViewInfo.Rows[I], False);
    end
    else
      if AHitInfo.HitTest = htView then
      begin
        CheckLevel(ViewInfo.Rows[I], False);
        CheckView(ViewInfo.Rows[I], (AHitInfo.RowIndex = I));
      end
      else
      begin
        CheckLevel(ViewInfo.Rows[I], False);
        CheckView(ViewInfo.Rows[I], False);
      end;
  end;
end;

procedure TcxGridStructureControl.ClearSelection;
begin
  AddToSelection(nil, True);
end;

procedure TcxGridStructureControl.SetGrid(Value: TcxCustomGrid);
begin
  if FGrid <> Value then
  begin
    FGrid := Value;
    Changed;
  end;
end;

procedure TcxGridStructureControl.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    ClearSelection;
  end;
end;

{ TcxGridLevelViewList }

constructor TcxGridLevelViewList.Create(ALevel: TcxGridLevel);
begin
  FLevel := ALevel;
  inherited Create(GetObjectDesigner(ALevel));
end;

procedure TcxGridLevelViewList.GetViewNames;
var
  I: Integer;
  AView: TcxCustomGridView;
begin
  inherited;
  for I := ViewNames.Count - 1 downto 0 do
  begin
    AView := Views[I];
    if (AView.Repository = nil) and (AView.StorageControl <> Level.Control) then
      ViewNames.Delete(I);
  end;
end;

{ TcxGridStructureHelper }

constructor TcxGridStructureHelper.Create(AStructureControl: TcxGridStructureControl);
begin
  inherited Create;
  FStructureControl := AStructureControl;
  FStructureControl.OnMouseDown := StructureControlMouseDown;
end;

destructor TcxGridStructureHelper.Destroy;
begin
  FViewMenuProvider.Free;
  FreeAndNil(FPopupMenu);
  inherited Destroy;
end;

function TcxGridStructureHelper.CanAddComponent: Boolean;
begin
  Result := cxDesignWindows.CanAddComponent(StructureControl.Grid.Owner);
end;

function TcxGridStructureHelper.CanDeleteComponent(AComponent: TComponent): Boolean;
var
  AOwner: TComponent;
begin
  if AComponent = nil then
    AOwner := StructureControl.Grid.Owner
  else
    AOwner := AComponent.Owner;
  Result := cxDesignWindows.CanDeleteComponent(AOwner, AComponent);
end;

procedure TcxGridStructureHelper.ExecuteLevelViewsMenuItem(ALevel: TcxGridLevel; AMenuItem: TMenuItem);
var
  ALevelViewList: TcxGridLevelViewList;
  AView: TcxCustomGridView;
begin
  ALevelViewList := TcxGridLevelViewList.Create(ALevel);
  try
    AView := ALevelViewList.Views[AMenuItem.MenuIndex];
    if ALevel.GridView <> AView then
    begin
      ALevel.GridView := AView;
      UpdateDesigner;
    end;
  finally
    ALevelViewList.Free;
  end;
end;

procedure TcxGridStructureHelper.FillLevelViewsMenu(AMenu: TMenuItem;
  ALevel: TcxGridLevel; AOnClick: TNotifyEvent);
var
  ALevelViewList: TcxGridLevelViewList;
  I: Integer;
begin
  ALevelViewList := TcxGridLevelViewList.Create(ALevel);
  try
    for I := 0 to ALevelViewList.ViewNames.Count - 1 do
      AMenu.Add(CreateMenuItem(AMenu.Owner, ALevelViewList.ViewNames[I], AOnClick));
    if ALevel.GridView <> nil then
      AMenu[ALevelViewList.GetViewIndex(ALevel.GridView)].Checked := True;
  finally
    ALevelViewList.Free;
  end;
  AMenu.Add(CreateMenuItem(AMenu.Owner, '-'));
  AMenu.Add(CreateMenuItem(AMenu.Owner, 'None', AOnClick, True, -1, ALevel.GridView = nil));
end;

procedure TcxGridStructureHelper.StructureControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  function IsViewEmpty(AView: TcxCustomGridView): Boolean;
  begin
    Result := AView.DataController.ItemCount = 0;
  end;

  procedure PrepareGridMenu;
  begin
    FPopupMenu.Items.Add(CreateMenuItem(FPopupMenu, 'Add Level', CreateLevelClick,
      CanAddComponent));
  end;

  procedure PrepareLevelMenu;
  var
    AMenuItem: TMenuItem;
  begin
    AMenuItem := CreateMenuItem(FPopupMenu, 'Create View', nil, CanAddComponent);
    FPopupMenu.Items.Add(AMenuItem);
    FillRegisteredViewsMenu(AMenuItem, CreateViewClick);

    AMenuItem := CreateMenuItem(FPopupMenu, 'Select View');
    FPopupMenu.Items.Add(AMenuItem);
    FillLevelViewsMenu(AMenuItem, FPopupMenuLevel, SelectViewClick);

    PrepareGridMenu;
    FPopupMenu.Items.Add(CreateMenuItem(FPopupMenu, '-'));
    FPopupMenu.Items.Add(CreateMenuItem(FPopupMenu, 'Delete Level',
      DeleteLevelClick, CanDeleteComponent(FPopupMenuLevel)));
    FPopupMenu.Items.Add(CreateMenuItem(FPopupMenu, '-'));
    FPopupMenu.Items.Add(CreateMenuItem(FPopupMenu, 'Move Level Up', MoveLevelClick,
      FPopupMenuLevel.Index > 0, -1));
    FPopupMenu.Items.Add(CreateMenuItem(FPopupMenu, 'Move Level Down', MoveLevelClick,
      FPopupMenuLevel.Index < FPopupMenuLevel.Parent.Count - 1, 1));
  end;

  function PrepareViewMenu(AView: TcxCustomGridView): TcxCustomGridViewMenuProvider;
  var
    I: Integer;
    AMenuItem: TMenuItem;
  begin
    FPopupMenuView := AView;
    Result := CreateViewMenuProvider(AView);
    if Result = nil then Exit;
    for I := 0 to Result.Items.Count - 1 do
    begin
      AMenuItem := CreateMenuItem(FPopupMenu, '');
      Result.Items[I].Prepare(AMenuItem);
      FPopupMenu.Items.Add(AMenuItem);
    end;
  end;

var
  AHitInfo: TcxGridStructureControlHitInfo;
  P: TPoint;
begin
  if Button <> mbRight then Exit;
  P := Point(X, Y);
  AHitInfo := FStructureControl.GetHitInfo(P);
  if AHitInfo.HitTest <> htNowhere then
  begin
    if FPopupMenu = nil then
      FPopupMenu := TPopupMenu.Create(nil);
    while FPopupMenu.Items.Count > 0 do
      FPopupMenu.Items[0].Free;

    FPopupMenuLevel := FStructureControl.GetLevelByRowIndex(AHitInfo.RowIndex);
    if FPopupMenuLevel <> nil then
    begin
      if FPopupMenuLevel.IsRoot then
        PrepareGridMenu
      else
        if AHitInfo.HitTest = htLevel then
          PrepareLevelMenu
        else
        begin
          FViewMenuProvider.Free;
          FViewMenuProvider := PrepareViewMenu(FPopupMenuLevel.GridView);
        end;
      P := FStructureControl.ClientToScreen(P);
      FPopupMenu.Popup(P.X, P.Y);
      FStructureControl.CancelPressed(Point(-1, -1));
    end;
  end;
end;

procedure TcxGridStructureHelper.UpdateDesigner;
begin
  if Assigned(FOnUpdateDesigner) then
    FOnUpdateDesigner(Self);
end;

procedure TcxGridStructureHelper.CreateLevelClick(Sender: TObject);
var
  ALevel: TcxGridLevel;
begin
  StructureControl.BeginUpdate;
  try
    ALevel := FPopupMenuLevel.Add;
    ALevel.Name := GenLevelName(StructureControl.Grid, ALevel);
    StructureControl.SelectComponent(ALevel);
    UpdateDesigner;
  finally
    StructureControl.EndUpdate;
  end;
end;

procedure TcxGridStructureHelper.CreateViewClick(Sender: TObject);
var
  AViewClass: TcxCustomGridViewClass;
  AView: TcxCustomGridView;
begin
  AViewClass := TcxCustomGridViewClass(
    cxGridRegisteredViews[((Sender as TMenuItem).Tag)]);
  StructureControl.BeginUpdate;
  try
    AView := StructureControl.Grid.CreateView(AViewClass);
    AView.Name := GenViewName(StructureControl.Grid, AView);
    RestoreViewFromTemplate(GetViewTemplateRegKey, AView);
    FPopupMenuLevel.GridView := AView;
    StructureControl.SelectComponent(AView);
    UpdateDesigner;
  finally
    StructureControl.EndUpdate;
  end;
end;

procedure TcxGridStructureHelper.SelectViewClick(Sender: TObject);
begin
  ExecuteLevelViewsMenuItem(FPopupMenuLevel, Sender as TMenuItem);
end;

procedure TcxGridStructureHelper.DeleteLevelClick(Sender: TObject);
begin
  StructureControl.BeginUpdate;
  try
    StructureControl.SelectComponent(FPopupMenuLevel.GetParentComponent);
    FPopupMenuLevel.Free;
    UpdateDesigner;
  finally
    StructureControl.EndUpdate;
  end;
end;

procedure TcxGridStructureHelper.MoveLevelClick(Sender: TObject);
var
  ASelection: TList;
begin
  ASelection := TList.Create;
  try
    StructureControl.GetSelection(ASelection);
    FPopupMenuLevel.Index := FPopupMenuLevel.Index + Integer(TMenuItem(Sender).Tag);
    UpdateDesigner;
    StructureControl.SyncSelection(ASelection);
  finally
    ASelection.Free;
  end;
end;

{ TcxGridStructureNavigator }

constructor TcxGridStructureNavigator.Create(AGrid: TcxCustomGrid);
begin
  inherited;
  FMakeSelectionVisible := True;
  FSelection := TList.Create;
  FStructureControl := TcxGridStructureControl.Create(Self);
  with FStructureControl do
  begin
    SetBounds(0, 0, 0, 0);
    Grid := Self.Grid;
    MultiSelect := True;
    OnDrawBackground := DrawStructureControlBackground;
    OnSelectComponent := Self.SelectComponent;
    Parent := Self;
  end;
  FStructureHelper := TcxGridStructureHelper.Create(FStructureControl);
  FStructureHelper.OnUpdateDesigner := UpdateDesigner;
  FCloseButton := TSpeedButton.Create(Self);
  with FCloseButton do
  begin
    CreateCloseBitmap(Glyph);
    Flat := True;
    SetBounds(0, 0, 0, 0);
    OnClick := CloseButtonClick;
    Parent := Self;
    Visible := False;
  end;
  FCustomizeButton := TSpeedButton.Create(Self);
  with FCustomizeButton do
  begin
    Caption := SCustomizeText;
    Flat := True;
    SetBounds(0, 0, 0, 0);
    OnClick := CustomizeButtonClick;
    Parent := Self;
  end;
  FSeparator := TBevel.Create(Self);
  with FSeparator do
  begin
    Shape := bsTopLine;
    SetBounds(0, 0, 0, 0);
    Parent := Self;
  end;
  CreateDefaultComponents;
  FDesignHelper := TcxDesignHelper.Create(Grid);
  DesignNotificationHandler.RegisterGrid(Grid);
end;

destructor TcxGridStructureNavigator.Destroy;
begin
  DesignNotificationHandler.UnregisterGrid(Grid);
  FreeAndNil(FDesignHelper);
  FreeAndNil(FStructureHelper);
  FreeAndNil(FSelection);
  inherited Destroy;
end;

procedure TcxGridStructureNavigator.Paint;
begin
  // Border
  Canvas.FrameRect(ClientRect, clWindowFrame, FrameBorderSize);
  // Background
  DrawBackground(Canvas);
end;

(*
procedure TcxGridStructureNavigator.Calculate;
var
  R: TRect;
  ACanvas: TcxCanvas;
  ACloseButtonSize: TSize;
  ACloseButtonArea: TRect;
  ACustomizeButtonArea: TRect;
  ASeparatorArea: TRect;
  ASeparatorHeight: Integer;
begin
  FStructureControl.ViewInfo.Calculate;
  R := FStructureControl.ViewInfo.Bounds;
  OffsetRect(R, FrameBorderSize, FrameBorderSize);
  FStructureControl.BoundsRect := R;
  // Calc Customize Button
  ASeparatorHeight := ButtonOffset * 2;
  ACanvas := FStructureControl.ViewInfo.Canvas;
  ACanvas.Font := FCustomizeButton.Font;
  ACloseButtonSize.cx := 2 * TextOffset + ACanvas.TextWidth(SCustomizeText) + 2 * ButtonBorderSize;
  ACloseButtonSize.cy := 2 * TextOffset + ACanvas.TextHeight(SCustomizeText) + 2 * ButtonBorderSize;
  ACustomizeButtonArea := Rect(R.Left, R.Bottom + ASeparatorHeight, R.Left + 2 * ButtonOffset + ACloseButtonSize.cx,
    R.Bottom + ASeparatorHeight + 2 * ButtonOffset + ACloseButtonSize.cy);
  FCustomizeButton.SetBounds(ACustomizeButtonArea.Left + ButtonOffset,
    ACustomizeButtonArea.Top + ButtonOffset, ACloseButtonSize.cx, ACloseButtonSize.cy);
  ASeparatorArea := Rect(ACustomizeButtonArea.Left, ACustomizeButtonArea.Top - ASeparatorHeight,
    ACustomizeButtonArea.Right, ACustomizeButtonArea.Top);
  if R.Right < ACustomizeButtonArea.Right then
    R.Right := ACustomizeButtonArea.Right;
  FSeparator.BoundsRect := Rect(ASeparatorArea.Left + ButtonOffset,
    ASeparatorArea.Top + ButtonOffset, R.Right - ButtonOffset,
    ASeparatorArea.Bottom);
  // Calc Close Button
  ACloseButtonArea := Rect(R.Right, R.Top,
    R.Right + 2 * ButtonOffset + CloseButtonWidth + 2 * ButtonBorderSize,
    R.Top + 2 * ButtonOffset + CloseButtonHeight + 2 * ButtonBorderSize);
  FCloseButton.SetBounds(ACloseButtonArea.Left + ButtonOffset,
    ACloseButtonArea.Top + ButtonOffset, CloseButtonWidth + 2 * ButtonBorderSize,
    CloseButtonHeight + 2 * ButtonBorderSize);
  if R.Bottom < ACloseButtonArea.Bottom then
    R.Bottom := ACloseButtonArea.Bottom;
  // Total Area
  R.Right := ACloseButtonArea.Right;
  R.Bottom := ACustomizeButtonArea.Bottom;
  InflateRect(R, FrameBorderSize, FrameBorderSize);
  SetBounds(Left, Top, R.Right - R.Left, R.Bottom - R.Top);
end;
*)

procedure TcxGridStructureNavigator.Calculate;
var
  R: TRect;
  ACanvas: TcxCanvas;
  ACloseButtonSize: TSize;
  ACloseButtonArea: TRect;
  ACustomizeButtonArea: TRect;
  ASeparatorArea: TRect;
  ASeparatorHeight: Integer;
begin
  FStructureControl.ViewInfo.Calculate;
  R := FStructureControl.ViewInfo.Bounds;
  OffsetRect(R, FrameBorderSize, FrameBorderSize);
  FStructureControl.BoundsRect := R;
  // Calc Customize Button
  ASeparatorHeight := ButtonOffset * 2;
  ACanvas := FStructureControl.ViewInfo.Canvas;
  ACanvas.Font := FCustomizeButton.Font;
  ACloseButtonSize.cx := 2 * TextOffset + ACanvas.TextWidth(SCustomizeText) + 2 * ButtonBorderSize;
  ACloseButtonSize.cy := 2 * TextOffset + ACanvas.TextHeight(SCustomizeText) + 2 * ButtonBorderSize;
  ACustomizeButtonArea := Rect(R.Left, R.Bottom + ASeparatorHeight, R.Left + 2 * ButtonOffset + ACloseButtonSize.cx,
    R.Bottom + ASeparatorHeight + 2 * ButtonOffset + ACloseButtonSize.cy);
  FCustomizeButton.SetBounds(ACustomizeButtonArea.Left + ButtonOffset,
    ACustomizeButtonArea.Top + ButtonOffset, ACloseButtonSize.cx, ACloseButtonSize.cy);
  ASeparatorArea := Rect(ACustomizeButtonArea.Left, ACustomizeButtonArea.Top - ASeparatorHeight,
    ACustomizeButtonArea.Right, ACustomizeButtonArea.Top);
  if R.Right < ACustomizeButtonArea.Right then
    R.Right := ACustomizeButtonArea.Right;
  FSeparator.BoundsRect := Rect(ASeparatorArea.Left + ButtonOffset,
    ASeparatorArea.Top + ButtonOffset, R.Right - ButtonOffset,
    ASeparatorArea.Bottom);
  // Calc Close Button
  ACloseButtonArea := Rect(R.Right, R.Top,
    R.Right + 2 * ButtonOffset + CloseButtonWidth + 2 * ButtonBorderSize,
    R.Top + 2 * ButtonOffset + CloseButtonHeight + 2 * ButtonBorderSize);
  FCloseButton.SetBounds(ACloseButtonArea.Left + ButtonOffset,
    ACloseButtonArea.Top + ButtonOffset, CloseButtonWidth + 2 * ButtonBorderSize,
    CloseButtonHeight + 2 * ButtonBorderSize);
  if R.Bottom < ACloseButtonArea.Bottom then
    R.Bottom := ACloseButtonArea.Bottom;
  // Total Area
  R.Right := ACloseButtonArea.Right;

  R.Bottom := ACustomizeButtonArea.Bottom;
  InflateRect(R, FrameBorderSize, FrameBorderSize);
  SetBounds(Left, Top, R.Right - R.Left, R.Bottom - R.Top);
end;

function TcxGridStructureNavigator.CalculateBoundsRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
  OffsetRect(Result, Grid.ClientBounds.Right - Result.Right - NavigatorOffset,
    Grid.ClientBounds.Bottom - Result.Bottom - NavigatorOffset);
end;

procedure TcxGridStructureNavigator.Changed;
begin
  if FStructureControl = nil then Exit;
  Calculate;
  inherited Changed;
  FStructureControl.Invalidate;
  NotifyEditors;
end;

procedure TcxGridStructureNavigator.CreateDefaultComponents;
var
  AViewClass: TcxCustomGridViewClass;
begin
  if Grid.ViewCount <> 0 then Exit;
  AViewClass := GetDefaultViewClass;
  if AViewClass <> nil then
  begin
    FDefaultView := Grid.CreateView(AViewClass);
    FDefaultView.Name := GenViewName(Grid, FDefaultView);
    RestoreViewFromTemplate(GetViewTemplateRegKey, FDefaultView);
    // TODO: load default
    FDefaultLevel := Grid.Levels.Add;
    FDefaultLevel.Name := GenLevelName(Grid, FDefaultLevel);
    FDefaultLevel.GridView := FDefaultView;
  end;
end;

procedure TcxGridStructureNavigator.DrawBackground(ACanvas: TcxCanvas);
begin
  Office11FillTubeGradientRect(ACanvas.Handle, ClientBounds, clWhite, {16248293}16115935{15785423}, False);
end;

procedure TcxGridStructureNavigator.DrawStructureControlBackground(Sender: TObject);
begin
  MoveWindowOrg(FStructureControl.Canvas.Handle, -FStructureControl.Left, -FStructureControl.Top);
  DrawBackground(FStructureControl.Canvas);
  MoveWindowOrg(FStructureControl.Canvas.Handle, FStructureControl.Left, FStructureControl.Top);
end;

function TcxGridStructureNavigator.GetClientBounds: TRect;
begin
  Result := inherited GetClientBounds;
  InflateRect(Result, -FrameBorderSize, -FrameBorderSize);
end;

function TcxGridStructureNavigator.GetDefaultViewClass: TcxCustomGridViewClass;
begin
  Result := TcxCustomGridAccess(Grid).GetDefaultViewClass;
  if Result = nil then
    Result := TcxGridDBTableView;
end;

function TcxGridStructureNavigator.GetDesigner: IDesigner;
begin
  Result := GetObjectDesigner(Grid);
end;

procedure TcxGridStructureNavigator.CloseButtonClick(Sender: TObject);
begin
  Visible := False;
end;

procedure TcxGridStructureNavigator.CustomizeButtonClick(Sender: TObject);
begin
  // TODO:
  if GetDesigner <> nil then
    ShowGridEditor(GetDesigner, Grid);
end;

procedure TcxGridStructureNavigator.SelectComponent(AObject: TPersistent;
  AClearSelection: Boolean);
begin
  if AClearSelection then
    FDesignHelper.SelectObject(AObject, True, False)
  else
    FDesignHelper.ChangeSelection(AObject);
end;

procedure TcxGridStructureNavigator.UpdateDesigner(Sender: TObject);
begin
  if GetDesigner <> nil then
    GetDesigner.Modified;
end;

procedure TcxGridStructureNavigator.CMDeferUpdate(var Message: TMessage);
begin
  UpdateDesignFormEditors(Grid);
end;

procedure TcxGridStructureNavigator.BeforeGridLoading;
begin
  FreeAndNil(FDefaultLevel);
  FreeAndNil(FDefaultView);
end;

function TcxGridStructureNavigator.CanAddComponent: Boolean;
begin
  Result := FDesignHelper.CanAddComponent(Grid);
end;

function TcxGridStructureNavigator.CanDeleteComponent(
  AComponent: TComponent): Boolean;
begin
  Result := FDesignHelper.CanDeleteComponent(Grid, AComponent);
end;

procedure TcxGridStructureNavigator.GetSelection(AList: TList);
begin
  FDesignHelper.GetSelection(AList);
end;

function TcxGridStructureNavigator.IsObjectSelected(AObject: TPersistent): Boolean;
begin
  Result := FDesignHelper.IsObjectSelected(AObject);
end;

procedure TcxGridStructureNavigator.NotifyEditors;
var
  Msg: TMsg;
begin
  if not HandleAllocated then Exit;
  if not PeekMessage(Msg, Handle, DXM_REFRESHCUSTOMIZATION, DXM_REFRESHCUSTOMIZATION, PM_NOREMOVE) then
    PostMessage(Handle, DXM_REFRESHCUSTOMIZATION, 0, 0);
end;

procedure TcxGridStructureNavigator.SelectionChanged(ASelection: TList);

  function AreEqual(AList1, AList2: TList): Boolean;
  var
    I: Integer;
  begin
    Result := AList1.Count = AList2.Count;
    if Result then
      for I := 0 to AList1.Count - 1 do
      begin
        Result := AList1[I] = AList2[I];
        if not Result then Break;
      end;
  end;

  procedure MakeObjectVisible(AObject: TPersistent);
  begin
    //!!! columns/rows/bands/chart items?
    if (AObject is TcxGridLevel) or (AObject is TcxCustomGridView) then
    begin
      if AObject is TcxCustomGridView then
        AObject := TcxCustomGridView(AObject).Level;
      if (AObject <> nil) and (TcxGridLevel(AObject).Control = Grid) then
        TcxGridLevel(AObject).MakeVisible;
    end;
  end;

begin
  if AreEqual(FSelection, ASelection) then
    Exit;
  FSelection.Assign(ASelection);

  if MakeSelectionVisible and (ASelection.Count = 1) then
    MakeObjectVisible(ASelection[0]);
  FStructureControl.SyncSelection(ASelection);
  if Grid.HandleAllocated then
    cxRedrawWindow(Grid.Handle, RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

procedure TcxGridStructureNavigator.SelectObject(AObject: TPersistent;
  AClearSelection: Boolean);
begin
  MakeSelectionVisible := False;
  try
    SelectComponent(AObject, AClearSelection);
  finally
    MakeSelectionVisible := True;
  end;
end;

procedure TcxGridStructureNavigator.SelectObjects(AObjects: TList);
begin
  MakeSelectionVisible := False;
  try
    FDesignHelper.SetSelection(AObjects);
  finally
    MakeSelectionVisible := True;
  end;
end;

procedure TcxGridStructureNavigator.UnselectObject(AObject: TPersistent);
begin
  MakeSelectionVisible := False;
  try
    FDesignHelper.UnselectObject(AObject);
  finally
    MakeSelectionVisible := True;
  end;
end;

initialization
  cxGridStructureNavigatorClass := TcxGridStructureNavigator;
  DesignNotificationHandler := TDesignNotificationHandler.Create;

finalization
  FreeAndNil(DesignNotificationHandler);

end.
