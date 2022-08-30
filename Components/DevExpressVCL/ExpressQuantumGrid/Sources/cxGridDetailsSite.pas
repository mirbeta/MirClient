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

unit cxGridDetailsSite;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Classes, Graphics, Controls,
  dxCoreClasses, cxClasses, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxPC, cxGridCommon, cxGridLevel, cxGridCustomView;

const
  htTab = 50;

  cxGridTabsRootSize: Integer = 1 + 3;

type
  TcxCustomGridDetailsSiteTabsViewInfo = class;
  TcxCustomGridDetailsSiteViewInfo = class;

  { hit tests }

  TcxGridDetailsSiteTabHitTestClass = class of TcxGridDetailsSiteTabHitTest;

  TcxGridDetailsSiteTabHitTest = class(TcxCustomGridHitTest)
  private
    FLevel: TcxGridLevel;
    FOwner: TObject;
  protected
    class function GetHitTestCode: Integer; override;
  public
    property Level: TcxGridLevel read FLevel write FLevel;
    property Owner: TObject read FOwner write FOwner;
  end;

  { painters }

  TcxGridDetailsSiteTabsPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxCustomGridDetailsSiteTabsViewInfo;
  protected
    procedure Paint; override;
    property ViewInfo: TcxCustomGridDetailsSiteTabsViewInfo read GetViewInfo;
  end;

  TcxGridDetailsSitePainterClass = class of TcxGridDetailsSitePainter;

  TcxGridDetailsSitePainter = class
  private
    FCanvas: TcxCanvas;
    FViewInfo: TcxCustomGridDetailsSiteViewInfo;
  protected
    procedure DrawBackground; virtual;
    procedure DrawFrame; virtual;
    procedure DrawTabs; virtual;
    property Canvas: TcxCanvas read FCanvas;
    property ViewInfo: TcxCustomGridDetailsSiteViewInfo read FViewInfo;
  public
    constructor Create(ACanvas: TcxCanvas; AViewInfo: TcxCustomGridDetailsSiteViewInfo); virtual;
    procedure Paint; virtual;
  end;

  { view infos }

  TcxGridLevelTabsViewInfo = class;

  TcxGridLevelTabsProperties = class(TcxCustomTabControlProperties)
  private
    FFocusActiveViewOnChange: Boolean;
    FFreeNotificator: TcxFreeNotificator;
    FUnderlineHotKeys: Boolean;
    function GetActiveLevel: TcxGridLevel;
    function GetLevelCount: Integer;
    function GetLevel(Index: Integer): TcxGridLevel;
    function GetSiteViewInfo: TcxCustomGridDetailsSiteViewInfo;
    procedure SetActiveLevel(Value: TcxGridLevel);
    procedure SetUnderlineHotKeys(Value: Boolean);
  protected
    function CanProcessChanged: Boolean; override;
    procedure FreeNotification(AComponent: TComponent);
    function GetLevelDisplayText(ALevel: TcxGridLevel): string;

    property FocusActiveViewOnChange: Boolean read FFocusActiveViewOnChange write FFocusActiveViewOnChange;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure AddLevel(ALevel: TcxGridLevel);

    property ActiveLevel: TcxGridLevel read GetActiveLevel write SetActiveLevel;
    property LevelCount: Integer read GetLevelCount;
    property Levels[Index: Integer]: TcxGridLevel read GetLevel;
    property SiteViewInfo: TcxCustomGridDetailsSiteViewInfo read GetSiteViewInfo;
    property UnderlineHotKeys: Boolean read FUnderlineHotKeys write SetUnderlineHotKeys;
  end;

  TcxGridLevelTabsController = class(TcxCustomTabControlController)
  private
    function GetProperties: TcxGridLevelTabsProperties;
  protected
    function HandleDialogChar(Key: Integer): Boolean; override;
    function GetClientToScreen(const APoint: TPoint): TPoint; override;
    function GetScreenToClient(const APoint: TPoint): TPoint; override;

    property Properties: TcxGridLevelTabsProperties read GetProperties;
  end;

  TcxGridLevelTabsViewInfo = class(TcxCustomTabControlViewInfo)
  private
    function GetController: TcxGridLevelTabsController;
    function GetProperties: TcxGridLevelTabsProperties;
    function GetSiteViewInfo: TcxCustomGridDetailsSiteViewInfo;
  protected
    procedure AfterPaintTab(ACanvas: TcxCanvas; ATab: TcxTab; AImageAndTextData: TcxPCOutTabImageAndTextData); override;
    function HasBorders: Boolean; override;

    property Controller: TcxGridLevelTabsController read GetController;
  public
    procedure PrepareTabCanvasFont(ATabViewInfo: TcxTabViewInfo; ACanvas: TcxCanvas); override;

    property Properties: TcxGridLevelTabsProperties read GetProperties;
    property SiteViewInfo: TcxCustomGridDetailsSiteViewInfo read GetSiteViewInfo;
  end;

  TcxCustomGridDetailsSiteTabsViewInfoClass = class of TcxCustomGridDetailsSiteTabsViewInfo;

  TcxCustomGridDetailsSiteTabsViewInfo = class(TcxCustomGridCellViewInfo, IcxTabControl, IcxControlComponentState)
  private
    FIcxTabControlBounds: TRect;
    FSiteViewInfo: TcxCustomGridDetailsSiteViewInfo;
    FTabsController: TcxGridLevelTabsController;
    FTabsPainter: TcxPCCustomPainter;
    FTabsProperties: TcxGridLevelTabsProperties;
    FTabsViewInfo: TcxGridLevelTabsViewInfo;
    FNeedActiveLevelChanged: Boolean;
    function GetActiveLevel: TcxGridLevel;
    function GetFirstTabVisibleIndex: Integer;
    function GetLevel: TcxGridLevel;
    procedure SetFirstTabVisibleIndex(Value: Integer);
    procedure TabsPropertiesChangeHandler(Sender: TObject);
  protected
    procedure DoInvalidate; override;
    function GetCanvas: TcxCanvas; override;
    function GetControl: TcxControl; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;

    function GetHeight: Integer; override;
    function GetWidth: Integer; override;

    function GetHotTrack: Boolean; override;
    procedure MouseLeave; override;
    function PtInCaller(const P: TPoint): Boolean; override;

    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;

    procedure CheckActiveLevel;

    procedure AdjustBounds; virtual;
    function AreTabsRotated: Boolean; virtual;
    function GetBkColor: TColor; virtual;
    function GetTabsPointFromHitTest(AHitTest: TcxCustomGridHitTest): TPoint;
    function GetIsVertical: Boolean;
    function GetTabPosition: TcxTabPosition;
    procedure InitTabs; virtual;
    procedure InitTabsData; virtual;
    procedure InitTabsHotKeyParams; virtual;
    procedure InitTabsLayout; virtual;
    procedure InitTabsStyle; virtual;
    procedure InitTabsViewParams; virtual;
    procedure SynchronizeActiveLevel;

    // IcxControlComponentState
    function IsDesigning: Boolean;
    function IsDestroying: Boolean;
    function IsLoading: Boolean;

    // IcxTabControl
    function GetController: TcxCustomTabControlController;
    function GetPainter: TcxPCCustomPainter;
    function GetProperties: TcxCustomTabControlProperties;
    function GetViewInfo: TcxCustomTabControlViewInfo;
    function CanDrawParentBackground: Boolean;
    function GetBoundsRect: TRect; virtual;
    function IcxTabControl.GetCanvas = GetTabCanvas;
    function GetTabCanvas: TcxCanvas;
    function IcxTabControl.GetControl = GetTabControl;
    function GetTabControl: TWinControl;
    function GetColor: TColor;
    function GetDragAndDropObject: TcxDragAndDropObject;
    function GetDragAndDropState: TcxDragAndDropState;
    function GetFont: TFont;
    function GetLookAndFeel: TcxLookAndFeel;
    procedure InvalidateRect(const R: TRect; AEraseBackground: Boolean);
    procedure SetModified;
    function IsEnabled: Boolean;
    function IsFocused: Boolean;
    function IsParentBackground: Boolean;
    procedure RequestLayout;

    property FirstTabVisibleIndex: Integer read GetFirstTabVisibleIndex write SetFirstTabVisibleIndex;
    property IsVertical: Boolean read GetIsVertical;
    property Level: TcxGridLevel read GetLevel;
    property TabsController: TcxGridLevelTabsController read FTabsController;
    property TabsPainter: TcxPCCustomPainter read FTabsPainter;
    property TabsProperties: TcxGridLevelTabsProperties read FTabsProperties;
    property TabsViewInfo: TcxGridLevelTabsViewInfo read FTabsViewInfo;
  public
    constructor Create(ASiteViewInfo: TcxCustomGridDetailsSiteViewInfo); virtual;
    destructor Destroy; override;
    procedure BeforeRecalculation; override;
    procedure Calculate(const ABounds: TRect); override;
    procedure GetClientBounds(var AClientBounds: TRect); virtual;
    function ProcessDialogChar(ACharCode: Word): Boolean; virtual;

    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    function MouseMove(AHitTest: TcxCustomGridHitTest; AShift: TShiftState): Boolean; override;
    function MouseUp(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;

    property ActiveLevel: TcxGridLevel read GetActiveLevel;
    property BkColor: TColor read GetBkColor;
    property SiteViewInfo: TcxCustomGridDetailsSiteViewInfo read FSiteViewInfo;
  end;

  // left tabs

  TcxGridDetailsSiteLeftTabsViewInfo = class(TcxCustomGridDetailsSiteTabsViewInfo)
  protected
    procedure AdjustBounds; override;
  end;

  // top tabs

  TcxGridDetailsSiteTopTabsViewInfo = class(TcxCustomGridDetailsSiteTabsViewInfo)
  protected
    procedure AdjustBounds; override;
  end;

  // custom details site

  TcxCustomGridDetailsSiteViewInfoCachedInfoClass = class of TcxCustomGridDetailsSiteViewInfoCachedInfo;

  TcxCustomGridDetailsSiteViewInfoCachedInfo = class
  public
    AssignedBounds: Boolean;
    FirstTabVisibleIndex: Integer;
    Bounds: TRect;
  end;

  TcxCustomGridDetailsSiteViewInfo = class(TcxComponent)
  private
    FActiveGridView: TcxCustomGridView;
    FFullyVisible: Boolean;
    FIsRightToLeftConverted: Boolean;
    FLevel: TcxGridLevel;
    FNormalHeight: Integer;
    FTabsViewInfo: TcxCustomGridDetailsSiteTabsViewInfo;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    function GetControl: TcxControl;
    function GetFrameBounds: TRect;
    function GetInternalHeight: Integer;
    function GetInternalWidth: Integer;
    function GetIsActiveGridViewDestroying: Boolean;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetMaxDetailHeight: Integer;
    function GetMaxDetailSize: TPoint;
    function GetMaxDetailWidth: Integer;
    function GetNonClientHeight: Integer;
    function GetNonClientWidth: Integer;
    function GetTabsPosition: TcxGridDetailTabsPosition;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;

    function CalculateHeight: Integer; virtual;
    function CalculateWidth: Integer; virtual;
    function GetBkColor: TColor; virtual;
    function GetCanvas: TcxCanvas; virtual; abstract;
    function GetClientBounds: TRect; virtual;
    function GetContainer: TcxControl; virtual; abstract;
    function GetDesignController: TcxCustomGridDesignController; virtual; abstract;
    function GetEmptyClientHeight: Integer; virtual;
    function GetEmptyClientWidth: Integer; virtual;
    function GetFrameColor: TColor; virtual;
    function GetFrameWidth: Integer; virtual;
    function GetFullyVisible: Boolean; virtual;
    function GetHeight: Integer; virtual;
    function GetMasterRecord: TObject; virtual; abstract;  // TcxCustomGridRecord
    function GetMaxHeight: Integer; virtual; abstract;
    function GetMaxNormalHeight: Integer; virtual;
    function GetMaxWidth: Integer; virtual; abstract;
    function GetMinWidth: Integer; virtual;
    function GetNormalHeight: Integer; virtual;
    function GetWidth: Integer; virtual;

    function GetPainterClass: TcxGridDetailsSitePainterClass; virtual;
    function GetTabsViewInfoClass: TcxCustomGridDetailsSiteTabsViewInfoClass; virtual;

    function GetActiveGridView: TcxCustomGridView; virtual; abstract;
    function GetActiveGridViewExists: Boolean; virtual;
    function GetActiveGridViewValue: TcxCustomGridView; virtual;
    function GetActiveLevel: TcxGridLevel; virtual; abstract;
    function GetVisible: Boolean; virtual;

    procedure GetLevelTabDefaultParams(var AParams: TcxViewParams); virtual;
    procedure GetLevelTabParams(ALevel: TcxGridLevel; var AParams: TcxViewParams); virtual;
    procedure InitTabHitTest(AHitTest: TcxGridDetailsSiteTabHitTest); virtual; abstract;

    procedure GetCachedInfo(var AInfo: TcxCustomGridDetailsSiteViewInfoCachedInfo); virtual;
    function GetCachedInfoClass: TcxCustomGridDetailsSiteViewInfoCachedInfoClass; virtual;
    function NeedCachedTabsBounds: Boolean; virtual;
    procedure SetCachedInfo(var AInfo: TcxCustomGridDetailsSiteViewInfoCachedInfo); virtual;

    procedure CreateTabsViewInfo;

    property ActiveGridView: TcxCustomGridView read GetActiveGridViewValue;
    property ActiveGridViewExists: Boolean read GetActiveGridViewExists;
    property Canvas: TcxCanvas read GetCanvas;
    property ClientHeight: Integer read GetClientHeight;
    property ClientWidth: Integer read GetClientWidth;
    property Container: TcxControl read GetContainer;  // grid or site
    property Control: TcxControl read GetControl;  // grid
    property DesignController: TcxCustomGridDesignController read GetDesignController;
    property EmptyClientHeight: Integer read GetEmptyClientHeight;
    property EmptyClientWidth: Integer read GetEmptyClientWidth;
    property InternalHeight: Integer read GetInternalHeight;
    property InternalWidth: Integer read GetInternalWidth;
    property IsActiveGridViewDestroying: Boolean read GetIsActiveGridViewDestroying;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property MaxDetailHeight: Integer read GetMaxDetailHeight;
    property MaxDetailSize: TPoint read GetMaxDetailSize;
    property MaxDetailWidth: Integer read GetMaxDetailWidth;
    property MaxHeight: Integer read GetMaxHeight;
    property MaxNormalHeight: Integer read GetMaxNormalHeight;
    property MaxWidth: Integer read GetMaxWidth;
    property MinWidth: Integer read GetMinWidth;
    property NonClientHeight: Integer read GetNonClientHeight;
    property NonClientWidth: Integer read GetNonClientWidth;
    property TabsPosition: TcxGridDetailTabsPosition read GetTabsPosition;
  public
    Bounds: TRect;
    constructor Create(ALevel: TcxGridLevel); reintroduce; virtual;
    destructor Destroy; override;
    procedure BeforeRecalculation; virtual;
    procedure Calculate(ALeftBound, ATopBound: Integer); virtual;
    procedure ChangeActiveTab(ALevel: TcxGridLevel; AFocusView: Boolean = False); virtual; abstract;
    function DetailHasData(ALevel: TcxGridLevel): Boolean; virtual; abstract;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; virtual;
    function IsTabVisible(ALevel: TcxGridLevel): Boolean; virtual;
    procedure Paint(ACanvas: TcxCanvas);
    function ProcessDialogChar(ACharCode: Word): Boolean;
    procedure RightToLeftConversion(const ABounds: TRect);
    function SupportsTabAccelerators: Boolean; virtual;
    procedure VisibilityChanged(AVisible: Boolean); virtual;

    property ActiveLevel: TcxGridLevel read GetActiveLevel;
    property BkColor: TColor read GetBkColor;
    property ClientBounds: TRect read GetClientBounds;
    property FrameBounds: TRect read GetFrameBounds;
    property FrameColor: TColor read GetFrameColor;
    property FrameWidth: Integer read GetFrameWidth;
    property FullyVisible: Boolean read GetFullyVisible;
    property Height: Integer read GetHeight;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted write FIsRightToLeftConverted;
    property Level: TcxGridLevel read FLevel;
    property MasterRecord: TObject read GetMasterRecord;
    property NormalHeight: Integer read GetNormalHeight;
    property TabsViewInfo: TcxCustomGridDetailsSiteTabsViewInfo read FTabsViewInfo;
    property Visible: Boolean read GetVisible;
    property Width: Integer read GetWidth;
  end;

implementation

uses
  Types, SysUtils, Math, Forms, Menus, cxPCPainters, cxGrid, cxGridCustomTableView, cxGeometry;

const
  ActiveLeftTabOffset = 3;
  ActiveTopTabOffset = 2;
  TabsAreaOffset = 2;
  TabTextOffset = 5;

type
  TWinControlAccess = class(TWinControl);

{ TcxGridDetailsSiteTabHitTest }

class function TcxGridDetailsSiteTabHitTest.GetHitTestCode: Integer;
begin
  Result := htTab;
end;

{ TcxGridDetailsSiteTabsPainter }

procedure TcxGridDetailsSiteTabsPainter.Paint;
var
  P: TPoint;
begin
  Canvas.SaveState;
  try
    P := Canvas.WindowOrg;
    P := cxPointOffset(ViewInfo.GetBoundsRect.TopLeft, P, False);
    Canvas.SetClipRegion(TcxRegion.Create(ViewInfo.Bounds), roIntersect);
    Canvas.WindowOrg := cxPointInvert(P);
    ViewInfo.TabsPainter.Paint(Canvas);
  finally
    Canvas.RestoreState;
  end;
end;

function TcxGridDetailsSiteTabsPainter.GetViewInfo: TcxCustomGridDetailsSiteTabsViewInfo;
begin
  Result := TcxCustomGridDetailsSiteTabsViewInfo(inherited ViewInfo);
end;

{ TcxGridDetailsSitePainter }

constructor TcxGridDetailsSitePainter.Create(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridDetailsSiteViewInfo);
begin
  inherited Create;
  FCanvas := ACanvas;
  FViewInfo := AViewInfo;
end;

procedure TcxGridDetailsSitePainter.DrawBackground;
begin
  Canvas.Brush.Color := ViewInfo.BkColor;
  Canvas.FillRect(ViewInfo.ClientBounds);
end;

procedure TcxGridDetailsSitePainter.DrawFrame;
begin
  Canvas.FrameRect(ViewInfo.FrameBounds, ViewInfo.FrameColor, ViewInfo.FrameWidth);
end;

procedure TcxGridDetailsSitePainter.DrawTabs;
var
  AViewInfo: TcxCustomGridDetailsSiteTabsViewInfo;
begin
  AViewInfo := ViewInfo.TabsViewInfo;
  if AViewInfo <> nil then
    with AViewInfo.GetPainterClass.Create(Canvas, AViewInfo) do
      try
        MainPaint;
      finally
        Free;
      end;
end;

procedure TcxGridDetailsSitePainter.Paint;
begin
  if ViewInfo.FrameWidth <> 0 then
    DrawFrame;
  DrawTabs;
  DrawBackground;
  //Canvas.ExcludeClipRect(ViewInfo.Bounds);
end;

{ TcxGridLevelTabProperties }

constructor TcxGridLevelTabsProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  IsTabsContainer := True;
  Options := Options + [pcoFixedTabWidthWhenRotated];
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotification;
end;

destructor TcxGridLevelTabsProperties.Destroy;
begin
  FreeAndNil(FFreeNotificator);
  inherited;
end;

procedure TcxGridLevelTabsProperties.AddLevel(ALevel: TcxGridLevel);
var
  ATabIndex: Integer;
begin
  ATabIndex := Tabs.AddObject(GetLevelDisplayText(ALevel), ALevel);
  Tabs[ATabIndex].ImageIndex := ALevel.ImageIndex;
  ALevel.FreeNotification(FFreeNotificator);
end;

function TcxGridLevelTabsProperties.CanProcessChanged: Boolean;
begin
  Result := (SiteViewInfo.Level <> nil) and inherited CanProcessChanged;
end;

procedure TcxGridLevelTabsProperties.FreeNotification(AComponent: TComponent);
var
  ATabIndex: Integer;
begin
  if AComponent is TcxGridLevel then
  begin
    ATabIndex := Tabs.IndexOfObject(AComponent);
    if ATabIndex <> -1 then
      Tabs.Delete(ATabIndex);
  end;
end;

function TcxGridLevelTabsProperties.GetLevelDisplayText(ALevel: TcxGridLevel): string;

  procedure ConvertTextToDisplayText(var AText: string);
  var
    I: Integer;
  begin
    for I := Length(AText) downto 1 do
      if AText[I] = cHotkeyPrefix then
        Insert(cHotkeyPrefix, AText, I);
  end;

begin
  Result := ALevel.Caption;
  if cxGridTabAccelSupport then
    if UnderlineHotKeys then
      // nothing to do
    else
    begin
      Result := StripHotkey(Result);
      ConvertTextToDisplayText(Result);
    end
  else
    ConvertTextToDisplayText(Result);
end;

function TcxGridLevelTabsProperties.GetActiveLevel: TcxGridLevel;
begin
  if TabIndex = -1 then
    Result := nil
  else
    Result := Levels[TabIndex];
end;

function TcxGridLevelTabsProperties.GetLevelCount: Integer;
begin
  Result := Tabs.Count;
end;

function TcxGridLevelTabsProperties.GetLevel(Index: Integer): TcxGridLevel;
begin
  Result := TcxGridLevel(Tabs.Objects[Index]);
end;

function TcxGridLevelTabsProperties.GetSiteViewInfo: TcxCustomGridDetailsSiteViewInfo;
begin
  Result := TcxCustomGridDetailsSiteTabsViewInfo(GetOwner).SiteViewInfo;
end;

procedure TcxGridLevelTabsProperties.SetActiveLevel(Value: TcxGridLevel);
var
  ATabIndex: Integer;
begin
  ATabIndex := Tabs.IndexOfObject(Value);
  if ATabIndex = -1 then ATabIndex := 0;
  TabIndex := ATabIndex;
end;

procedure TcxGridLevelTabsProperties.SetUnderlineHotKeys(Value: Boolean);
var
  I: Integer;
begin
  if FUnderlineHotKeys <> Value then
  begin
    FUnderlineHotKeys := Value;
    for I := 0 to Tabs.Count - 1 do
      Tabs[I].Caption := GetLevelDisplayText(Levels[I]);
  end;
end;

{ TcxGridLevelTabsController }

function TcxGridLevelTabsController.HandleDialogChar(Key: Integer): Boolean;
begin
  Properties.FocusActiveViewOnChange := (Properties.SiteViewInfo.ActiveGridView <> nil) and
    Properties.SiteViewInfo.ActiveGridView.Focused;
  try
    Result := inherited HandleDialogChar(Key);
  finally
    Properties.FocusActiveViewOnChange := False;
  end;
end;

function TcxGridLevelTabsController.GetClientToScreen(const APoint: TPoint): TPoint;
begin
  Result := inherited GetClientToScreen(APoint);
  Result := cxPointOffset(Result, Properties.SiteViewInfo.Bounds.TopLeft);
end;

function TcxGridLevelTabsController.GetScreenToClient(const APoint: TPoint): TPoint;
begin
  Result := inherited GetScreenToClient(APoint);
  Result := cxPointOffset(Result, Properties.SiteViewInfo.Bounds.TopLeft, False);
end;

function TcxGridLevelTabsController.GetProperties: TcxGridLevelTabsProperties;
begin
  Result := TcxGridLevelTabsProperties(inherited Properties);
end;

{ TcxGridLevelTabViewInfo }

procedure TcxGridLevelTabsViewInfo.AfterPaintTab(ACanvas: TcxCanvas; ATab: TcxTab;
  AImageAndTextData: TcxPCOutTabImageAndTextData);

  function GetDesignSelectionBounds: TRect;
  begin
    Result := AImageAndTextData.TabTextRect;
    InflateRect(Result, cxDesignSelectionWidth, cxDesignSelectionWidth);
    if IsVerticalText then
      InflateRect(Result, 0, 1)
    else
      InflateRect(Result, 1, 0);
  end;

begin
  inherited;
  if IControl.IsDesigning and
    SiteViewInfo.DesignController.IsObjectSelected(Properties.Levels[ATab.Index]) then
    ACanvas.DrawDesignSelection(GetDesignSelectionBounds);
end;

function TcxGridLevelTabsViewInfo.HasBorders: Boolean;
begin
  Result := False;
end;

procedure TcxGridLevelTabsViewInfo.PrepareTabCanvasFont(ATabViewInfo: TcxTabViewInfo; ACanvas: TcxCanvas);
var
  AParams: TcxViewParams;
  AColor: TColor;
begin
  inherited PrepareTabCanvasFont(ATabViewInfo, ACanvas);
  if Properties.Levels[ATabViewInfo.Index] = nil then Exit;
  SiteViewInfo.GetLevelTabParams(Properties.Levels[ATabViewInfo.Index], AParams);
  AColor := ACanvas.Font.Color;
  ACanvas.Font := AParams.Font;
  if (GetLookAndFeel <> nil) and (GetLookAndFeel.Painter <> nil) and
    (AParams.TextColor <> GetLookAndFeel.Painter.DefaultTabTextColor) then
    ACanvas.Font.Color := AParams.TextColor
  else
    ACanvas.Font.Color := AColor;
end;

function TcxGridLevelTabsViewInfo.GetController: TcxGridLevelTabsController;
begin
  Result := TcxGridLevelTabsController(inherited Controller);
end;

function TcxGridLevelTabsViewInfo.GetProperties: TcxGridLevelTabsProperties;
begin
  Result := TcxGridLevelTabsProperties(inherited Properties);
end;

function TcxGridLevelTabsViewInfo.GetSiteViewInfo: TcxCustomGridDetailsSiteViewInfo;
begin
  Result := TcxCustomGridDetailsSiteTabsViewInfo(Owner).SiteViewInfo;
end;

{ TcxCustomGridDetailsSiteTabsViewInfo }

constructor TcxCustomGridDetailsSiteTabsViewInfo.Create(ASiteViewInfo: TcxCustomGridDetailsSiteViewInfo);
const
  cxDefaultTabsBounds: TRect = (Left: 0; Top: 0; Right: 289; Bottom: 193);// from TcxCustomTabControl
begin
  inherited Create;
  FSiteViewInfo := ASiteViewInfo;

  FTabsProperties := TcxGridLevelTabsProperties.Create(Self);
  InitTabs;
  FTabsController := TcxGridLevelTabsController.Create(Self);
  FTabsViewInfo := TcxGridLevelTabsViewInfo.Create(Self);
  FTabsPainter := TabsViewInfo.GetPainterClass.Create(TabsViewInfo);
  Bounds := cxDefaultTabsBounds;
end;

destructor TcxCustomGridDetailsSiteTabsViewInfo.Destroy;
begin
  cxClearObjectLinks(Self);
  FreeAndNil(FTabsPainter);
  FreeAndNil(FTabsViewInfo);
  FreeAndNil(FTabsController);
  FreeAndNil(FTabsProperties);
  inherited Destroy;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.TabsPropertiesChangeHandler(Sender: TObject);
begin
  FNeedActiveLevelChanged := True;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetActiveLevel: TcxGridLevel;
begin
  Result := FSiteViewInfo.ActiveLevel;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetLevel: TcxGridLevel;
begin
  Result := FSiteViewInfo.Level;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.DoInvalidate;
begin
  SiteViewInfo.Container.InvalidateRect(Bounds, False);
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetCanvas: TcxCanvas;
begin
  Result := SiteViewInfo.Canvas;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetControl: TcxControl;
begin
  Result := SiteViewInfo.Control;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridDetailsSiteTabHitTest;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
var
  ATabIndex: Integer;
  P: TPoint;
begin
  inherited InitHitTest(AHitTest);
  SiteViewInfo.InitTabHitTest(TcxGridDetailsSiteTabHitTest(AHitTest));
  P := GetTabsPointFromHitTest(AHitTest);
  ATabIndex := TabsViewInfo.IndexOfTabAt(P.X, P.Y);
  if ATabIndex <> -1 then
    TcxGridDetailsSiteTabHitTest(AHitTest).Level := TabsProperties.Levels[ATabIndex]
  else
    TcxGridDetailsSiteTabHitTest(AHitTest).Level := TabsProperties.ActiveLevel;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetHeight: Integer;
begin
  Result := CalculateHeight;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetWidth: Integer;
begin
  Result := CalculateWidth;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetHotTrack: Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.MouseLeave;
begin
  inherited;
  TabsController.MouseLeave;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.PtInCaller(const P: TPoint): Boolean;

  function GetControlClientPoint: TPoint;
  begin
    Result := dxMapWindowPoint(SiteViewInfo.Control.Handle, SiteViewInfo.Container.Handle, P);
  end;

begin
  if SiteViewInfo.Control <> SiteViewInfo.Container then
    Result := PtInRect(Bounds, GetControlClientPoint)
  else
    Result := inherited PtInCaller(P);
end;

function TcxCustomGridDetailsSiteTabsViewInfo.CalculateHeight: Integer;
begin
  if IsVertical then
    Result := TabsViewInfo.GetOptimalSize
  else
    Result := TabsViewInfo.PageClientRect.Top;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.CalculateWidth: Integer;
begin
  if IsVertical then
    if UseRightToLeftAlignment then
      Result := TabsViewInfo.PageClientRectOffset.Right
    else
      Result := TabsViewInfo.PageClientRect.Left
  else
    Result := TabsViewInfo.GetOptimalSize;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.CheckActiveLevel;
var
  ALink: TcxObjectLink;
begin
  if FNeedActiveLevelChanged then
  begin
    FNeedActiveLevelChanged := False;
    if Control.IsDesigning then
      TcxCustomGrid(Control).StructureNavigator.SelectObject(TabsProperties.ActiveLevel, True);

    ALink := cxAddObjectLink(Self);
    try
      SiteViewInfo.ChangeActiveTab(TabsProperties.ActiveLevel, TabsProperties.FocusActiveViewOnChange);
    finally
      if ALink.Ref <> nil then
        SynchronizeActiveLevel;
      cxRemoveObjectLink(ALink);
    end;
  end;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetBkColor: TColor;
var
  AParams: TcxViewParams;
begin
  Level.Styles.GetViewParams(lsTabsBackground, nil, nil, AParams);
  Result := AParams.Color;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetFirstTabVisibleIndex: Integer;
begin
  Result := TabsViewInfo.FirstTabVisibleIndex;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.SetFirstTabVisibleIndex(Value: Integer);
begin
  TabsViewInfo.FirstTabVisibleIndex := Value;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetIsVertical: Boolean;
begin
  Result := GetTabPosition = tpLeft;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridDetailsSiteTabsPainter;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.AreTabsRotated: Boolean;
begin
  Result := IsVertical;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetTabPosition: TcxTabPosition;
const
  ResultMap: array[TcxGridDetailTabsPosition] of TcxTabPosition = (tpTop, tpLeft, tpTop);
begin
  Result := ResultMap[SiteViewInfo.TabsPosition];
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.InitTabs;
begin
  TabsProperties.BeginUpdate;
  try
    InitTabsHotKeyParams;
    InitTabsData;
    InitTabsLayout;
    InitTabsStyle;
    TabsProperties.OnChange := TabsPropertiesChangeHandler;
  finally
    TabsProperties.CancelUpdate;
  end;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.InitTabsData;
var
  I: Integer;
  ALevel: TcxGridLevel;
begin
  for I := 0 to Level.VisibleCount - 1 do
  begin
    ALevel := Level.VisibleItems[I];
    if SiteViewInfo.IsTabVisible(ALevel) then
      TabsProperties.AddLevel(ALevel);
  end;
  SynchronizeActiveLevel;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.InitTabsHotKeyParams;
begin
  TabsProperties.UnderlineHotKeys := SiteViewInfo.SupportsTabAccelerators;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.InitTabsLayout;
begin
  TabsProperties.Rotate := AreTabsRotated;
  TabsProperties.TabPosition := GetTabPosition;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.InitTabsStyle;
var
  ATabsInfo: TcxGridLevelTabs;
begin
  ATabsInfo := TcxCustomGrid(SiteViewInfo.Control).LevelTabs;
  TabsProperties.ImageBorder := ATabsInfo.ImageBorder;
  TabsProperties.Images := ATabsInfo.GetImages;
  TabsProperties.TabCaptionAlignment := ATabsInfo.CaptionAlignment;
  TabsProperties.Style := ATabsInfo.Style;
  TabsProperties.TabSlants := ATabsInfo.Slants;
  InitTabsViewParams;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.InitTabsViewParams;
var
  ATabDefaultParams, ATabParams: TcxViewParams;
  I: Integer;
begin
  SiteViewInfo.GetLevelTabDefaultParams(ATabDefaultParams);
  for I := 0 to TabsProperties.Tabs.Count - 1 do
  begin
    SiteViewInfo.GetLevelTabParams(TabsProperties.Levels[I], ATabParams);
    if ATabParams.Color <> ATabDefaultParams.Color then
      TabsProperties.Tabs[I].Color := ATabParams.Color;
  end;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.SynchronizeActiveLevel;
begin
  TabsProperties.BeginUpdate;
  try
    TabsProperties.ActiveLevel := ActiveLevel;
  finally
    TabsProperties.CancelUpdate;
  end;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetTabsPointFromHitTest(
  AHitTest: TcxCustomGridHitTest): TPoint;
begin
  Result := cxPointOffset(AHitTest.Pos, FIcxTabControlBounds.TopLeft, False);
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.AdjustBounds;
begin
end;

function TcxCustomGridDetailsSiteTabsViewInfo.IsDesigning: Boolean;
begin
  Result := SiteViewInfo.Control.IsDesigning;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.IsDestroying: Boolean;
begin
  Result := inherited IsDestroying or SiteViewInfo.Control.IsDestroying;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.IsLoading: Boolean;
begin
  Result := SiteViewInfo.Control.IsLoading;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetController: TcxCustomTabControlController;
begin
  Result := TabsController;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetPainter: TcxPCCustomPainter;
begin
  Result := TabsPainter;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetProperties: TcxCustomTabControlProperties;
begin
  Result := TabsProperties;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetViewInfo: TcxCustomTabControlViewInfo;
begin
  Result := TabsViewInfo;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.CanDrawParentBackground: Boolean;
begin
  Result := True;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetBoundsRect: TRect;
begin
  Result := FIcxTabControlBounds;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetTabCanvas: TcxCanvas;
begin
  Result := GetCanvas;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetTabControl: TWinControl;
begin
  Result := SiteViewInfo.Container;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetColor: TColor;
begin
  Result := BkColor;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetDragAndDropObject: TcxDragAndDropObject;
begin
  Result := nil;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetDragAndDropState: TcxDragAndDropState;
begin
  Result := ddsNone;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetFont: TFont;
begin
  Result := TWinControlAccess(SiteViewInfo.Control).Font;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := SiteViewInfo.LookAndFeel;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.InvalidateRect(const R: TRect; AEraseBackground: Boolean);
begin
  SiteViewInfo.Container.InvalidateRect(cxRectOffset(R, Bounds.TopLeft), AEraseBackground);
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.SetModified;
begin
end;

function TcxCustomGridDetailsSiteTabsViewInfo.IsEnabled: Boolean;
begin
  Result := SiteViewInfo.Control.Enabled;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.IsFocused: Boolean;
begin
  Result := SiteViewInfo.Control.Focused;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.IsParentBackground: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.RequestLayout;
begin
  TabsViewInfo.Calculate;
  Invalidate;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.BeforeRecalculation;
begin
  inherited;
  InitTabsHotKeyParams;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.Calculate(const ABounds: TRect);
begin
  inherited;
  FIcxTabControlBounds := ABounds;
  TabsViewInfo.Calculate;
  AdjustBounds;
end;

procedure TcxCustomGridDetailsSiteTabsViewInfo.GetClientBounds(var AClientBounds: TRect);
begin
  if not Calculated then
  begin
    if cxRectIsEmpty(AClientBounds) then
      Calculate(Bounds)
    else
      Calculate(AClientBounds);
  end;
  if GetIsVertical then
  begin
    if UseRightToLeftAlignment then
      Dec(AClientBounds.Right, Width)
    else
      Inc(AClientBounds.Left, Width);
  end
  else
    Inc(AClientBounds.Top, Height);
end;

function TcxCustomGridDetailsSiteTabsViewInfo.ProcessDialogChar(ACharCode: Word): Boolean;
begin
  Result := TabsController.HandleDialogChar(ACharCode);
  CheckActiveLevel;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
var
  P: TPoint;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  P := GetTabsPointFromHitTest(AHitTest);
  TabsController.MouseDown(AButton, AShift, P.X, P.Y);
  if not TabsViewInfo.Painter.IsTabPressable then
    CheckActiveLevel;
end;

function TcxCustomGridDetailsSiteTabsViewInfo.MouseMove(AHitTest: TcxCustomGridHitTest;
  AShift: TShiftState): Boolean;
var
  P: TPoint;
begin
  Result := inherited MouseMove(AHitTest, AShift);
  P := GetTabsPointFromHitTest(AHitTest);
  TabsController.MouseMove(AShift, P.X, P.Y);
end;

function TcxCustomGridDetailsSiteTabsViewInfo.MouseUp(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
  AShift: TShiftState): Boolean;
var
  P: TPoint;
begin
  Result := inherited MouseUp(AHitTest, AButton, AShift);
  P := GetTabsPointFromHitTest(AHitTest);
  TabsController.MouseUp(AButton, AShift, P.X, P.Y);
  if TabsViewInfo.Painter.IsTabPressable then
    CheckActiveLevel;
end;

{ TcxGridDetailsSiteLeftTabsViewInfo }

procedure TcxGridDetailsSiteLeftTabsViewInfo.AdjustBounds;
begin
  inherited;
  if UseRightToLeftAlignment then
    Bounds.Left := Max(Bounds.Right - Width, Bounds.Left)
  else
    Bounds.Right := Min(Bounds.Left + Width, Bounds.Right);
end;

{ TcxGridDetailsSiteTopTabsViewInfo }

procedure TcxGridDetailsSiteTopTabsViewInfo.AdjustBounds;
begin
  inherited;
  Bounds.Bottom := Min(Bounds.Top + Height, Bounds.Bottom);
end;

{ TcxCustomGridDetailsSiteViewInfo }

constructor TcxCustomGridDetailsSiteViewInfo.Create(ALevel: TcxGridLevel);
begin
  inherited Create(nil);
  FLevel := ALevel;
  FLevel.FreeNotification(self);

  CreateTabsViewInfo;
end;

destructor TcxCustomGridDetailsSiteViewInfo.Destroy;
begin
  VisibilityChanged(False);
  FreeAndNil(FTabsViewInfo);
  inherited;
end;

function TcxCustomGridDetailsSiteViewInfo.GetClientHeight: Integer;
begin
  with ClientBounds do
    Result := Bottom - Top;
end;

function TcxCustomGridDetailsSiteViewInfo.GetClientWidth: Integer;
begin
  with ClientBounds do
    Result := Right - Left;
end;

function TcxCustomGridDetailsSiteViewInfo.GetControl: TcxControl;
begin
  if FLevel = nil then
    Result := nil
  else
    Result := FLevel.Control;
end;

function TcxCustomGridDetailsSiteViewInfo.GetFrameBounds: TRect;
begin
  Result := ClientBounds;
  InflateRect(Result, FrameWidth, FrameWidth);
end;

function TcxCustomGridDetailsSiteViewInfo.GetInternalHeight: Integer;
begin
  with Bounds do
    Result := Bottom - Top;
end;

function TcxCustomGridDetailsSiteViewInfo.GetInternalWidth: Integer;
begin
  with Bounds do
    Result := Right - Left;
end;

function TcxCustomGridDetailsSiteViewInfo.GetIsActiveGridViewDestroying: Boolean;
begin
  Result := (FActiveGridView <> nil) and FActiveGridView.IsDestroying;
end;

function TcxCustomGridDetailsSiteViewInfo.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TcxCustomGrid(Control).LookAndFeel;
end;

function TcxCustomGridDetailsSiteViewInfo.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := LookAndFeel.Painter;
end;

function TcxCustomGridDetailsSiteViewInfo.GetMaxDetailHeight: Integer;
begin
  Result := MaxHeight - NonClientHeight;
end;

function TcxCustomGridDetailsSiteViewInfo.GetMaxDetailSize: TPoint;
begin
  Result := Point(MaxDetailWidth, MaxDetailHeight);
end;

function TcxCustomGridDetailsSiteViewInfo.GetMaxDetailWidth: Integer;
begin
  Result := MaxWidth - NonClientWidth;
end;

function TcxCustomGridDetailsSiteViewInfo.GetNonClientHeight: Integer;
begin
  Result := InternalHeight - ClientHeight;
end;

function TcxCustomGridDetailsSiteViewInfo.GetNonClientWidth: Integer;
begin
  Result := InternalWidth - ClientWidth;
end;

function TcxCustomGridDetailsSiteViewInfo.GetTabsPosition: TcxGridDetailTabsPosition;
begin
  Result := Level.Options.DetailTabsPosition;
end;

procedure TcxCustomGridDetailsSiteViewInfo.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FActiveGridView) then
    FActiveGridView := nil;

  if (AOperation = opRemove) and (AComponent = FLevel) then
    FLevel := nil;
end;

function TcxCustomGridDetailsSiteViewInfo.CalculateHeight: Integer;
begin
  if ActiveGridView = nil then
  begin
    FFullyVisible := True;
    Result := EmptyClientHeight;
  end
  else
  begin
    Result := 0;
    FFullyVisible := False;
    ActiveGridView.ViewInfo.GetHeight(MaxDetailSize, Result, FFullyVisible);
  end;
  Inc(Result, NonClientHeight);
  if FTabsViewInfo <> nil then
    Result := Max(Result, FTabsViewInfo.Height);
  FNormalHeight := Result;
  if FNormalHeight > MaxNormalHeight then FNormalHeight := MaxNormalHeight;
  if not FullyVisible then
    Result := MaxHeight
  else
    Result := Max(Min(Result, MaxHeight), 0);
  if FNormalHeight < Result then
    FNormalHeight := Result;
end;

function TcxCustomGridDetailsSiteViewInfo.CalculateWidth: Integer;
begin
  if ActiveGridView = nil then
    Result := EmptyClientWidth
  else
    ActiveGridView.ViewInfo.GetWidth(MaxDetailSize, Result);
  Inc(Result, NonClientWidth);
  if FTabsViewInfo <> nil then
    Result := Max(Result, FTabsViewInfo.Width);
  Result := Min(Max(Result, MinWidth), MaxWidth);
end;

function TcxCustomGridDetailsSiteViewInfo.GetBkColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultGridDetailsSiteColor;
end;

function TcxCustomGridDetailsSiteViewInfo.GetClientBounds: TRect;
begin
  Result := Bounds;
  if FTabsViewInfo <> nil then
    FTabsViewInfo.GetClientBounds(Result);
  InflateRect(Result, -FrameWidth, -FrameWidth);
end;

function TcxCustomGridDetailsSiteViewInfo.GetEmptyClientHeight: Integer;
begin
  Result := 100;
end;

function TcxCustomGridDetailsSiteViewInfo.GetEmptyClientWidth: Integer;
begin
  Result := 200;
end;

function TcxCustomGridDetailsSiteViewInfo.GetFrameColor: TColor;
begin
  Result := FLevel.Options.GetDetailFrameColor;
end;

function TcxCustomGridDetailsSiteViewInfo.GetFrameWidth: Integer;
begin
  Result := Level.Options.DetailFrameWidth;
end;

function TcxCustomGridDetailsSiteViewInfo.GetFullyVisible: Boolean;
begin
  Result := FFullyVisible;
end;

function TcxCustomGridDetailsSiteViewInfo.GetHeight: Integer;
begin
  Result := CalculateHeight;
end;

function TcxCustomGridDetailsSiteViewInfo.GetMaxNormalHeight: Integer;
begin
  Result := cxMaxRectSize;
end;

function TcxCustomGridDetailsSiteViewInfo.GetMinWidth: Integer;
begin
  Result := MaxWidth;
end;

function TcxCustomGridDetailsSiteViewInfo.GetNormalHeight: Integer;
begin
  Result := FNormalHeight;
end;

function TcxCustomGridDetailsSiteViewInfo.GetWidth: Integer;
begin
  Result := CalculateWidth;
end;

function TcxCustomGridDetailsSiteViewInfo.GetPainterClass: TcxGridDetailsSitePainterClass;
begin
  Result := TcxGridDetailsSitePainter;
end;

function TcxCustomGridDetailsSiteViewInfo.GetTabsViewInfoClass: TcxCustomGridDetailsSiteTabsViewInfoClass;
begin
  if TabsPosition = dtpLeft then
    Result := TcxGridDetailsSiteLeftTabsViewInfo
  else
    Result := TcxGridDetailsSiteTopTabsViewInfo;
end;

function TcxCustomGridDetailsSiteViewInfo.GetActiveGridViewExists: Boolean;
begin
  Result := (FActiveGridView <> nil) and not IsActiveGridViewDestroying;
end;

function TcxCustomGridDetailsSiteViewInfo.GetActiveGridViewValue: TcxCustomGridView;
begin
  if FActiveGridView = nil then
  begin
    FActiveGridView := GetActiveGridView;
    if FActiveGridView <> nil then
      FActiveGridView.FreeNotification(Self);
  end;
  Result := FActiveGridView;
  if IsActiveGridViewDestroying then
    Result := nil;
end;

function TcxCustomGridDetailsSiteViewInfo.GetVisible: Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridDetailsSiteViewInfo.GetLevelTabDefaultParams(var AParams: TcxViewParams);
begin
  Level.Styles.GetTabDefaultParams(AParams);
end;

procedure TcxCustomGridDetailsSiteViewInfo.GetLevelTabParams(ALevel: TcxGridLevel;
  var AParams: TcxViewParams);
begin
  Level.Styles.GetTabParams(TcxCustomGridRecord(MasterRecord), ALevel, AParams);
end;

procedure TcxCustomGridDetailsSiteViewInfo.GetCachedInfo(var AInfo: TcxCustomGridDetailsSiteViewInfoCachedInfo);
begin
  AInfo := GetCachedInfoClass.Create;
  if TabsViewInfo <> nil then
  begin
    AInfo.FirstTabVisibleIndex := TabsViewInfo.FirstTabVisibleIndex;
    if NeedCachedTabsBounds then
    begin
      AInfo.Bounds := TabsViewInfo.Bounds;
      AInfo.AssignedBounds := True;
    end;
  end;
end;

function TcxCustomGridDetailsSiteViewInfo.GetCachedInfoClass: TcxCustomGridDetailsSiteViewInfoCachedInfoClass;
begin
  Result := TcxCustomGridDetailsSiteViewInfoCachedInfo;
end;

function TcxCustomGridDetailsSiteViewInfo.NeedCachedTabsBounds: Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridDetailsSiteViewInfo.SetCachedInfo(var AInfo: TcxCustomGridDetailsSiteViewInfoCachedInfo);
begin
  if TabsViewInfo <> nil then
  begin
    if AInfo.AssignedBounds then
      TabsViewInfo.Bounds := AInfo.Bounds;
    TabsViewInfo.FirstTabVisibleIndex := AInfo.FirstTabVisibleIndex;
  end;
  FreeAndNil(AInfo);
end;

procedure TcxCustomGridDetailsSiteViewInfo.CreateTabsViewInfo;
begin
  if TabsPosition <> dtpNone then
    FTabsViewInfo := GetTabsViewInfoClass.Create(Self);
end;

procedure TcxCustomGridDetailsSiteViewInfo.BeforeRecalculation;
begin
  if FTabsViewInfo <> nil then
    FTabsViewInfo.BeforeRecalculation;
end;

procedure TcxCustomGridDetailsSiteViewInfo.Calculate(ALeftBound, ATopBound: Integer);
begin
  IsRightToLeftConverted := False;
  Bounds.Left := ALeftBound;
  Bounds.Top := ATopBound;
  Bounds.Right := Bounds.Left + Width;
  Bounds.Bottom := Bounds.Top + Height;

  if FTabsViewInfo <> nil then
    FTabsViewInfo.Calculate(Bounds);

  if ActiveGridView <> nil then
    ActiveGridView.ViewInfo.MainCalculate(ClientBounds);
end;

procedure TcxCustomGridDetailsSiteViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  Bounds := TdxRightToLeftLayoutConverter.ConvertRect(Bounds, ABounds);
  if FTabsViewInfo <> nil then
    FTabsViewInfo.Calculate(Bounds);
  if ActiveGridView <> nil then
    ActiveGridView.ViewInfo.RightToLeftConversion(ClientBounds);
end;

function TcxCustomGridDetailsSiteViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;

  function GetDetailP: TPoint;
  begin
    with ActiveGridView.Site do
    begin
      Result.X := P.X - Left;
      Result.Y := P.Y - Top;
    end;
  end;

begin
  if FTabsViewInfo <> nil then
    Result := FTabsViewInfo.GetHitTest(P)
  else
    Result := nil;
  if (Result = nil) and (ActiveGridView <> nil) then
    Result := ActiveGridView.ViewInfo.GetHitTest(GetDetailP);
end;

function TcxCustomGridDetailsSiteViewInfo.IsTabVisible(ALevel: TcxGridLevel): Boolean;
begin
  Result := Level.Options.TabsForEmptyDetails or DetailHasData(ALevel);
end;

procedure TcxCustomGridDetailsSiteViewInfo.Paint(ACanvas: TcxCanvas);
begin
  with GetPainterClass.Create(ACanvas, Self) do
    try
      Paint;
    finally
      Free;
    end;
end;

function TcxCustomGridDetailsSiteViewInfo.ProcessDialogChar(ACharCode: Word): Boolean;
begin
  Result := (FTabsViewInfo <> nil) and FTabsViewInfo.ProcessDialogChar(ACharCode);
end;

procedure TcxCustomGridDetailsSiteViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not IsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    IsRightToLeftConverted := True;
  end;
end;

function TcxCustomGridDetailsSiteViewInfo.SupportsTabAccelerators: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridDetailsSiteViewInfo.VisibilityChanged(AVisible: Boolean);
begin
  if ActiveGridViewExists then
    ActiveGridView.ViewInfo.DoVisibilityChanged(AVisible);
end;

end.
