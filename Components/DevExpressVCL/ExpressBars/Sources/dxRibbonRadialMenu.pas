{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxRibbonRadialMenu;

interface

uses
  Windows, Forms, Messages, Classes, SysUtils, Graphics, Controls, ExtCtrls, ImgList, IniFiles, Contnrs, Math, Types,
  dxCore, dxCoreClasses, dxCoreGraphics, cxClasses, cxGraphics, cxControls, cxContainer, cxLookAndFeels, dxGDIPlusApi,
  dxGDIPlusClasses, cxGeometry, dxBar, dxBarSkinConsts, dxRibbon, dxAnimation, dxBarStrs, cxAccessibility,
  dxBarAccessibility;

type
  TdxRadialMenuState = (rmsClosed, rmsCollapsed, rmsExpanded);
  TdxRadialMenuMode = (rmmAutoHide, rmmAutoCollapse);

  { TdxRadialMenuDropDownList }

  TdxRadialMenuDropDownList = class(TcxDoublyLinkedDataList)
  public
    function GetLinkedObjectClass: TcxDoublyLinkedObjectClass; override;
  end;

  { TdxRadialMenuDropDownData }

  TdxRadialMenuDropDownData = class(TcxDoublyLinkedData)
  private
    FStartIndex: Integer;
  end;

  { TdxRibbonCustomRadialMenu }

  TdxRibbonCustomRadialMenu = class(TdxBarCustomPopupMenuComponent)
  strict private
    FGlyph: TdxSmartGlyph;
    FInnerSize: Integer;
    FMenuSize: Integer;
    FRibbonHolder: TcxComponentHolder;
    FSectorCount: Integer;

    FOnCloseUp: TNotifyEvent;
    FOnCollapse: TNotifyEvent;
    FOnExpand: TNotifyEvent;
    FOnPopup: TNotifyEvent;

    procedure InternalShowRadialMenu(X, Y: Integer; AMode: TdxRadialMenuMode);

    function GetRibbon: TdxCustomRibbon;
    procedure SetGlyph(AGlyph: TdxSmartGlyph);
    procedure SetRibbon(AValue: TdxCustomRibbon);
  protected
    procedure ChangeScaleCore(M, D: Integer); override;
    function CreateBarControl: TCustomdxBarControl; override;
    function GetControlClass: TCustomdxBarControlClass; override;
    function GetItemLinksClass: TdxBarItemLinksClass; override;

    property MenuSize: Integer read FMenuSize write FMenuSize default 0;
    property Ribbon: TdxCustomRibbon read GetRibbon write SetRibbon;

    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnCollapse: TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Popup(X, Y: Integer); override;
    procedure Show(X, Y: Integer);
    procedure Hide;

    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
  end;

  { TdxRibbonRadialMenu }

  TdxRibbonRadialMenu = class(TdxRibbonCustomRadialMenu)
  published
    property Glyph;
    property ItemLinks;
    property Images;
    property Ribbon;

    property Font;
    property UseOwnFont;

    property OnCloseUp;
    property OnCollapse;
    property OnExpand;
    property OnPopup;
  end;

  { TdxRibbonRadialMenuHitTest }

  TdxRibbonRadialMenuHitTest = record
    SectorIndex: Integer;
    Item: TdxBarItem;
    HitTestCode: Integer;
  end;

  { TdxRibbonRadialMenuControl }

  TdxRibbonRadialMenuControl = class(TCustomdxBarControl)
  private
    FRadialMenu: TdxRibbonCustomRadialMenu;

    FBackArrowImage: TdxSmartGlyph;
    FColorPalette: IdxColorPalette;
    FDropDownsList: TdxRadialMenuDropDownList;
    FExpandState: TdxRadialMenuState;
    FHitTest: TdxRibbonRadialMenuHitTest;
    FKeyTipsWaiting: Boolean;
    FMenuMode: TdxRadialMenuMode;

    //internal lists
    FInternalVisibleLinks: TList;
    FInternalItems: TObjectList;
    FInternalItemLinksOwner: TdxBarInternalLinksOwner;

    //colors
    FBaseAccentColor: TColor;
    FBaseBackgroundColor: TColor;
    FItemRingDefaultColor: TColor;
    FItemTextColor: TColor;
    FDropDownItemRingNormalColor: TColor;
    FDropDownItemRingHotColor: TColor;
    FBackArrowColor: TColor;

    //settings
    FMenuSize: Integer;
    FInnerSize: Integer;
    FSectorCount: Integer;

    //sizes
    FMenuCenter: TPoint;
    FCoreRingRect: TRect;
    FFullRingRect: TRect;
    FItemContentRingRect: TRect;
    FSmallRingRect: TRect;
    FThickRingRect: TRect;

    FShowAnimation: TdxAnimationTransition;
    FHideAnimation: TdxAnimationTransition;
    FAnimationStep: Integer;

    //HitTest
    procedure SetHotItem(const P: TPoint); overload;
    procedure SetHotItem(ASectorIndex: Integer; AInEdge: Boolean = False); overload;

    //get/set
    function GetClickableItem(AItem: TdxBarItem): TdxBarItem;
    function GetCoreItemLink: TdxBarItemLink;
    function GetCurrentCoreImage: TdxSmartImage;
    function GetCurrentItemLinks: TdxBarItemLinks;
    function GetCurrentItemLinksStartIndex: Integer;
    function GetItemSector(ASectorIndex: Integer): TdxSector;
    function GetVisibleItem(ASectorIndex: Integer): TdxBarItem;
    function GetVisibleItemCount(AItemLinks: TdxBarItemLinks): Integer;
    procedure SetCurrentItemLinks(AValue: TdxBarItemLinks; AStartIndex: Integer);
    procedure SetExpandState(Value: TdxRadialMenuState);

    // internal logic
    procedure ActivateRootLevel;
    procedure InternalHide;
    function InternalClick(AItem: TdxBarItem): Boolean;
    procedure InternalCoreClick;
    function InternalCloseLastLevel: Boolean;
    function InternalPopup(ASectorIndex: Integer): Boolean;
    function HandleNavigationKey(var AKey: Word): Boolean;
    procedure SynchronizeInternalItems;

    //animation
    function IsAnimationInProcess: Boolean;
    procedure ShowAnimationStep(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
    procedure HideAnimationStep(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
    procedure ShowAnimationTerminated(Sender: TObject);
    procedure HideAnimationTerminated(Sender: TObject);

    function GetRealState: TdxRadialMenuState;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;

    property CoreItemLink: TdxBarItemLink read GetCoreItemLink;
  protected
    //Controls
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;

    //BarControl
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetViewInfoClass: TCustomdxBarControlViewInfoClass; override;
    procedure SetItemLinks(Value: TdxBarItemLinks); override;
    procedure DoCreateControls; override;
    procedure SetFont; override;
    procedure ViewStateChanged(APrevValue: TdxBarViewState); override;
    procedure DoHideAll(AReason: TdxBarCloseUpReason); override;
    procedure NavigationHandler(var ACharCode: Word; AShiftState: TShiftState); override;
    function NeedsKey(AKey: Word; AShift: TShiftState): Boolean; override;

    //user actions
    procedure DoKeyDown(var Message: TWMKey); override;
    procedure DoLButtonUp(var Message: TWMLButtonUp); override;
    procedure DoBarMouseDown(Button: TMouseButton; Shift: TShiftState;
      const APoint: TPoint; AItemControl: TdxBarItemControl; APointInClientRect: Boolean); override;
    procedure DoBarMouseMove(Shift: TShiftState; const APoint: TPoint; AItemControl: TdxBarItemControl); override;

    procedure CalculateRects;

    //drawing
    procedure InitializeColors(ASkin: IdxSkin);
    procedure DrawRadialItemBackGround(ACanvas: TcxCanvas; AGPGraphics: TdxGPGraphics; AItem: TdxBarItem; ASector: TdxSector);
    procedure DrawRadialItemContent(ACanvas: TcxCanvas; AGPGraphics: TdxGPGraphics; AItem: TdxBarItem; ARect: TRect);
    procedure DrawRadialItems(ACanvas: TcxCanvas; AGPGraphics: TdxGPGraphics);
    procedure DrawRadialMenu(ACanvas: TcxCanvas);
    procedure DrawRadialCore(ACanvas: TcxCanvas);
    procedure UpdateLayer;

    property ExpandState: TdxRadialMenuState read FExpandState write SetExpandState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Show; override;
    procedure Hide; override;
  end;

  { TdxRibbonRadialMenuControlViewInfo }

  TdxRibbonRadialMenuControlViewInfo = class(TCustomdxBarControlViewInfo)
  public
    constructor Create(ABarControl: TCustomdxBarControl); override;
    destructor Destroy; override;
  end;

  { TdxRibbonRadialMenuAccessibilityHelper }

  TdxRibbonRadialMenuAccessibilityHelper = class(TCustomdxBarControlAccessibilityHelper)
  private
    FKeyTipWindowsManager: IdxBarKeyTipWindowsManager;

    function GetBarControl: TdxRibbonRadialMenuControl;
    procedure ActivateKeyTips(AHelper: IdxBarAccessibilityHelper);
  protected
    // IdxBarAccessibilityHelper
    function AreKeyTipsSupported(out AKeyTipWindowsManager: IdxBarKeyTipWindowsManager): Boolean; override;
    function GetDefaultAccessibleObject: IdxBarAccessibilityHelper; override;
    procedure InitializeItemKeyTipPosition(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; var AKeyTipInfo: TdxBarKeyTipInfo); override;

    function GetChildSectorIndex(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper): Integer; overload;
    function GetChildSectorIndex(AChildIndex: Integer): Integer; overload;
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;

    procedure DoClickItem(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper); override;
    procedure DoDropDownItem(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper); override;

    function HandleNavigationKey(var AKey: Word): Boolean; override;

    property BarControl: TdxRibbonRadialMenuControl read GetBarControl;
  public
    constructor Create(AOwnerObject: TObject); override;
    destructor Destroy; override;
  end;

  { TdxRibbonRadialMenuKeyTipWindows }

  TdxRibbonRadialMenuKeyTipWindows = class(TdxRibbonCustomKeyTipWindows);

implementation

uses
  dxShadowWindow, dxDPIAwareUtils;

type
  TdxBarItemAccess = class(TdxBarItem);
  TdxBarItemLinkAccess = class(TdxBarItemLink);
  TdxBarItemLinksAccess = class(TdxBarItemLinks);
  TCustomdxBarControlAccess = class(TCustomdxBarControl);
  TdxCustomRibbonAccess = class(TdxCustomRibbon);

const
//sizes
  DefaultMenuSize = 260;
  ThickRingSize = 21;
  ItemImageSize = 24;
  SmallRingOffset = 5;

//indexes
  htNone = -2;
  htCore = -1;
  htItem = 0;
  htDropDown = 1;

  AnimationTurn1 = 90;
  AnimationTurn2 = 135;
  AnimationTurn3 = 180;
  AnimationShowCycle = 180;
  AnimationHideCycle = 130;
  AnimationShowTime = 650;
  AnimationHideTime = 500;

function GetLinksOwner(AObject: TObject): IdxBarLinksOwner;
begin
  Supports(AObject, IdxBarLinksOwner, Result);
end;

function GetOwnedLinks(AObject: TObject): TdxBarItemLinks;
begin
  if GetLinksOwner(AObject) <> nil then
    Result := GetLinksOwner(AObject).GetItemLinks
  else
    Result := nil;
end;

function GetSubmenuOwner(AItemLinks: TdxBarItemLinks): IdxBarSubMenuOwner;
begin
  Supports(AItemLinks.Owner, IdxBarSubMenuOwner, Result);
end;

function GetSkin(AObject: TObject): IdxSkin;
begin
  Supports(AObject, IdxSkin, Result);
end;

function GetVisibleItemCount(AItemLinks: TdxBarItemLinks; ASectorCount: Integer): Integer;
begin
  Result := Min(ASectorCount, AItemLinks.VisibleItemCount);
end;

{ TdxRadialMenuDropDownList }

function TdxRadialMenuDropDownList.GetLinkedObjectClass: TcxDoublyLinkedObjectClass;
begin
  Result := TdxRadialMenuDropDownData;
end;

function dxRectI(ALeft, ATop, ARight, ABottom: Single): TRect;
begin
  Result := cxRect(Round(ALeft), Round(ATop), Round(ARight), Round(ABottom));
end;

function ExpandRectFromCenter(ARectCenter: TdxPointF; ARectSize: TdxSizeF): TRect;
var
  AHalfSize: TdxSizeF;
begin
  AHalfSize := dxSizeF(ARectSize.cx / 2, ARectSize.cy / 2);
  Result := dxRectI(ARectCenter.X - AHalfSize.cx, ARectCenter.Y - AHalfSize.cy,
    ARectCenter.X + ARectSize.cx - AHalfSize.cx, ARectCenter.Y + ARectSize.cy - AHalfSize.cy);
end;

function dxCreateRoundRectRgn(const R: TRect; const ARoundSize: TSize): HRGN;
begin
  Result := CreateRoundRectRgn(R.Left, R.Top, R.Right+1, R.Bottom+1, ARoundSize.cx, ARoundSize.cy);
end;

function dxCreateRoundRgn(const R: TRect): HRGN;
begin
  Result := dxCreateRoundRectRgn(R, cxSize(R));
end;

{ TdxRibbonCustomRadialMenu }

constructor TdxRibbonCustomRadialMenu.Create(AOwner: TComponent);
begin
  inherited;
  FInnerSize := 50;
  FSectorCount := 8;
  FGlyph := TdxSmartGlyph.Create;
  FRibbonHolder := TcxComponentHolder.Create;
  if csDesigning in AOwner.ComponentState then
    Ribbon := FindRibbonForComponent(AOwner);
end;

destructor TdxRibbonCustomRadialMenu.Destroy;
begin
  FreeAndNil(FRibbonHolder);
  FreeAndNil(FGlyph);
  inherited;
end;

//procedure TdxRibbonCustomRadialMenu.Popup(const P: TPoint);
procedure TdxRibbonCustomRadialMenu.Popup(X, Y: Integer);
begin
  InternalShowRadialMenu(X, Y, rmmAutoHide);
end;

procedure TdxRibbonCustomRadialMenu.Show(X, Y: Integer);
begin
  InternalShowRadialMenu(X, Y, rmmAutoCollapse);
end;

procedure TdxRibbonCustomRadialMenu.Hide;
begin
  if ItemLinks.BarControl <> nil then
    (ItemLinks.BarControl as TdxRibbonRadialMenuControl).ExpandState := rmsClosed;
end;

procedure TdxRibbonCustomRadialMenu.ChangeScaleCore(M, D: Integer);
begin
  inherited ChangeScaleCore(M, D);
  FInnerSize := MulDiv(FInnerSize, M, D);
  FMenuSize := MulDiv(FMenuSize, M, D);
end;

function TdxRibbonCustomRadialMenu.CreateBarControl: TCustomdxBarControl;
begin
  Result := inherited CreateBarControl;
  if (csDesigning in ComponentState) and (Ribbon <> nil) then
    TCustomdxBarControlAccess(Result).FPainter := TdxCustomRibbonAccess(Ribbon).GroupsPainter;
end;

function TdxRibbonCustomRadialMenu.GetControlClass: TCustomdxBarControlClass;
begin
  if csDesigning in ComponentState then
    Result := inherited GetControlClass
  else
    Result := TdxRibbonRadialMenuControl;
end;

function TdxRibbonCustomRadialMenu.GetItemLinksClass: TdxBarItemLinksClass;
begin
  if csDesigning in ComponentState then
    Result := inherited GetItemLinksClass
  else
    Result := TdxBarControlItemLinks;
end;

procedure TdxRibbonCustomRadialMenu.InternalShowRadialMenu(X, Y: Integer; AMode: TdxRadialMenuMode);

  function GetMaximumCaptionSize(AItemLinks: TdxBarItemLinks): Integer;
  var
    ANestedItemLinks:  TdxBarItemLinks;
    I: Integer;
  begin
    Result := 0;
    for I := 0 to GetVisibleItemCount(AItemLinks, FSectorCount) - 1 do
    begin
      Result := Max(Result, cxRectWidth(cxGetTextRect(
        ItemLinks.BarControl.Canvas.Handle, AItemLinks.VisibleItems[I].Caption, 2)));
      ANestedItemLinks := GetOwnedLinks(AItemLinks.VisibleItems[I].Item);
      if ANestedItemLinks <> nil then
        Result := Max(Result, GetMaximumCaptionSize(ANestedItemLinks));
    end;
  end;

  function GetMaximumMenuSize: Integer;
  begin
    Result := Round(GetMaximumCaptionSize(ItemLinks) / Sin(pi / FSectorCount)) * 2;
    Result := Max(Result, ScaleFactor.Apply(DefaultMenuSize));
  end;

  procedure ShowRadialMenu(const P: TPoint; AMenuMode: TdxRadialMenuMode);
  var
    ABounds: TRect;
    AControl: TdxRibbonRadialMenuControl;
  begin
    ItemLinks.CreateBarControl;
    AControl := ItemLinks.BarControl as TdxRibbonRadialMenuControl;
    AControl.FRadialMenu := Self;
    if Ribbon <> nil then
      TdxCustomRibbonAccess(Ribbon).AddAffiliatedBarControlForAccessibility(AControl.ItemLinks.BarControl);

    AControl.FInnerSize := FInnerSize;
    AControl.FMenuSize := IfThen(MenuSize = 0, GetMaximumMenuSize, MenuSize);
    AControl.FSectorCount := FSectorCount;

    ABounds := ExpandRectFromCenter(dxPointF(P), dxSizeF(AControl.FMenuSize, AControl.FMenuSize));
    MakeVisibleOnDesktop(ABounds, P);
    AControl.Left := ABounds.Left;
    AControl.Top := ABounds.Top;
    AControl.Width := AControl.FMenuSize;
    AControl.Height := AControl.FMenuSize;

    AControl.FMenuMode := AMenuMode;
    AControl.InitializeColors(GetSkin(Ribbon));

    if AControl.FMenuMode = rmmAutoHide then
      AControl.ExpandState := rmsExpanded
    else
      AControl.ExpandState := rmsCollapsed;

    AControl.Show;
  end;

begin
  if ItemLinks.BarControl = nil then
    ShowRadialMenu(Point(X, Y), AMode);
end;

function TdxRibbonCustomRadialMenu.GetRibbon: TdxCustomRibbon;
begin
  Result := FRibbonHolder.Component as TdxCustomRibbon;
end;

procedure TdxRibbonCustomRadialMenu.SetGlyph(AGlyph: TdxSmartGlyph);
begin
  FGlyph.Assign(AGlyph);
end;

procedure TdxRibbonCustomRadialMenu.SetRibbon(AValue: TdxCustomRibbon);
begin
  FRibbonHolder.Component := AValue;
end;

{ TdxRibbonRadialMenuControl }

constructor TdxRibbonRadialMenuControl.Create(AOwner: TComponent);
var
  ACoreItem: TdxBarButton;
begin
  inherited;
  FDropDownsList := TdxRadialMenuDropDownList.Create;
  FInternalItems := TObjectList.Create;

  FInternalItemLinksOwner := TdxBarInternalLinksOwner.Create(BarManager, nil);
  FInternalItemLinksOwner.ItemLinks.BarControl := Self;
  FInternalVisibleLinks := TList.Create;
  SetHotItem(htNone);

  ACoreItem := TdxBarButton.Create(Owner);
  ACoreItem.Caption := 'Exit';
  ACoreItem.KeyTip := '0';
  BarDesignController.AddInternalItem(ACoreItem, FInternalItems);
  FInternalItemLinksOwner.ItemLinks.Add(ACoreItem);
end;

destructor TdxRibbonRadialMenuControl.Destroy;
begin
  if BarNavigationController.KeyTipsHandlingMode then
    BarNavigationController.SetKeyTipsShowingState(nil, '');
  ActivateRootLevel;
  FreeAndNil(FHideAnimation);
  FreeAndNil(FShowAnimation);
  FreeAndNil(FInternalVisibleLinks);
  FreeAndNil(FInternalItemLinksOwner);
  FreeAndNil(FInternalItems);
  FreeAndNil(FBackArrowImage);
  FreeAndNil(FDropDownsList);
  if HandleAllocated then DestroyWindowHandle;
  inherited;
end;

procedure TdxRibbonRadialMenuControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := WS_POPUP;
  Params.WndParent := BarManager.Owner.Handle;
  Params.WindowClass.style := 0;
  Params.ExStyle := WS_EX_LAYERED
end;

procedure TdxRibbonRadialMenuControl.DoKeyDown(var Message: TWMKey);
begin
  if IsAnimationInProcess then Exit;

  inherited;

  case Message.CharCode of
    VK_ESCAPE:
      InternalCoreClick;
    VK_RETURN:
      case FHitTest.HitTestCode of
        htCore:
          InternalCoreClick;
        htDropDown, htItem:
          InternalClick(FHitTest.Item);
      end;
  end;
end;

procedure TdxRibbonRadialMenuControl.DoLButtonUp(var Message: TWMLButtonUp);
begin
  if IsAnimationInProcess then Exit;
  SetHotItem(SmallPointToPoint(Message.Pos));

  case FHitTest.HitTestCode of
    htNone:
      Hide;
    htCore:
      InternalCoreClick;
    htItem:
      InternalClick(FHitTest.Item);
    htDropDown:
      if not InternalPopup(FHitTest.SectorIndex) then
        InternalClick(FHitTest.Item);
  end;
end;

procedure TdxRibbonRadialMenuControl.DoBarMouseDown(Button: TMouseButton; Shift: TShiftState;
 const APoint: TPoint; AItemControl: TdxBarItemControl; APointInClientRect: Boolean);
begin
  if IsAnimationInProcess then Exit;
  SetHotItem(APoint);
end;

procedure TdxRibbonRadialMenuControl.DoBarMouseMove(Shift: TShiftState;
  const APoint: TPoint; AItemControl: TdxBarItemControl);
begin
  if IsAnimationInProcess then Exit;
  SetHotItem(APoint);
end;

procedure TdxRibbonRadialMenuControl.CalculateRects;
var
  ASize: Integer;
begin
  FMenuCenter := cxRectCenter(ClientRect);
  FCoreRingRect := cxRectCenter(ClientRect, FInnerSize, FInnerSize);
  FSmallRingRect := cxRectInflate(FCoreRingRect, -ScaleFactor.Apply(SmallRingOffset));
  FFullRingRect := ClientRect;

  ASize := ScaleFactor.Apply(ThickRingSize);
  FItemContentRingRect := cxRectContent(FFullRingRect, cxRect(ASize, ASize, ASize, ASize));
  FThickRingRect := cxRectInflate(FFullRingRect, -ASize div 2);
end;

procedure TdxRibbonRadialMenuControl.DoCreateControls;
begin
  // do nothing
end;

function TdxRibbonRadialMenuControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonRadialMenuAccessibilityHelper;
end;

function TdxRibbonRadialMenuControl.GetViewInfoClass: TCustomdxBarControlViewInfoClass;
begin
  Result := TdxRibbonRadialMenuControlViewInfo;
end;

procedure TdxRibbonRadialMenuControl.SetItemLinks(Value: TdxBarItemLinks);
begin
  inherited;
  SetCurrentItemLinks(Value, 0);
end;

procedure TdxRibbonRadialMenuControl.SetFont;
begin
  if ItemLinks.Owner is TdxBarCustomPopupMenuComponent then
    Font := TdxBarCustomPopupMenuComponent(ItemLinks.Owner).Font
  else
    Font := BarManager.Font;
end;

procedure TdxRibbonRadialMenuControl.Show;
begin
  inherited;
  UpdateLayer;
  CallNotify(FRadialMenu.OnPopup, FRadialMenu);
end;

procedure TdxRibbonRadialMenuControl.Hide;
begin
  ExpandState := rmsCollapsed;
end;

procedure TdxRibbonRadialMenuControl.ViewStateChanged(APrevValue: TdxBarViewState);
begin
  if FViewState = bvsNormal then
    SetHotItem(cxInvalidPoint);
end;

procedure TdxRibbonRadialMenuControl.DoHideAll(AReason: TdxBarCloseUpReason);
begin
  Hide;
end;

procedure TdxRibbonRadialMenuControl.NavigationHandler(var ACharCode: Word; AShiftState: TShiftState);
begin
  // do nothing
end;

function TdxRibbonRadialMenuControl.NeedsKey(AKey: Word; AShift: TShiftState): Boolean;
begin
  Result := (AKey <> VK_MENU);
end;

procedure TdxRibbonRadialMenuControl.InitializeColors(ASkin: IdxSkin);

  function IsColorLight(AColor: TColor): Boolean;
  var
    AHSV: TdxHSV;
  begin
    AHSV := dxColorToHSV(AColor);
    Result := AHSV.V > 0.8;
  end;

  function GetAccent(AColor: TColor; ATintPercent: Integer): TColor;
  begin
    Result := dxGetColorTint(AColor, ATintPercent);
  end;

const
  RadialMenuBaseColor = $7B3980;
  BackgroundColor = $FEFEFE;
begin
  if ASkin <> nil then
  begin
    FBaseAccentColor := ASkin.GetPartColor(DXBAR_RADIALMENUACCENT);
    FBaseBackgroundColor := ASkin.GetPartColor(DXBAR_RADIALMENUBACKGROUND);
    FColorPalette := FRadialMenu.Ribbon.ColorScheme.GetRadialMenuColorPalette;
  end
  else
  begin
    FBaseAccentColor := RadialMenuBaseColor;
    FBaseBackgroundColor := BackgroundColor;
    FColorPalette := nil;
  end;

  if dxColorToHSV(FBaseAccentColor).V > 0.9 then
    FItemRingDefaultColor := dxGetColorTint(FBaseAccentColor, 60)
  else
    FItemRingDefaultColor := dxGetColorTint(FBaseAccentColor, 85);

  if IsColorLight(FBaseBackgroundColor) then
    FItemTextColor := dxGetColorTint(FBaseBackgroundColor, -80)
  else
    FItemTextColor := dxGetColorTint(FBaseBackgroundColor, 80);

  FBackArrowColor := dxGetColorTint(FBaseAccentColor, 20);
  FDropDownItemRingNormalColor := FBaseAccentColor;
  FDropDownItemRingHotColor := dxGetColorTint(FBaseBackgroundColor, -75);
end;

procedure TdxRibbonRadialMenuControl.DrawRadialItemBackGround(ACanvas: TcxCanvas; AGPGraphics: TdxGPGraphics; AItem: TdxBarItem; ASector: TdxSector);

  procedure DrawDropDownArrow;
  var
    APoints: array of TdxPointF;
    A: Single;
  begin
    SetLength(APoints, 5);

    APoints[0] := dxPointF(1, -4);
    APoints[1] := dxPointF(1, 3);
    APoints[2] := dxPointF(4, 0);
    APoints[3] := dxPointF(4, -1);
    APoints[4] := dxPointF(1, -4);

    cxPointsOffset(APoints, dxPointF(FItemContentRingRect.Right + ScaleFactor.Apply(ThickRingSize) / 3, FMenuCenter.Y));

    A := DegToRad(ASector.StartAngle + ASector.SweepAngle / 2);
    dxRingRotatePoints(dxPointF(FMenuCenter), APoints, A);
    AGPGraphics.Polygon(APoints, clWhite, clWhite, 2, psSolid, 170, 255);
  end;

var
  ARingBrushColor, ARingPenColor, ABackgroundColor, AArcColor: TColor;
begin
  ABackgroundColor := FBaseBackgroundColor;

  if GetOwnedLinks(AItem) <> nil then
  begin
    if (FHitTest.HitTestCode = htDropDown) and (FHitTest.Item = AItem) and (AItem <> nil) and AItem.Enabled then
      ARingBrushColor := FDropDownItemRingHotColor
    else
      ARingBrushColor := FDropDownItemRingNormalColor;

    ARingPenColor := FItemRingDefaultColor;
    AGPGraphics.Pie(FFullRingRect, ASector.StartAngle + 0.5, ASector.SweepAngle - 0.5, ARingPenColor, ARingBrushColor, 2,
      psSolid, 255, 255);
    DrawDropDownArrow;

    AGPGraphics.Pie(FItemContentRingRect, ASector.StartAngle - 1, ASector.SweepAngle + 2, ABackgroundColor, ABackgroundColor, 2,
      psSolid, 255, 255);
  end;

  if (AItem is TdxBarButton) and TdxBarButton(AItem).Down then
  begin
    AArcColor := FBaseAccentColor;
    AGPGraphics.Arc(cxRectInflate(FItemContentRingRect, -2, -2), ASector.StartAngle + 2, ASector.SweepAngle - 3, AArcColor, 2, psSolid, 125);
  end;

  if (FHitTest.Item = AItem) and (AItem <> nil) and AItem.Enabled and
     (
       (FHitTest.HitTestCode = htItem) or
         (FHitTest.HitTestCode = htDropDown) and
         ((ssLeft in KeyboardStateToShiftState) or (GetOwnedLinks(AItem) = nil))
      ) then
  begin
    AArcColor := FBaseAccentColor;
    AGPGraphics.Arc(cxRectInflate(FItemContentRingRect, -2, -2), ASector.StartAngle + 2, ASector.SweepAngle - 3, AArcColor, 2, psSolid, 200);
  end;
end;

procedure TdxRibbonRadialMenuControl.DrawRadialItemContent(ACanvas: TcxCanvas; AGPGraphics: TdxGPGraphics; AItem: TdxBarItem; ARect: TRect);

  function GetCurrentImage(AItem: TdxBarItem; out ACurrentGlyph: TdxSmartGlyph; out ACurrentImages: TCustomImageList; out ACurrentImageIndex: Integer): Boolean;
  begin
    if AItem = nil then
      Result := False
    else
    begin
      ACurrentImages := GetCurrentImages;
      if ACurrentImages = nil then
        ACurrentImages := BarManager.Images;
      ACurrentImageIndex := AItem.ImageIndex;

      if IsImageAssigned(AItem.LargeGlyph) then
        ACurrentGlyph := AItem.LargeGlyph
      else
        ACurrentGlyph := AItem.Glyph;

      Result := IsImageAssigned(ACurrentGlyph, ACurrentImages, ACurrentImageIndex);
    end;
  end;

  function GetImageRect: TRect;
  var
    AAnimationImageOffset: Integer;
  begin
    Result := cxRectBounds(ARect.Left, ARect.Top, cxRectWidth(ARect), ScaleFactor.Apply(ItemImageSize));
    Result := cxRectCenter(Result, ScaleFactor.Apply(ItemImageSize), ScaleFactor.Apply(ItemImageSize));

    if FAnimationStep > AnimationTurn1 then
      AAnimationImageOffset := cxTextHeight(ACanvas.Font) - MulDiv(cxTextHeight(ACanvas.Font), FAnimationStep - AnimationTurn1, AnimationTurn3 - AnimationTurn1)
    else
      AAnimationImageOffset := cxTextHeight(ACanvas.Font);
    Result := cxRectOffset(Result, 0, AAnimationImageOffset);
  end;

  procedure DrawImage(AItem: TdxBarItem; var ARect: TRect);
  var
    ACurrentGlyph: TdxSmartGlyph;
    ACurrentImageIndex: Integer;
    ACurrentImages: TCustomImageList;
    ATempBitmap: TcxAlphaBitmap;
    AEnabled: Boolean;
  begin
    if GetCurrentImage(AItem, ACurrentGlyph, ACurrentImages, ACurrentImageIndex) or
      GetCurrentImage(GetClickableItem(AItem), ACurrentGlyph, ACurrentImages, ACurrentImageIndex) then
    begin
      ATempBitmap := TcxAlphaBitmap.CreateSize(GetImageRect, True);
      try
        AEnabled := AItem.Enabled and ((GetClickableItem(AItem) = nil) or (GetClickableItem(AItem).Enabled));
        cxDrawImage(ATempBitmap.cxCanvas, ATempBitmap.ClientRect,
          ACurrentGlyph, ACurrentImages, ACurrentImageIndex, AEnabled, FColorPalette, ScaleFactor);
        AGPGraphics.DrawBitmap(ATempBitmap, GetImageRect);
      finally
        ATempBitmap.free;
      end;
    end;

    ARect.Top := ARect.Top + ScaleFactor.Apply(ItemImageSize);
  end;

  procedure DrawText(var ARect: TRect; ATextColor: TColor; AAlpha: Integer);

    procedure BufferedDrawText(const AText: string; const ARect: TRect);
    var
      ABitmap: TcxAlphaBitmap;
      AGPBitmapCanvas: TdxGPGraphics;
    begin
      ABitmap := TcxAlphaBitmap.CreateSize(ARect);
      try
        ABitmap.Clear;
        AGPBitmapCanvas := TdxGPGraphics.Create(ABitmap.Canvas.Handle);
        try
          dxGPDrawText(AGPBitmapCanvas, AText, ABitmap.ClientRect, ACanvas.Font, dxColorToAlphaColor(ATextColor, AAlpha),
            taCenter, taAlignTop, True, TextRenderingHintAntiAliasGridFit, StringTrimmingEllipsisCharacter, UseRightToLeftReading);
        finally
          AGPBitmapCanvas.Free;
        end;
        ABitmap.RecoverAlphaChannel(0);

        AGPGraphics.DrawBitmap(ABitmap, ARect);
      finally
        ABitmap.Free;
      end;
    end;

  begin
    BufferedDrawText(GetTextOf(AItem.Caption), ARect);
  end;

const
  ATextAlphaMap: array [Boolean] of Integer = (85, 255);
begin
  if AItem = nil then Exit;

  DrawImage(AItem, ARect);
  if FAnimationStep > AnimationTurn2 then
    DrawText(ARect, FItemTextColor, MulDiv(ATextAlphaMap[AItem.Enabled], FAnimationStep - AnimationTurn2, AnimationTurn3 - AnimationTurn2));
end;

procedure TdxRibbonRadialMenuControl.DrawRadialItems(ACanvas: TcxCanvas; AGPGraphics: TdxGPGraphics);

  function GetItemRect(ASectorIndex: Integer): TRect;
  const
    ItemOffsetDivisor = 30;
    ItemHeight = 60;

    function GetItemRectWidth(R: Single): Single;
    begin
      Result := Sin(pi / FSectorCount) * R * 2;
    end;

  var
    R: Single;
    AAngle, ASweepAngle: Single;
    AItemSize: TdxSizeF;
    AItemOffset: Integer;
    AItemRingCenter: TdxPointF;
  begin
    R := FMenuSize / 4 * 1.1;
    ASweepAngle := 2 * Pi / FSectorCount;
    AAngle := ASweepAngle * ASectorIndex;
    if UseRightToLeftAlignment then
      AAngle := Pi - AAngle;
    AItemOffset := FMenuSize div ItemOffsetDivisor;
    AItemRingCenter := dxRingPoint(dxPointF(FMenuCenter), R, AAngle);
    AItemRingCenter.Y := AItemRingCenter.Y + AItemOffset;
    AItemSize := dxSizeF(GetItemRectWidth(R), ScaleFactor.Apply(ItemHeight));
    Result := ExpandRectFromCenter(AItemRingCenter, AItemSize);
  end;

var
  I: Integer;
begin
  for I := 0 to FSectorCount - 1 do
    DrawRadialItemBackGround(ACanvas, AGPGraphics, GetVisibleItem(I), GetItemSector(I));
  for I := 0 to FSectorCount - 1 do
    DrawRadialItemContent(ACanvas, AGPGraphics, GetVisibleItem(I), GetItemRect(I));
end;

procedure TdxRibbonRadialMenuControl.DrawRadialMenu(ACanvas: TcxCanvas);
var
  AGPGraphics: TdxGPGraphics;
begin
  ACanvas.Font := Font;
  ACanvas.Font.Size := Min(ScaleFactor.Apply(8), Font.Size);

  AGPGraphics := TdxGPGraphics.Create(ACanvas.Handle);
  try
    AGPGraphics.SmoothingMode := smAntiAlias;

    //Background
     AGPGraphics.Ellipse(FFullRingRect, FBaseBackgroundColor, FBaseBackgroundColor, 1, psSolid, 255, 255);

    // Items
    AGPGraphics.Ellipse(FThickRingRect, FItemRingDefaultColor, 0, ScaleFactor.Apply(ThickRingSize), psSolid, 255, 0);
    DrawRadialItems(ACanvas, AGPGraphics);
  finally
    AGPGraphics.Free;
  end;
end;

procedure TdxRibbonRadialMenuControl.DrawRadialCore(ACanvas: TcxCanvas);
var
  AGlyphRect: TRect;
  AGPGraphics: TdxGPGraphics;
begin
  AGPGraphics := TdxGPGraphics.Create(ACanvas.Handle);
  try
    AGPGraphics.SmoothingMode := smAntiAlias;

    //Background
     AGPGraphics.Ellipse(FSmallRingRect, FBaseBackgroundColor, FBaseBackgroundColor, 1, psSolid, 255, 255);

    //Adornments
    //Ring1
    AGPGraphics.Ellipse(FSmallRingRect, FBaseAccentColor, 0, 2, psSolid, 255, 0);
    //Ring2
    if FHitTest.HitTestCode = htCore then
      AGPGraphics.Ellipse(FCoreRingRect, FBaseAccentColor, 0, 2, psSolid, 200, 0);

    //Core
    if IsImageAssigned(GetCurrentCoreImage) then
    begin
      AGlyphRect := cxRectCenter(ClientRect, dxGetImageSize(GetCurrentCoreImage, ScaleFactor));
      ACanvas.SetClipRegion(TcxRegion.Create(dxCreateRoundRgn(cxRectInflate(FSmallRingRect, -2, -2))), roSet);
      cxDrawImage(ACanvas, AGlyphRect, GetCurrentCoreImage, ifmNormal, FColorPalette, ScaleFactor);
    end;
  finally
    AGPGraphics.Free;
  end;
end;

procedure TdxRibbonRadialMenuControl.UpdateLayer;

  procedure TransformBitmapCanvas(ABitmap: TcxBitmap);
  var
    ARotationMatrix, AScaleMatrix, ATotalMatrix: TXForm;
    A, H: Single;
    P1, P2: TdxPointF;
  begin
    if UseRightToLeftAlignment then
      A := DegToRad(AnimationTurn1 - FAnimationStep)
    else
      A := DegToRad(-AnimationTurn1 + FAnimationStep);
    ARotationMatrix.eM11 := cos(A);
    ARotationMatrix.eM12 := sin(A);
    ARotationMatrix.eM21 := -sin(A);
    ARotationMatrix.eM22 := cos(A);
    H := cxRectWidth(ABitmap.ClientRect) / 2 / Cos(Pi/4);
    P1 := dxRingPoint(dxPointF(0, 0), H, DegToRad(AnimationTurn1+45));
    P2 := dxRingPoint(dxPointF(0, 0), H, DegToRad(AnimationTurn1+45) - A);
    ARotationMatrix.eDx :=  P2.X - P1.X;
    ARotationMatrix.eDy := P2.Y - P1.Y;

    A := Max(0.01, FAnimationStep / AnimationTurn1);
    AScaleMatrix.eM11 := A;
    AScaleMatrix.eM12 := 0;
    AScaleMatrix.eM21 := 0;
    AScaleMatrix.eM22 := A;
    AScaleMatrix.eDx := cxRectWidth(ABitmap.ClientRect) / 2 - cxRectWidth(ABitmap.ClientRect) / 2 * A;
    AScaleMatrix.eDy := cxRectWidth(ABitmap.ClientRect) / 2 - cxRectWidth(ABitmap.ClientRect) / 2 * A;

    CombineTransform(ATotalMatrix, ARotationMatrix, AScaleMatrix);
    SetGraphicsMode(ABitmap.Canvas.Handle, GM_ADVANCED);
    SetWorldTransform(ABitmap.Canvas.Handle, ATotalMatrix);
  end;

  procedure NormalizeBitmapCanvas(ABitmap: TcxBitmap);
  var
    ANormalMatrix: TXForm;
  begin
    ANormalMatrix.eM11 := 1;
    ANormalMatrix.eM12 := 0;
    ANormalMatrix.eM21 := 0;
    ANormalMatrix.eM22 := 1;
    ANormalMatrix.eDx := 0;
    ANormalMatrix.eDy := 0;
    SetWorldTransform(ABitmap.Canvas.Handle, ANormalMatrix);
  end;

var
  ARotateBitmap, ABasicBitmap: TcxBitmap32;
begin
  if HandleAllocated then
  begin
    ABasicBitmap := TcxBitmap32.CreateSize(ClientRect, True);
    ARotateBitmap := TcxBitmap32.CreateSize(ClientRect, True);
    try
      CalculateRects;

      if FAnimationStep < AnimationTurn1 then
        TransformBitmapCanvas(ARotateBitmap);

      if GetRealState = rmsExpanded then
        DrawRadialMenu(ARotateBitmap.cxCanvas);
      if FAnimationStep < AnimationTurn1 then
      begin
        NormalizeBitmapCanvas(ARotateBitmap);
        cxAlphaBlend(ABasicBitmap, ARotateBitmap, ABasicBitmap.ClientRect, ARotateBitmap.ClientRect, False, MulDiv(255, FAnimationStep, AnimationTurn1));
      end
      else
        ABasicBitmap.CopyBitmap(ARotateBitmap);
      DrawRadialCore(ABasicBitmap.cxCanvas);

      cxUpdateLayeredWindow(Handle, ABasicBitmap);
    finally
      ARotateBitmap.Free;
      ABasicBitmap.Free;
    end;
  end;
end;

procedure TdxRibbonRadialMenuControl.Paint;
begin
//  inherited;
//  raise EdxException.Create('TdxRibbonRadialMenuControl.Paint fails');
end;

function TdxRibbonRadialMenuControl.GetVisibleItem(ASectorIndex: Integer): TdxBarItem;
var
  AIndex: Integer;
begin
  AIndex := (ASectorIndex - GetCurrentItemLinksStartIndex) mod FSectorCount;
  if AIndex < 0 then
    AIndex := FSectorCount + AIndex;

  if (AIndex < FInternalVisibleLinks.Count) and (FInternalVisibleLinks[AIndex] <> nil) then
    Result := TdxBarItemLink(FInternalVisibleLinks[AIndex]).Item
  else
    Result := nil;
end;

procedure TdxRibbonRadialMenuControl.SetCurrentItemLinks(AValue: TdxBarItemLinks; AStartIndex: Integer);
var
  AData: TdxRadialMenuDropDownData;
begin
  AData := (FDropDownsList.Add(AValue) as TdxRadialMenuDropDownData);
  AData.FStartIndex := AStartIndex;
  GetCurrentItemLinks.BarControl := Self;
  SynchronizeInternalItems;
end;

procedure TdxRibbonRadialMenuControl.SetHotItem(const P: TPoint);

  function GetSectorIndex: Integer;
  var
    AAngle: Single;
    I: Integer;
    ASector: TdxSector;
  begin
    Result := -1;
    AAngle := RadToDeg(dxRingAngle(dxPointF(FMenuCenter), dxPointF(P)));
    for I := 0 to FSectorCount - 1 do
    begin
      ASector := GetItemSector(I);
      if (AAngle >= ASector.StartAngle) and (AAngle < ASector.StartAngle + ASector.SweepAngle) or
         (AAngle >= ASector.StartAngle + 360) and (AAngle < ASector.StartAngle + 360 + ASector.SweepAngle) then
      begin
        Result := I;
        Break;
      end;
    end;
    if Result = -1 then
      raise EdxException.Create('TdxRibbonRadialMenuControl.GetHitTest fails');
  end;

begin
  if dxRingPtIn(FCoreRingRect, P) then
    SetHotItem(htCore)
  else
  begin
    if dxRingPtIn(FItemContentRingRect, P) then
      SetHotItem(GetSectorIndex)
    else
      if dxRingPtIn(FFullRingRect, P) then
        SetHotItem(GetSectorIndex, True)
      else
        SetHotItem(htNone)
  end;
end;

procedure TdxRibbonRadialMenuControl.SetHotItem(ASectorIndex: Integer; AInEdge: Boolean = False);
var
  AHitTest: TdxRibbonRadialMenuHitTest;
begin
  AHitTest.SectorIndex := ASectorIndex;
  case ASectorIndex of
    htNone, htCore:
      begin
        AHitTest.Item := nil;
        AHitTest.HitTestCode := ASectorIndex;
      end;
  else
    AHitTest.Item := GetVisibleItem(ASectorIndex);
    if AInEdge then
      AHitTest.HitTestCode := htDropDown
    else
      AHitTest.HitTestCode := htItem;
  end;

  if (AHitTest.HitTestCode <> FHitTest.HitTestCode) or (AHitTest.Item <> FHitTest.Item) or (AHitTest.SectorIndex <> FHitTest.SectorIndex) then
  begin
    FHitTest := AHitTest;
    UpdateLayer;
  end;
end;

function TdxRibbonRadialMenuControl.GetClickableItem(AItem: TdxBarItem): TdxBarItem;
var
  AItemLinks: TdxBarItemLinks;
  AIndex: Integer;
begin
  Result := nil;
  if (AItem <> nil) and TdxBarItemAccess(AItem).CanClicked then
    Result := AItem
  else
  begin
    AItemLinks := GetOwnedLinks(AItem);
    if (AItemLinks <> nil) and (AItemLinks.VisibleItemCount > 0) then
    begin
      AIndex := GetVisibleItemCount(AItemLinks) div 2;
      Result := GetClickableItem(AItemLinks.VisibleItems[AIndex].Item);
    end;
  end;
end;

function TdxRibbonRadialMenuControl.GetVisibleItemCount(AItemLinks: TdxBarItemLinks): Integer;
begin
  Result := dxRibbonRadialMenu.GetVisibleItemCount(AItemLinks, FSectorCount);
end;

function TdxRibbonRadialMenuControl.GetCoreItemLink: TdxBarItemLink;
begin
  Result := FInternalItemLinksOwner.ItemLinks[0];
end;

function TdxRibbonRadialMenuControl.GetCurrentItemLinks: TdxBarItemLinks;
begin
  Result := (FDropDownsList.Last as TdxRadialMenuDropDownData).Data;
end;

function TdxRibbonRadialMenuControl.GetCurrentItemLinksStartIndex: Integer;
begin
  Result := (FDropDownsList.Last as TdxRadialMenuDropDownData).FStartIndex;
end;

function TdxRibbonRadialMenuControl.GetCurrentCoreImage: TdxSmartImage;

  function Scale(V: Integer): Integer;
  begin
    Result := ScaleFactor.Apply(V);
  end;

var
  ABackArrowBitmap: TcxAlphaBitmap;
  AGPGraphics: TdxGPGraphics;
begin
  if FDropDownsList.Count > 1 then
  begin
    if FBackArrowImage = nil then
    begin
      ABackArrowBitmap := TcxAlphaBitmap.CreateSize(Scale(18), Scale(13), True);
      try
        AGPGraphics := TdxGPGraphics.Create(ABackArrowBitmap.Canvas.Handle);
        try
          AGPGraphics.SmoothingMode := smAntiAlias;
          AGPGraphics.Line(Scale(2), Scale(6), Scale(18), Scale(6), FBackArrowColor, Scale(3), psSolid, MaxByte);
          AGPGraphics.Line(Scale(10), Scale(-2), Scale(1), Scale(7), FBackArrowColor, Scale(3), psSolid, MaxByte);
          AGPGraphics.Line(Scale(10), Scale(14), Scale(1), Scale(5), FBackArrowColor, Scale(3), psSolid, MaxByte);
        finally
          AGPGraphics.Free;
        end;
        FBackArrowImage := TdxSmartGlyph.CreateFromBitmap(ABackArrowBitmap);
        if UseRightToLeftAlignment then
          FBackArrowImage.Flip(True, False);
        FBackArrowImage.SourceDPI := Scale(dxDefaultDPI);
      finally
        ABackArrowBitmap.Free;
      end;
    end;
    Result := FBackArrowImage;
  end
  else
    Result := FRadialMenu.Glyph;
end;

function TdxRibbonRadialMenuControl.GetItemSector(ASectorIndex: Integer): TdxSector;
var
  ASweepAngle, AAngle: Single;
begin
  ASweepAngle := 360 / FSectorCount;
  AAngle := ASweepAngle * ASectorIndex;
  if UseRightToLeftAlignment then
    AAngle := 180 - AAngle;
  Result.StartAngle := AAngle - (ASweepAngle / 2);
  Result.SweepAngle := ASweepAngle;
end;

procedure TdxRibbonRadialMenuControl.SetExpandState(Value: TdxRadialMenuState);
var
  APrevState: TdxRadialMenuState;
begin
  if FExpandState <> Value then
  begin
    APrevState := FExpandState;
    FExpandState := Value;
    case FExpandState of
      rmsExpanded:
        begin
          FreeAndNil(FHideAnimation);
          FAnimationStep := 0;
          FShowAnimation := TdxAnimationTransition.Create(AnimationShowTime, ateLinear, AnimationShowCycle);
          FShowAnimation.OnTerminate := ShowAnimationTerminated;
          FShowAnimation.OnAnimate := ShowAnimationStep;
          FShowAnimation.Resume;
          IsActive := True;
        end;

      rmsCollapsed:
        if APrevState = rmsExpanded then
        begin
          FreeAndNil(FShowAnimation);
          FAnimationStep := AnimationHideCycle;
          FHideAnimation := TdxAnimationTransition.Create(AnimationHideTime, ateLinear, AnimationHideCycle);
          FHideAnimation.OnAnimate := HideAnimationStep;
          FHideAnimation.OnTerminate := HideAnimationTerminated;
          FHideAnimation.Resume;
          IsActive := False;
        end
        else
          FAnimationStep := AnimationHideCycle;

      rmsClosed:
        InternalHide;
    end;
  end;
end;

procedure TdxRibbonRadialMenuControl.ActivateRootLevel;
begin
  while InternalCloseLastLevel do {loop};
end;

procedure TdxRibbonRadialMenuControl.InternalHide;
begin
  CallNotify(FRadialMenu.OnCloseUp, FRadialMenu);
  Free;
end;

function TdxRibbonRadialMenuControl.InternalClick(AItem: TdxBarItem): Boolean;
begin
  AItem := GetClickableItem(AItem);
  Result := AItem <> nil;
  if Result then
  begin
    if not (AItem is TdxBarButton) or TdxBarButton(AItem).CloseSubMenuOnClick then
      Hide;
    AItem.Click;
  end;
end;

procedure TdxRibbonRadialMenuControl.InternalCoreClick;
begin
  if ExpandState = rmsCollapsed then
    ExpandState := rmsExpanded
  else
    if InternalCloseLastLevel then
      UpdateLayer
    else
      ExpandState := rmsCollapsed;
end;

function TdxRibbonRadialMenuControl.InternalCloseLastLevel: Boolean;
begin
  Result := FDropDownsList.Count > 1;
  if Result then
  begin
    if GetSubmenuOwner(GetCurrentItemLinks) <> nil then
      GetSubmenuOwner(GetCurrentItemLinks).DoCloseUp;
    GetCurrentItemLinks.BarControl := nil;
    FDropDownsList.Delete(FDropDownsList.Last);
    SynchronizeInternalItems;
  end;
end;

function TdxRibbonRadialMenuControl.InternalPopup(ASectorIndex: Integer): Boolean;

  function GetStartIndex(AItemLinks: TdxBarItemLinks): Integer;
  begin
    Result := ASectorIndex + GetCurrentItemLinksStartIndex - GetVisibleItemCount(AItemLinks) div 2;
    if Result < 0 then
      Result := FSectorCount + Result;
  end;

var
  AItem: TdxBarItem;
  AItemLinks: TdxBarItemLinks;
begin
  AItem := GetVisibleItem(ASectorIndex);
  AItemLinks := GetOwnedLinks(AItem);
  Result := AItemLinks <> nil;
  if Result then
  begin
    if GetSubmenuOwner(AItemLinks) <> nil then
      GetSubmenuOwner(AItemLinks).DoPopup;
    SetCurrentItemLinks(AItemLinks, GetStartIndex(AItemLinks));
    UpdateLayer;
  end;
end;

function TdxRibbonRadialMenuControl.HandleNavigationKey(var AKey: Word): Boolean;

  procedure Navigate(ADirection: Integer);
  var
    ANavigationList: TList;
    ASectorIndex, ANavigationIndex: Integer;
    AItem: TdxBarItem;
  begin
    ANavigationList := TList.Create;
    try
      for ASectorIndex := 0 to FSectorCount - 1 do
      begin
        AItem := GetVisibleItem(ASectorIndex);
        if (AItem <> nil) and
          (AItem.Enabled or (GetOwnedLinks(AItem) <> nil)) then
          ANavigationList.Add(Pointer(ASectorIndex));
      end;

      ANavigationIndex := ANavigationList.IndexOf(Pointer(FHitTest.SectorIndex)) + ADirection;
      if ANavigationIndex = -2 then
        ANavigationIndex := ANavigationList.Count - 1
      else
        if ANavigationIndex = ANavigationList.Count then
          ANavigationIndex := -1;

      if ANavigationIndex = -1 then
        SetHotItem(htCore)
      else
        SetHotItem(Integer(ANavigationList[ANavigationIndex]));
    finally
      ANavigationList.Free;
    end;
  end;

var
  ASectorIndex: Integer;
begin
  Result := True;
  case AKey of
    VK_LEFT:
        InternalCoreClick;
    VK_RIGHT:
      begin
        ASectorIndex := FHitTest.SectorIndex;
        InternalPopup(FHitTest.SectorIndex);
        SetHotItem(ASectorIndex);
      end;
    VK_UP:
      begin
        Navigate(-1);
      end;
    VK_DOWN, VK_TAB:
      begin
        Navigate(1);
      end;
  else
    Result := False;
  end;
end;

procedure TdxRibbonRadialMenuControl.SynchronizeInternalItems;
var
  I, J: Integer;
  AMoreButtonsItem: TdxBarSubItem;
  AMoreButtonsLink: TdxBarItemLink;
begin
  FInternalVisibleLinks.Clear;
  for I := 0 to GetCurrentItemLinks.VisibleItemCount - 1 do
  begin
    if GetCurrentItemLinks.VisibleItems[I].BeginGroup then
      FInternalVisibleLinks.Add(nil);
    FInternalVisibleLinks.Add(GetCurrentItemLinks.VisibleItems[I]);
    if (FInternalVisibleLinks.Count > 8) or (FInternalVisibleLinks.Count = 8) and (I < GetCurrentItemLinks.VisibleItemCount - 1) then
    begin
      AMoreButtonsItem := TdxBarSubItem.Create(Owner);
      AMoreButtonsItem.Caption := cxGetResourceString(@dxSBAR_MOREBUTTONS);
      for J := I to GetCurrentItemLinks.VisibleItemCount - 1 do
        AMoreButtonsItem.ItemLinks.Add.Assign(GetCurrentItemLinks.VisibleItems[J]);

      BarDesignController.AddInternalItem(AMoreButtonsItem, FInternalItems);
      AMoreButtonsLink := FInternalItemLinksOwner.ItemLinks.Add(AMoreButtonsItem);
      FInternalVisibleLinks.Insert(7, AMoreButtonsLink);
      Break;
    end;
  end;
end;

function TdxRibbonRadialMenuControl.IsAnimationInProcess: Boolean;
begin
  Result := (FShowAnimation <> nil) or (FHideAnimation <> nil);
end;

procedure TdxRibbonRadialMenuControl.ShowAnimationStep(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
begin
  FAnimationStep := APosition;
  UpdateLayer;
end;

procedure TdxRibbonRadialMenuControl.HideAnimationStep(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
begin
  FAnimationStep := AnimationHideCycle - APosition;
  UpdateLayer;
end;

procedure TdxRibbonRadialMenuControl.ShowAnimationTerminated(Sender: TObject);
begin
  FShowAnimation := nil;
  if FKeyTipsWaiting then
  begin
    FKeyTipsWaiting := False;
    (IAccessibilityHelper.GetBarHelper as TdxRibbonRadialMenuAccessibilityHelper).ActivateKeyTips(IAccessibilityHelper);
  end;
  CallNotify(FRadialMenu.OnExpand, FRadialMenu);
end;

procedure TdxRibbonRadialMenuControl.HideAnimationTerminated(Sender: TObject);
begin
  CallNotify(FRadialMenu.OnCollapse, FRadialMenu);
  FHideAnimation := nil;
  if FMenuMode = rmmAutoHide then
    ExpandState := rmsClosed
  else
  begin
    ActivateRootLevel;
    FAnimationStep := AnimationHideCycle;
    UpdateLayer;
    if FKeyTipsWaiting then
    begin
      FKeyTipsWaiting := False;
      (IAccessibilityHelper.GetBarHelper as TdxRibbonRadialMenuAccessibilityHelper).ActivateKeyTips(FRadialMenu.Ribbon.IAccessibilityHelper);
    end;
  end;
end;

function TdxRibbonRadialMenuControl.GetRealState: TdxRadialMenuState;
begin
  Result := FExpandState;
  if (Result = rmsCollapsed) and (FHideAnimation <> nil) then
    Result := rmsExpanded;
end;

procedure TdxRibbonRadialMenuControl.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  FreeAndNil(FBackArrowImage);
end;

{ TdxRibbonRadialMenuControlViewInfo }

constructor TdxRibbonRadialMenuControlViewInfo.Create(
  ABarControl: TCustomdxBarControl);
begin
  inherited;

end;

destructor TdxRibbonRadialMenuControlViewInfo.Destroy;
begin

  inherited;
end;

{ TdxRibbonRadialMenuAccessibilityHelper }

constructor TdxRibbonRadialMenuAccessibilityHelper.Create(AOwnerObject: TObject);
begin
  inherited;
end;

destructor TdxRibbonRadialMenuAccessibilityHelper.Destroy;
begin
  inherited;
end;

function TdxRibbonRadialMenuAccessibilityHelper.AreKeyTipsSupported(out AKeyTipWindowsManager: IdxBarKeyTipWindowsManager): Boolean;
begin
  Result := BarControl.FRadialMenu.Ribbon <> nil;
  if Result and (FKeyTipWindowsManager = nil) then
    FKeyTipWindowsManager := TdxRibbonRadialMenuKeyTipWindows.Create(BarControl.FRadialMenu.Ribbon);
  AKeyTipWindowsManager := FKeyTipWindowsManager;
end;

function TdxRibbonRadialMenuAccessibilityHelper.GetDefaultAccessibleObject: IdxBarAccessibilityHelper;
begin
  Result := Self;
end;

procedure TdxRibbonRadialMenuAccessibilityHelper.InitializeItemKeyTipPosition(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; var AKeyTipInfo: TdxBarKeyTipInfo);
var
  ASectorIndex: Integer;
  ASector: TdxSector;
  FP: TdxPointF;
begin
  if AItemLinkHelper.ItemLink = BarControl.CoreItemLink then
    AKeyTipInfo.BasePoint := BarControl.FMenuCenter
  else
  begin
    ASectorIndex := GetChildSectorIndex(AItemLinkHelper);
    ASector := BarControl.GetItemSector(ASectorIndex);
    FP := dxRingPoint(dxPointF(BarControl.FMenuCenter), BarControl.FMenuSize / 2, DegToRad(ASector.StartAngle + ASector.SweepAngle / 2));
    AKeyTipInfo.BasePoint.X := Round(FP.X);
    AKeyTipInfo.BasePoint.Y := Round(FP.Y);
  end;
  AKeyTipInfo.BasePoint := BarControl.ClientToScreen(AKeyTipInfo.BasePoint);
  AKeyTipInfo.HorzAlign := taCenter;
  AKeyTipInfo.VertAlign := vaCenter;
end;

function TdxRibbonRadialMenuAccessibilityHelper.GetChildSectorIndex(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper): Integer;
begin
  Result := BarControl.FInternalVisibleLinks.IndexOf(AItemLinkHelper.ItemLink);
  Result := Result + BarControl.GetCurrentItemLinksStartIndex;
end;

function TdxRibbonRadialMenuAccessibilityHelper.GetChildSectorIndex(AChildIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := AChildIndex;
  for I := 0 to BarControl.FInternalVisibleLinks.Count - 1 do
  begin
    if BarControl.FInternalVisibleLinks[I] = nil then
      Inc(Result);
    if I = Result then
      Break;
  end;
end;

function TdxRibbonRadialMenuAccessibilityHelper.GetChild(AIndex: Integer): TcxAccessibilityHelper;
var
  ASectorIndex: Integer;
begin
  if AIndex = 0 then
    Result := TdxBarItemLinkAccess(BarControl.CoreItemLink).IAccessibilityHelper.GetHelper
  else
  begin
    ASectorIndex := GetChildSectorIndex(AIndex - 1);
    Result := TdxBarItemLinkAccess(BarControl.FInternalVisibleLinks[ASectorIndex]).IAccessibilityHelper.GetHelper;
  end;
end;

function TdxRibbonRadialMenuAccessibilityHelper.GetChildCount: Integer;
var
  I: Integer;
begin
  Result := 1;
  if BarControl.ExpandState = rmsExpanded then
    for I := 0 to BarControl.FInternalVisibleLinks.Count - 1 do
      if BarControl.FInternalVisibleLinks[I] <> nil then
        Inc(Result);
end;

function TdxRibbonRadialMenuAccessibilityHelper.GetChildIndex(AChild: TcxAccessibilityHelper): Integer;
begin
  if AChild = TdxBarItemLinkAccess(BarControl.CoreItemLink).IAccessibilityHelper.GetHelper then
    Result := 0
  else
    Result := BarControl.FInternalVisibleLinks.IndexOf((AChild as TdxBarItemLinkAccessibilityHelper).ItemLink) + 1;
end;

procedure TdxRibbonRadialMenuAccessibilityHelper.DoClickItem(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper);
begin
  if AItemLinkHelper.ItemLink = BarControl.CoreItemLink then
  begin
    BarControl.InternalCoreClick;
    if IsOwnerObjectLive then
    begin
      if (ActiveBarControl = BarControl) and not BarControl.IsAnimationInProcess then
        ActivateKeyTips(ActiveBarControl.IAccessibilityHelper)
      else
        BarControl.FKeyTipsWaiting := True;
    end;
  end
  else
    BarControl.InternalClick(AItemLinkHelper.Item);
end;

procedure TdxRibbonRadialMenuAccessibilityHelper.DoDropDownItem(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper);
var
  ASectorIndex: Integer;
begin
  ASectorIndex := GetChildSectorIndex(AItemLinkHelper);
  if BarControl.InternalPopup(ASectorIndex) then
    ActivateKeyTips(ActiveBarControl.IAccessibilityHelper);
end;

function TdxRibbonRadialMenuAccessibilityHelper.HandleNavigationKey(var AKey: Word): Boolean;
begin
  Result := BarControl.HandleNavigationKey(AKey);
end;

function TdxRibbonRadialMenuAccessibilityHelper.GetBarControl: TdxRibbonRadialMenuControl;
begin
  Result := inherited GetBarControl as TdxRibbonRadialMenuControl;
end;

procedure TdxRibbonRadialMenuAccessibilityHelper.ActivateKeyTips(AHelper: IdxBarAccessibilityHelper);
begin
  BarNavigationController.SetKeyTipsShowingState(nil, '');
  BarNavigationController.SetKeyTipsShowingState(AHelper, '');
end;

end.
