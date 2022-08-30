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

unit dxBarApplicationMenu;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Forms, Messages, Classes, SysUtils, Graphics,
  Types, Controls, ExtCtrls, ImgList, IniFiles, Contnrs,
  dxCore, cxClasses, cxGraphics, cxGeometry, cxControls, cxContainer, cxLookAndFeels, dxGDIPlusClasses,
  cxAccessibility, dxBar, dxBarAccessibility, dxScreenTip;

type
  TdxBarApplicationMenuButtons = class;
  TdxBarApplicationMenuControl = class;
  TdxBarCustomApplicationMenu = class;
  TdxBarExtraPaneListItem = class;

  { TdxBarCustomApplicationMenu }

  TdxBarExtraPaneNotifyEvent = procedure(Sender: TObject; AIndex: Integer) of object;
  TdxBarExtraPaneGetDisplayTextEvent = procedure(Sender: TObject; AIndex: Integer; var ADisplayText: string) of object;

  TdxBarExtraPaneItem = class(TCollectionItem)
  private
    FData: TcxTag;
    FDisplayText: string;
    FImageIndex: Integer;
    FPin: Boolean;
    FText: string;
    procedure SetDisplayText(const Value: string);
    procedure SetText(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;

    property Data: TcxTag read FData write FData;
  published
    property DisplayText: string read FDisplayText write SetDisplayText;
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
    property Pin: Boolean read FPin write FPin default False;
    property Text: string read FText write SetText;
  end;

  { TdxBarExtraPaneItems }

  TdxBarExtraPaneItems = class(TCollection)
  private
    FApplicationMenu: TdxBarCustomApplicationMenu;
    function GetItem(Index: Integer): TdxBarExtraPaneItem;
    procedure SetItem(Index: Integer; Value: TdxBarExtraPaneItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AApplicationMenu: TdxBarCustomApplicationMenu);
    function Add: TdxBarExtraPaneItem;
    function IndexOf(const AItemText: string): Integer;
    function Insert(AIndex: Integer): TdxBarExtraPaneItem;

    property Items[Index: Integer]: TdxBarExtraPaneItem read GetItem write SetItem; default;
  end;

  { TdxBarApplicationMenuExtraPane }

  TdxBarApplicationMenuExtraPane = class(TInterfacedPersistent, IdxLocalizerListener)
  private
    FApplicationMenu: TdxBarCustomApplicationMenu;
    FIsExtraPaneHeaderAssigned: Boolean;
    FItems: TdxBarExtraPaneItems;
    FListItem: TdxBarExtraPaneListItem;
    FSize: Integer;
    FVisible: Boolean;
    FWidthRatio: Double;
    FOnGetItemDisplayText: TdxBarExtraPaneGetDisplayTextEvent;
    FOnItemClick: TdxBarExtraPaneNotifyEvent;
    function DefaultExtraPaneHeader: string;
    function GetAllowPinItems: Boolean;
    function GetHeader: string;
    function IsHeaderStored: Boolean;
    function IsWidthRatioStored: Boolean;
    procedure SetAllowPinItems(AValue: Boolean);
    procedure SetHeader(const AHeader: string);
    procedure SetItems(AValue: TdxBarExtraPaneItems);
    procedure SetSize(AValue: Integer);
    procedure SetWidthRatio(AValue: Double);

    procedure DoItemClick(Sender: TObject);
    procedure DoGetItemDisplayText(Sender: TObject; AIndex: Integer; var ADisplayText: string);
  protected
    function GetOwner: TPersistent; override;
    procedure PopulateListItem;
    // IdxLocalizerListener
    procedure TranslationChanged;
    //
    property ListItem: TdxBarExtraPaneListItem read FListItem;
  public
    constructor Create(AApplicationMenu: TdxBarCustomApplicationMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AllowPinItems: Boolean read GetAllowPinItems write SetAllowPinItems default True;
    property Header: string read GetHeader write SetHeader stored IsHeaderStored;
    property Items: TdxBarExtraPaneItems read FItems write SetItems;
    property Size: Integer read FSize write SetSize default 0;
    property Visible: Boolean read FVisible write FVisible default True;
    property WidthRatio: Double read FWidthRatio write SetWidthRatio stored IsWidthRatioStored;

    property OnItemClick: TdxBarExtraPaneNotifyEvent read FOnItemClick write FOnItemClick;
    property OnGetItemDisplayText: TdxBarExtraPaneGetDisplayTextEvent read FOnGetItemDisplayText write FOnGetItemDisplayText;
  end;

  { TdxBarApplicationMenuButtonItem }

  TdxBarApplicationMenuButtonItem = class(TdxBarButtonLikeItem)
  private
    FExternalItem: TdxBarButton;
    procedure SetExternalItem(AValue: TdxBarButton);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetCaption: string; override;
    function GetEnabled: Boolean; override;
    function GetGlyph: TdxSmartGlyph; override;
    function GetHint: string; override;
    function GetImageIndex: Integer; override;
    function GetScreenTip: TdxScreenTip; override;
    function GetVisible: TdxBarItemVisible; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DirectClick; override;

    property ExternalItem: TdxBarButton read FExternalItem write SetExternalItem;
  end;

  { TdxBarApplicationMenuButton }

  TdxBarApplicationMenuButton = class(TCollectionItem)
  private
    FInternalItem: TdxBarApplicationMenuButtonItem;
    function GetCollection: TdxBarApplicationMenuButtons;
    function GetItem: TdxBarButton;
    function GetWidth: Integer;
    procedure SetItem(Value: TdxBarButton);
    procedure SetWidth(Value: Integer);
  protected
    property InternalItem: TdxBarApplicationMenuButtonItem read FInternalItem;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Collection: TdxBarApplicationMenuButtons read GetCollection;
  published
    property Item: TdxBarButton read GetItem write SetItem;
    property Width: Integer read GetWidth write SetWidth default 0;
  end;

  { TdxBarApplicationMenuButtons }

  TdxBarApplicationMenuButtons = class(TCollection)
  private
    FApplicationMenu: TdxBarCustomApplicationMenu;
    function GetItem(Index: Integer): TdxBarApplicationMenuButton;
    procedure PopulateItemLinks(AItemLinks: TdxBarItemLinks);
    procedure SetItem(Index: Integer; Value: TdxBarApplicationMenuButton);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AApplicationMenu: TdxBarCustomApplicationMenu);
    function Add: TdxBarApplicationMenuButton;
    function Insert(AIndex: Integer): TdxBarApplicationMenuButton;
    //
    property Items[Index: Integer]: TdxBarApplicationMenuButton read GetItem write SetItem; default;
  end;

  { TdxBarApplicationMenuButtonItemLinks }

  TdxBarApplicationMenuButtonItemLinks = class(TdxBarSubMenuControlItemLinks)
  protected
    procedure CalcItemsRects(ARect: TRect);
    function IsScrollable: Boolean; override;
  end;

  { TdxBarCustomApplicationMenu }

  TdxBarCustomApplicationMenu = class(TdxBarCustomPopupMenu)
  private
    FButtons: TdxBarApplicationMenuButtons;
    FExtraPane: TdxBarApplicationMenuExtraPane;
    FExtraPaneEvents: TNotifyEvent;

    // ExtraPane
    function GetExtraPaneWidthRatio: Double;
    function GetExtraPaneSize: Integer;
    function GetExtraPaneItems: TdxBarExtraPaneItems;
    function GetExtraPaneHeader: string;
    function GetOnExtraPaneItemClick: TdxBarExtraPaneNotifyEvent;
    procedure SetExtraPaneWidthRatio(AValue: Double);
    procedure SetExtraPaneSize(AValue: Integer);
    procedure SetExtraPaneItems(AValue: TdxBarExtraPaneItems);
    procedure SetExtraPaneHeader(AValue: string);
    procedure SetOnExtraPaneItemClick(AValue: TdxBarExtraPaneNotifyEvent);

    procedure SetButtons(AValue: TdxBarApplicationMenuButtons);
    procedure SetExtraPane(AValue: TdxBarApplicationMenuExtraPane);
  protected
    function GetControlClass: TCustomdxBarControlClass; override;
    function GetItemLinksClass: TdxBarItemLinksClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetBarManager(Value: TdxBarManager); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsShortCut(AShortCut: TShortCut): Boolean; override;

    property Buttons: TdxBarApplicationMenuButtons read FButtons write SetButtons;
    property ExtraPane: TdxBarApplicationMenuExtraPane read FExtraPane write SetExtraPane;
    property ExtraPaneEvents: TNotifyEvent read FExtraPaneEvents write FExtraPaneEvents;

    // obsolete
    property ExtraPaneWidthRatio: Double read GetExtraPaneWidthRatio write SetExtraPaneWidthRatio;
    property ExtraPaneSize: Integer read GetExtraPaneSize write SetExtraPaneSize;
    property ExtraPaneItems: TdxBarExtraPaneItems read GetExtraPaneItems write SetExtraPaneItems;
    property ExtraPaneHeader: string read GetExtraPaneHeader write SetExtraPaneHeader;
    property OnExtraPaneItemClick: TdxBarExtraPaneNotifyEvent read GetOnExtraPaneItemClick write SetOnExtraPaneItemClick;
  end;

  { TdxBarApplicationMenuControlViewInfo }

  TdxBarApplicationMenuControlViewInfo = class(TdxBarSubMenuControlViewInfo)
  private
    function GetBarControl: TdxBarApplicationMenuControl;
  public
    procedure Calculate; override;
    //
    property BarControl: TdxBarApplicationMenuControl read GetBarControl;
  end;

  { TdxBarExtraPaneControl }

  TdxBarExtraPaneControl = class(TdxBarControl)
  private
    FHeaderRect: TRect;
    FIsControlsCreated: Boolean;
    function CalculateHeaderHeight: Integer;
    function GetActuallyVisibleItemsCount: Integer;
    function GetAllowPinItems: Boolean;
    function GetBasicControl: TdxBarApplicationMenuControl;
    function GetHeaderSeparatorRect: TRect;
    function GetHeaderTextRect: TRect;
    procedure SetHeaderFontTo(ACanvas: TcxCanvas);
  protected
    procedure CalcControlsPositions; override;
    procedure CalcItemsRect; override;
    function CanMoving: Boolean; override;
    function CanProcessMouseMessage: Boolean; override;
    function CanSetMouseSelectedItem(const P: TPoint; AItemControl: TdxBarItemControl): Boolean; override;
    function CanShowHint: Boolean; override;
    function CanShowPopupMenuOnMouseClick(AMousePressed: Boolean): Boolean; override;
    function CanUpdateControlByMouseOnLostFocus: Boolean; override;

    procedure DoCreateControls; override;
    procedure DoDestroyControls; override;
    procedure DoDrawBeginGroup(const ASeparatorRect: TRect; AHorz: Boolean); override;
    procedure DoNCPaint(DC: HDC); override;
    procedure DoHideAll(AReason: TdxBarCloseUpReason); override;
    procedure DrawBackground(ACanvas: TcxCanvas; const ARect: TRect; ABrush: HBRUSH; AColor: TColor);
    procedure DrawContentBackground; override;
    procedure DrawHeaderSeparator(ACanvas: TcxCanvas);
    procedure DrawHeaderText(ACanvas: TcxCanvas);
    procedure FillBackground(ADC: HDC; const ARect: TRect; ABrush: HBRUSH;
      AColor: TColor; AIsClientArea: Boolean); override;
    function FindLinkWithAccel(AKey: Word; AShift: TShiftState;
      ACurrentLink: TdxBarItemLink; out ADuplicate: Boolean): TdxBarItemLink; override;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetBehaviorOptions: TdxBarBehaviorOptions; override;
    function GetPainter: TdxBarPainter; override;
    function GetViewInfoClass: TCustomdxBarControlViewInfoClass; override;
    function IsMeaningParent(AWnd: HWND): Boolean; override;
    procedure LayoutChanged; override;
    procedure Resize; override;
    procedure SetAccelSelectedItem(AItemLink: TdxBarItemLink; ADuplicate: Boolean); override;
    procedure SetMouseSelectedItem(Value: TdxBarItemControl); override;

    procedure InitCustomizationPopup(AItemLinks: TdxBarItemLinks); override;
    procedure ShowPopup(AItem: TdxBarItemControl); override;

    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    //
    property ActuallyVisibleItemsCount: Integer read GetActuallyVisibleItemsCount;
    property AllowPinItems: Boolean read GetAllowPinItems;
    property BasicControl: TdxBarApplicationMenuControl read GetBasicControl;
    property HeaderRect: TRect read FHeaderRect;
    property HeaderSeparatorRect: TRect read GetHeaderSeparatorRect;
    property HeaderTextRect: TRect read GetHeaderTextRect;
  public
    procedure HideAllByEscape; override;
  end;

  { TdxBarExtraPaneDockControl }

  TdxBarExtraPaneDockControl = class(TdxBarDockControl)
  protected
    procedure CalcRowToolbarPositions(ARowIndex: Integer; AClientSize: Integer); override;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetDockedBarControlClass: TdxBarControlClass; override;
    function GetSunkenBorder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxBarExtraPaneControlViewInfo }

  TdxBarExtraPaneControlViewInfo = class(TdxBarControlViewInfo)
  protected
    procedure DoCalcSeparatorInfo(AItemLink: TdxBarItemLink; const AItemRect: TRect); override;
  end;

  { TdxBarExtraPaneButton }

  TdxBarExtraPaneButton = class(TdxBarButton)
  private
    FExtraPaneItem: TdxBarExtraPaneItem;
    function GetPin: Boolean;
    procedure SetExtraPaneItem(AValue: TdxBarExtraPaneItem);
    procedure SetPin(AValue: Boolean);
  protected
    function GetControlClass(AIsVertical: Boolean): TdxBarItemControlClass; override;
    //
    property ExtraPaneItem: TdxBarExtraPaneItem read FExtraPaneItem write SetExtraPaneItem;
    property Pin: Boolean read GetPin write SetPin;
  end;

  { TdxBarExtraPanePinnedButton }

  TdxBarExtraPanePinnedButton = class(TdxBarExtraPaneButton)
  protected
    function GetControlClass(AIsVertical: Boolean): TdxBarItemControlClass; override;
  end;

  { TdxBarExtraPaneButtonControl }

  TdxBarExtraPaneButtonControl = class(TdxBarButtonControl)
  private
    function GetDrawParams: TdxBarExtraMenuButtonControlDrawParams;
    function GetItem: TdxBarExtraPaneButton;
  protected
    procedure DoPaint(ARect: TRect; PaintType: TdxBarPaintType); override;
    function GetDrawParamsClass: TdxBarItemControlDrawParamsClass; override;
    procedure GetTextColors(AEnabled: Boolean; ASelected: Boolean;
      AFlat: Boolean; var AColor1: TColor; var AColor2: TColor); override;
  public
    property DrawParams: TdxBarExtraMenuButtonControlDrawParams read GetDrawParams;
    property Item: TdxBarExtraPaneButton read GetItem;
  end;

  { TdxBarExtraPanePinnedButtonControl }

  TdxBarExtraPanePinnedButtonControl = class(TdxBarExtraPaneButtonControl)
  private
    function GetPin: Boolean;
    procedure SetPin(AValue: Boolean);
  protected
    procedure CalcDrawParams(AFull: Boolean = True); override;
    procedure CalcParts; override;
    procedure ControlUnclick(ByMouse: Boolean); override;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetPartCount: Integer; override;
  public
    property Pin: Boolean read GetPin write SetPin;
  end;

  { TdxBarExtraPaneListItem }

  TdxBarExtraPaneListItem = class(TdxBarListItem)
  private
    FActuallyVisibleItemsCount: Integer;
    FAllowPinItems: Boolean;
    FPaintStyle: TdxBarPaintStyle;
    FOnGetDisplayText: TdxBarExtraPaneGetDisplayTextEvent;
    function CalculatePaintStyle: TdxBarPaintStyle;
    function GetExtraPaneItem(AIndex: Integer): TdxBarExtraPaneItem;
    function GetExtraPaneItemIndex: Integer;
    procedure SetActuallyVisibleItemsCount(AValue: Integer);
  protected
    function CreateItem(AIndex, ACurIndex: Integer): TdxBarButton; override;
    function GetDisplayHint(const AText: string): string; override;
    function GetDisplayText(AItemIndex: Integer): string; override;
    function GetItemClass: TdxBarButtonClass; override;
    function InternalActuallyVisible: Boolean; override;
    procedure PopulateListedItemLinks(AItemLinks: TdxBarItemLinks;
      ALinkData: TObject; AIndex: Integer); override;

    property ActuallyVisibleItemsCount: Integer read FActuallyVisibleItemsCount write SetActuallyVisibleItemsCount;
    property AllowPinItems: Boolean read FAllowPinItems write FAllowPinItems;
    property ExtraPaneItem[AIndex: Integer]: TdxBarExtraPaneItem read GetExtraPaneItem;
    property ExtraPaneItemIndex: Integer read GetExtraPaneItemIndex;
    property OnGetDisplayText: TdxBarExtraPaneGetDisplayTextEvent read FOnGetDisplayText write FOnGetDisplayText;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxBarApplicationMenuControl }

  TdxBarApplicationMenuControl = class(TdxBarSubMenuControl)
  private
    FButtonLinks: TdxBarApplicationMenuButtonItemLinks;
    FButtonsRect: TRect;
    FDockControl: TdxBarDockControl;
    FExtraPaneBar: TdxBar;
    FExtraPaneRect: TRect;
    FExtraPaneSize: Integer;
    FTotalItemLinks: TdxBarItemLinks;
    procedure DoGetExtraPaneListItemData(Sender: TObject);
    function GetApplicationMenu: TdxBarCustomApplicationMenu;
    function GetExtraPane: TdxBarApplicationMenuExtraPane;
    function GetExtraPaneControl: TdxBarExtraPaneControl;
    function GetExtraPaneItemLinks: TdxBarItemLinks;
    function GetHasExtraPane: Boolean;
    function GetTotalItemLinks: TdxBarItemLinks;
  protected
    function CalcChildBarBounds(out ARect: TRect): Boolean; override;
    function CalcExtraPaneWidth(const AMenuBarSize: TSize): Integer; virtual;
    function CanProcessMouseMessage: Boolean; override;
    procedure CalcItemRects(APaintStyle: TdxBarPaintType; ATopIndex: Integer); override;
    procedure CalcItemsRect; override;
    procedure DoCalcSize(out ASize: TSize); override;

    function GetExtraPaneDockControlClass: TdxBarDockControlClass; virtual;
    function GetViewInfoClass: TCustomdxBarControlViewInfoClass; override;

    procedure DoShow; override;

    procedure DrawContent; override;
    procedure DrawContentArea(ACanvas: TcxCanvas);
    procedure DoFillBackgroundEx(ACanvas: TcxCanvas; const ARect: TRect;
      ABrush: HBRUSH; AColor: TColor; AIsClientArea: Boolean); override;

    function DoFindLinkWithAccel(AKey: Word;
      AShift: TShiftState; ACurrentLink: TdxBarItemLink): TdxBarItemLink; override;
    function FindLinkWithAccel(AKey: Word; AShift: TShiftState;
      ACurrentLink: TdxBarItemLink; out ADuplicate: Boolean): TdxBarItemLink; override;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetBehaviorOptions: TdxBarBehaviorOptions; override;
    function GetClientOffset(AIncludeDetachCaption: Boolean = True): TRect; override;
    function GetItemsRectOffset: TRect; override;
    function IsControlExists(ABarItemControl: TdxBarItemControl): Boolean; override;
    procedure SetAccelSelectedItem(AItemLink: TdxBarItemLink; ADuplicate: Boolean); override;
    procedure SetControlVisible(AControl: TdxBarItemControl); override;
    procedure SetItemLinks(Value: TdxBarItemLinks); override;
    procedure SetMouseSelectedItem(Value: TdxBarItemControl); override;

    function CanCreateExtraPaneControl: Boolean;
    procedure CreateExtraPaneControls;
    procedure ShowExtraPaneControl;

    property ApplicationMenu: TdxBarCustomApplicationMenu read GetApplicationMenu;
    property ButtonLinks: TdxBarApplicationMenuButtonItemLinks read FButtonLinks;
    property ExtraPane: TdxBarApplicationMenuExtraPane read GetExtraPane;
    property ExtraPaneControl: TdxBarExtraPaneControl read GetExtraPaneControl;
    property ExtraPaneControlDockControl: TdxBarDockControl read FDockControl;
    property ExtraPaneItemLinks: TdxBarItemLinks read GetExtraPaneItemLinks;
    property HasExtraPane: Boolean read GetHasExtraPane;
    property TotalItemLinks: TdxBarItemLinks read GetTotalItemLinks;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  dxBarStrs, Math;

const
  dxBarExtraPaneHeaderTextIndent = 4;
  dxBarExtraPaneWidthRatio = 2.5;
  dxBarMenuBarMinClientWidth = 240;

type
  TCustomdxBarControlAccess = class(TCustomdxBarControl);
  TdxBarItemLinksAccess = class(TdxBarItemLinks);
  TdxBarItemControlAccess = class(TdxBarItemControl);
  TdxBarItemOptionsAccess = class(TdxBarItemOptions);

  { TdxBarExtraPaneDockControlAccessibilityHelper }

  TdxBarExtraPaneDockControlAccessibilityHelper = class(TdxDockControlAccessibilityHelper)
  protected
    function GetParent: TcxAccessibilityHelper; override;
  end;

  { TdxBarExtraPaneControlAccessibilityHelper }

  TdxBarExtraPaneControlAccessibilityHelper = class(TdxBarControlAccessibilityHelper)
  private
    function GetBarControl: TdxBarExtraPaneControl;
  protected
    procedure DoGetKeyTipsData(AKeyTipsData: TList); override;
    function GetNextAccessibleObject(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
      ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper; override;
    function HandleNavigationKey(var AKey: Word): Boolean; override;
  public
    property BarControl: TdxBarExtraPaneControl read GetBarControl;
  end;

  { TdxBarApplicationMenuControlAccessibilityHelper }

  TdxBarApplicationMenuControlAccessibilityHelper = class(TdxBarSubMenuControlAccessibilityHelper)
  private
    function GetBarControl: TdxBarApplicationMenuControl;
    function GetExtraPaneAccessibilityHelper: TcxAccessibilityHelper;
    function GetHasExtraPane: Boolean;
    function GetHasExtraPaneItemLinksItems: Boolean;
    function GetNextLinks(ACurrentLinks: TdxBarItemLinks): TdxBarItemLinks;
    function GetPreviousLinks(ACurrentLinks: TdxBarItemLinks): TdxBarItemLinks;
    function InternalGetNextItemLink(ACurrentLinks: TdxBarItemLinks;
      ASelectedLink: TdxBarItemLink; AUpKey: Boolean): TdxBarItemLink;
    procedure PopulateNavigationList(AList: TList);
  protected
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;

    procedure InitializeItemKeyTipPosition(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
      var AKeyTipInfo: TdxBarKeyTipInfo); override;
    function GetNextAccessibleObject(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
      ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper; override;
    function GetNextItemLink(AItemLink: TdxBarItemLink;
      AGoForward: Boolean): TdxBarItemLink; override;
    function GetParentForKeyTip: TdxBarAccessibilityHelper; override;

    property BarControl: TdxBarApplicationMenuControl read GetBarControl;
    property ExtraPaneAccessibilityHelper: TcxAccessibilityHelper read GetExtraPaneAccessibilityHelper;
    property HasExtraPane: Boolean read GetHasExtraPane;
    property HasExtraPaneItemLinksItems: Boolean read GetHasExtraPaneItemLinksItems;
  end;

  TdxBarApplicationMenuControlDesignHelper = class(TCustomdxBarControlDesignHelper);

  { TdxBarExtraPanePinnedButtonControlAccessibilityHelper }

  TdxBarExtraPanePinnedButtonControlAccessibilityHelper = class(TdxBarButtonControlAccessibilityHelper)
  private
    function GetPinnedButtonControl: TdxBarExtraPanePinnedButtonControl;
  protected
    function HandleNavigationKey(var AKey: Word): Boolean; override;
    procedure Select(ASetFocus: Boolean); override;
    procedure Unselect(ANextSelectedObject: IdxBarAccessibilityHelper); override;
  public
    property PinnedButtonControl: TdxBarExtraPanePinnedButtonControl read GetPinnedButtonControl;
  end;

  { TdxBarApplicationMenuItemLinks }

  TdxBarApplicationMenuItemLinks = class(TdxBarSubMenuControlItemLinks)
  private
    function GetInternalDefaultValue(AOption: TdxBarItemOptionValue; AItemLinks: TdxBarItemLinks): Integer;
    function GetIsExtraPaneVisible: Boolean;
  protected
    function GetDefaultValue(AOption: TdxBarItemOptionValue): Integer; override;
    function GetOptionsValue(AOption: TdxBarItemOptionValue; AItemLinks: TdxBarItemLinks): Integer; override;
  end;

{ TdxExtraPaneItem }

constructor TdxBarExtraPaneItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  ImageIndex := -1;
end;

procedure TdxBarExtraPaneItem.Assign(Source: TPersistent);
begin
  if Source is TdxBarExtraPaneItem then
  begin
    Data := TdxBarExtraPaneItem(Source).Data;
    DisplayText := TdxBarExtraPaneItem(Source).DisplayText;
    ImageIndex := TdxBarExtraPaneItem(Source).ImageIndex;
    Text := TdxBarExtraPaneItem(Source).Text;
    Pin := TdxBarExtraPaneItem(Source).Pin;
  end
  else
    inherited;
end;

procedure TdxBarExtraPaneItem.SetDisplayText(const Value: string);
begin
  if DisplayText <> Value then
  begin
    FDisplayText := Value;
    Changed(False);
  end;
end;

procedure TdxBarExtraPaneItem.SetText(const Value: string);
begin
  if Text <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;

{ TdxExtraPaneItems }

constructor TdxBarExtraPaneItems.Create(AApplicationMenu: TdxBarCustomApplicationMenu);
begin
  inherited Create(TdxBarExtraPaneItem);
  FApplicationMenu := AApplicationMenu;
end;

function TdxBarExtraPaneItems.Add: TdxBarExtraPaneItem;
begin
  Result := TdxBarExtraPaneItem(inherited Add);
end;

function TdxBarExtraPaneItems.IndexOf(const AItemText: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].Text = AItemText then
    begin
      Result := I;
      Break;
    end;
end;

function TdxBarExtraPaneItems.Insert(AIndex: Integer): TdxBarExtraPaneItem;
begin
  Result := TdxBarExtraPaneItem(inherited Insert(AIndex));
end;

function TdxBarExtraPaneItems.GetOwner: TPersistent;
begin
  Result := FApplicationMenu;
end;

function TdxBarExtraPaneItems.GetItem(Index: Integer): TdxBarExtraPaneItem;
begin
  Result := TdxBarExtraPaneItem(inherited GetItem(Index));
end;

procedure TdxBarExtraPaneItems.SetItem(Index: Integer; Value: TdxBarExtraPaneItem);
begin
  inherited SetItem(Index, Value);
end;

{ TdxBarApplicationMenuExtraPane }

constructor TdxBarApplicationMenuExtraPane.Create(AApplicationMenu: TdxBarCustomApplicationMenu);
begin
  inherited Create;
  FApplicationMenu := AApplicationMenu;
  FWidthRatio := dxBarExtraPaneWidthRatio;
  FItems := TdxBarExtraPaneItems.Create(FApplicationMenu);
  FListItem := TdxBarExtraPaneListItem.Create(FApplicationMenu.BarManager);
  FListItem.OnGetDisplayText := DoGetItemDisplayText;
  FListItem.OnClick := DoItemClick;
  FListItem.Caption := DefaultExtraPaneHeader;
  BarDesignController.RemoveItemFromBarManagerList(FListItem);
  FVisible := True;
  dxResourceStringsRepository.AddListener(Self);
end;

destructor TdxBarApplicationMenuExtraPane.Destroy;
begin
  dxResourceStringsRepository.RemoveListener(Self);
  FreeAndNil(FItems);
  FreeAndNil(FListItem);
  inherited Destroy;
end;

procedure TdxBarApplicationMenuExtraPane.Assign(Source: TPersistent);
var
  AExtraPane: TdxBarApplicationMenuExtraPane;
begin
  if Source is TdxBarApplicationMenuExtraPane then
  begin
    AExtraPane := TdxBarApplicationMenuExtraPane(Source);
    OnItemClick := AExtraPane.OnItemClick;
    AllowPinItems := AExtraPane.AllowPinItems;
    WidthRatio := AExtraPane.WidthRatio;
    Visible := AExtraPane.Visible;
    Header := AExtraPane.Header;
    Items := AExtraPane.Items;
    Size := AExtraPane.Size;
  end
  else
    inherited Assign(Source);
end;

procedure TdxBarApplicationMenuExtraPane.DoItemClick(Sender: TObject);
begin
  if Assigned(OnItemClick) then
    OnItemClick(FApplicationMenu, ListItem.ExtraPaneItemIndex);
end;

procedure TdxBarApplicationMenuExtraPane.DoGetItemDisplayText(
  Sender: TObject; AIndex: Integer; var ADisplayText: string);
begin
  if Assigned(OnGetItemDisplayText) then
    OnGetItemDisplayText(FApplicationMenu, AIndex, ADisplayText);
end;

function TdxBarApplicationMenuExtraPane.IsHeaderStored: Boolean;
begin
  Result := FIsExtraPaneHeaderAssigned;
end;

function TdxBarApplicationMenuExtraPane.IsWidthRatioStored: Boolean;
begin
  Result := FWidthRatio <> dxBarExtraPaneWidthRatio;
end;

procedure TdxBarApplicationMenuExtraPane.PopulateListItem;

  procedure PopulateListItem(AStrings: TStrings);
  var
    I: Integer;
  begin
    AStrings.BeginUpdate;
    try
      AStrings.Clear;
      for I := 0 to Items.Count - 1 do
        AStrings.AddObject(Items[I].Text, Items[I]);
    finally
      AStrings.EndUpdate;
    end;
  end;

begin
  PopulateListItem(ListItem.Items);
end;

procedure TdxBarApplicationMenuExtraPane.TranslationChanged;
begin
  if not FIsExtraPaneHeaderAssigned then
    ListItem.Caption := DefaultExtraPaneHeader;
end;

function TdxBarApplicationMenuExtraPane.GetOwner: TPersistent;
begin
  Result := FApplicationMenu;
end;

function TdxBarApplicationMenuExtraPane.GetHeader: string;
begin
  Result := ListItem.Caption;
end;

function TdxBarApplicationMenuExtraPane.DefaultExtraPaneHeader: string;
begin
  Result := cxGetResourceString(@dxSBAR_EXTRAPANEHEADER);
end;

function TdxBarApplicationMenuExtraPane.GetAllowPinItems: Boolean;
begin
  Result := ListItem.AllowPinItems;
end;

procedure TdxBarApplicationMenuExtraPane.SetHeader(const AHeader: string);
begin
  if ListItem.Caption <> AHeader then
  begin
    FIsExtraPaneHeaderAssigned := AHeader <> DefaultExtraPaneHeader;
    ListItem.Caption := AHeader;
  end;
end;

procedure TdxBarApplicationMenuExtraPane.SetItems(AValue: TdxBarExtraPaneItems);
begin
  FItems.Assign(AValue);
end;

procedure TdxBarApplicationMenuExtraPane.SetAllowPinItems(AValue: Boolean);
begin
  ListItem.AllowPinItems := AValue;
end;

procedure TdxBarApplicationMenuExtraPane.SetSize(AValue: Integer);
begin
  if AValue >= 0 then
    FSize := AValue;
end;

procedure TdxBarApplicationMenuExtraPane.SetWidthRatio(AValue: Double);
begin
  if AValue >= 0 then
    FWidthRatio := AValue;
end;

{ TdxBarApplicationMenuButton }

constructor TdxBarApplicationMenuButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FInternalItem := TdxBarApplicationMenuButtonItem.Create(Self.Collection.FApplicationMenu.BarManager);
  BarDesignController.RemoveItemFromBarManagerList(FInternalItem);
end;

destructor TdxBarApplicationMenuButton.Destroy;
begin
  FreeAndNil(FInternalItem);
  inherited Destroy;
end;

procedure TdxBarApplicationMenuButton.Assign(Source: TPersistent);
var
  ASource: TdxBarApplicationMenuButton;
begin
  if Source is TdxBarApplicationMenuButton then
  begin
    ASource := TdxBarApplicationMenuButton(Source);
    Width := ASource.Width;
    Item := ASource.Item;
  end
  else
    inherited Assign(Source);
end;

function TdxBarApplicationMenuButton.GetCollection: TdxBarApplicationMenuButtons;
begin
  Result := TdxBarApplicationMenuButtons(inherited Collection);
end;

function TdxBarApplicationMenuButton.GetWidth: Integer;
begin
  Result := InternalItem.Width;
end;

function TdxBarApplicationMenuButton.GetItem: TdxBarButton;
begin
  Result := InternalItem.ExternalItem;
end;

procedure TdxBarApplicationMenuButton.SetItem(Value: TdxBarButton);
begin
  InternalItem.ExternalItem := Value;
end;

procedure TdxBarApplicationMenuButton.SetWidth(Value: Integer);
begin
  InternalItem.Width := Value;
end;

{ TdxBarApplicationMenuButtons }

constructor TdxBarApplicationMenuButtons.Create(AApplicationMenu: TdxBarCustomApplicationMenu);
begin
  inherited Create(TdxBarApplicationMenuButton);
  FApplicationMenu := AApplicationMenu;
end;

function TdxBarApplicationMenuButtons.Add: TdxBarApplicationMenuButton;
begin
  Result := TdxBarApplicationMenuButton(inherited Add);
end;

function TdxBarApplicationMenuButtons.Insert(AIndex: Integer): TdxBarApplicationMenuButton;
begin
  Result := TdxBarApplicationMenuButton(inherited Insert(AIndex));
end;

function TdxBarApplicationMenuButtons.GetOwner: TPersistent;
begin
  Result := FApplicationMenu;
end;

function TdxBarApplicationMenuButtons.GetItem(Index: Integer): TdxBarApplicationMenuButton;
begin
  Result := TdxBarApplicationMenuButton(inherited GetItem(Index));
end;

procedure TdxBarApplicationMenuButtons.SetItem(Index: Integer; Value: TdxBarApplicationMenuButton);
begin
  inherited SetItem(Index, Value);
end;

procedure TdxBarApplicationMenuButtons.PopulateItemLinks(AItemLinks: TdxBarItemLinks);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AItemLinks.Add(Items[I].InternalItem);
end;

{ TdxBarApplicationMenuItemLinks }

function TdxBarApplicationMenuItemLinks.GetDefaultValue(AOption: TdxBarItemOptionValue): Integer;
begin
  Result := GetInternalDefaultValue(AOption, nil);
end;

function TdxBarApplicationMenuItemLinks.GetOptionsValue(
  AOption: TdxBarItemOptionValue; AItemLinks: TdxBarItemLinks): Integer;
begin
  if (ParentLinks <> nil) or TdxBarItemOptionsAccess(ItemOptions).IsValueStored(AOption) then
    Result := inherited GetOptionsValue(AOption, AItemLinks)
  else
    Result := GetInternalDefaultValue(AOption, AItemLinks);
end;

function TdxBarApplicationMenuItemLinks.GetInternalDefaultValue(
  AOption: TdxBarItemOptionValue; AItemLinks: TdxBarItemLinks): Integer;
begin
  case AOption of
    ioShowDescriptions:
      Result := Ord((AItemLinks <> Self) or not GetIsExtraPaneVisible);
    ioShowShortCuts:
      Result := Ord(False);
  else {ioSize}
    Result := Ord(misLarge);
  end;
end;

function TdxBarApplicationMenuItemLinks.GetIsExtraPaneVisible: Boolean;
begin
  Result := (Owner as TdxBarCustomApplicationMenu).ExtraPane.Visible;
end;

{ TdxBarApplicationMenuButtonItemLinks }

procedure TdxBarApplicationMenuButtonItemLinks.CalcItemsRects(ARect: TRect);
const
  ApplicationMenuButtonsControlOffset = 6; // !!!
var
  I: Integer;
  AItemLink: TdxBarItemLink;
  AItemRect: TRect;
  AOffsetItemBounds: Integer;
begin
  EmptyItemRects;
  AItemRect := ARect;
  for I := 0 to VisibleItemCount - 1 do
  begin
    AItemLink := VisibleItems[I];
    AItemLink.CreateControl;
    AItemRect.Right := AItemRect.Left + TdxBarItemControlAccess(AItemLink.Control).Width;
    if AItemRect.Right > ARect.Right then
      Break;
    AItemLink.ItemRect := AItemRect;
    AItemRect.Left := AItemRect.Right + ApplicationMenuButtonsControlOffset;
  end;

  if RealVisibleItemCount > 0 then
    AOffsetItemBounds := ARect.Right - VisibleItems[RealVisibleItemCount - 1].ItemRect.Right
  else
    AOffsetItemBounds := 0;

  for I := 0 to RealVisibleItemCount - 1 do
  begin
    AItemLink := VisibleItems[I];
    AItemLink.ItemRect := cxRectOffsetHorz(AItemLink.ItemRect, AOffsetItemBounds);
  end;

  if BarControl.UseRightToLeftAlignment then
    for I := 0 to VisibleItemCount - 1 do
    begin
      AItemLink := VisibleItems[I];
      AItemLink.ItemRect := TdxRightToLeftLayoutConverter.ConvertRect(AItemLink.ItemRect, ARect);
    end;
end;

function TdxBarApplicationMenuButtonItemLinks.IsScrollable: Boolean;
begin
  Result := False;
end;

{ TdxBarCustomApplicationMenu }

constructor TdxBarCustomApplicationMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExtraPane := TdxBarApplicationMenuExtraPane.Create(Self);
  FButtons := TdxBarApplicationMenuButtons.Create(Self);
end;

destructor TdxBarCustomApplicationMenu.Destroy;
begin
  FreeAndNil(FButtons);
  FreeAndNil(FExtraPane);
  inherited Destroy;
end;

function TdxBarCustomApplicationMenu.GetControlClass: TCustomdxBarControlClass;
begin
  Result := TdxBarApplicationMenuControl;
end;

function TdxBarCustomApplicationMenu.GetItemLinksClass: TdxBarItemLinksClass;
begin
  Result := TdxBarApplicationMenuItemLinks;
end;

procedure TdxBarCustomApplicationMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FBarManager) and (Operation = opRemove) then
  begin
    inherited;
    Free;
  end
  else
    inherited;
end;

procedure TdxBarCustomApplicationMenu.SetBarManager(Value: TdxBarManager);
begin
  if Value <> nil then
    inherited;
end;

function TdxBarCustomApplicationMenu.IsShortCut(AShortCut: TShortCut): Boolean;
begin
  Result := TdxBarItemLinksAccess(ItemLinks).IsShortCut(AShortCut);
end;

function TdxBarCustomApplicationMenu.GetExtraPaneWidthRatio: Double;
begin
  Result := FExtraPane.WidthRatio;
end;

function TdxBarCustomApplicationMenu.GetExtraPaneSize: Integer;
begin
  Result := FExtraPane.Size;
end;

function TdxBarCustomApplicationMenu.GetExtraPaneItems: TdxBarExtraPaneItems;
begin
  Result := FExtraPane.Items;
end;

function TdxBarCustomApplicationMenu.GetExtraPaneHeader: string;
begin
  Result := FExtraPane.Header;
end;

function TdxBarCustomApplicationMenu.GetOnExtraPaneItemClick: TdxBarExtraPaneNotifyEvent;
begin
  Result := FExtraPane.OnItemClick;
end;

procedure TdxBarCustomApplicationMenu.SetButtons(AValue: TdxBarApplicationMenuButtons);
begin
  FButtons.Assign(AValue);
end;

procedure TdxBarCustomApplicationMenu.SetExtraPane(AValue: TdxBarApplicationMenuExtraPane);
begin
  FExtraPane.Assign(AValue);
end;

procedure TdxBarCustomApplicationMenu.SetExtraPaneWidthRatio(AValue: Double);
begin
  FExtraPane.WidthRatio := AValue;
end;

procedure TdxBarCustomApplicationMenu.SetExtraPaneSize(AValue: Integer);
begin
  FExtraPane.Size := AValue;
end;

procedure TdxBarCustomApplicationMenu.SetExtraPaneItems(AValue: TdxBarExtraPaneItems);
begin
  FExtraPane.Items := AValue;
end;

procedure TdxBarCustomApplicationMenu.SetExtraPaneHeader(AValue: string);
begin
  FExtraPane.Header := AValue;
end;

procedure TdxBarCustomApplicationMenu.SetOnExtraPaneItemClick(AValue: TdxBarExtraPaneNotifyEvent);
begin
  FExtraPane.OnItemClick := AValue;
end;

{ TdxBarExtraPaneDockControl }

constructor TdxBarExtraPaneDockControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AllowDocking := False;
end;

procedure TdxBarExtraPaneDockControl.CalcRowToolbarPositions(ARowIndex: Integer; AClientSize: Integer);
var
  AToolbar: TdxBarControl;
  R: TRect;
begin
  R := ClientRect;
  AToolbar := Rows[0].Cols[0].BarControl;
  if (AToolbar.Left <> R.Left) or (AToolbar.Top <> R.Top) or
    (AToolbar.Width <> cxRectWidth(R)) or (AToolbar.Height <> cxRectHeight(R)) then
  begin
    if AToolbar.HandleAllocated then
    begin
      SetWindowPos(AToolbar.Handle, 0, R.Left, R.Top,
        cxRectWidth(R), cxRectHeight(R), SWP_NOZORDER or SWP_NOACTIVATE);
      SendMessage(AToolbar.Handle, WM_NCPAINT, 0, 0);
    end
    else
      AToolbar.SetBounds(R.Left, R.Top, cxRectWidth(R), cxRectHeight(R));
  end;
end;

function TdxBarExtraPaneDockControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxBarExtraPaneDockControlAccessibilityHelper;
end;

function TdxBarExtraPaneDockControl.GetDockedBarControlClass: TdxBarControlClass;
begin
  Result := TdxBarExtraPaneControl;
end;

function TdxBarExtraPaneDockControl.GetSunkenBorder: Boolean;
begin
  Result := False;
end;

{ TdxBarExtraPaneDockControlAccessibilityHelper }

function TdxBarExtraPaneDockControlAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := TdxBarApplicationMenuControl(DockControl.Parent).IAccessibilityHelper.GetBarHelper;
end;

{ TdxBarExtraPaneControl }

procedure TdxBarExtraPaneControl.CalcControlsPositions;
begin
  if not FIsControlsCreated then
    CreateControls;
  inherited CalcControlsPositions;
  CalcItemRects(ptMenu);
end;

procedure TdxBarExtraPaneControl.CalcItemsRect;
begin
  inherited CalcItemsRect;
  Inc(FItemsRect.Left, Painter.ExtraMenuSeparatorSize);
  FHeaderRect := cxRectSetHeight(FItemsRect, CalculateHeaderHeight);
  FItemsRect.Top := HeaderRect.Bottom;
  if UseRightToLeftAlignment then
  begin
    FItemsRect := TdxRightToLeftLayoutConverter.ConvertRect(FItemsRect, ClientRect);
    FHeaderRect := TdxRightToLeftLayoutConverter.ConvertRect(FHeaderRect, ClientRect);
  end;
end;

function TdxBarExtraPaneControl.CanMoving: Boolean;
begin
  Result := False;
end;

function TdxBarExtraPaneControl.CanProcessMouseMessage: Boolean;
begin
  Result := inherited CanProcessMouseMessage or BasicControl.CanProcessMouseMessage;
end;

procedure TdxBarExtraPaneControl.DoCreateControls;
begin
  FIsControlsCreated := True;
  if not IsCustomizing then
  begin
    CalcItemsRect;
    ExpandContainerItems;
  end;
  inherited DoCreateControls;
end;

procedure TdxBarExtraPaneControl.DoDestroyControls;
begin
  inherited DoDestroyControls;
  if not IsCustomizing then
    CollapseContainerItems;
  FIsControlsCreated := False;
end;

procedure TdxBarExtraPaneControl.DoDrawBeginGroup(const ASeparatorRect: TRect; AHorz: Boolean);
begin
  Painter.ExtraMenuControlDrawBeginGroup(Self, Canvas, ASeparatorRect, ToolbarBrush, AHorz);
end;

procedure TdxBarExtraPaneControl.DoNCPaint(DC: HDC);
begin
  // do nothing
end;

procedure TdxBarExtraPaneControl.DoHideAll(AReason: TdxBarCloseUpReason);
begin
  inherited;
  BasicControl.HideAll;
end;

procedure TdxBarExtraPaneControl.DrawContentBackground;
begin
  DrawBackground(Canvas, ClientRect, ToolbarBrush, clNone);
  DrawHeaderSeparator(Canvas);
  DrawHeaderText(Canvas);
end;

procedure TdxBarExtraPaneControl.DrawHeaderSeparator(ACanvas: TcxCanvas);
begin
  DoDrawBeginGroup(HeaderSeparatorRect, True);
end;

procedure TdxBarExtraPaneControl.DrawHeaderText(ACanvas: TcxCanvas);
var
  AAlignment: TAlignment;
begin
  ACanvas.SaveState;
  try
    SetHeaderFontTo(ACanvas);
    ACanvas.Brush.Style := bsClear;
    AAlignment := taLeftJustify;
    if UseRightToLeftAlignment then
      ChangeBiDiModeAlignment(AAlignment);
    ACanvas.DrawTexT(Caption, HeaderTextRect, AAlignment, vaCenter, False, False);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TdxBarExtraPaneControl.FillBackground(ADC: HDC;
  const ARect: TRect; ABrush: HBRUSH; AColor: TColor; AIsClientArea: Boolean);
begin
  cxPaintCanvas.BeginPaint(ADC);
  try
    DrawBackground(cxPaintCanvas, ARect, ABrush, AColor);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

function TdxBarExtraPaneControl.FindLinkWithAccel(AKey: Word; AShift: TShiftState;
  ACurrentLink: TdxBarItemLink; out ADuplicate: Boolean): TdxBarItemLink;
begin
  Result := BasicControl.FindLinkWithAccel(AKey, AShift, ACurrentLink, ADuplicate);
end;

function TdxBarExtraPaneControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxBarExtraPaneControlAccessibilityHelper;
end;

function TdxBarExtraPaneControl.GetBehaviorOptions: TdxBarBehaviorOptions;
begin
  Result := BasicControl.BehaviorOptions;
end;

function TdxBarExtraPaneControl.GetPainter: TdxBarPainter;
begin
  Result := BasicControl.Painter;
end;

function TdxBarExtraPaneControl.GetViewInfoClass: TCustomdxBarControlViewInfoClass;
begin
  Result := TdxBarExtraPaneControlViewInfo;
end;

function TdxBarExtraPaneControl.IsMeaningParent(AWnd: HWND): Boolean;
begin
  Result := BasicControl.IsMeaningParent(AWnd) or
    BasicControl.HandleAllocated and (BasicControl.Handle = AWnd);
end;

procedure TdxBarExtraPaneControl.HideAllByEscape;
begin
  BasicControl.HideByEscape;
end;

procedure TdxBarExtraPaneControl.LayoutChanged;
begin
  // do nothing;
end;

procedure TdxBarExtraPaneControl.Resize;
begin
  inherited Resize;
  RecreateControls;
end;

procedure TdxBarExtraPaneControl.SetAccelSelectedItem(AItemLink: TdxBarItemLink; ADuplicate: Boolean);
begin
  BasicControl.SetAccelSelectedItem(AItemLink, ADuplicate);
end;

procedure TdxBarExtraPaneControl.SetMouseSelectedItem(Value: TdxBarItemControl);
begin
  if not IsActive then
    BarNavigationController.ChangeSelectedObject(True, IAccessibilityHelper);
  inherited SetMouseSelectedItem(Value);
end;

function TdxBarExtraPaneControl.CanSetMouseSelectedItem(
  const P: TPoint; AItemControl: TdxBarItemControl): Boolean;
begin
  Result := True;
end;

function TdxBarExtraPaneControl.CanShowHint: Boolean;
begin
  Result := True;
end;

function TdxBarExtraPaneControl.CanShowPopupMenuOnMouseClick(AMousePressed: Boolean): Boolean;
begin
  Result := BasicControl.CanShowPopupMenuOnMouseClick(AMousePressed);
end;

function TdxBarExtraPaneControl.CanUpdateControlByMouseOnLostFocus: Boolean;
begin
  Result := False;
end;

function TdxBarExtraPaneControl.CalculateHeaderHeight: Integer;
begin
  SetHeaderFontTo(cxScreenCanvas);
  Result := cxTextHeight(cxScreenCanvas.Handle) +
    Painter.ExtraMenuSeparatorSize + 2 * dxBarExtraPaneHeaderTextIndent;
  cxScreenCanvas.Dormant;
end;

procedure TdxBarExtraPaneControl.DrawBackground(
  ACanvas: TcxCanvas; const ARect: TRect; ABrush: HBRUSH; AColor: TColor);
begin
  Painter.ExtraMenuControlDrawBackground(Self, ACanvas, ARect, ABrush, AColor);
end;

procedure TdxBarExtraPaneControl.InitCustomizationPopup(AItemLinks: TdxBarItemLinks);
begin
  BasicControl.InitCustomizationPopup(AItemLinks);
end;

procedure TdxBarExtraPaneControl.ShowPopup(AItem: TdxBarItemControl);
begin
  BasicControl.ShowPopup(AItem);
end;

procedure TdxBarExtraPaneControl.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  if Message.CalcValidRects then
    Message.Result := 0
  else
    inherited;
end;

function TdxBarExtraPaneControl.GetActuallyVisibleItemsCount: Integer;
begin
  Result := cxRectHeight(ItemsRect) div Painter.GetButtonHeight(Painter.GetSmallIconSize(ScaleFactor), GetTextSize, ScaleFactor);
end;

function TdxBarExtraPaneControl.GetAllowPinItems: Boolean;
begin
  Result := BasicControl.ExtraPane.AllowPinItems;
end;

function TdxBarExtraPaneControl.GetBasicControl: TdxBarApplicationMenuControl;
begin
  Result := TdxBarApplicationMenuControl(DockControl.Parent);
end;

function TdxBarExtraPaneControl.GetHeaderSeparatorRect: TRect;
begin
  Result := cxRectSetBottom(HeaderRect, HeaderRect.Bottom, Painter.ExtraMenuSeparatorSize);
end;

function TdxBarExtraPaneControl.GetHeaderTextRect: TRect;
begin
  Result := HeaderRect;
  Result.Bottom := HeaderSeparatorRect.Top;
  InflateRect(Result, -dxBarExtraPaneHeaderTextIndent, -dxBarExtraPaneHeaderTextIndent);
end;

procedure TdxBarExtraPaneControl.SetHeaderFontTo(ACanvas: TcxCanvas);
begin
  ACanvas.Font := Font;
  ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
  ACanvas.Font.Color := Painter.ExtraMenuHeaderTextColor;
end;

{ TdxBarExtraPaneControlViewInfo }

procedure TdxBarExtraPaneControlViewInfo.DoCalcSeparatorInfo(
  AItemLink: TdxBarItemLink; const AItemRect: TRect);
begin
  AddSeparatorInfo(cxRectSetBottom(AItemRect, AItemRect.Top,
    TCustomdxBarControlAccess(BarControl).BeginGroupSize),
    skHorizontal, AItemLink.Control);
end;

{ TdxBarExtraPaneControlAccessibilityHelper }

procedure TdxBarExtraPaneControlAccessibilityHelper.DoGetKeyTipsData(AKeyTipsData: TList);
begin
end;

function TdxBarExtraPaneControlAccessibilityHelper.GetNextAccessibleObject(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;

  function BasicControlObject: TdxBarApplicationMenuControlAccessibilityHelper;
  begin
    Result := TdxBarApplicationMenuControlAccessibilityHelper(
      BarControl.BasicControl.IAccessibilityHelper.GetHelper);
  end;

begin
  Result := BasicControlObject.GetNextAccessibleObject(AItemLinkHelper, ADirection);
end;

function TdxBarExtraPaneControlAccessibilityHelper.HandleNavigationKey(var AKey: Word): Boolean;
begin
  Result := False;
  if AKey <> VK_TAB then
    Result := inherited HandleNavigationKey(AKey)
  else
    if KeyboardStateToShiftState = [] then
      AKey := VK_DOWN
    else
      AKey := VK_UP;
end;

function TdxBarExtraPaneControlAccessibilityHelper.GetBarControl: TdxBarExtraPaneControl;
begin
  Result := TdxBarExtraPaneControl(inherited BarControl);
end;

{ TdxBarApplicationMenuControl }

constructor TdxBarApplicationMenuControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonLinks := TdxBarApplicationMenuButtonItemLinks.Create(BarManager, nil);
  FButtonLinks.BarControl := Self;
end;

destructor TdxBarApplicationMenuControl.Destroy;
begin
  FreeAndNil(FButtonLinks);
  FreeAndNil(FTotalItemLinks);
  FreeAndNil(FExtraPaneBar);
  FreeAndNil(FDockControl);
  inherited Destroy;
end;

procedure TdxBarApplicationMenuControl.CalcItemRects(
  APaintStyle: TdxBarPaintType; ATopIndex: Integer);
begin
  inherited CalcItemRects(APaintStyle, ATopIndex);
  ButtonLinks.CalcItemsRects(FButtonsRect);
end;

procedure TdxBarApplicationMenuControl.CalcItemsRect;
begin
  inherited CalcItemsRect;
  FButtonsRect := FItemsRect;
  FButtonsRect.Top := FItemsRect.Bottom;
  FButtonsRect.Bottom := ContentRect.Bottom;
  FButtonsRect := cxRectContent(FButtonsRect, Painter.ApplicationMenuContentButtonOffsets);

  FExtraPaneRect := FItemsRect;
  Dec(FItemsRect.Right, FExtraPaneSize);
  FExtraPaneRect.Left := FItemsRect.Right;
  if UseRightToLeftAlignment then
  begin
    FItemsRect := TdxRightToLeftLayoutConverter.ConvertRect(FItemsRect, ClientRect);
    FExtraPaneRect := TdxRightToLeftLayoutConverter.ConvertRect(FExtraPaneRect, ClientRect);
    FButtonsRect := TdxRightToLeftLayoutConverter.ConvertRect(FButtonsRect, ClientRect);
  end;
end;

function TdxBarApplicationMenuControl.CalcChildBarBounds(out ARect: TRect): Boolean;
begin
  Result := HasExtraPane;
  if Result then
    ARect := dxMapWindowRect(Handle, 0, FExtraPaneRect);
end;

function TdxBarApplicationMenuControl.CalcExtraPaneWidth(const AMenuBarSize: TSize): Integer;
begin
  if not HasExtraPane then
    Result := 0
  else
    if ExtraPane.Size <> 0 then
      Result := MulDiv(ExtraPane.Size, TextSize, Painter.SubMenuControlNormalItemHeight(ScaleFactor))
    else
      Result := Round(AMenuBarSize.cx * ExtraPane.WidthRatio);
end;

function TdxBarApplicationMenuControl.CanProcessMouseMessage: Boolean;
begin
  Result := True;
end;

procedure TdxBarApplicationMenuControl.DoCalcSize(out ASize: TSize);
begin
  inherited DoCalcSize(ASize);
  FExtraPaneSize := CalcExtraPaneWidth(ASize);
  ASize.cx := Max(ASize.cx + FExtraPaneSize, dxBarMenuBarMinClientWidth);
end;

function TdxBarApplicationMenuControl.GetViewInfoClass: TCustomdxBarControlViewInfoClass;
begin
  Result := TdxBarApplicationMenuControlViewInfo;
end;

function TdxBarApplicationMenuControl.GetExtraPaneDockControlClass: TdxBarDockControlClass;
begin
  Result := TdxBarExtraPaneDockControl;
end;

procedure TdxBarApplicationMenuControl.DoShow;
begin
  ApplicationMenu.Buttons.PopulateItemLinks(ButtonLinks);
  if HasExtraPane then
  begin
    ExtraPane.PopulateListItem;
    ExtraPaneItemLinks.Add(ExtraPane.ListItem);
    ExtraPane.ListItem.OnGetData := DoGetExtraPaneListItemData;
  end;
  HandleNeeded;
  CalcControlsPositions;
  if HasExtraPane then
    ShowExtraPaneControl;
  inherited DoShow;
end;

procedure TdxBarApplicationMenuControl.DrawContent;
begin
  inherited DrawContent;
  DrawContentArea(Canvas);
end;

procedure TdxBarApplicationMenuControl.DrawContentArea(ACanvas: TcxCanvas);
var
  AItemsArea: TRect;
begin
  UnionRect(AItemsArea, ItemsRect, FExtraPaneRect);
  UnionRect(AItemsArea, AItemsArea, BarRect);
  ACanvas.SaveClipRegion;
  try
    ACanvas.SetClipRegion(TcxRegion.Create(ContentRect), roIntersect);
    Painter.ApplicationMenuDrawBackground(Self, ACanvas, ContentRect, AItemsArea);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxBarApplicationMenuControl.DoFillBackgroundEx(ACanvas: TcxCanvas;
  const ARect: TRect; ABrush: HBRUSH; AColor: TColor; AIsClientArea: Boolean);
begin
  inherited DoFillBackgroundEx(ACanvas, ARect, ABrush, AColor, AIsClientArea);
  DrawContentArea(ACanvas);
end;

function TdxBarApplicationMenuControl.DoFindLinkWithAccel(
  AKey: Word; AShift: TShiftState; ACurrentLink: TdxBarItemLink): TdxBarItemLink;
begin
  Result := TdxBarItemLinksAccess(TotalItemLinks).FindItemWithAccel(AKey, AShift, ACurrentLink);
end;

procedure TdxBarApplicationMenuControl.DoGetExtraPaneListItemData(Sender: TObject);
begin
  ExtraPane.ListItem.ActuallyVisibleItemsCount := ExtraPaneControl.ActuallyVisibleItemsCount;
end;

function TdxBarApplicationMenuControl.FindLinkWithAccel(
  AKey: Word; AShift: TShiftState; ACurrentLink: TdxBarItemLink;
  out ADuplicate: Boolean): TdxBarItemLink;

  procedure PopulateTotalItemLinks;

    procedure InternalAssign(AItemLinks: TdxBarItemLinks);
    var
      I: Integer;
      ALink: TdxBarItemLink;
    begin
      if AItemLinks <> nil then
        for I := 0 to AItemLinks.Count - 1 do
        begin
          ALink := TotalItemLinks.Add;
          ALink.Assign(AItemLinks[I]);
          ALink.Data := TdxNativeInt(AItemLinks[I]);
        end;
    end;

  begin
    TotalItemLinks.BeginUpdate;
    try
      TotalItemLinks.Clear;
      InternalAssign(ItemLinks);
      InternalAssign(ExtraPaneItemLinks);
      InternalAssign(ButtonLinks);
    finally
      TotalItemLinks.EndUpdate;
    end;
  end;

  function ImportCurrentLink(ACurrentLink: TdxBarItemLink): TdxBarItemLink;
  var
    I: Integer;
  begin
    Result := nil;
    if ACurrentLink <> nil then
      for I := 0 to TotalItemLinks.Count - 1 do
        if TotalItemLinks[I].Data = TdxNativeInt(ACurrentLink) then
        begin
          Result := TotalItemLinks[I];
          Break;
        end;
  end;

  function ExportCurrentLink(ACurrentLink: TdxBarItemLink): TdxBarItemLink;
  begin
    if ACurrentLink <> nil then
      Result := TdxBarItemLink(ACurrentLink.Data)
    else
      Result := nil;
  end;

begin
  PopulateTotalItemLinks;
  Result := ExportCurrentLink(inherited FindLinkWithAccel(
    AKey, AShift, ImportCurrentLink(ACurrentLink), ADuplicate));
end;

function TdxBarApplicationMenuControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxBarApplicationMenuControlAccessibilityHelper;
end;

function TdxBarApplicationMenuControl.GetBehaviorOptions: TdxBarBehaviorOptions;
begin
  Result := [bboAllowSelectWindowItemsWithoutFocusing, bboAllowShowHints, bboClickItemsBySpaceKey,
    bboExtendItemWhenAlignedToClient, bboUnmoved, bboCanShowPopupMenuOnMouseClick];
end;

function TdxBarApplicationMenuControl.GetClientOffset(AIncludeDetachCaption: Boolean = True): TRect;
begin
  Result := cxNullRect;
end;

function TdxBarApplicationMenuControl.GetItemsRectOffset: TRect;
const
  IndentBetweenTextAndBorders = 3;
var
  AFrameSizes: TRect;
begin
  Result := inherited GetItemsRectOffset;

  if not (csDesigning in ComponentState) then
  begin
    AFrameSizes := Painter.ApplicationMenuGetFrameSizes;
    AFrameSizes.Bottom := Max(AFrameSizes.Bottom,
      cxTextHeight(Font) + 2 * IndentBetweenTextAndBorders +
      cxMarginsHeight(Painter.ApplicationMenuContentButtonOffsets));

    Inc(Result.Bottom, AFrameSizes.Bottom);
    Inc(Result.Left, AFrameSizes.Left);
    Inc(Result.Right, AFrameSizes.Right);
    Inc(Result.Top, AFrameSizes.Top);
  end;
end;

function TdxBarApplicationMenuControl.IsControlExists(ABarItemControl: TdxBarItemControl): Boolean;
begin
  Result := inherited IsControlExists(ABarItemControl) or (ButtonLinks.IsControlExists(ABarItemControl));
end;

procedure TdxBarApplicationMenuControl.SetAccelSelectedItem(AItemLink: TdxBarItemLink; ADuplicate: Boolean);
begin
  if BarNavigationController.SelectedObject <> nil then
    BarNavigationController.SelectedObject.Unselect(AItemLink.Control.IAccessibilityHelper);
  AItemLink.Control.IAccessibilityHelper.Select(True);
  if not ADuplicate then
    SendMessage(AItemLink.Control.Parent.Handle, WM_KEYDOWN, VK_RETURN, 0);
end;

procedure TdxBarApplicationMenuControl.SetControlVisible(AControl: TdxBarItemControl);
begin
  if TdxBarItemControlAccess(AControl).GetPaintType = ptMenu then
    inherited SetControlVisible(AControl);
end;

procedure TdxBarApplicationMenuControl.SetItemLinks(Value: TdxBarItemLinks);
begin
  inherited SetItemLinks(Value);
  FButtonLinks.FLinksOwner := ApplicationMenu;
  if CanCreateExtraPaneControl then
    CreateExtraPaneControls;
end;

procedure TdxBarApplicationMenuControl.SetMouseSelectedItem(Value: TdxBarItemControl);
begin
  if HasExtraPane and ExtraPaneControl.IsActive then
    ExtraPaneControl.IsActive := False;
  if not IsActive then
    BarNavigationController.ChangeSelectedObject(True, IAccessibilityHelper);
  inherited SetMouseSelectedItem(Value);
end;

function TdxBarApplicationMenuControl.CanCreateExtraPaneControl: Boolean;
begin
  //Note: IsCustomizing - to prohibit FBar from appearing in the Object TreeView
  Result := (FExtraPaneBar = nil) and not IsCustomizing and ExtraPane.Visible;
end;

procedure TdxBarApplicationMenuControl.CreateExtraPaneControls;
begin
  FDockControl := GetExtraPaneDockControlClass.Create(Owner);
  FDockControl.Align := dalNone;
  FDockControl.BarManager := BarManager;
  FDockControl.Parent := Self;

  BarManager.Bars.BeginUpdate;
  try
    FExtraPaneBar := BarManager.Bars.Add;
    FExtraPaneBar.Caption := ExtraPane.Header;
    FExtraPaneBar.AllowQuickCustomizing := False;
    FExtraPaneBar.AllowCustomizing := False;
    FExtraPaneBar.NotDocking := [dsNone];
    FExtraPaneBar.Hidden := True;
    FExtraPaneBar.Font := Font;
    FExtraPaneBar.UseOwnFont := False;
  finally
    BarManager.Bars.EndUpdate(False);
  end;
end;

procedure TdxBarApplicationMenuControl.ShowExtraPaneControl;
begin
  FExtraPaneBar.DockControl := FDockControl;
  FExtraPaneBar.BorderStyle := bbsNone;
  FExtraPaneBar.Visible := True;
  SetWindowPos(FDockControl.Handle, 0, FExtraPaneRect.Left, FExtraPaneRect.Top,
    cxRectWidth(FExtraPaneRect), cxRectHeight(FExtraPaneRect),
    SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW);
end;

function TdxBarApplicationMenuControl.GetApplicationMenu: TdxBarCustomApplicationMenu;
begin
  Result := TdxBarCustomApplicationMenu(ItemLinks.Owner);
end;

function TdxBarApplicationMenuControl.GetExtraPane: TdxBarApplicationMenuExtraPane;
begin
  Result := ApplicationMenu.ExtraPane;
end;

function TdxBarApplicationMenuControl.GetExtraPaneControl: TdxBarExtraPaneControl;
begin
  Result := FExtraPaneBar.Control as TdxBarExtraPaneControl;
end;

function TdxBarApplicationMenuControl.GetExtraPaneItemLinks: TdxBarItemLinks;
begin
  if HasExtraPane then
    Result := FExtraPaneBar.ItemLinks
  else
    Result := nil;
end;

function TdxBarApplicationMenuControl.GetTotalItemLinks: TdxBarItemLinks;
begin
  if FTotalItemLinks = nil then
    FTotalItemLinks := TdxBarSubMenuControlItemLinks.Create(BarManager, ApplicationMenu);
  Result := FTotalItemLinks;
end;

function TdxBarApplicationMenuControl.GetHasExtraPane: Boolean;
begin
  Result := Assigned(FExtraPaneBar);
end;

{ TdxBarApplicationMenuControlViewInfo }

function TdxBarApplicationMenuControlViewInfo.GetBarControl: TdxBarApplicationMenuControl;
begin
  Result := TdxBarApplicationMenuControl(inherited BarControl);
end;

procedure TdxBarApplicationMenuControlViewInfo.Calculate;
var
  I: Integer;
  AControl: TdxBarItemControl;
  ALinks: TdxBarItemLinks;
  AVisibleItemRect: TRect;
begin
  inherited Calculate;
  if ItemControlCount > 0 then
  begin
    ALinks := BarControl.ButtonLinks;
    for I := 0 to ALinks.VisibleItemCount - 1 do
    begin
      AControl := ALinks.VisibleItems[I].Control;
      AVisibleItemRect := BarControl.GetItemRect(AControl);
      if not IsRectEmpty(AVisibleItemRect) then
      begin
        AddItemControlViewInfo(AControl.ViewInfo);
        IdxBarItemControlViewInfo(AControl.ViewInfo).SetBounds(AVisibleItemRect);
      end;
    end;
  end;
end;

{ TdxBarApplicationMenuControlAccessibilityHelper }

function TdxBarApplicationMenuControlAccessibilityHelper.GetChild(
  AIndex: Integer): TcxAccessibilityHelper;
begin
  if HasExtraPane and (AIndex = ChildCount - 1) then
    Result := ExtraPaneAccessibilityHelper
  else
    Result := inherited GetChild(AIndex);
end;

function TdxBarApplicationMenuControlAccessibilityHelper.GetChildCount: Integer;
begin
  Result := inherited GetChildCount + Ord(HasExtraPane);
end;

function TdxBarApplicationMenuControlAccessibilityHelper.GetChildIndex(
  AChild: TcxAccessibilityHelper): Integer;
begin
  if HasExtraPane and (AChild = ExtraPaneAccessibilityHelper) then
    Result := inherited GetChildCount
  else
    Result := inherited GetChildIndex(AChild);
end;

procedure TdxBarApplicationMenuControlAccessibilityHelper.InitializeItemKeyTipPosition(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; var AKeyTipInfo: TdxBarKeyTipInfo);
begin
  if BarControl.ButtonLinks.IndexOf(AItemLinkHelper.ItemLink) <> -1 then
  begin
    AKeyTipInfo.BasePoint := cxRectCenter(GetItemScreenBounds(AItemLinkHelper));
    AKeyTipInfo.HorzAlign := taCenter;
    AKeyTipInfo.VertAlign := vaBottom;
  end
  else
    inherited;
end;

function TdxBarApplicationMenuControlAccessibilityHelper.GetNextAccessibleObject(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
  ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;

  function GetScreenBoundsForNavigation(AItemControl: TdxBarItemControl): TRect;
  begin
    if BarControl.ItemLinks.IsControlExists(AItemControl) then
    begin
      Result := AItemControl.ItemBounds;
      Result.Top := BarControl.ItemsRect.Top;
      if HasExtraPaneItemLinksItems then
        Result.Bottom := BarControl.ItemsRect.Bottom
      else
        Result.Bottom := BarControl.ContentRect.Bottom;

      Result.BottomRight := BarControl.ClientToScreen(Result.BottomRight);
      Result.TopLeft := BarControl.ClientToScreen(Result.TopLeft);
    end
    else
      Result := AItemControl.IAccessibilityHelper.GetBarHelper.GetScreenBounds(cxAccessibleObjectSelfID);
  end;

var
  AObjects: TList;
  AItemLink: TdxBarItemLink;
  AItemControl: TdxBarItemControl;
begin
  if ADirection in [andUp, andDown] then
  begin
    if HasExtraPane and (TCustomdxBarControlAccess(BarControl.FExtraPaneBar.Control).SelectedLink <> nil) then
      AItemLink := InternalGetNextItemLink(BarControl.ExtraPaneItemLinks,
        TCustomdxBarControlAccess(BarControl.FExtraPaneBar.Control).SelectedLink,
        ADirection = andUp)
    else
      AItemLink := BarControl.ItemLinks.First;

    if AItemLink <> nil then
      Result := AItemLink.Control.IAccessibilityHelper
    else
      Result := nil;
  end
  else
  begin
    AObjects := TList.Create;
    try
      AItemControl := AItemLinkHelper.ItemControl;
      GetChildrenForNavigation(AItemLinkHelper, Self, GetScreenBoundsForNavigation(AItemControl), ADirection, True, AObjects);
      Result := dxBar.GetNextAccessibleObject(AItemLinkHelper, AObjects, ADirection, True);
    finally
      AObjects.Free;
    end;
  end;
end;

function TdxBarApplicationMenuControlAccessibilityHelper.GetNextItemLink(
  AItemLink: TdxBarItemLink; AGoForward: Boolean): TdxBarItemLink;
var
  ACurrentLinks: TdxBarItemLinks;
begin
  if AItemLink <> nil then
  begin
    ACurrentLinks := ItemLinks;
    if BarControl.ButtonLinks.IsControlExists(AItemLink.Control) then
      ACurrentLinks := BarControl.ButtonLinks;
    Result := InternalGetNextItemLink(ACurrentLinks, AItemLink, not AGoForward);
  end
  else
    if ItemLinks.RealVisibleItemCount > 0 then
      Result := ItemLinks.First
    else
      Result := nil;
end;

function TdxBarApplicationMenuControlAccessibilityHelper.GetParentForKeyTip: TdxBarAccessibilityHelper;
var
  AIAccessibleObject: IdxBarAccessibleObject;
begin
  if Supports(BarControl.RealOwnerControl, IdxBarAccessibleObject, AIAccessibleObject) then
    Result := AIAccessibleObject.GetAccessibilityHelper.GetBarHelper
  else
    Result := nil;
end;

function TdxBarApplicationMenuControlAccessibilityHelper.GetBarControl: TdxBarApplicationMenuControl;
begin
  Result := TdxBarApplicationMenuControl(FOwnerObject);
end;

function TdxBarApplicationMenuControlAccessibilityHelper.GetHasExtraPane: Boolean;
begin
  Result := BarControl.HasExtraPane;
end;

function TdxBarApplicationMenuControlAccessibilityHelper.GetHasExtraPaneItemLinksItems: Boolean;
begin
  Result := HasExtraPane and (BarControl.ExtraPaneItemLinks.RealVisibleItemCount > 0);
end;

function TdxBarApplicationMenuControlAccessibilityHelper.GetExtraPaneAccessibilityHelper: TcxAccessibilityHelper;
begin
  Result := BarControl.ExtraPaneControlDockControl.IAccessibilityHelper.GetHelper;
end;

function TdxBarApplicationMenuControlAccessibilityHelper.GetNextLinks(
  ACurrentLinks: TdxBarItemLinks): TdxBarItemLinks;
var
  AIndex: Integer;
  AList: TList;
begin
  AList := TList.Create;
  try
    PopulateNavigationList(AList);
    AIndex := AList.IndexOf(ACurrentLinks);
    if AIndex < AList.Count - 1 then
      Inc(AIndex)
    else
      AIndex := 0;
    Result := TdxBarItemLinks(AList[AIndex])
  finally
    AList.Free;
  end;
end;

function TdxBarApplicationMenuControlAccessibilityHelper.GetPreviousLinks(
  ACurrentLinks: TdxBarItemLinks): TdxBarItemLinks;
var
  AIndex: Integer;
  AList: TList;
begin
  AList := TList.Create;
  try
    PopulateNavigationList(AList);
    AIndex := AList.IndexOf(ACurrentLinks);
    if AIndex > 0 then
      Dec(AIndex)
    else
      AIndex := AList.Count - 1;
    Result := TdxBarItemLinks(AList[AIndex])
  finally
    AList.Free;
  end;
end;

function TdxBarApplicationMenuControlAccessibilityHelper.InternalGetNextItemLink(
  ACurrentLinks: TdxBarItemLinks; ASelectedLink: TdxBarItemLink; AUpKey: Boolean): TdxBarItemLink;
begin
  if AUpKey then
    if ASelectedLink.VisibleIndex = 0 then
      Result := GetPreviousLinks(ACurrentLinks).Last
    else
      Result := ACurrentLinks.Prev(ASelectedLink)
  else
    if ASelectedLink.VisibleIndex = ACurrentLinks.RealVisibleItemCount - 1 then
      Result := GetNextLinks(ACurrentLinks).First
    else
      Result := ACurrentLinks.Next(ASelectedLink);
end;

procedure TdxBarApplicationMenuControlAccessibilityHelper.PopulateNavigationList(AList: TList);
begin
  if BarControl.ItemLinks.RealVisibleItemCount > 0 then
    AList.Add(BarControl.ItemLinks);
  if HasExtraPaneItemLinksItems then
    AList.Add(BarControl.ExtraPaneItemLinks);
  if BarControl.ButtonLinks.RealVisibleItemCount > 0 then
    AList.Add(BarControl.ButtonLinks);
end;

{ TdxBarExtraPaneListItem }

constructor TdxBarExtraPaneListItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowPinItems := True;
end;

function TdxBarExtraPaneListItem.CalculatePaintStyle: TdxBarPaintStyle;
var
  I: Integer;
begin
  Result := psStandard;
  for I := 0 to Items.Count - 1 do
    if ExtraPaneItem[I].ImageIndex >= 0 then
    begin
      Result := psCaptionGlyph;
      Break;
    end;
end;

function TdxBarExtraPaneListItem.CreateItem(AIndex, ACurIndex: Integer): TdxBarButton;
begin
  Result := inherited CreateItem(AIndex, ACurIndex);
  TdxBarExtraPaneButton(Result).ExtraPaneItem := ExtraPaneItem[AIndex];
  Result.PaintStyle := FPaintStyle;
end;

function TdxBarExtraPaneListItem.InternalActuallyVisible: Boolean;
begin
  Result := inherited InternalActuallyVisible or (Caption <> '');
end;

procedure TdxBarExtraPaneListItem.PopulateListedItemLinks(
  AItemLinks: TdxBarItemLinks; ALinkData: TObject; AIndex: Integer);

  function CreateActuallyVisibleItemsList: TList;
  var
    I, ASkipCount: Integer;
  begin
    Result := TList.Create;
    Result.Capacity := ActuallyVisibleItemsCount;
    ASkipCount := Items.Count - ActuallyVisibleItemsCount;
    for I := Items.Count - 1 downto 0 do
    begin
      if AllowPinItems and ExtraPaneItem[I].Pin or (ASkipCount <= 0) then
        Result.Insert(0, Pointer(I))
      else
        Dec(ASkipCount);
    end;
  end;

  procedure PopulatePinnedItemLinks;
  var
    AList: TList;
    I: Integer;
  begin
    if (ItemList.Count = 0) and (ActuallyVisibleItemsCount > 0) then
    begin
      AList := CreateActuallyVisibleItemsList;
      try
        for I := 0 to AList.Count - 1 do
          CreateItem(Integer(AList[I]), I);
      finally
        AList.Free;
      end;
    end;
    for I := 0 to ItemList.Count - 1 do
      AddListedItemLink(AItemLinks, ALinkData, AIndex + I, TdxBarItem(ItemList[I]));
  end;

begin
  FPaintStyle := CalculatePaintStyle;
  if AllowPinItems then
    PopulatePinnedItemLinks
  else
    inherited PopulateListedItemLinks(AItemLinks, ALinkData, AIndex);
end;

function TdxBarExtraPaneListItem.GetDisplayHint(const AText: string): string;
begin
  Result := AText;
end;

function TdxBarExtraPaneListItem.GetDisplayText(AItemIndex: Integer): string;
begin
  Result := ExtraPaneItem[AItemIndex].DisplayText;
  if Assigned(OnGetDisplayText) then
    FOnGetDisplayText(Self, AItemIndex, Result);
  if Result = '' then
    Result := ChangeFileExt(ExtractFileName(ExtraPaneItem[AItemIndex].Text), '');
end;

function TdxBarExtraPaneListItem.GetExtraPaneItem(AIndex: Integer): TdxBarExtraPaneItem;
begin
  Result := TdxBarExtraPaneItem(Items.Objects[AIndex]);
end;

function TdxBarExtraPaneListItem.GetExtraPaneItemIndex: Integer;
begin
  if ItemIndex >= 0 then
    Result := ExtraPaneItem[ItemIndex].Index
  else
    Result := ItemIndex;
end;

function TdxBarExtraPaneListItem.GetItemClass: TdxBarButtonClass;
begin
  if AllowPinItems then
    Result := TdxBarExtraPanePinnedButton
  else
    Result := TdxBarExtraPaneButton;
end;

procedure TdxBarExtraPaneListItem.SetActuallyVisibleItemsCount(AValue: Integer);
begin
  if AValue <> FActuallyVisibleItemsCount then
  begin
    FActuallyVisibleItemsCount := AValue;
    NeedClearItemList;
  end;
end;

{ TdxBarExtraPaneButton }

function TdxBarExtraPaneButton.GetControlClass(AIsVertical: Boolean): TdxBarItemControlClass;
begin
  Result := TdxBarExtraPaneButtonControl;
end;

function TdxBarExtraPaneButton.GetPin: Boolean;
begin
  Result := Assigned(ExtraPaneItem) and ExtraPaneItem.Pin
end;

procedure TdxBarExtraPaneButton.SetExtraPaneItem(AValue: TdxBarExtraPaneItem);
begin
  if FExtraPaneItem <> AValue then
  begin
    FExtraPaneItem := AValue;
    if ExtraPaneItem <> nil then
      ImageIndex := ExtraPaneItem.ImageIndex;
  end;
end;

procedure TdxBarExtraPaneButton.SetPin(AValue: Boolean);
begin
  if ExtraPaneItem <> nil then
  begin
    ExtraPaneItem.Pin := AValue;
    Update;
  end;
end;

{ TdxBarExtraPanePinnedButton }

function TdxBarExtraPanePinnedButton.GetControlClass(AIsVertical: Boolean): TdxBarItemControlClass;
begin
  Result := TdxBarExtraPanePinnedButtonControl;
end;

{ TdxBarExtraPaneButtonControl }

procedure TdxBarExtraPaneButtonControl.DoPaint(ARect: TRect; PaintType: TdxBarPaintType);
begin
  Painter.ExtraMenuDrawButton(DrawParams, ARect);
end;

function TdxBarExtraPaneButtonControl.GetDrawParams: TdxBarExtraMenuButtonControlDrawParams;
begin
  Result := TdxBarExtraMenuButtonControlDrawParams(inherited DrawParams);
end;

function TdxBarExtraPaneButtonControl.GetDrawParamsClass: TdxBarItemControlDrawParamsClass;
begin
  Result := TdxBarExtraMenuButtonControlDrawParams;
end;

function TdxBarExtraPaneButtonControl.GetItem: TdxBarExtraPaneButton;
begin
  Result := inherited Item as TdxBarExtraPaneButton;
end;

procedure TdxBarExtraPaneButtonControl.GetTextColors(
  AEnabled, ASelected, AFlat: Boolean; var AColor1, AColor2: TColor);
begin
  AColor1 := Painter.ExtraMenuButtonTextColor(AEnabled, ASelected);
  if AColor1 = clDefault then
    inherited GetTextColors(AEnabled, ASelected, AFlat, AColor1, AColor2)
  else
    AColor2 := AColor1;
end;

{ TdxBarExtraPanePinnedButtonControl }

procedure TdxBarExtraPanePinnedButtonControl.CalcDrawParams(AFull: Boolean = True);
begin
  inherited CalcDrawParams(AFull);
  if AFull then
    DrawParams.Pin := Pin;
end;

procedure TdxBarExtraPanePinnedButtonControl.CalcParts;
begin
  inherited CalcParts;
  Painter.CalculateExtraMenuButtonParts(DrawParams, FParts, ItemBounds);
end;

procedure TdxBarExtraPanePinnedButtonControl.ControlUnclick(ByMouse: Boolean);
begin
  if HotPartIndex = epbcpPinButton then
    Pin := not Pin
  else
    inherited ControlUnclick(ByMouse);
end;

function TdxBarExtraPanePinnedButtonControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxBarExtraPanePinnedButtonControlAccessibilityHelper;
end;

function TdxBarExtraPanePinnedButtonControl.GetPartCount: Integer;
begin
  Result := 2;
end;

function TdxBarExtraPanePinnedButtonControl.GetPin: Boolean;
begin
  Result := Item.Pin;
end;

procedure TdxBarExtraPanePinnedButtonControl.SetPin(AValue: Boolean);
begin
  if AValue <> Pin then
  begin
    Item.Pin := AValue;
    Repaint;
  end;
end;

{ TdxBarExtraPanePinnedButtonControlAccessibilityHelper }

function TdxBarExtraPanePinnedButtonControlAccessibilityHelper.HandleNavigationKey(var AKey: Word): Boolean;

  function HandleHorizontalNavigation(ADirection: Integer): Boolean;
  begin
    Result := (PinnedButtonControl.HotPartIndex <> icpNone) and
      (PinnedButtonControl.HotPartIndex + ADirection >= 0) and
      (PinnedButtonControl.HotPartIndex + ADirection < PinnedButtonControl.GetPartCount);
    if Result then
      PinnedButtonControl.HotPartIndex := PinnedButtonControl.HotPartIndex + ADirection;
  end;

begin
  Result := inherited HandleNavigationKey(AKey);
  if not Result then
  begin
    case AKey of
      VK_LEFT:
        Result := HandleHorizontalNavigation(-1);
      VK_RIGHT:
        Result := HandleHorizontalNavigation(1);
    end;
  end;
end;

procedure TdxBarExtraPanePinnedButtonControlAccessibilityHelper.Select(ASetFocus: Boolean);
begin
  inherited Select(ASetFocus);
  if PinnedButtonControl.HotPartIndex = icpNone then
    PinnedButtonControl.HotPartIndex := epbcpButton;
end;

procedure TdxBarExtraPanePinnedButtonControlAccessibilityHelper.Unselect(
  ANextSelectedObject: IdxBarAccessibilityHelper);
var
  ABarHelper: TdxBarExtraPanePinnedButtonControlAccessibilityHelper;
begin
  if ANextSelectedObject <> nil then
  begin
    if ANextSelectedObject.GetBarHelper is TdxBarExtraPanePinnedButtonControlAccessibilityHelper then
    begin
      ABarHelper := TdxBarExtraPanePinnedButtonControlAccessibilityHelper(ANextSelectedObject.GetBarHelper);
      ABarHelper.PinnedButtonControl.HotPartIndex := PinnedButtonControl.HotPartIndex;
    end;
  end;
  PinnedButtonControl.HotPartIndex := icpNone;
  inherited Unselect(ANextSelectedObject);
end;

function TdxBarExtraPanePinnedButtonControlAccessibilityHelper.GetPinnedButtonControl: TdxBarExtraPanePinnedButtonControl;
begin
  Result := TdxBarExtraPanePinnedButtonControl(inherited ButtonControl);
end;

{ TdxBarApplicationMenuButtonItem }

constructor TdxBarApplicationMenuButtonItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible := ivNever;
end;

procedure TdxBarApplicationMenuButtonItem.DirectClick;
begin
  if FExternalItem <> nil then
    FExternalItem.DirectClick
  else
    inherited DirectClick;
end;

procedure TdxBarApplicationMenuButtonItem.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FExternalItem) then
    FExternalItem := nil;
end;

function TdxBarApplicationMenuButtonItem.GetCaption: string;
begin
  if FExternalItem <> nil then
    Result := FExternalItem.Caption
  else
    Result := inherited GetCaption;
end;

function TdxBarApplicationMenuButtonItem.GetHint: string;
begin
  if FExternalItem <> nil then
    Result := FExternalItem.Hint
  else
    Result := inherited GetHint;
end;

function TdxBarApplicationMenuButtonItem.GetEnabled: Boolean;
begin
  if FExternalItem <> nil then
    Result := FExternalItem.Enabled
  else
    Result := inherited GetEnabled;
end;

function TdxBarApplicationMenuButtonItem.GetGlyph: TdxSmartGlyph;
begin
  if FExternalItem <> nil then
    Result := FExternalItem.Glyph
  else
    Result := inherited GetGlyph;
end;

function TdxBarApplicationMenuButtonItem.GetImageIndex: Integer;
begin
  if FExternalItem <> nil then
    Result := FExternalItem.ImageIndex
  else
    Result := inherited GetImageIndex;
end;

function TdxBarApplicationMenuButtonItem.GetScreenTip: TdxScreenTip;
begin
  if FExternalItem <> nil then
    Result := FExternalItem.ScreenTip
  else
    Result := inherited GetScreenTip;
end;

function TdxBarApplicationMenuButtonItem.GetVisible: TdxBarItemVisible;
begin
  if FExternalItem <> nil then
    Result := FExternalItem.Visible
  else
    Result := inherited GetVisible;
end;

procedure TdxBarApplicationMenuButtonItem.SetExternalItem(AValue: TdxBarButton);
begin
  if FExternalItem <> AValue then
  begin
    if FExternalItem <> nil then
      RemoveFreeNotification(FExternalItem);
    FExternalItem := AValue;
    if FExternalItem <> nil then
      FreeNotification(FExternalItem);
  end;
end;

initialization
  RegisterClasses([TdxBarApplicationMenuButtons, TdxBarApplicationMenuButton]);
  dxBarRegisterItem(TdxBarExtraPaneListItem, TdxBarContainerItemControl, False);
  dxBarRegisterItem(TdxBarApplicationMenuButtonItem, TdxBarApplicationMenuButtonControl, False);
  BarDesignController.RegisterBarControlDesignHelper(TdxBarApplicationMenuControl, TdxBarApplicationMenuControlDesignHelper);

finalization
  dxBarUnregisterItem(TdxBarExtraPaneListItem);
  dxBarUnregisterItem(TdxBarApplicationMenuButtonItem);

end.
