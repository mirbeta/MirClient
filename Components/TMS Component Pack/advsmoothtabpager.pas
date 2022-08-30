{***************************************************************************}
{ TAdvSmoothTabPager component                                              }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvSmoothTabPager;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Math, Menus,
  Dialogs, Forms, ExtCtrls, GDIPFill, AdvStyleIF,
  AdvGDIP, AxCtrls, Types;


const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First Release
  // v1.0.0.1 : Fixed : issue with incorrect nr. of design time created pages
  // v1.0.0.2 : Fixed : Issue with component initialization during reparenting
  // v1.0.0.3 : Fixed : Issue with Tab line painting when PageMargin > 0
  // v1.0.1.0 : New : Focus indication on active tab
  //          : Fixed : Issue with Tab drawing when TabVisible is false
  // v1.0.2.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.0.3.0 : New : Built-in support for reduced color set for use with terminal servers
  //          : Fixed : Issue with Tab Line drawing after Tab reordering
  // v1.0.3.1 : Fixed : Issue with default TabStop = true
  // v1.0.3.2 : Improved text alignment
  // v1.0.3.3 : Fixed : Issue with Tab text drawing
  // v1.0.4.0 : New : Delphi 2010 Touch Support
  // v1.0.4.1 : Improved : Border and tabheight = 0
  // v1.0.4.2 : Improved : Added Color property to set background color of pager
  // v1.0.4.3 : Fixed : Issue with client aligned controls and maximized form
  // v1.0.5.0 : New : Built-in support for Office 2010 colors
  // v1.0.5.1 : Fixed : Issue with caption size of tabs
  // v1.0.5.2 : Fixed : Issue with status indicator text rendering
  // v1.0.5.3 : Fixed : Issue with Caption position of button
  // v1.0.6.0 : New : property Transparent
  // v1.0.6.1 : Fixed : Access violation when adding pages at designtime
  // v1.1.0.0 : New : PictureContainer and ImageList support in tabs
  //          : New : Background fill
  // v1.2.0.0 : New : Metro Style support
  // v1.2.5.0 : New : Status indicator click events
  // v1.3.0.0 : New : Windows 8, Office 2013 styles added
  // v1.3.1.0 : New : TabHint added on TAdvSmoothTabPage
  // v1.3.2.0 : New : TabTextOrientation property
  // v1.3.2.1 : Fixed : Rare issue with sequence in destroying TAdvSmoothTabPager
  // v1.3.2.2 : Fixed : Tab width calculation
  // v1.4.0.0 : New : Windows 10, Office 2016 styles added

type

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothTabPager = class;
  TAdvSmoothTabPage = class;

  TAdvSmoothTabSettings = class(TPersistent)
  private
    FOwner: TAdvSmoothTabPager;
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FOnChange: TNotifyEvent;
    FHeight: Integer;
    FStartMargin: Integer;
    FEndMargin: Integer;
    FSpacing: Integer;
    FWidth: Integer;
    procedure SetLeftMargin(const Value: Integer);
    procedure SetRightMargin(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetStartMargin(const Value: Integer);
    procedure SetEndMargin(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    procedure Changed;
    function GetHeight(APageIndex: Integer): Integer;
    function GetWidth(APageIndex: Integer): Integer;
    function CalcWidth(APageIndex: Integer): Integer;
    property EndMargin: Integer read FEndMargin write SetEndMargin;
  public
    constructor Create(AOwner: TAdvSmoothTabPager);
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 4;
    property RightMargin: Integer read FRightMargin write SetRightMargin default 4;
    property StartMargin: Integer read FStartMargin write SetStartMargin default 60;
    property Height: Integer read FHeight write SetHeight default 26;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Width: Integer read FWidth write SetWidth default 0;
  end;

  TDbgList = class(TList)
  private
    function GetItemsEx(Index: Integer): Pointer;
    procedure SetItemsEx(Index: Integer; const Value: Pointer);
  public
    property Items[Index: Integer]: Pointer read GetItemsEx write SetItemsEx; default;
  end;

  TProWinControl = class(TWinControl);

  TAdvSmoothTabAppearance = class;

  TAdvSmoothTabStatus = class(TPersistent)
  private
    FOwner: TAdvSmoothTabAppearance;
    FOffsetTop: integer;
    FOffsetLeft: integer;
    FVisible: Boolean;
    FCaption: String;
    FAppearance: TGDIPStatus;
    FOnChange: TNotifyEvent;
    procedure SetAppearance(const Value: TGDIPStatus);
    procedure SetCaption(const Value: String);
    procedure SetOffsetLeft(const Value: integer);
    procedure SetOffsetTop(const Value: integer);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed;
    procedure AppearanceChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothTabAppearance);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default false;
    property Caption: String read FCaption write SetCaption;
    property OffsetLeft: integer read FOffsetLeft write SetOffsetLeft default 0;
    property OffsetTop: integer read FOffsetTop write SetOffsetTop default 0;
    property Appearance: TGDIPStatus read FAppearance write SetAppearance;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothTabAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothTabPage;
    FPicture: TAdvGDIPPicture;
    FBevel: boolean;
    FVerticalSpacing: integer;
    FColor: TColor;
    FAppearance: TGDIPButton;
    FBevelColor: TColor;
    FShadow: boolean;
    FStatus: TAdvSmoothTabStatus;
    FHorizontalSpacing: integer;
    FOnChange: TNotifyEvent;
    FColorSelected: TColor;
    FColorDown: TColor;
    FColorDisabled: TColor;
    FColorHot: TColor;
    FBevelColorDisabled: TColor;
    FBevelColorSelected: TColor;
    FBevelColorDown: TColor;
    FBevelColorHot: TColor;
    FColorTextHot: TColor;
    FColorTextDisabled: TColor;
    FColorTextSelected: TColor;
    procedure SetAppearance(const Value: TGDIPButton);
    procedure SetBevel(const Value: boolean);
    procedure SetBevelColor(const Value: TColor);
    procedure SetStatus(const Value: TAdvSmoothTabStatus);
    procedure SetColor(const Value: TColor);
    procedure SetHorizontalSpacing(const Value: integer);
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure SetShadow(const Value: boolean);
    procedure SetVerticalSpacing(const Value: integer);
    procedure SetColorDown(const Value: TColor);
    procedure SetColorSelected(const Value: TColor);
    procedure SetColorDisabled(const Value: TColor);
    procedure SetColorHot(const Value: TColor);
    procedure SetBevelColorDisabled(const Value: TColor);
    procedure SetBevelColorDown(const Value: TColor);
    procedure SetBevelColorSelected(const Value: TColor);
    procedure SetBevelColorHot(const Value: TColor);
    procedure SetColorTextDisabled(const Value: TColor);
    procedure SetColorTextSelected(const Value: TColor);
  protected
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
    procedure AppearanceChanged(Sender: TObject);
    procedure StatusChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothTabPage);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Appearance: TGDIPButton read FAppearance write SetAppearance;
    property Status: TAdvSmoothTabStatus read FStatus write SetStatus;
    property Bevel: boolean read FBevel write SetBevel default true;
    property BevelColor: TColor read FBevelColor write SetBevelColor default clGray;
    property BevelColorDown: TColor read FBevelColorDown write SetBevelColorDown default clGray;
    property BevelColorSelected: TColor read FBevelColorSelected write SetBevelColorSelected default $AAD9FF;
    property BevelColorHot: TColor read FBevelColorHot write SetBevelColorHot default clWhite;
    property BevelColorDisabled: TColor read FBevelColorDisabled write SetBevelColorDisabled default $545454;
    property Color: TColor read FColor write SetColor default clSilver;
    property ColorDown: TColor read FColorDown write SetColorDown default clGray;
    property ColorSelected: TColor read FColorSelected write SetColorSelected default $AAD9FF;
    property ColorHot: TColor read FColorHot write SetColorHot default clWhite;
    property ColorDisabled: TColor read FColorDisabled write SetColorDisabled  default $545454;
    property ColorTextHot: TColor read FColorTextHot write FColorTextHot default clBlack;
    property ColorTextDisabled: TColor read FColorTextDisabled write SetColorTextDisabled default clGray;
    property ColorTextSelected: TColor read FColorTextSelected write SetColorTextSelected default clBlack;
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
    property Shadow: boolean read FShadow write SetShadow default false;
    property HorizontalSpacing: integer read FHorizontalSpacing write SetHorizontalSpacing default 0;
    property VerticalSpacing: integer read FVerticalSpacing write SetVerticalSpacing default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothTabPage = class(TCustomControl, ITMSStyleEx)
  private
    FTMSStyle: TTMSStyle;
    FMetroStyle: Boolean;
    FDesignTime: Boolean;
    FPageIndex: integer;  
    FTabVisible: Boolean;
    FAdvSmoothTabPager: TAdvSmoothTabPager;
    FCaption: TCaption;
    FTabEnabled: Boolean;
    FImageIndex: Integer;
    FUpdatingParent: Boolean;
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FTabAppearance: TAdvSmoothTabAppearance;
    FPageAppearance: TGDIPFill;
    FTabHint: string;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMControlListChange(var Message: TCMControlListChange); message CM_CONTROLLISTCHANGE;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure OnTabAppearanceChanged(Sender: TObject);
    procedure OnPageAppearanceChanged(Sender: TObject);
    procedure SetAdvSmoothTabPager(const Value: TAdvSmoothTabPager);
    procedure SetTabVisible(const Value: Boolean);
    procedure SetCaption(const Value: TCaption);
    procedure SetTabEnabled(const Value: Boolean);
    procedure SetImageIndex(const Value: Integer);
    function GetPageIndex: Integer;
    procedure SetPageIndex(const Value: Integer);
    procedure SetTabAppearance(const Value: TAdvSmoothTabAppearance);
    procedure SetPageAppearance(const Value: TGDIPFill);
  protected
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Paint; override;
    procedure ReadState(Reader: TReader); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property AdvSmoothTabPager: TAdvSmoothTabPager read FAdvSmoothTabPager write SetAdvSmoothTabPager;
    procedure SetCompStyle(AStyle: TTMSStyle; AppColor: TColor);
    procedure SetComponentStyleAndAppColor(AStyle: TTMSStyle; AppColor: TColor);
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
    function XYToIndicator(X, Y: Integer): Boolean;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property PageAppearance: TGDIPFill read FPageAppearance write SetPageAppearance;
    property TabVisible: Boolean read FTabVisible write SetTabVisible default true;
    property TabEnabled: Boolean read FTabEnabled write SetTabEnabled default true;
    property ShowHint;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored false;
    property TabAppearance: TAdvSmoothTabAppearance read FTabAppearance write SetTabAppearance;
    property TabHint: string read FTabHint write FTabHint;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property PopupMenu;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
    property OnExit;
    property OnEnter;
  end;

  TTabChangingEvent = procedure(Sender: TObject; FromPage, ToPage: Integer; var AllowChange: Boolean) of object;
  TOnClosePage = procedure (Sender:TObject; PageIndex: integer; var Allow: boolean) of object;
  TOnClosedPage = procedure (Sender:TObject; PageIndex: integer) of object;

  TOnPageListClick = procedure (Sender: TObject; X, Y: integer) of object;
  TTabMovedEvent = procedure(Sender: TObject; FromPos: integer; ToPos: Integer)of object;
  TDrawTabEvent = procedure(Sender:TObject; TabIndex: integer; TabRect: TRect) of object;

  TStatusClickEvent = procedure(Sender: TObject; TabIndex: Integer) of object;

  TAdvSmoothTabPosition = (tpTopRight, tpTopCenter, tpTopLeft, tpLeftTop, tpLeftBottom, tpLeftCenter, tpRightTop, tpRightBottom, tpRightCenter ,
    tpBottomLeft, tpBottomRight, tpBottomCenter);

  TAdvSmoothTabTextOrientation = (toAuto, toHorizontal, toVertical);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothTabPager = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    Ffocused: Boolean;
    FPageMargin: integer;
    FOffSetY: integer;
    FOffSetX: integer;
    FAdvSmoothTabPages: TDbgList;
    FPropertiesLoaded: Boolean;
    FTabSettings: TAdvSmoothTabSettings;
    FActivePageIndex: Integer;
    FHotPageIndex: Integer;
    FDownPageIndex, FStatusPageIndex: Integer;
    FOldHotPageIndex: Integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TTabChangingEvent;
    FTabPosition: TAdvSmoothTabPosition;
    FTabOffSet: Integer;
    FFormWndProc: TWndMethod;
    FTabReorder: Boolean;
    FOnTabMoved: TTabMovedEvent;
    FOnDrawTab: TDrawTabEvent;
    FConstructed, FDesignTime: boolean;
    FShowFocus: Boolean;
    FTransparent: Boolean;
    FFill: TGDIPFill;
    FOnStatusClick: TStatusClickEvent;
    FTabTextOrientation: TAdvSmoothTabTextOrientation;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMHintShow(var Msg: TCMHintShow); message CM_HINTSHOW;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMControlListChange(var Message: TCMControlListChange); message CM_CONTROLLISTCHANGE;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure OnTabSettingsChanged(Sender: TObject);
    procedure OnEnterTab(PageIndex: Integer);
    procedure OnExitTab(PageIndex: Integer);
    procedure SetPagePosition(SmoothTab: TAdvSmoothTabPage);
    procedure SetAllPagesPosition;
    function GetAdvSmoothTabPageCount: integer;
    function GetAdvSmoothTabPages(index: integer): TAdvSmoothTabPage;
    function GetPopupMenuEx: TPopupMenu;
    procedure SetPopupMenuEx(const Value: TPopupMenu);
    function GetActivePage: TAdvSmoothTabPage;
    function GetActivePageIndex: Integer;
    procedure SetActivePage(const Value: TAdvSmoothTabPage);
    procedure SetActivePageIndex(const Value: Integer);
    procedure SetTabSettings(const Value: TAdvSmoothTabSettings);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SeTAdvSmoothTabPosition(const Value: TAdvSmoothTabPosition);
    procedure SetPageMargin(const Value: integer);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetTabTextOrientation(const Value: TAdvSmoothTabTextOrientation);
  protected
    procedure FillChanged(Sender: TObject);
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure WndProc(var Msg: TMessage); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure DrawTab(PageIndex: Integer);
    procedure DrawAllTabs;
    procedure Paint; override;

    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetSmoothTabBounds(SmoothTab: TAdvSmoothTabPage; var ALeft, ATop, AWidth, AHeight: Integer);

    procedure UpdateMe(PropID: integer);
    procedure ChangeActivePage(PageIndex: Integer);

    function CanShowTab(PageIndex: Integer): Boolean;
    function GetVisibleTabCount: Integer;

    procedure InvalidateTab(PageIndex: Integer);
    procedure DoExit; override;
    procedure DoEnter; override;
    function GetSmoothTabRect: TRect;
    function GetTabsArea: TRect;
    function GetTabsRect: TRect;
    function GetTabRect(StartIndex, PageIndex: Integer; totalsize: integer): TRect;  overload;
    function GetTabRect(PageIndex: Integer): TRect;  overload;
    function GetTabRect(Page: TAdvSmoothTabPage): TRect; overload;
    function PTOnTab(X, Y: Integer): Integer;

    function IsActivePageNeighbour(PageIndex: Integer): Integer; 
    function UseOldDrawing: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Init;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function GetVersionNr: integer;
    function AddAdvSmoothTabPage(SmoothTab: TAdvSmoothTabPage): integer; overload;
    function AddAdvSmoothTabPage(PageCaption: TCaption): integer; overload;
    procedure RemoveAdvSmoothTabPage(SmoothTab: TAdvSmoothTabPage);
    procedure MoveAdvSmoothTabPage(CurIndex, NewIndex: Integer);
    function FindNextPage(CurPage: TAdvSmoothTabPage; GoForward, CheckTabVisible: Boolean): TAdvSmoothTabPage;
    procedure SelectNextPage(GoForward: Boolean);
    function IndexOfPage(SmoothTab: TAdvSmoothTabPage): Integer;
    function IndexOfTabAt(X,Y: Integer): integer;
    property ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex;
    property AdvSmoothTabPageCount: integer read GetAdvSmoothTabPageCount;
    property AdvSmoothTabPages[index: integer]: TAdvSmoothTabPage read GetAdvSmoothTabPages;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
  published
    property Fill: TGDIPFill read FFill write SetFill;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Align;
    property Anchors;
    property ActivePage: TAdvSmoothTabPage read GetActivePage write SetActivePage;
    property Constraints;
    property Color;
    property PageMargin: integer read FPageMargin write SetPageMargin default 1;
    property PopupMenu: TPopupMenu read GetPopupMenuEx write SetPopupMenuEx;
    property ShowHint;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default true;
    property TabPosition: TAdvSmoothTabPosition read FTabPosition write SeTAdvSmoothTabPosition default tpTopCenter;
    property TabTextOrientation: TAdvSmoothTabTextOrientation read FTabTextOrientation write SetTabTextOrientation default toAuto;
    property TabSettings: TAdvSmoothTabSettings read FTabSettings write SetTabSettings;
    property TabReorder: Boolean read FTabReorder write FTabReorder;
    property Version: string read GetVersion write SetVersion stored false;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnDrawTab: TDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnTabMoved: TTabMovedEvent read FOnTabMoved write FOnTabMoved;
    property OnStatusClick: TStatusClickEvent read FOnStatusClick write FOnStatusClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property TabOrder;
    property TabStop default true;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
  end;

implementation

{$IFDEF ISDELPHI}
{$IFDEF DELPHI_UNICODE}
type
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
{$ENDIF}
{$ENDIF}

procedure DrawFocus(g: TGPGraphics; r: TGPRectF; rn: Integer; rt: TFillRoundingType);
var
  pathfocus: TGPGraphicsPath;
  pfocus: TGPPen;
begin
  pathfocus := GDIPFill.CreateRoundRectangle(r, rn, rt, false);
  g.SetSmoothingMode(SmoothingModeDefault);
  pfocus := TGPPen.Create(MakeColor(255, clBlack), 1);
  pfocus.SetDashStyle(DashStyleDot);
  g.DrawPath(pfocus, pathfocus);
  pfocus.Free;
  pathfocus.Free;
end;

{ TAdvSmoothTabSettings }

constructor TAdvSmoothTabSettings.Create(AOwner: TAdvSmoothTabPager);
begin
  FOwner := AOwner;
  FLeftMargin := 4;
  FRightMargin := 4;
  FHeight := 26;
  FStartMargin := 4;
  FEndMargin := 0;
  FSpacing := 4;
  FWidth := 0;
end;

function TAdvSmoothTabSettings.GetHeight(APageIndex: Integer): Integer;
begin
  Result := FHeight;
  if not Assigned(FOwner) then
    Exit;

  case FOwner.TabTextOrientation of
    toHorizontal:
    begin
      case FOwner.TabPosition of
        tpLeftTop, tpLeftBottom, tpLeftCenter, tpRightTop, tpRightBottom, tpRightCenter:
        Result := CalcWidth(APageIndex);
      end;
    end;
    toVertical:
    begin
      case FOwner.TabPosition of
        tpTopRight, tpTopCenter, tpTopLeft, tpBottomLeft, tpBottomCenter, tpBottomRight:
        Result := CalcWidth(APageIndex);
      end;
    end;
  end;
end;

function TAdvSmoothTabSettings.GetWidth(APageIndex: Integer): Integer;
begin
  Result := CalcWidth(APageIndex);
  if not Assigned(FOwner) then
    Exit;

  case FOwner.TabTextOrientation of
    toHorizontal:
    begin
      case FOwner.TabPosition of
        tpLeftTop, tpLeftBottom, tpLeftCenter, tpRightTop, tpRightBottom, tpRightCenter:
        Result := FHeight;
      end;
    end;
    toVertical:
    begin
      case FOwner.TabPosition of
        tpTopRight, tpTopCenter, tpTopLeft, tpBottomLeft, tpBottomCenter, tpBottomRight:
        Result := FHeight;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabSettings.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTabSettings) then
  begin
    LeftMargin := (Source as TAdvSmoothTabSettings).LeftMargin;
    RightMargin := (Source as TAdvSmoothTabSettings).RightMargin;
    Height := (Source as TAdvSmoothTabSettings).Height;
    StartMargin := (Source as TAdvSmoothTabSettings).StartMargin;
    EndMargin := (Source as TAdvSmoothTabSettings).EndMargin;
    Width := (Source as TAdvSmoothTabSettings).Width;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabSettings.CalcWidth(APageIndex: Integer): Integer;
var
  R2: TRect;
begin
  Result := FWidth;
  if Result = 0 then
  begin
    if (APageIndex >= 0) and (APageIndex <= FOwner.AdvSmoothTabPageCount - 1) then
    begin
      if (FOwner.AdvSmoothTabPages[APageIndex].Caption <> '') then
      begin
        R2 := Rect(0,0, 1000, 100);
        DrawText(FOwner.Canvas.Handle,PChar(FOwner.AdvSmoothTabPages[APageIndex].Caption),
          Length(FOwner.AdvSmoothTabPages[APageIndex].Caption), R2, DT_CALCRECT or DT_LEFT or DT_SINGlELINE);
      end
      else
        R2 := Rect(0, 0, 0, 0);

      Result := LeftMargin + R2.Right + 10 + RightMargin;
    end;
  end;
end;

procedure TAdvSmoothTabSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabSettings.SetLeftMargin(const Value: Integer);
begin
  if (FLeftMargin <> Value) then
  begin
    FLeftMargin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabSettings.SetRightMargin(const Value: Integer);
begin
  if (FRightMargin <> Value) then
  begin
    FRightMargin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabSettings.SetHeight(const Value: Integer);
begin
  if (FHeight <> Value) then
  begin
    FHeight := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabSettings.SetStartMargin(const Value: Integer);
begin
  if (FStartMargin <> Value) then
  begin
    FStartMargin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabSettings.SetEndMargin(const Value: Integer);
begin
  if (FEndMargin <> Value) then
  begin
    FEndMargin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabSettings.SetSpacing(const Value: Integer);
begin
  if (FSpacing <> Value) then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabSettings.SetWidth(const Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    changed;
  end;
end;

{ TDbgList }

function TDbgList.GetItemsEx(Index: Integer): Pointer;
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list read access');
  end;

  if Index < Count then
    Result := inherited Items[Index]
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

procedure TDbgList.SetItemsEx(Index: Integer; const Value: Pointer);
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list write access');
  end;
  if Index < Count then
    inherited Items[Index] := value;
end;


{ TAdvSmoothTabPage }

constructor TAdvSmoothTabPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls] - [csOpaque];
  FCaption := '';
  FTabVisible := True;
  FTabEnabled := True;
  FImageIndex := -1;

  FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  FTabAppearance := TAdvSmoothTabAppearance.Create(Self);
  FTabAppearance.OnChange := OnTabAppearanceChanged;

  FPageAppearance := TGDIPFill.Create;
  FPageAppearance.OnChange := OnPageAppearanceChanged;

  DoubleBuffered := true;

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothTabPage.Destroy;
begin
  if (FAdvSmoothTabPager <> nil) then
  begin
    FAdvSmoothTabPager.RemoveAdvSmoothTabPage(Self);
  end;

  FTabAppearance.Free;
  FPageAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.AlignControls(AControl: TControl; var ARect: TRect);
begin
  inherited;
end;

procedure TAdvSmoothTabPage.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothTabPage then
  begin
    FCaption := (Source as TAdvSmoothTabPage).caption;
    FImageIndex := (Source as TAdvSmoothTabPage).ImageIndex;
    FPageAppearance.Assign((Source as TAdvSmoothTabPage).PageAppearance);
    FTabVisible := (Source as TAdvSmoothTabPage).TabVisible;
    FTabEnabled := (Source as TAdvSmoothTabPage).TabEnabled;
    FPageIndex := (Source as TAdvSmoothTabPage).PageIndex;
    FTabHint := (Source as TAdvSmoothTabPage).TabHint;
    FTabAppearance.Assign((Source as TAdvSmoothTabPage).TabAppearance);
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then
  begin
    if Assigned(FOnShow) then
      FOnShow(self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.Loaded;
begin
  inherited;

end;

procedure TAdvSmoothTabPage.LoadFromTheme(FileName: String);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if Assigned(FTabAppearance) then
    if Assigned(FTabAppearance.FAppearance) then
      FTabAppearance.FAppearance.DoNotification(Self, AComponent, Operation);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.Paint;
var
  R: TRect;
  g: TGPGraphics;
  TabRect: TRect;
  p1, p2: TGPPen;
  bevel: integer;
begin
  R := ClientRect;
  with PageAppearance do
  begin
    g := TGPGraphics.Create(Canvas.Handle);
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    PageAppearance.Fill(g, MakeRect(0, 0, R.Right - R.Left - 1
      , R.Bottom - R.Top - 1));

    if not Assigned(FAdvSmoothTabPager) then
      Exit;
      
    with FAdvSmoothTabPager do
    begin
      if (ActivePageIndex = PageIndex) and (ActivePageIndex < FAdvSmoothTabPager.FAdvSmoothTabPages.Count)
        and (ActivePageIndex > -1) then
      begin
        TabRect := GetTabRect(ActivePageIndex);
        if (FPageAppearance.GradientType <> gtSolid) and (FPageAppearance.ColorTo <> clNone) then
        begin
          p1 := TGPPen.Create(MakeColor(FPageAppearance.Opacity, FPageAppearance.Color), FPageAppearance.BorderWidth + 1);
          p2 := TGPPen.Create(MakeColor(FPageAppearance.OpacityTo, FPageAppearance.ColorTo), FPageAppearance.BorderWidth + 1);
        end
        else
        begin
          p1 := TGPPen.Create(MakeColor(FPageAppearance.Opacity, FPageAppearance.Color), FPageAppearance.BorderWidth + 1);
          p2 := TGPPen.Create(MakeColor(FPageAppearance.Opacity, FPageAppearance.Color), FPageAppearance.BorderWidth + 1);
        end;

        bevel := 0;
        if TabAppearance.Bevel then
          bevel := 1;

        if TabSettings.GetHeight(ActivePageIndex) > 0 then
        begin
          case TabPosition of
            tpTopLeft, tpTopCenter, tpTopRight:
            begin
              if (FPageAppearance.GradientType = gtSolid) or (FPageAppearance.GradientType = gtVertical) or (FPageAppearance.ColorTo = clNone) then
              begin
                g.DrawLine(p1, TabRect.Left - PageMargin + bevel, 0, TabRect.Right - PageMargin  - bevel, 0);
              end;
            end;
            tpLeftCenter, tpLeftBottom, tpLeftTop:
            begin
              if (FPageAppearance.GradientType = gtSolid) or (FPageAppearance.GradientType = gtHorizontal) or (FPageAppearance.ColorTo = clNone) then
              begin
                g.DrawLine(p1, 0, TabRect.Top + bevel, 0, TabRect.Bottom - bevel);
              end;
            end;
            tpBottomCenter, tpBottomRight, tpBottomLeft:
            begin
              if (FPageAppearance.GradientType = gtSolid) or (FPageAppearance.GradientType = gtVertical) or (FPageAppearance.ColorTo = clNone) then
              begin
                g.DrawLine(p2, TabRect.Left + bevel - PageMargin, r.Bottom - 1, TabRect.Right  - bevel, r.Bottom - 1);
              end;
            end;
            tpRightCenter, tpRightBottom, tpRightTop:
            begin
              if (FPageAppearance.GradientType = gtSolid) or (FPageAppearance.GradientType = gtHorizontal) or (FPageAppearance.ColorTo = clNone) then
              begin
                g.DrawLine(p2, R.Right - 1, TabRect.Top + bevel, R.Right - 1, TabRect.Bottom - bevel);
              end;
            end;
          end;
        end;
        p1.Free;
        p2.Free;
      end;
    end;
    g.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothTabPage.SetAdvSmoothTabPager(const Value: TAdvSmoothTabPager);
begin
  if (FAdvSmoothTabPager <> Value) then
  begin
    if FAdvSmoothTabPager <> nil then FAdvSmoothTabPager.RemoveAdvSmoothTabPage(Self);
    Parent := Value;
    if (Value <> nil) then
    begin
      Value.AddAdvSmoothTabPage(Self);
      if not (csLoading in ComponentState) then
      begin
        if Value.AdvSmoothTabPageCount > 1 then
        begin
          TabAppearance.Assign(Value.AdvSmoothTabPages[Value.AdvSmoothTabPageCount - 2].TabAppearance);
          PageAppearance.Assign(Value.AdvSmoothTabPages[Value.AdvSmoothTabPageCount - 2].PageAppearance);
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.SetParent(AParent: TWinControl);
var
  ci, ni: Integer;
  AOfficePager: TAdvSmoothTabPager;
begin
  if ((AParent is TAdvSmoothTabPage) or (AParent is TAdvSmoothTabPager)) and not (FUpdatingParent) then
  begin
    AOfficePager := nil;
    if (AParent is TAdvSmoothTabPage) then
    begin
      AOfficePager := TAdvSmoothTabPage(AParent).FAdvSmoothTabPager;
    end
    else if (AParent is TAdvSmoothTabPager) then
    begin
      AOfficePager := TAdvSmoothTabPager(AParent);
    end;

    if Assigned(FAdvSmoothTabPager) and Assigned(AOfficePager) then
    begin

      if (FAdvSmoothTabPager <> AOfficePager) then
      begin
        FUpdatingParent := True;
        AdvSmoothTabPager := AOfficePager;
        FUpdatingParent := False;
      end;

      if (FAdvSmoothTabPager = AOfficePager) then
      begin
        if (AParent is TAdvSmoothTabPage) then
        begin
          ci := FAdvSmoothTabPager.IndexOfPage(self);
          ni := FAdvSmoothTabPager.IndexOfPage(TAdvSmoothTabPage(AParent));
          AParent := AOfficePager;
          if (ci >= 0) and (ci < FAdvSmoothTabPager.FAdvSmoothTabPages.Count) and
             (ni >= 0) and (ni < FAdvSmoothTabPager.FAdvSmoothTabPages.Count) then
          begin
            FAdvSmoothTabPager.MoveAdvSmoothTabPage(ci, ni);
          end
          else
            raise Exception.Create('Invalid Parent '+inttostr(ci)+':'+inttostr(ni));
        end
        else if (AParent is TAdvSmoothTabPager) then
        begin
          AParent := AOfficePager;
        end;
        
        FAdvSmoothTabPager.Invalidate;
        Invalidate;
      end
      else
        raise Exception.Create('Invalid Parent');
    end;
    //else
      //raise Exception.Create('Invalid Parent3');
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.SetTabVisible(const Value: Boolean);
begin
  if (FTabVisible <> Value) then
  begin
    FTabVisible := Value;
    if Assigned(FAdvSmoothTabPager) then
    begin
      if Assigned(FAdvSmoothTabPager.ActivePage) then
        FAdvSmoothTabPager.ActivePage.Invalidate;
      FAdvSmoothTabPager.Invalidate;
    end;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.WMSize(var Message: TWMSize);
begin
  inherited;
end;

function TAdvSmoothTabPage.XYToIndicator(X, Y: Integer): Boolean;
var
  g: TGPGraphics;
  xt, yt: Integer;
  R: TRect;
begin
  Result := False;
  if Assigned(FAdvSmoothTabPager) and TabAppearance.Status.Visible then
  begin
    R := FAdvSmoothTabPager.GetTabRect(Self);
    g := TGPGraphics.Create(Canvas.Handle);
    TabAppearance.Status.Appearance.CalculateSize(g, TabAppearance.Status.Caption);
    xt := R.Right + TabAppearance.Status.OffsetLeft - TabAppearance.Status.Appearance.GetWidth;
    yt := TabAppearance.Status.OffsetTop + R.Top;
    Result := PtInRect(Bounds(TabAppearance.Status.OffsetLeft + xt, yt, TabAppearance.Status.Appearance.GetWidth, TabAppearance.Status.Appearance.getHeight), Point(X, Y));
    g.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  with Message do
  begin

  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.CMControlListChange(var Message: TCMControlListChange);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.WMEraseBkGnd(var Message: TWMEraseBkGnd);
{var
  DC: HDC;
  i: Integer;
  p: TPoint; }
begin
  if {FTransparent} False then
  begin
    if Assigned(Parent) then
    begin
     { DC := Message.DC;
      i := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(DC, p.x, p.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(DC, nil);
      RestoreDC(DC, i); }
    end;
  end
  else
  begin
    inherited;
  end;
end;

procedure TAdvSmoothTabPage.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  {$IFNDEF DELPHI_UNICODE}
  dbl: boolean;
  {$ENDIF}
  p: TPoint;
  i: integer;
begin
  if not Assigned(FAdvSmoothTabPager) then
  begin
    inherited;
    Exit;
  end;

  if not FAdvSmoothTabPager.Transparent then
  begin
    inherited;
    Exit;
  end;

  if Assigned(Parent) {and (Fill.ShadowOffset > 0) ?} then
  begin
    DC := Message.DC;
    if DC <> 0 then
    begin
      {$IFNDEF DELPHI_UNICODE}
      dbl := Parent.DoubleBuffered;
      Parent.DoubleBuffered := false;
      {$ENDIF}
      i := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(DC, p.x, p.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(DC, nil);
      RestoreDC(DC, i);
      {$IFNDEF DELPHI_UNICODE}
      Parent.DoubleBuffered := dbl;
      {$ENDIF}
    end;
  end;

  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TAdvSmoothTabPager then
    AdvSmoothTabPager := TAdvSmoothTabPager(Reader.Parent);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Invalidate;
    if Assigned(FAdvSmoothTabPager) then
    begin
      FAdvSmoothTabPager.Invalidate;
    end;
  end;
end;

procedure TAdvSmoothTabPage.SetColorTones(ATones: TColorTones);
begin
  FMetroStyle := True;
  Font.Name := GetMetroFont;
  Font.Color := ATones.Background.TextColor;
  TabAppearance.FAppearance.SimpleLayout := True;
  PageAppearance.Color := ATones.Background.BrushColor;
  PageAppearance.ColorTo := ATones.Background.BrushColor;
  PageAppearance.ColorMirror := ATones.Background.BrushColor;
  PageAppearance.ColorMirrorTo := ATones.Background.BrushColor;
  PageAppearance.BorderColor := ATones.Background.BorderColor;

  TabAppearance.Appearance.Font.Color := ATones.BackGround.TextColor;

  TabAppearance.Color := ATones.Background.BrushColor;
  TabAppearance.ColorDown := ATones.Selected.BrushColor;
  TabAppearance.ColorDisabled := ATones.Disabled.BrushColor;
  TabAppearance.ColorHot := ATones.Hover.BrushColor;
  TabAppearance.ColorSelected := ATones.Selected.BrushColor;
  TabAppearance.ColorTextHot := ATones.Hover.TextColor;
  TabAppearance.ColorTextSelected := ATones.Selected.TextColor;
  TabAppearance.ColorTextDisabled := ATones.Disabled.TextColor;

  TabAppearance.BevelColor := ATones.Background.BrushColor;
  TabAppearance.BevelColorDown := ATones.Selected.BrushColor;
  TabAppearance.BevelColorDisabled := ATones.Disabled.BrushColor;
  TabAppearance.BevelColorHot := ATones.Hover.BrushColor;
  TabAppearance.BevelColorSelected := ATones.Selected.BrushColor;
end;

procedure TAdvSmoothTabPage.SetComponentStyle(AStyle: TTMSStyle);
begin
  inherited;
  FTMSStyle := AStyle;
  SetCompStyle(AStyle, clNone);
end;

procedure TAdvSmoothTabPage.SetComponentStyleAndAppColor(AStyle: TTMSStyle;
  AppColor: TColor);
begin
  FTMSStyle := AStyle;
  SetCompStyle(AStyle, AppColor);
end;

procedure TAdvSmoothTabPage.SetCompStyle(AStyle: TTMSStyle; AppColor: TColor);
begin
  FMetroStyle := False;
  TabAppearance.FAppearance.SimpleLayout := False;
  TabAppearance.FAppearance.SimpleLayoutBorder := False;
  TabAppearance.FAppearance.Rounding := 8;
  TabAppearance.ColorTextSelected := clBlack;
  TabAppearance.Appearance.Font.Color := clBlack;
  TabAppearance.ColorTextHot := clBlack;

  with TabAppearance do
  begin
  ColorHot := clWhite;
    case astyle of
      tsOffice2003Blue:
      begin
        Color := $00E3B28D;
        ColorDown := $AAD9FF;
        ColorDisabled := $00F2F2F2;
      end;
      tsOffice2003Silver:
      begin
        Color := $00927476;
        ColorDown := $AAD9FF;
        ColorDisabled := $947C7C;
      end;
      tsOffice2003Olive:
      begin
        Color := $447A63;
        ColorDown := $AAD9FF;
        ColorDisabled := $947C7C;
      end;
      tsOffice2003Classic:
      begin
        Color := $00C9D1D5;
        ColorDown := $AAD9FF;
        ColorDisabled := $FFD1AD;
      end;
      tsOffice2007Luna:
      begin
        Color := $00FDEADA;
        ColorDown := $AAD9FF;
        ColorDisabled := $FFD1AD;
      end;
      tsOffice2007Obsidian:
      begin
        Color := $006E6E6D;
        ColorDown := $AAD9FF;
        ColorDisabled := $FFD1AD;
      end;
      tsWindowsXP:
      begin
        Color := $B9D8DC;
        ColorDown := $AAD9FF;
        ColorDisabled := $FFD1AD;
      end;
      tsWhidbey:
      begin
        Color := $00828F92;
        ColorDown := $AAD9FF;
        ColorDisabled := $FFD1AD;
      end;
      tsCustom: ;
      tsOffice2007Silver:
      begin
        Color := $00E7DCD5;
        ColorDown := $AAD9FF;
        ColorDisabled := $FFD1AD;
      end;
      tsWindowsVista:
      begin
        Color := $FFFDF9;
        ColorDown := $F7DAA2;
        ColorDisabled := $FFD1AD;
      end;
      tsWindows7:
      begin
        Color := $FDFBFA;
        ColorDown := $FCEBDC;
        ColorDisabled := $FFD1AD;
      end;
      tsTerminal:
      begin
        Color := clSilver;
        ColorDown := clHighLight;
        ColorDisabled := clBtnFace;
      end;
      tsOffice2010Blue:
      begin
        Color := $FDFAF8;
        ColorDown := $F0DAC7;
        ColorDisabled := $00F2F2F2;
        ColorHot := $F2EBE4;
      end;
      tsOffice2010Silver:
      begin
        Color := $F9EFE4;
        ColorDown := $FFFFFF;
        ColorDisabled := $00F2F2F2;
      end;
      tsOffice2010Black:
      begin
        Color := $828282;
        ColorDown := $C9C9C9;
        ColorDisabled := $00F2F2F2;
        ColorHot := $949494; //$A49990;
      end;
       tsWindows8, tsWindows10:
      begin
        TabAppearance.FAppearance.SimpleLayout := True;
        TabAppearance.FAppearance.Rounding := 0;
        Color := clWhite;
        ColorDown := $F7F6F5;
        ColorDisabled := $00F2F2F2;
        ColorHot := $FFFDFD;
      end;
       tsOffice2013White:
      begin
        TabAppearance.FAppearance.SimpleLayout := True;
        TabAppearance.FAppearance.Rounding := 0;
        Color := clWhite;
        ColorDown := clWhite;
        ColorDisabled := $EEEEEE;
        ColorHot := clWhite;
      end;
       tsOffice2013LightGray:
      begin
        TabAppearance.FAppearance.SimpleLayout := True;
        TabAppearance.FAppearance.Rounding := 0;
        Color := $F6F6F6;
        ColorDown := $FAFAFA;
        ColorDisabled := $EEEEEE;
        ColorHot := $FAFAFA;
      end;
       tsOffice2013Gray:
      begin
        TabAppearance.FAppearance.SimpleLayout := True;
        TabAppearance.FAppearance.Rounding := 0;
        Color := $E5E5E5;
        ColorDown := $F3F3F3;
        ColorDisabled := $EEEEEE;
        ColorHot := $F3F3F3;
      end;
       tsOffice2016White:
      begin
        TabAppearance.FAppearance.SimpleLayout := True;
        TabAppearance.FAppearance.SimpleLayoutBorder := True;
        TabAppearance.FAppearance.Rounding := 0;
        TabAppearance.ColorTextSelected := AppColor;
        TabAppearance.ColorTextHot := AppColor;
        TabAppearance.Appearance.Font.Color := $444444;
        Color := clWhite;
        ColorDown := clWhite;
        ColorDisabled := $EEEEEE;
        ColorHot := clWhite;
      end;
       tsOffice2016Gray:
      begin
        TabAppearance.FAppearance.SimpleLayout := True;
        TabAppearance.FAppearance.SimpleLayoutBorder := True;
        TabAppearance.FAppearance.Rounding := 0;
        TabAppearance.ColorTextSelected := $262626;
        TabAppearance.Appearance.Font.Color := $F0F0F0;
        TabAppearance.ColorTextHot := $FFFFFF;
        Color := $444444;
        ColorDown := $B2B2B2;
        ColorDisabled := $EEEEEE;
        ColorHot := $454545;
      end;
       tsOffice2016Black:
      begin
        TabAppearance.FAppearance.SimpleLayout := True;
        TabAppearance.FAppearance.SimpleLayoutBorder := True;
        TabAppearance.FAppearance.Rounding := 0;
        TabAppearance.ColorTextSelected := $FFFFFF;
        TabAppearance.ColorTextHot := $FFFFFF;
        TabAppearance.Appearance.Font.Color := $DADADA;
        Color := $252525;
        ColorDown := $363636;
        ColorDisabled := $EEEEEE;
        ColorHot := $262626;
      end;
    end;

    BevelColor := Color;
    ColorSelected := ColorDown;

    case AStyle of
      tsOffice2003Blue: BevelColor := $962D00;
    end;

    BevelColorDown := BevelColor;
    BevelColorSelected := BevelColor;
    BevelColorHot := BevelColor;
    BevelColorDisabled := BevelColor;


    case AStyle of
      tsOffice2010Blue:
      begin
      BevelColorDown := $C7B29F;
      BevelColorHot := $BAB5B1;
      BevelColorSelected := BevelColorDown;
      end;

      tsOffice2010Silver:
      begin
      BevelColorDown := $D2CDC8;
      BevelColorHot := $F8F5F3;
      BevelColorSelected := BevelColorDown;
      end;

      tsOffice2016White, tsOffice2016Gray, tsOffice2016Black:
      begin
        BevelColorSelected := PageAppearance.BorderColor;
      end;
      tsOffice2010Black:
      begin
      BevelColorDown := $6D6D6D;
      BevelColorHot := $BAB5B1;
      BevelColorSelected := BevelColorDown;
      end;
    end;
  end;

  case AStyle of
    tsOffice2003Blue:
      begin
        PageAppearance.Color := $FCE1CB;
        PageAppearance.ColorTo := $E0A57D;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $962D00;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Silver:
      begin
        TabAppearance.Color := $ECE2E1;
        TabAppearance.BevelColor := $947C7C;
        PageAppearance.Color := $ECE2E1;
        PageAppearance.ColorTo := $B39698;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $947C7C;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Olive:
      begin
        TabAppearance.Color := $CFF0EA;
        TabAppearance.BevelColor := $588060;
        PageAppearance.Color := $CFF0EA;
        PageAppearance.ColorTo := $8CC0B1;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $588060;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Classic:
      begin
        TabAppearance.Color := clWhite;
        TabAppearance.BevelColor := $808080;
        PageAppearance.Color := clWhite;
        PageAppearance.ColorTo := $C9D1D5;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $808080;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Luna:
      begin
        TabAppearance.Color := $FFEFE3;
        TabAppearance.BevelColor := $FFD1AD;
        PageAppearance.Color := $00F3E5DA;
        PageAppearance.ColorTo := $00F0DED0;
        PageAppearance.ColorMirror := $00EDD9C8;
        PageAppearance.ColorMirrorTo := $00FFF4E3;
        PageAppearance.BorderColor := $FFD1AD;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Obsidian:
      begin
        TabAppearance.Color := $F9F8F8;
        TabAppearance.BevelColor := clBlack;
        PageAppearance.Color := RGB(215, 219, 224);
        PageAppearance.ColorTo := RGB(194, 199, 207);
        PageAppearance.ColorMirror := RGB(181, 188, 198);
        PageAppearance.ColorMirrorTo := RGB(231, 240, 240);
        PageAppearance.BorderColor := clBlack;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsWindowsXP:
      begin
        TabAppearance.Color := clWhite;
        TabAppearance.BevelColor := clBlack;
        PageAppearance.Color := clWhite;
        PageAppearance.ColorTo := clBtnFace;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := clBlack;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsWhidbey:
      begin
        TabAppearance.Color := $F5F9FA;
        TabAppearance.BevelColor := $962D00;
        PageAppearance.Color := $F5F9FA;
        PageAppearance.ColorTo := $A8C0C0;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $962D00;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        TabAppearance.Color := $FAEEEB;
        TabAppearance.BevelColor := clBlack;
        PageAppearance.Color := RGB(241, 244, 248);
        PageAppearance.ColorTo := RGB(227, 232, 240);
        PageAppearance.ColorMirror := $00E8DED9;
        PageAppearance.ColorMirrorTo := RGB(239, 246, 247);
        PageAppearance.BorderColor := clBlack;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
     tsWindowsVista:
      begin
        TabAppearance.Color := $FFFDF9;
        TabAppearance.BevelColor := $FCF2DA;
        PageAppearance.Color := $FFFDF9;
        PageAppearance.ColorTo := $FFFAF0;
        PageAppearance.ColorMirror := $FEF9F0;
        PageAppearance.ColorMirrorTo := $FDF0D7;
        PageAppearance.BorderColor := $FCF2DA;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
     tsWindows7:
      begin
        TabAppearance.Color := $FCEBDC;
        TabAppearance.BevelColor := $CEA27D;
        PageAppearance.Color := $FCEBDC;
        PageAppearance.ColorTo := $FCDBC1;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $CEA27D;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
      tsTerminal:
      begin
        TabAppearance.Color := clSilver;
        TabAppearance.BevelColor := clGray;
        PageAppearance.Color := clBtnFace;
        PageAppearance.ColorTo := clBtnFace;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := clGray;
      end;
        tsOffice2010Blue:
      begin
        PageAppearance.Color := $FDF6EF;
        PageAppearance.ColorTo := $F0DAC7;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $C7B29F;
        PageAppearance.GradientMirrorType := gtVertical;

      end;
        tsOffice2010Silver:
      begin
        PageAppearance.Color := $FFFFFF;
        PageAppearance.ColorTo := $EDE5E0;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $D2CDC8;
        PageAppearance.GradientMirrorType := gtVertical;

      end;
        tsOffice2010Black:
      begin
        PageAppearance.Color := $BFBFBF;
        PageAppearance.ColorTo := $919191;
        PageAppearance.ColorMirror := clNone;
        PageAppearance.ColorMirrorTo := clNone;
        PageAppearance.BorderColor := $6D6D6D;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsWindows8, tsWindows10:
      begin
        PageAppearance.BorderColor := $DCDBDA;
        PageAppearance.Color := $F7F6F5;
        PageAppearance.ColorTo := $F7F6F5;
        PageAppearance.ColorMirror := $F7F6F5;
        PageAppearance.ColorMirrorTo := $F7F6F5;
        PageAppearance.GradientMirrorType := gtVertical;
      end;

    tsOffice2013White:
      begin
        PageAppearance.BorderColor := $D4D4D4;
        PageAppearance.Color := clWhite;
        PageAppearance.ColorTo := clWhite;
        PageAppearance.ColorMirror := clWhite;
        PageAppearance.ColorMirrorTo := clWhite;
        PageAppearance.GradientMirrorType := gtVertical;
      end;

    tsOffice2013LightGray:
      begin
        PageAppearance.BorderColor := $C6C6C6;
        PageAppearance.Color := $FAFAFA;
        PageAppearance.ColorTo := $FAFAFA;
        PageAppearance.ColorMirror := $FAFAFA;
        PageAppearance.ColorMirrorTo := $FAFAFA;
        PageAppearance.GradientMirrorType := gtVertical;
      end;

    tsOffice2013Gray:
      begin
        PageAppearance.BorderColor := $ABABAB;
        PageAppearance.Color := $F3F3F3;
        PageAppearance.ColorTo := $F3F3F3;
        PageAppearance.ColorMirror := $F3F3F3;
        PageAppearance.ColorMirrorTo := $F3F3F3;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
    tsOffice2016White:
      begin
        PageAppearance.BorderColor := $D4D4D4;
        PageAppearance.Color := clWhite;
        PageAppearance.ColorTo := clWhite;
        PageAppearance.ColorMirror := clWhite;
        PageAppearance.ColorMirrorTo := clWhite;
        PageAppearance.GradientMirrorType := gtVertical;
      end;

    tsOffice2016Gray:
      begin
        PageAppearance.BorderColor := $444444;
        PageAppearance.Color := $B2B2B2;
        PageAppearance.ColorTo := $B2B2B2;
        PageAppearance.ColorMirror := $B2B2B2;
        PageAppearance.ColorMirrorTo := $B2B2B2;
        PageAppearance.GradientMirrorType := gtVertical;
      end;

    tsOffice2016Black:
      begin
        PageAppearance.BorderColor := $4E4E4E;
        PageAppearance.Color := $363636;
        PageAppearance.ColorTo := $363636;
        PageAppearance.ColorMirror := $363636;
        PageAppearance.ColorMirrorTo := $363636;
        PageAppearance.GradientMirrorType := gtVertical;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.SetTabEnabled(const Value: Boolean);
begin
  if (FTabEnabled <> Value) then
  begin
    FTabEnabled := Value;
    Invalidate;
    if Assigned(FAdvSmoothTabPager) then
      FAdvSmoothTabPager.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  if Assigned(FAdvSmoothTabPager) then
    FAdvSmoothTabPager.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothTabPage.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothTabPage.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

function TAdvSmoothTabPage.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothTabPage.GetPageIndex: Integer;
begin
  if Assigned(FAdvSmoothTabPager) then
    Result := FAdvSmoothTabPager.IndexOfPage(Self)
  else
    Result := -1;
end;

function TAdvSmoothTabPage.GetThemeID: String;
begin
  Result := ClassName;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.SetPageIndex(const Value: Integer);
begin
  if Assigned(FAdvSmoothTabPager) and (Value >= 0) and (Value < FAdvSmoothTabPager.AdvSmoothTabPageCount) then
  begin
    FAdvSmoothTabPager.MoveAdvSmoothTabPage(FAdvSmoothTabPager.IndexOfPage(Self), Value);
    FAdvSmoothTabPager.Invalidate;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.AdjustClientRect(var Rect: TRect);
begin
  Rect := Classes.Rect(2, 2, Rect.Right-2, Rect.Bottom - 2);
  inherited AdjustClientRect(Rect);
end;

procedure TAdvSmoothTabPage.SetTabAppearance(const Value: TAdvSmoothTabAppearance);
begin
  FTabAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.OnPageAppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvSmoothTabPage.OnTabAppearanceChanged(Sender: TObject);
begin
  if Assigned(FAdvSmoothTabPager) then
    FAdvSmoothTabPager.Invalidate;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPage.SetPageAppearance(const Value: TGDIPFill);
begin
  FPageAppearance.Assign(Value);
end;


{ TAdvSmoothTabPager }

constructor TAdvSmoothTabPager.Create(AOwner: TComponent);
begin
  FConstructed := false;
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls] - [csOpaque];

  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;

  FOffSetX := 0;
  FOffSetY := 0;

  FTabTextOrientation := toAuto;

  FTabOffSet := 4;
  FPageMargin := 1;

  FTabPosition := tpTopCenter;

  FAdvSmoothTabPages := TDbgList.Create;

  FTabSettings := TAdvSmoothTabSettings.Create(Self);
  FTabSettings.OnChange := OnTabSettingsChanged;

  FActivePageIndex := -1;
  FHotPageIndex := -1;
  FOldHotPageIndex := -1;
  FDownPageIndex := -1;

  DoubleBuffered := true;
  Height := 250;
  Width := 500;

  FTabReorder := False;
  FTransparent := False;

  FShowFocus := true;
  TabStop := true;

  FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.CreateWnd;
var
  p: TWinControl;
  t: TAdvSmoothTabPage;
  gotpages: boolean;
  i: integer;

begin
  inherited;

  if FConstructed then
    Exit;  

  gotpages := false;

  if not (csDesigning in ComponentState) then
  begin
    p := self;

    repeat
      p := p.Parent;
    until (p is TForm) or (p is TActiveForm) or not Assigned(p);

    if Assigned(p) then
    begin
      for i := 0 to p.ComponentCount - 1 do
      begin
        if p.Components[i].Name = Name + '1' then
          gotpages := true;
      end;
    end;

  end;

  if FDesignTime and (Name <> '') and not gotpages then
  begin
    t := TAdvSmoothTabPage.Create(Owner);
    t.AdvSmoothTabPager := self;
    t.Name := Name + '1';
    t.Caption := t.Name;
    t := TAdvSmoothTabPage.Create(Owner);
    t.AdvSmoothTabPager := self;
    t.Name := Name + '2';
    t.Caption := t.Name;
    t := TAdvSmoothTabPage.Create(Owner);
    t.AdvSmoothTabPager := self;
    t.Name := Name + '3';
    t.Caption := t.Name;
    ActivePageIndex := 0;
  end;

  FConstructed := true;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothTabPager.Destroy;
var
  i: Integer;
begin
  FFill.Free;
  for I := 0 to FAdvSmoothTabPages.Count - 1 do
    TAdvSmoothTabPage(FAdvSmoothTabPages[I]).FAdvSmoothTabPager := nil;

  FAdvSmoothTabPages.Free;
  FTabSettings.Free;
  inherited;
end;

procedure TAdvSmoothTabPager.DoEnter;
begin
  inherited;
  FFocused := true;
  Changed;
end;

procedure TAdvSmoothTabPager.DoExit;
begin
  inherited;
  FFocused := false;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.MoveAdvSmoothTabPage(CurIndex, NewIndex: Integer);
var
  OldActivePage: TAdvSmoothTabPage;
begin
  if (CurIndex >= 0) and (CurIndex < FAdvSmoothTabPages.Count) and
     (NewIndex >= 0) and (NewIndex < FAdvSmoothTabPages.Count) then
  begin
    OldActivePage := ActivePage;
    FAdvSmoothTabPages.Move(CurIndex, NewIndex);
    ActivePage := OldActivePage;

    if Assigned(FOnTabMoved) then
      FOnTabMoved(Self, CurIndex, NewIndex);
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.AddAdvSmoothTabPage(SmoothTab: TAdvSmoothTabPage): integer;
begin
  Result := FAdvSmoothTabPages.IndexOf(SmoothTab);
  if (FAdvSmoothTabPages.IndexOf(SmoothTab) < 0) then
  begin
    FAdvSmoothTabPages.Add(SmoothTab);
    SmoothTab.FPageIndex := FAdvSmoothTabPages.Count - 1;    
    Result := FAdvSmoothTabPages.Count - 1;
  end;

  if (SmoothTab.Parent <> Self) then
    SmoothTab.Parent := Self;
  SmoothTab.FAdvSmoothTabPager := Self;
  SetPagePosition(SmoothTab);
  if (SmoothTab <> ActivePage) then
    SmoothTab.Visible := False;

  InvalidateTab(-1);
  if Assigned(ActivePage) then
  begin
    ActivePage.BringToFront;
    ActivePage.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.AddAdvSmoothTabPage(PageCaption: TCaption): integer;
var
  aPage: TAdvSmoothTabPage;
begin
  aPage := TAdvSmoothTabPage.Create(Self);
  aPage.Caption := PageCaption;
  aPage.FPageIndex := FAdvSmoothTabPages.Count - 1;
  Result := AddAdvSmoothTabPage(aPage);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.AlignControls(AControl: TControl;
  var ARect: TRect);
begin
  inherited;
  {if (AControl <> nil) and (AControl is TAdvSmoothTabPage) then
    SetPagePosition(TAdvSmoothTabPage(AControl))
  else if (AControl is TAdvSmoothTabPage) then}
    SetAllPagesPosition;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.Loaded;
begin
  inherited;
  FPropertiesLoaded := True;
end;

procedure TAdvSmoothTabPager.LoadFromTheme(FileName: String);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if (AComponent = PopupMenu) then
      PopupMenu := nil;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
end;

//------------------------------------------------------------------------------
procedure TAdvSmoothTabPager.DrawTab(PageIndex: Integer);
var
  R: TRect;
  g: TGPGraphics;
  roundingtype: TFillRoundingType;
  vertical: Boolean;
  righttoleft: Boolean;
  x, y, hs, vs: integer;
  c, bc, tc: TColor;
  tf: TFont;
  lf: TLogFont;
  tw, th: integer;
  d: Boolean;
  textr: TRect;
  pt: TPoint;
  textrw, textrh: integer;
  ly: TGDIPButtonLayout;
  picname: String;
  picidx: integer;

begin
  if (PageIndex < 0) or (PageIndex >= FAdvSmoothTabPages.Count) or (TabSettings.GetHeight(PageIndex) <= 0) then
    Exit;

  R := GetTabRect(PageIndex);

  if (R.Left <= -1) and (R.Right <= -1) then
    Exit;

  with AdvSmoothTabPages[PageIndex].TabAppearance do
  begin
    roundingtype := rtTop;
    vertical := false;
    righttoleft := false;
    case TabPosition of
      tpTopLeft, tpTopCenter, tpTopRight:
      begin
        vertical := false;
        righttoleft := false;
        roundingtype := rtTop;
      end;
      tpLeftCenter, tpLeftBottom, tpLeftTop:
      begin
        vertical := true;
        righttoleft := true;
        roundingtype := rtLeft;
      end;
      tpRightCenter, tpRightTop, tpRightBottom:
      begin
        vertical := true;
        righttoleft := false;
        roundingtype := rtRight;
      end;
      tpBottomCenter, tpBottomLeft, tpBottomRight:
      begin
        vertical := false;
        righttoleft := false;
        roundingtype := rtBottom;
      end;
    end;

    if TabTextOrientation = toHorizontal then
      vertical := False;

    if TabTextOrientation = toVertical then
      vertical := True;

    if FOwner.FMetroStyle then
      roundingtype := rtNone;

    hs := 0;
    vs := 0;
    if Status.Visible then
    begin
      hs := HorizontalSpacing;
      vs := VerticalSpacing;
    end;

    tc := FAppearance.Font.Color;

    d := (FDownPageIndex = PageIndex);
    if AdvSmoothTabPages[PageIndex].TabEnabled then
    begin
      if (FActivePageIndex = PageIndex) then
      begin
        c := ColorSelected;
        bc := BevelColorSelected;
        tc := ColorTextSelected;
      end
      else if (FHotPageIndex = PageIndex) then
      begin
        c := ColorHot;
        bc := BevelColorHot;
        tc := ColorTextHot;
      end
      else
      begin
        c := Color;
        bc := BevelColor;
      end;
    end
    else
    begin
      c := ColorDisabled;
      bc := BevelColorDisabled;
      tc := ColorTextDisabled;
      d := false;
    end;

    if d then
      bc := BevelColorDown;

    th := 0;
    tw := 0;
    textrw := 0;
    textrh := 0;
    if vertical then
    begin
      tf := TFont.Create;
      tf.Assign(FAppearance.Font);
      tf.Color := tc;
      GetObject(tf.Handle, SizeOf(lf), @lf);
      if RightToLeft then
      begin
        lf.lfEscapement := 900;
        lf.lfOrientation := 900;
      end
      else
      begin
        lf.lfEscapement := -900;
        lf.lfOrientation := -900;
      end;

      tf.Handle := CreateFontIndirect(lf);
      Canvas.Font.Assign(tf);
      Canvas.Brush.Style := bsClear;
      tw := Canvas.TextWidth(AdvSmoothTabPages[PageIndex].Caption);
      th := Canvas.TextHeight('gh');

      textr := Rect(R.Left + hs, r.Top + vs, r.Right - hs, r.Bottom - vs);
      textrw := textr.Right - textr.Left;
      textrh := textr.Bottom - textr.Top;
      tf.Free;
    end;

    g := TGPGraphics.Create(Canvas.Handle);
    g.SetSmoothingMode(SmoothingModeAntiAlias);

    if AdvSmoothTabPages[PageIndex].Enabled then
    begin
      picidx := FAppearance.ImageIndex;
      picname := FAppearance.PictureName;
    end
    else
    begin
      picidx := FAppearance.DisabledImageIndex;
      picname := FAppearance.DisabledPictureName;
    end;

    FAppearance.Draw(g, AdvSmoothTabPages[PageIndex].Caption, R.Left - hs, R.Top - vs, R.Right - R.Left + (hs * 2), R.Bottom - R.Top + (vs * 2), hs, vs,
      c, ColorDown, bc, tc, Shadow, d, Bevel, vertical, righttoleft, roundingtype, Picture, th, tw, true, picidx, picname);

    g.Free;

    if vertical then
    begin
      pt := Point(textr.Left + (textrw - th) div 2, textr.Top + (textrh - tw) div 2);
      if (Picture <> nil) and not Picture.Empty then
      begin
        ly := FAppearance.Layout;
        Picture.GetImageSizes;
        if TabPosition in [tpRightTop, tpRightBottom, tpRightCenter] then
        begin
          case FAppearance.Layout of
            blPictureLeft: ly := blPictureTop;
            blPictureRight: ly := blPictureBottom;
            blPictureBottom: ly := blPictureRight;
            blPictureTop: ly := blPictureLeft;
          end;
        end
        else
        begin
          case FAppearance.Layout of
            blPictureLeft: ly := blPictureBottom;
            blPictureRight: ly := blPictureTop;
            blPictureBottom: ly := blPictureRight;
            blPictureTop: ly := blPictureLeft;
          end;
        end;

        case ly of
          blPictureLeft:
          begin
            pt := Point(textr.Left + (textrw - th - picture.Width - Appearance.Spacing) div 2,
              textr.Top + (textrh - tw) div 2);
            pt.X := pt.X + Picture.Width + Appearance.Spacing;
          end;
          blPictureRight:
          begin
            pt := Point(textr.Left + (textrw - th - picture.Width - Appearance.Spacing) div 2,
              textr.Top + (textrh - tw) div 2);
          end;
          blPictureBottom:
          begin
            pt := Point(textr.Left + (textrw - th) div 2,
              textr.top + (textrh - tw - Appearance.Spacing - Picture.Height) div 2);
          end;
          blPictureTop:
          begin
            pt := Point(textr.Left + (textrw - th) div 2,
              textr.Top + (textrh - tw - Appearance.Spacing - Picture.Height) div 2);
            pt.Y := pt.Y + Picture.Height + Appearance.Spacing;
          end;
        end;
      end;

      if RightToLeft then
        Canvas.TextOut(pt.X, pt.Y + tw , AdvSmoothTabPages[PageIndex].Caption)
      else
        Canvas.TextOut(pt.X + th, pt.Y , AdvSmoothTabPages[PageIndex].Caption);
    end;

    g := TGPGraphics.Create(Canvas.Handle);
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);

    with Status do
    begin
      Appearance.CalculateSize(g, Status.Caption);
      x := R.Right + FStatus.OffsetLeft - FStatus.Appearance.GetWidth;
      y := Status.OffsetTop + R.Top;
      if Status.Visible then
        Appearance.Draw(g, Status.OffsetLeft + x, y, 0, 0, true,Status.Caption);
    end;

    if TabStop and Ffocused and (ActivePageIndex = PageIndex) and ShowFocus then
      DrawFocus(g, MakeRect(R.Left - hs, R.Top - vs, R.Right - R.Left + (hs * 2), R.Bottom - R.Top + (vs * 2)), Appearance.Rounding, roundingtype);

    g.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.DrawAllTabs;
var
  i: Integer;
begin
  for i:= 0 to FAdvSmoothTabPages.Count-1 do
    DrawTab(i);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.Paint;
var
  R: TRect;
  th: integer;
  g: TGPGraphics;
begin
  inherited;

  R := ClientRect;

  if not Transparent then
  begin
    g := TGPGraphics.Create(Canvas.Handle);
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    Fill.Fill(g, MakeRect(r.Left, r.Top, r.Right - r.Left - 1, r.Bottom - r.Top - 1));
    g.Free;
  end;

  DrawAllTabs;

  if (csDesigning in ComponentState) and (FAdvSmoothTabPages.Count = 0) then
  begin
    Canvas.Font.Assign(self.Font);
    Canvas.Brush.Style := bsClear;
    th := Canvas.TextHeight('gh');
    Canvas.TextOut(10, Height div 2, 'Right-click and choose "New Page"');
    Canvas.TextOut(10, (Height div 2) + th,'to insert a new tabsheet');
    Canvas.Font.Style := [fsItalic];
    Canvas.TextOut(10, Height div 2 + 3*th, 'If no such right-click menu option appears');
    Canvas.TextOut(10, Height div 2 + 4*th, 'please install designtime package!');
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.ReMoveAdvSmoothTabPage(SmoothTab: TAdvSmoothTabPage);
var
  i, ni: Integer;
begin
  i := FAdvSmoothTabPages.IndexOf(SmoothTab);
  if (i >= 0) then
  begin
    if i < ActivePageIndex then
      ni := ActivePageIndex - 1
    else
      ni := ActivePageIndex;

    if (ActivePage = SmoothTab) then
      SelectNextPage(True);

    FAdvSmoothTabPages.Delete(i);
    SmoothTab.FAdvSmoothTabPager := nil;

    ActivePageIndex := ni;
    InvalidateTab(-1);
    Invalidate;
    if Assigned(ActivePage) then
      ActivePage.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SetParent(AParent: TWinControl);
begin
  if (AParent is TAdvSmoothTabPager) then
    raise Exception.Create('Invalid Parent');

  inherited;

  if (not FPropertiesLoaded) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    Init;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.GetAdvSmoothTabPageCount: integer;
begin
  Result := FAdvSmoothTabPages.Count;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.GetAdvSmoothTabPages(index: integer): TAdvSmoothTabPage;
begin
  Result := TAdvSmoothTabPage(FAdvSmoothTabPages[index]);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabPager.SetSmoothTabBounds(SmoothTab: TAdvSmoothTabPage;
  var ALeft, ATop, AWidth, AHeight: Integer);
begin
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SetChildOrder(Child: TComponent; Order: Integer);
begin
  inherited SetChildOrder(Child, Order);
end;

procedure TAdvSmoothTabPager.SetColorTones(ATones: TColorTones);
var
  i: integer;
begin
  fill.Color := ATones.Foreground.BrushColor;
  fill.ColorTo := ATones.Foreground.BrushColor;
  fill.ColorMirror := ATones.Foreground.BrushColor;
  fill.ColorMirrorTo := ATones.Foreground.BrushColor;
  fill.BorderColor := ATones.Foreground.BorderColor;
  for I := 0 to FAdvSmoothTabPages.Count - 1 do
    AdvSmoothTabPages[I].SetColorTones(ATones);
end;

procedure TAdvSmoothTabPager.SetComponentStyle(AStyle: TTMSStyle);
var
  i: integer;
begin
  inherited;
  case AStyle of
    tsOffice2003Blue:
      begin
        Fill.Color := $FCE1CB;
        Fill.ColorTo := $E0A57D;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $962D00;
        Fill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Silver:
      begin
        Fill.Color := $ECE2E1;
        Fill.ColorTo := $B39698;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $947C7C;
        Fill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Olive:
      begin
        Fill.Color := $CFF0EA;
        Fill.ColorTo := $8CC0B1;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $588060;
        Fill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Classic:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := $C9D1D5;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $808080;
        Fill.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Luna:
      begin
        Fill.Color := $FFEFE3;
        Fill.ColorTo := $FFDDC4;
        Fill.ColorMirror := $FFD1AD;
        Fill.ColorMirrorTo := $FFDBC0;
        Fill.BorderColor := $FFD1AD;
        Fill.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Obsidian:
      begin
        Fill.Color := $F9F8F8;
        Fill.ColorTo := $E4E2DF;
        Fill.ColorMirror := $D1CBC7;
        Fill.ColorMirrorTo := $E2DEDB;
        Fill.BorderColor := clBlack;//$D1CBC7;
        Fill.GradientMirrorType := gtVertical;
      end;
    tsWindowsXP:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clBtnFace;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clBlack;
        Fill.GradientMirrorType := gtVertical;
      end;
    tsWhidbey:
      begin
        Fill.Color := $F5F9FA;
        Fill.ColorTo := $A8C0C0;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $962D00;
        Fill.GradientMirrorType := gtVertical;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        Fill.Color := $FAEEEB;
        Fill.ColorTo := $E5DBD7;
        Fill.ColorMirror := $E2D8D4;
        Fill.ColorMirrorTo := $D1C7C5;
        Fill.BorderColor := clBlack;//$E2D8D4;
        Fill.GradientMirrorType := gtVertical;
      end;
     tsWindowsVista:
      begin
        Fill.Color := $FFFDF9;
        Fill.ColorTo := $FFFAF0;
        Fill.ColorMirror := $FEF9F0;
        Fill.ColorMirrorTo := $FDF0D7;
        Fill.BorderColor := $FCF2DA;
        Fill.GradientMirrorType := gtVertical;
      end;
     tsWindows7:
      begin
        Fill.Color := $FCEBDC;
        Fill.ColorTo := $FCDBC1;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $CEA27D;
        Fill.GradientMirrorType := gtVertical;
      end;
      tsTerminal:
      begin
        Fill.Color := clBtnFace;
        Fill.ColorTo := clBtnFace;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clGray;
      end;
        tsOffice2010Blue:
      begin
        Fill.Color := $FDF6EF;
        Fill.ColorTo := $F0DAC7;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $C7B29F;
        Fill.GradientMirrorType := gtVertical;

      end;
        tsOffice2010Silver:
      begin
        Fill.Color := $FFFFFF;
        Fill.ColorTo := $EDE5E0;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $D2CDC8;
        Fill.GradientMirrorType := gtVertical;

      end;
        tsOffice2010Black:
      begin
        Fill.Color := $BFBFBF;
        Fill.ColorTo := $919191;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $6D6D6D;
        Fill.GradientMirrorType := gtVertical;
      end;

        tsWindows8, tsWindows10:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clWhite;
        Fill.GradientMirrorType := gtVertical;
      end;

        tsOffice2013White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clWhite;
        Fill.GradientMirrorType := gtVertical;
      end;

        tsOffice2013LightGray:
      begin
        Fill.Color := $F6F6F6;
        Fill.ColorTo := $F6F6F6;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $C6C6C6;
        Fill.GradientMirrorType := gtVertical;
      end;

        tsOffice2013Gray:
      begin
        Fill.Color := $E5E5E5;
        Fill.ColorTo := $E5E5E5;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $ABABAB;
        Fill.GradientMirrorType := gtVertical;
      end;

        tsOffice2016White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clWhite;
        Fill.GradientMirrorType := gtVertical;
      end;

        tsOffice2016Gray:
      begin
        Fill.Color := $444444;
        Fill.ColorTo := $444444;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $444444;
        Fill.GradientMirrorType := gtVertical;
      end;

        tsOffice2016Black:
      begin
        Fill.Color := $252525;
        Fill.ColorTo := $252525;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $252525;
        Fill.GradientMirrorType := gtVertical;
      end;

  end;
  for I := 0 to FAdvSmoothTabPages.Count - 1 do
     AdvSmoothTabPages[I].SetComponentStyle(AStyle);
end;

procedure TAdvSmoothTabPager.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    FillChanged(Self);
  end;
end;

//------------------------------------------------------------------------------


procedure TAdvSmoothTabPager.WMSize(var Message: TWMSize);
begin
  inherited;
  SetAllPagesPosition;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SetPopupMenuEx(const Value: TPopupMenu);
begin
  Inherited PopupMenu := Value;
  (*if Assigned(PopupMenu) and (PopupMenu is TAdvPopupMenu) and Assigned(FCurrentOfficePagerStyler) then
    TAdvPopupMenu(PopupMenu).MenuStyler := FCurrentOfficePagerStyler.CurrentAdvMenuStyler; *)
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.CMShowingChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.GetPopupMenuEx: TPopupMenu;
begin
  Result := Inherited PopupMenu;
end;

procedure TAdvSmoothTabPager.CMControlChange(var Message: TCMControlChange);
begin
  inherited;

  with Message do
  begin
    if (Control is TAdvSmoothTabPage) then
    begin
      if Inserting then
        //InsertControl(Control)
      else
        //RemoveControl(Control);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.CMControlListChange(
  var Message: TCMControlListChange);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
end;

procedure TAdvSmoothTabPager.CMMouseLeave(var Msg: TMessage);
var
  P: TPoint;
  R: TRect;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  // work around to avoid false call
  GetCursorPos(P);
  P := ScreenToClient(P);
  R := GetTabsRect;
  case (TabPosition) of
    tpTopLeft, tpTopRight, tpTopCenter: R.Bottom := R.Bottom - 4;
    tpBottomRight, tpBottomLeft, tpBottomCenter: R.Top := R.Top + 4;
    tpLeftTop, tpLeftBottom, tpLeftCenter: R.Right := R.Right - 4;
    tpRightTop, tpRightBottom, tpRightCenter: R.Left := R.Left + 4;
  end;

  if PtInRect(R, P) then
    Exit;

  if (FHotPageIndex = FActivePageIndex) then
  begin
    FHotPageIndex := -1;
    Invalidate;
  end
  else if (FHotPageIndex >= 0) then
  begin
    FHotPageIndex := -1;
    InvalidateTab(-1);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Tab: Integer;
begin
  inherited;
  if TabStop and not (csDesigning in ComponentState) then
    SetFocus;

  P := Point(X, Y);

  FDownPageIndex := -1;

  if PtInRect(GetTabsArea, p) then
  begin
    Tab := PTOnTab(X, Y);

    if (Tab >= 0) and (Tab <= AdvSmoothTabPageCount - 1) then
    begin
      if AdvSmoothTabPages[Tab].XYToIndicator(X, Y) then
      begin
        FStatusPageIndex := Tab;
        Exit;
      end;
    end;


    if (Tab >= 0) then
    begin
      if (Tab <> ActivePageIndex) and AdvSmoothTabPages[Tab].TabEnabled then
      begin
        // Select Tab
        FDownPageIndex := Tab;
        InvalidateTab(Tab);
        ChangeActivePage(Tab);
        Invalidate;
      end
      else
      begin
        FDownPageIndex := Tab;
        InvalidateTab(FDownPageIndex);
      end;

      if (Button = mbLeft) and AdvSmoothTabPages[Tab].TabEnabled and TabReorder then
      begin
        BeginDrag(false,4);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Tab: Integer;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  P := Point(X, Y);

  if PtInRect(GetTabsArea, p) then
  begin
    Tab := PTOnTab(X, Y);
    if (Tab >= 0) and (Tab <= AdvSmoothTabPageCount - 1) then
    begin
      if AdvSmoothTabPages[Tab].XYToIndicator(X, Y) then
      begin
        FDownPageIndex := -1;
        FHotPageIndex := -1;
        InvalidateTab(-1);
        Cursor := crHandPoint;
        Exit;
      end
      else
        Cursor := crDefault;
    end;


    if (Tab >= 0) and (Tab <> FHotPageIndex) then
    begin
      if (FDownPageIndex >= 0) then
      begin
        FDownPageIndex := -1;
        InvalidateTab(-1);
      end;

      if (FHotPageIndex >= 0) then
      begin
        OnExitTab(FHotPageIndex);
        begin
          FHotPageIndex := -1;
          InvalidateTab(-1);
        end;
      end;

      OnEnterTab(Tab);
      if AdvSmoothTabPages[Tab].TabEnabled then
      begin
        FHotPageIndex := Tab;
        FOldHotPageIndex := FHotPageIndex;
        Invalidate;
      end;
    end
    else if (Tab < 0) and (FHotPageIndex >= 0) then
    begin
      if (FDownPageIndex >= 0) then
      begin
        FDownPageIndex := -1;
        InvalidateTab(-1);
      end;
      OnExitTab(FHotPageIndex);
      if (FHotPageIndex = FActivePageIndex) and false then
      begin
        FHotPageIndex := -1;
        Invalidate;
      end
      else
      begin
        FHotPageIndex := -1;
        InvalidateTab(-1);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  P := Point(X, Y);

  if (FStatusPageIndex >= 0) and (FStatusPageIndex <= AdvSmoothTabPageCount - 1) then
  begin
    if AdvSmoothTabPages[FStatusPageIndex].XYToIndicator(X, Y) then
    begin
      if Assigned(OnStatusClick) then
        OnStatusClick(Self, FStatusPageIndex);
      Exit;
    end;
  end;


  if (FDownPageIndex >= 0) then
  begin
    FDownPageIndex := -1;
    InvalidateTab(-1);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I: Integer;
  Control: TControl;
begin
  for I := 0 to FAdvSmoothTabPages.Count - 1 do Proc(TComponent(FAdvSmoothTabPages[I]));

  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if (Control.Owner = Root) and (FAdvSmoothTabPages.IndexOf(Control) < 0) then Proc(Control);
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.FillChanged(Sender: TObject);
begin
  Invalidate;
end;

function TAdvSmoothTabPager.FindNextPage(CurPage: TAdvSmoothTabPage; GoForward,
  CheckTabVisible: Boolean): TAdvSmoothTabPage;
var
  i, j, CurIndex: Integer;
begin
  Result := nil;
  CurIndex := FAdvSmoothTabPages.IndexOf(CurPage);


  if (CurPage = nil) or (CurIndex < 0) then
  begin

    if FAdvSmoothTabPages.Count > 0 then
    begin
      if GoForward then
        Result := FAdvSmoothTabPages[0]
      else
        Result := FAdvSmoothTabPages[FAdvSmoothTabPages.Count - 1];
    end;
    Exit;
  end;
    
  if GoForward then
  begin
    i := CurIndex;
    j := 0; //1;
    while (j < FAdvSmoothTabPages.Count) do
    begin
      Inc(i);    
      if (i >= FAdvSmoothTabPages.Count) then
        i := 0;
      if (CheckTabVisible and AdvSmoothTabPages[i].TabVisible) or not CheckTabVisible then
      begin
        Result := AdvSmoothTabPages[i];
        Break;
      end;
      Inc(j);
    end;
  end
  else  // BackWard
  begin
    i := CurIndex;
    j := 0; //1;
    while (j < FAdvSmoothTabPages.Count) do
    begin
      dec(i);
      if (i >= FAdvSmoothTabPages.Count) then
        i := 0;
      if (i < 0) then
        i := FAdvSmoothTabPages.Count-1;
      if (CheckTabVisible and AdvSmoothTabPages[i].TabVisible) or not CheckTabVisible then
      begin
        Result := AdvSmoothTabPages[i];
        Break;
      end;
      Inc(j);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.GetActivePage: TAdvSmoothTabPage;
begin
  Result := nil;
  if (ActivePageIndex >= 0) and (ActivePageIndex < FAdvSmoothTabPages.Count) then
    Result := AdvSmoothTabPages[FActivePageIndex];
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.GetActivePageIndex: Integer;
begin
  Result := FActivePageIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothTabPager.SelectNextPage(GoForward: Boolean);
var
  i, j: Integer;
begin
  if (ActivePageIndex < 0) then
    Exit;

  if GoForward then
  begin
    i := ActivePageIndex;
    j := 0; //1;
    while (j < FAdvSmoothTabPages.Count) do
    begin
      Inc(i);
      if (i >= FAdvSmoothTabPages.Count) then
        i := 0;
      if (ActivePage <> AdvSmoothTabPages[i]) and AdvSmoothTabPages[i].TabVisible and AdvSmoothTabPages[i].TabEnabled then
      begin
        ActivePageIndex := i;
        Break;
      end;
      Inc(j);
    end;
  end
  else  // BackWard
  begin
    i := ActivePageIndex;
    j := 0; //1;
    while (j < FAdvSmoothTabPages.Count) do
    begin
      dec(i);
      if (i >= FAdvSmoothTabPages.Count) then
        i := 0;
      if (i < 0) then
        i := FAdvSmoothTabPages.Count-1;
      if (ActivePage <> AdvSmoothTabPages[i]) and AdvSmoothTabPages[i].TabVisible and AdvSmoothTabPages[i].TabEnabled then
      begin
        ActivePageIndex := i;
        Break;
      end;
      Inc(j);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.IndexOfPage(SmoothTab: TAdvSmoothTabPage): Integer;
begin
  Result := FAdvSmoothTabPages.IndexOf(SmoothTab);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SetActivePage(const Value: TAdvSmoothTabPage);
begin
  if (FAdvSmoothTabPages.IndexOf(Value) >= 0) then
    ActivePageIndex := FAdvSmoothTabPages.IndexOf(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.ChangeActivePage(PageIndex: Integer);
var
  aForm: TCustomForm;
  AllowChange: Boolean;
begin
  if (PageIndex >= 0) and (PageIndex < FAdvSmoothTabPages.Count) and (PageIndex <> ActivePageIndex) then
  begin
    AllowChange := True;
    if Assigned(FOnChanging) and FPropertiesLoaded and not (csDestroying in ComponentState) then
      FOnChanging(Self, ActivePageIndex, PageIndex, AllowChange);

    if not AllowChange then
      Exit;

    if (ActivePageIndex >= 0) and (ActivePageIndex < FAdvSmoothTabPages.Count) then
    begin
      AdvSmoothTabPages[FActivePageIndex].Visible := False;

      if Assigned(AdvSmoothTabPages[FActivePageIndex].FOnHide) then
        AdvSmoothTabPages[FActivePageIndex].FOnHide(AdvSmoothTabPages[FActivePageIndex]);
    end;

    FActivePageIndex := PageIndex;
    AdvSmoothTabPages[FActivePageIndex].Visible := True;
    AdvSmoothTabPages[FActivePageIndex].BringToFront;

    if Assigned(FOnChange) and not (csDestroying in ComponentState)
      and not (csLoading in ComponentState) then
      FOnChange(Self);

    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      aForm := GetParentForm(Self);
      if (aForm <> nil) and (aForm.Designer <> nil) then
        aForm.Designer.Modified;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SetActivePageIndex(const Value: Integer);
var
  R: TRect;
begin
  ChangeActivePage(Value);
  R := GetTabsArea;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SetTabTextOrientation(
  const Value: TAdvSmoothTabTextOrientation);
begin
  if (FTabTextOrientation <> Value) then
  begin
    FTabTextOrientation := Value;
    SetAllPagesPosition;
    Invalidate;
    if Assigned(ActivePage) then
      ActivePage.Invalidate;
  end;
end;

procedure TAdvSmoothTabPager.SetTabSettings(const Value: TAdvSmoothTabSettings);
begin
  FTabSettings.Assign(Value);
end;

procedure TAdvSmoothTabPager.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SetAllPagesPosition;
var
  i: Integer;
begin
  for i:= 0 to FAdvSmoothTabPages.Count-1 do
  begin
    SetPagePosition(TAdvSmoothTabPage(FAdvSmoothTabPages[i]));
  end;
end;

function TAdvSmoothTabPager.GetSmoothTabRect: TRect;
begin
  Result := ClientRect;
  if TabSettings.GetHeight(ActivePageIndex) = 0 then
    result := ClientRect
  else
  begin
    case TabPosition of
      tpTopLeft, tpTopRight, tpTopCenter:
      begin
        Result.Top := Result.Top + TabSettings.GetHeight(ActivePageIndex);
        Result.Left := Result.Left + FPageMargin;
        Result.Right := Result.Right-FPageMargin;
        Result.Bottom := Result.Bottom - FPageMargin-1;
      end;
      tpBottomLeft, tpBottomRight, tpBottomCenter:
      begin
        Result.Top := Result.Top + FPageMargin+1;
        Result.Left := Result.Left + FPageMargin;
        Result.Right := Result.Right-FPageMargin;
        Result.Bottom := Result.Bottom - TabSettings.GetHeight(ActivePageIndex);
      end;
      tpLeftTop, tpLeftBottom, tpLeftCenter:
      begin
        Result.Top := Result.Top + FPageMargin+1;
        Result.Left := Result.Left + TabSettings.GetHeight(ActivePageIndex);
        Result.Right := Result.Right-FPageMargin;
        Result.Bottom := Result.Bottom - FPageMargin-1;
      end;
      tpRightTop, tpRightBottom, tpRightCenter:
      begin
        Result.Top := Result.Top + FPageMargin+1;
        Result.Left := Result.Left + FPageMargin;
        Result.Right := Result.Right- TabSettings.GetHeight(ActivePageIndex);
        Result.Bottom := Result.Bottom - FPageMargin-1;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SetPagePosition(SmoothTab: TAdvSmoothTabPage);
var
  R: TRect;
begin
  if (SmoothTab <> nil) and (FAdvSmoothTabPages.IndexOf(SmoothTab) >= 0) then
  begin
    R := GetSmoothTabRect;
    {
    SmoothTab.Left := R.Left;
    SmoothTab.Top := R.Top;
    SmoothTab.Width := R.Right - R.Left;
    SmoothTab.Height := R.Bottom - R.Top;
    }
    SmoothTab.SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  end;
end;

function TAdvSmoothTabPager.IndexOfTabAt(X,Y: Integer): integer;
begin
  Result := PtOnTab(x,y);
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.PTOnTab(X, Y: Integer): Integer;
var
  i: Integer;
  P: TPoint;
  TabR: TRect;
begin
  Result := -1;
  P := Point(X, Y);
  for i:= 0 to FAdvSmoothTabPages.Count-1 do
  begin
    TabR := GetTabRect(i);
    if PtInRect(TabR, P) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.GetTabsArea: TRect;
begin
  Result := ClientRect;
  case TabPosition of
    tpTopLeft, tpTopRight, tpTopCenter: Result.Bottom := Result.Top + FTabSettings.GetHeight(ActivePageIndex);
    tpBottomLeft, tpBottomRight, tpBottomCenter: Result.Top := Result.Bottom - FTabSettings.GetHeight(ActivePageIndex);
    tpLeftTop, tpLeftBottom, tpLeftCenter: Result.Right := Result.Left + FTabSettings.GetHeight(ActivePageIndex);
    tpRightTop, tpRightBottom, tpRightCenter: Result.Left := Result.Right - FTabSettings.GetHeight(ActivePageIndex);
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.GetTabsRect: TRect;
begin
  Result := ClientRect;
  case TabPosition of
    tpTopLeft, tpTopRight, tpTopCenter:
    begin
      Result.Top := Result.Top + FTabOffSet;
      Result.Bottom := Result.Top + FTabSettings.GetHeight(ActivePageIndex);
      Result.Left := Result.Left + FTabSettings.StartMargin + FPageMargin;
      Result.Right := Result.Right - FTabSettings.EndMargin;
    end;
    tpBottomLeft, tpBottomRight, tpBottomCenter:
    begin
      Result.Top := Result.Bottom - FTabSettings.GetHeight(ActivePageIndex) - FTabOffSet;
      Result.Bottom := Result.Bottom - FTabOffSet;
      Result.Left := Result.Left + FTabSettings.StartMargin + FPageMargin;
      Result.Right := Result.Right - FTabSettings.EndMargin;
    end;
    tpLeftTop, tpLeftBottom, tpLeftCenter:
    begin
      Result.Top := Result.Top + FTabSettings.StartMargin + FPageMargin;
      Result.Bottom := Result.Bottom - FTabSettings.EndMargin;
      Result.Left := Result.Left + FTabOffSet;
      Result.Right := Result.Left + FTabSettings.GetHeight(ActivePageIndex);
    end;
    tpRightTop, tpRightBottom, tpRightCenter:
    begin
      Result.Top := Result.Top + FTabSettings.StartMargin + FPageMargin;
      Result.Bottom := Result.Bottom - FTabSettings.EndMargin;
      Result.Left := Result.Right - TabSettings.GetHeight(ActivePageIndex) - FTabOffSet;
      Result.Right := Result.Right - FTabOffSet;
    end;
  end;
end;

function TAdvSmoothTabPager.GetThemeID: String;
begin
  Result := ClassName;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.GetTabRect(PageIndex: Integer): TRect;
var
  totals: integer;
  r, rlast: TREct;
  ilast: integer;
  I: Integer;
begin
  r := GetTabRect(0, 0, -1);
  iLast := 0;
  for I := FAdvSmoothTabPages.Count - 1 downto 0 do
  begin
    if AdvSmoothTabPages[I].TabVisible then
    begin
      iLast := I;
      break;
    end;
  end;

  rlast := GetTabRect(0,ilast, -1);
  totals := -1;
  case TabPosition of
    tpBottomCenter, tpTopCenter, tpBottomRight, tpTopRight: totals := (rlast.Right - r.left) + TabSettings.Spacing;
    tpLeftCenter, tpLeftBottom, tpRightCenter, tpRightBottom: totals := (rlast.Bottom - r.Top) + TabSettings.Spacing;
  end;

  Result := GetTabRect(0, PageIndex, totals);
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.GetTabRect(StartIndex, PageIndex: Integer; totalsize: integer): TRect;
var
  i, TbW, Sp: Integer;
  R, CR: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  Sp := FTabSettings.Spacing; //0;
  if (PageIndex >= 0) and (PageIndex < FAdvSmoothTabPages.Count) then
  begin
    if not AdvSmoothTabPages[PageIndex].TabVisible then
      Exit;

    CR := GetTabsRect; // ClientRect;

    if totalsize <> -1 then
    begin
      case TabPosition of
        tpBottomRight, tpTopRight: CR.Left := CR.Right - totalsize;
        tpBottomCenter, tpTopCenter: CR.Left := (CR.Right - totalsize) div 2;
        tpLeftBottom, tpRightBottom: CR.Top := CR.Bottom - totalsize;
        tpLeftCenter, tpRightCenter: CR.Top := (CR.Bottom - totalsize) div 2;
      end;
    end;

    begin
      for i := StartIndex to PageIndex do
      begin
        if not AdvSmoothTabPages[i].TabVisible then
          Continue;

        Canvas.Font.Assign(AdvSmoothTabPages[i].TabAppearance.Appearance.Font);

        if (TabPosition in [tpTopLeft, tpTopRight, tpTopCenter, tpBottomLeft, tpBottomRight, tpBottomCenter]) then
        begin
          TbW := TabSettings.GetWidth(i);

          if not AdvSmoothTabPages[i].TabAppearance.Picture.Empty then
          begin
            AdvSmoothTabPages[i].TabAppearance.Picture.GetImageSizes;
            R := Rect(CR.Left, CR.Top, CR.Left + TbW + Sp + AdvSmoothTabPages[i].TabAppearance.Picture.Width + AdvSmoothTabPages[i].TabAppearance.Appearance.Spacing, CR.Bottom);
            CR.Left := CR.Left + TbW + Sp + AdvSmoothTabPages[i].TabAppearance.Picture.Width + AdvSmoothTabPages[i].TabAppearance.Appearance.Spacing;
          end
          else
          begin
            R := Rect(CR.Left, CR.Top, CR.Left + TbW, CR.Bottom);
            CR.Left := CR.Left + TbW + Sp;
          end;

          if (i = PageIndex) then
            Result := R;
        end
        else 
        begin
          TbW := TabSettings.GetWidth(i);

          if not AdvSmoothTabPages[i].TabAppearance.Picture.Empty then
          begin
            AdvSmoothTabPages[i].TabAppearance.Picture.GetImageSizes;
            R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbW + AdvSmoothTabPages[i].TabAppearance.Picture.Height + AdvSmoothTabPages[i].TabAppearance.Appearance.Spacing);
            CR.Top := CR.Top + TbW + Sp + AdvSmoothTabPages[i].TabAppearance.Picture.Height + AdvSmoothTabPages[i].TabAppearance.Appearance.Spacing;
          end
          else
          begin
            R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbW);
            CR.Top := CR.Top + TbW + Sp;
          end;

          if (i = PageIndex) then
            Result := R;          
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.GetTabRect(Page: TAdvSmoothTabPage): TRect;
begin
  Result := GetTabRect(FAdvSmoothTabPages.IndexOf(Page));
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  p: TPoint;
  Tab: Integer;
begin
  Tab := -1;
  if (csDesigning in ComponentState) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);

    if PtInRect(GetTabsRect, p) and (GetAsyncKeyState(VK_LBUTTON) <> 0) then
    begin
      Tab := PTOnTab(P.X, P.Y);
      if (Tab >= 0) then
      begin
        // Select Tab
        Msg.Result := 1;
      end;
    end;

  end;

  if (Tab = -1) then
    inherited;
end;

procedure TAdvSmoothTabPager.InvalidateTab(PageIndex: Integer);
var
  R: TRect;
begin
  if (PageIndex >= 0) and (PageIndex < FAdvSmoothTabPages.Count) then
    R := GetTabRect(PageIndex)
  else
    R := GetTabsArea;
  InvalidateRect(Handle, @R, True);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.OnEnterTab(PageIndex: Integer);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.OnExitTab(PageIndex: Integer);
begin

end;

procedure TAdvSmoothTabPager.OnTabSettingsChanged(Sender: TObject);
begin
  SetAllPagesPosition;
  Invalidate;
  if Assigned(ActivePage) then
    ActivePage.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.UpdateMe(PropID: integer);
begin
  Invalidate;
  if Assigned(ActivePage) then
    ActivePage.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;
end;

procedure TAdvSmoothTabPager.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  {$IFNDEF DELPHI_UNICODE}
  dbl: boolean;
  {$ENDIF}
  p: TPoint;
  i: integer;
begin
  if not Transparent then
  begin
    inherited;
    Exit;
  end;

  if Assigned(Parent) {and (Fill.ShadowOffset > 0) ?} then
  begin
    DC := Message.DC;
    if DC <> 0 then
    begin
      {$IFNDEF DELPHI_UNICODE}
      dbl := Parent.DoubleBuffered;
      Parent.DoubleBuffered := false;
      {$ENDIF}
      i := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(DC, p.x, p.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(DC, nil);
      RestoreDC(DC, i);
      {$IFNDEF DELPHI_UNICODE}
      Parent.DoubleBuffered := dbl;
      {$ENDIF}
    end;
  end;

  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SeTAdvSmoothTabPosition(const Value: TAdvSmoothTabPosition);
begin
  if (FTabPosition <> Value) then
  begin
    FTabPosition := Value;
    SetAllPagesPosition;
    Invalidate;
    if Assigned(ActivePage) then
      ActivePage.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  for I:= 0 to FAdvSmoothTabPages.Count-1 do
    if IsAccel(Message.CharCode, AdvSmoothTabPages[I].Caption) and CanShowTab(I) and CanFocus then
    begin
      Message.Result := 1;
      ActivePageIndex := I;
      Exit;
    end;
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.CanShowTab(PageIndex: Integer): Boolean;
begin
  Result := (PageIndex >= 0) and (PageIndex < FAdvSmoothTabPages.Count) and (AdvSmoothTabPages[PageIndex].TabVisible);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.SetPageMargin(const Value: integer);
begin
  if FPageMargin <> Value then
  begin
    FPageMargin := Value;
    Changed;
  end;
end;

function TAdvSmoothTabPager.GetVisibleTabCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FAdvSmoothTabPages.Count-1 do
  begin
    if (AdvSmoothTabPages[I].TabVisible) then
      Result := Result + 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.Init;
begin
  FPropertiesLoaded := true;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.WMKeyDown(var Message: TWMKeyDown);
var
  Ctrl: TWinControl;
begin
  case Message.CharCode of
    VK_LEFT, VK_UP:
    begin
      SelectNextPage(False);
    end;
    VK_RIGHT, VK_DOWN:
    begin
      SelectNextPage(True);
    end;
    VK_TAB:
    begin
      if Assigned(Self.Parent) then
      begin
        Ctrl := TProWinControl(Self.Parent).FindNextControl(Self, True, True, True);
        if Assigned(Ctrl) and Ctrl.CanFocus then
        begin
          Ctrl.SetFocus;
        end;
      end;
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS{ + DLGC_WANTTAB};
  {using DLGC_WANTTAB, disabled default Tab key functioning}
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.CMDialogKey(var Message: TCMDialogKey);
begin
  (*if TabStop and Assigned(ActivePage) and (Message.CharCode = 18{ALT}) then
  begin
    if not FTabShortCutHintShowing and (CanFocus) then
    begin
      if not Focused then
        Self.SetFocus;
      Message.Result := 1;
      ShowShortCutHintOfAllPages;
      Exit;
    end
    else if FTabShortCutHintShowing then
    begin
      HideShortCutHintOfAllPages;
      Message.Result := 1;
      Exit;
    end;
  end; *)
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.CMFocusChanged(var Message: TCMFocusChanged);
{var
  i: Integer;
  h: HWND;
  Active: Boolean;}
begin
  inherited;
  InvalidateTab(-1);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.CMHintShow(var Msg: TCMHintShow);
var
  hi: PHintInfo;
  Tab: integer;
  CanShow: boolean;
begin
  hi := PHintInfo(Msg.HintInfo);
  hi^.HintStr := Hint;

  if PtInRect(GetTabsArea, hi^.CursorPos) then
  begin
    Tab := PTOnTab(hi^.CursorPos.X, hi^.CursorPos.Y);
    if (Tab >= 0) and (Tab <= AdvSmoothTabPageCount - 1) then
    begin
      hi^.HintStr := AdvSmoothTabPages[Tab].TabHint;
    end;
  end;

  CanShow := (hi^.HintStr <> '');

  Msg.Result := Ord(Not CanShow);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.WndProc(var Msg: TMessage);
var
  p: TWinControl;
begin
  if (Msg.Msg = WM_DESTROY) then
  begin
    // restore subclassed proc
    if not (csDesigning in ComponentState) and Assigned(FFormWndProc) then
    begin
      p := self;
      repeat
        p := p.Parent;
      until (p is TForm) or (p is TActiveForm) or not Assigned(p);

      if (p <> nil) then
      begin
        p.WindowProc := FFormWndProc;
        FFormWndProc := nil;
      end;
    end;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.DragDrop(Source: TObject; X, Y: Integer);
var
  CurIndex, NewIndex: Integer;
begin
  inherited;
  CurIndex := ActivePageIndex;
  NewIndex := PTOnTab(X, Y);
  if (CurIndex >= 0) and (CurIndex < AdvSmoothTabPageCount) and (NewIndex >= 0) and (NewIndex < AdvSmoothTabPageCount) and (CurIndex <> NewIndex) then
  begin
    MoveAdvSmoothTabPage(CurIndex, NewIndex);
    Invalidate;
    if Assigned(ActivePage) then
      ActivePage.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTabPager.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  i: Integer;
begin
  inherited;
  i := PTOnTab(X, Y);
  Accept := (i >= 0) and (i < AdvSmoothTabPageCount) and (Source = Self);
end;

function TAdvSmoothTabPager.UseOldDrawing: Boolean;
begin
  Result := true;
end;

//------------------------------------------------------------------------------

function TAdvSmoothTabPager.IsActivePageNeighbour(
  PageIndex: Integer): Integer;
var
  i: Integer;  
begin
  Result := 0;
  if (PageIndex = ActivePageIndex) or (PageIndex < 0) or (PageIndex >= AdvSmoothTabPageCount) then
    Exit;

  if (PageIndex < ActivePageIndex) then
  begin
    for i:= ActivePageIndex - 1 downto PageIndex do
    begin
      if AdvSmoothTabPages[i].TabVisible then
      begin
        if (i = PageIndex) then
          Result := -1;
        Break;
      end;
    end;
  end
  else // if (PageIndex > ActivePageIndex) then
  begin
    for i:= ActivePageIndex + 1 to PageIndex do
    begin
      if AdvSmoothTabPages[i].TabVisible then
      begin
        if (i = PageIndex) then
          Result := 1;
        Break;
      end;
    end;
  end;
end;

{ TAdvSmoothTabAppearance }

procedure TAdvSmoothTabAppearance.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTabAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTabAppearance) then
  begin
    FAppearance.Assign((Source as TAdvSmoothTabAppearance).Appearance);
    FStatus.Assign((Source as TAdvSmoothTabAppearance).Status);
    FBevel := (Source as TAdvSmoothTabAppearance).Bevel;
    FBevelColor := (Source as TAdvSmoothTabAppearance).BevelColor;
    FColor := (Source as TAdvSmoothTabAppearance).Color;
    FPicture.Assign((Source as TAdvSmoothTabAppearance).Picture);
    FShadow := (Source as TAdvSmoothTabAppearance).Shadow;
    FHorizontalSpacing := (Source as TAdvSmoothTabAppearance).HorizontalSpacing;
    FVerticalSpacing := (Source as TAdvSmoothTabAppearance).VerticalSpacing;
    FColorSelected := (Source as TAdvSmoothTabAppearance).ColorSelected;
    FColorDown := (Source as TAdvSmoothTabAppearance).ColorDown;
    FColorDisabled := (Source as TAdvSmoothTabAppearance).ColorDisabled;
    FColorHot := (Source as TAdvSmoothTabAppearance).ColorHot;
    FBevelColorSelected := (Source as TAdvSmoothTabAppearance).BevelColorSelected;
    FBevelColorDown := (Source as TAdvSmoothTabAppearance).BevelColorDown;
    FBevelColorDisabled := (Source as TAdvSmoothTabAppearance).BevelColorDisabled;
    FBevelColorHot := (Source as TAdvSmoothTabAppearance).BevelColorHot;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothTabAppearance.Create(AOwner: TAdvSmoothTabPage);
begin
  FOwner := AOwner;
  FAppearance := TGDIPButton.Create;
  FAppearance.OnChange := AppearanceChanged;
  FStatus :=  TAdvSmoothTabStatus.Create(Self);
  FStatus.OnChange := StatusChanged;
  FBevel := true;
  FBevelColor := clGray;
  FColor := clSilver;
  FPicture := TAdvGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FShadow := false;
  FHorizontalSpacing := 0;
  FVerticalSpacing := 0;
  FColorDown := clGray;
  FColorSelected := $AAD9FF;
  FColorDisabled := $545454;
  FColorHot := clWhite;
  FColorTextHot := clBlack;
  FColorTextDisabled := clGray;
  FColorTextSelected := clBlack;

  FBevelColorDown := clGray;
  FBevelColorSelected := $AAD9FF;
  FBevelColorDisabled := $545454;
  FBevelColorHot := clWhite;

  if FOwner.FDesigntime then
  begin
    FStatus.FAppearance.Fill.Color := clRed;
    FStatus.FAppearance.Fill.GradientType := gtSolid;
    FStatus.FAppearance.Fill.BorderColor := clGray;
    FStatus.FAppearance.Font.Color := clWhite;
  end;
end;

destructor TAdvSmoothTabAppearance.Destroy;
begin
  FAppearance.Free;
  FStatus.Free;
  FPicture.Free;
  inherited;
end;

procedure TAdvSmoothTabAppearance.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTabAppearance.SetAppearance(const Value: TGDIPButton);
begin
  if FAppearance <> value then
  begin
    FAppearance := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetBevel(const Value: boolean);
begin
  if FBevel <> value then
  begin
    FBevel := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetBevelColor(const Value: TColor);
begin
  if FBevelColor <> value then
  begin
    FBevelColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetBevelColorDisabled(const Value: TColor);
begin
  if FBevelColorDisabled <> Value then
  begin
    FBevelColorDisabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetBevelColorDown(const Value: TColor);
begin
  if FBevelColorDown <> Value then
  begin
    FBevelColorDown := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetBevelColorHot(const Value: TColor);
begin
  if FBevelColorHot <> Value then
  begin
    FBevelColorHot := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetBevelColorSelected(const Value: TColor);
begin
  if FBevelColorSelected <> Value then
  begin
    FBevelColorSelected := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetStatus(
  const Value: TAdvSmoothTabStatus);
begin
  if FStatus <> value then
  begin
    FStatus.Assign(Value);
    StatusChanged(Self);
  end;
end;

procedure TAdvSmoothTabAppearance.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetColorDisabled(const Value: TColor);
begin
  if FColorDisabled <> value then
  begin
    FColorDisabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetColorDown(const Value: TColor);
begin
  if FColorDown <> value then
  begin
    FColorDown := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetColorHot(const Value: TColor);
begin
  if FColorHot <> value then
  begin
    FColorHot := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetColorSelected(const Value: TColor);
begin
  if FColorSelected <> value then
  begin
    FColorSelected := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetColorTextDisabled(const Value: TColor);
begin
  if (FColorTextDisabled <> Value) then
  begin
    FColorTextDisabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetColorTextSelected(const Value: TColor);
begin
  if (FColorTextSelected <> Value) then
  begin
    FColorTextSelected := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetHorizontalSpacing(const Value: integer);
begin
  if FHorizontalSpacing <> value then
  begin
    FHorizontalSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetPicture(const Value: TAdvGDIPPicture);
begin
  if FPicture <> value then
  begin
    FPicture.Assign(Value);
    PictureChanged(Self);
  end;
end;

procedure TAdvSmoothTabAppearance.SetShadow(const Value: boolean);
begin
  if FShadow <> value then
  begin
    FShadow := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.SetVerticalSpacing(const Value: integer);
begin
  if FVerticalSpacing <> value then
  begin
    FVerticalSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabAppearance.StatusChanged(Sender: TObject);
begin
  Changed;
end;

{ TAdvSmoothTabStatus }

procedure TAdvSmoothTabStatus.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTabStatus.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTabStatus) then
  begin
    FAppearance.Assign((Source as TAdvSmoothTabStatus).Appearance);
    FCaption := (Source as TAdvSmoothTabStatus).Caption;
    FOffsetTop := (Source as TAdvSmoothTabStatus).OffsetTop;
    FOffsetLeft := (Source as TAdvSmoothTabStatus).OffsetLeft;
    FVisible := (Source as TAdvSmoothTabStatus).Visible;
  end;
end;

procedure TAdvSmoothTabStatus.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothTabStatus.Create(AOwner: TAdvSmoothTabAppearance);
begin
  FOwner := AOwner;
  FOwner := AOwner;
  FOffsetTop := 0;
  FOffsetLeft := 0;
  FVisible := False;
  FCaption := '0';
  FAppearance := TGDIPStatus.Create;
  FAppearance.OnChange := AppearanceChanged;
end;

destructor TAdvSmoothTabStatus.Destroy;
begin
  FAppearance.Free;
  inherited;
end;

procedure TAdvSmoothTabStatus.SetAppearance(const Value: TGDIPStatus);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTabStatus.SetCaption(const Value: String);
begin
  if FCaption <> value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabStatus.SetOffsetLeft(const Value: integer);
begin
  if FOffsetLeft <> value then
  begin
    FOffsetLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabStatus.SetOffsetTop(const Value: integer);
begin
  if FOffsetTop <> value then
  begin
    FOffsetTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTabStatus.SetVisible(const Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

end.
