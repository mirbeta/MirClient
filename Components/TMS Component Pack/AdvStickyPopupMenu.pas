{***************************************************************************}
{ TAdvStickyPopupMenu component                                             }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2007 - 2014                                        }
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

unit AdvStickyPopupMenu;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Forms, Dialogs, Controls, Graphics, Messages, ExtCtrls,
  SysUtils, Math, ImgList, Menus, AdvMenus, AtbXPVS, ActnList, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 6; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history

  // v1.0.0.0 : first release
  // v1.1.0.0 : New : GroupIndex added for radiobutton items
  //          : Fixed : issue with relative positioning on child forms
  //          : Fixed : issue with checkbox/radiobutton painting on classic themed Vista
  // v1.1.1.0 : Improved : changed from TImageList to TCustomImageList
  //          : Fixed : update of checkbox from button click event solved
  // v1.1.2.0 : New : public property IsVisible added, true when menu is being displayed
  // v1.2.0.0 : New : TStickyMenuItem.Visible property added
  // v1.3.0.0 : New : menu item style isControlItem added to just display a control
  // v1.3.0.1 : Fixed : issue with HideOnClick for ButtonBar buttons
  // v1.3.0.2 : Improved : display of menu at borders of screen
  // v1.3.0.3 : Improved : multimonitor support
  // v1.3.0.4 : Fixed : issue with updating IsVisible property
  // v1.3.0.5 : Improved : position of checkbox on iconbar
  // v1.3.0.6 : Fixed : issue with using datepicker control on the menu
  // v1.3.0.7 : Fixed : drawing item image for itControl type
  // v1.3.1.0 : New : Added BeginUpdate/EndUpdate methods
  // v1.3.1.1 : Fixed : Issue with use on multi-monitor systems
  // v1.3.1.2 : Fixed : Issue with splitter appearance
  // v1.3.1.3 : Fixed : Issue with initial popup menu positioning in newer Delphi versions
  // v1.3.1.4 : Fixed : Issue with OnMenuHide on destroy
  // v1.3.1.5 : Fixed : Issue with using panels with controls as menu control
  // v1.3.2.0 : New : Name property added in TStickyMenuItem
  // v1.4.0.0 : New : Support for Actions added
  // v1.4.0.1 : Fixed : Issue with transparent disabled image drawing
  // v1.5.0.0 : New : Hint property added to menu items
  // v1.5.0.1 : Improved : Issue with controls on popupmenu that did not have alNone alignment
  // v1.6.0.0 : New : Added keyboard support
  // v1.6.1.0 : New : Hint support added to buttonbar buttons

  BTNOFFSET_X = 4;
  BTN_SPACE = 1;
  IMG_GAP = 3;

  ITEM_SPACE = 1;
  MIN_MENUWIDTH = 80;
  MIN_MENUHEIGHT = 50;

  ROUND_RADIUS = 2;
  CHECK_SIZE = 15;
  RADIO_SIZE = 10;

  SEPARATOR_SIZE = 5;

type
  TAdvStickyPopupMenu = class;
  TStickyMenuItem = class;

  TStickyMenuItemStyle = (isMenuItem, isCheckBox, isRadioButton, isSplitter, isControlItem);
  TAdvControlStyle = (csClassic,csFlat, csTMS, csGlyph, csTheme, csWinXP, csBorland);
  TDisplayRelative = (drScreen, drForm);

  TItemClickEvent = procedure (Sender: TObject; Index: Integer) of object;
  TCheckClickEvent = procedure (Sender: TObject; Index: Integer; Checked: boolean) of object;

  TOnDrawImage = procedure (Sender: TObject; Canvas: Tcanvas; ARect: TRect; Selected: Boolean) of object;
  TOnDrawItem = procedure (Sender: TObject; Canvas: TCanvas; ARect: TRect; Selected: Boolean) of object;
  TOnHideQuery = procedure (Sender: TObject; var CanHide: Boolean) of object;


  TStickyMenuItemActionLink = class(TActionLink)
  protected
    FClient: TStickyMenuItem;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetEnabled(Value: Boolean); override;
  end;

  TStickyMenuItem = class(TCollectionItem)
  private
    FHideOnClick: Boolean;
    FEnabled: boolean;
    FImageIndex: Integer;
    FTag: integer;
    FCaption: string;
    FControl: TComponent;
    FItemObject: TObject;
    FStyle: TStickyMenuItemStyle;
    FAutoCheck: Boolean;
    FChecked: Boolean;
    FOnClick: TNotifyEvent;
    FOnDrawImage: TOnDrawImage;
    FOnDrawItem: TOnDrawItem;
    FGroupIndex: Integer;
    FVisible: Boolean;
    FName: string;
    FActionLink: TStickyMenuItemActionLink;
    FHint: string;
    procedure SetCaption(const Value: string);
    procedure SetControl(const Value: TComponent);
    procedure SetEnabled(const Value: boolean);
    procedure SetHideOnClick(const Value: Boolean);
    procedure SetImageIndex(const Value: Integer);
    procedure SetItemObject(const Value: TObject);
    procedure SetStyle(const Value: TStickyMenuItemStyle);
    procedure SetAutoCheck(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);
    procedure UpdateExclusive;
    procedure SetGroupIndex(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    function GetAction: TBasicAction;
    procedure SetAction(const Value: TBasicAction);
    procedure DoActionChange(Sender: TObject);
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean);
  protected
    procedure Click;
    function GetDisplayName: string; override;
    procedure InvalidatePanel;

    property ActionLink: TStickyMenuItemActionLink read FActionLink write FActionLink;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ItemObject: TObject read FItemObject write SetItemObject;
  published
    property Action: TBasicAction read GetAction write SetAction;
    property AutoCheck: Boolean read FAutoCheck write SetAutoCheck default true;
    property Checked: Boolean read FChecked write SetChecked default false;
    property Style: TStickyMenuItemStyle read FStyle write SetStyle;
    property Control: TComponent read FControl write SetControl;
    property Caption: string read FCaption write SetCaption;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property HideOnClick: Boolean read FHideOnClick write SetHideOnClick default false;
    property Hint: string read FHint write FHint;
    property Enabled: boolean read FEnabled write SetEnabled  default true;
    property ImageIndex: Integer read FImageIndex write SetImageIndex  default -1;
    property Name: string read FName write FName;
    property Tag: integer read FTag write FTag default 0;
    property Visible: Boolean read FVisible write SetVisible default true;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDrawImage: TOnDrawImage read FOnDrawImage write FOnDrawImage;
    property OnDrawItem: TOnDrawItem read FOnDrawItem write FOnDrawItem;
  end;

  TStickyMenuItems = class(TCollection)
  private
    FOwner: TAdvStickyPopupMenu;
    FOnVisibleChanged: TNotifyEvent;
    function GetItem(Index: Integer): TStickyMenuItem;
    procedure SetItem(Index: Integer; const Value: TStickyMenuItem);
  protected
    function GetOwner: TPersistent; override;
    
    property OnVisibleChanged: TNotifyEvent read FOnVisibleChanged write FOnVisibleChanged;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TStickyMenuItem read GetItem write SetItem; default;
    function Add: TStickyMenuItem;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer): TStickyMenuItem;
  end;

  TButtonBarItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FTag: integer;
    FCaption: string;
    FHideOnClick: Boolean;
    FOnClick: TNotifyEvent;
    FEnabled: Boolean;
    FHint: string;
    procedure SetCaption(const Value: string);
    procedure SetImageIndex(const Value: Integer);
    function GetLeft: Integer;
    function GetWidth: Integer;
    procedure SetHideOnClick(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure Click;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Width: Integer read GetWidth;
    property Left: Integer read GetLeft;
  published
    property Caption: string read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property HideOnClick: Boolean read FHideOnClick write SetHideOnClick default false;
    property Hint: string read FHint write FHint;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Tag: integer read FTag write FTag default 0;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TButtonBar = class(TCollection)
  private
    FOwner: TAdvStickyPopupMenu;
    function GetItem(Index: Integer): TButtonBarItem;
    procedure SetItem(Index: Integer; const Value: TButtonBarItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TButtonBarItem read GetItem write SetItem; default;
    function Add: TButtonBarItem;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer): TButtonBarItem;
  end;

  TAdvStickyMenuPanel = class(TCustomControl)
  private
    FOwner: TComponent;
    FOnShouldHide: TNotifyEvent;
    FShowBorder: Boolean;
    FAdvStickyPopupMenu: TAdvStickyPopupMenu;
    FButtonIndexHot: Integer;
    FButtonIndexDown: Integer;
    FMenuItemIndexHot: Integer;
    FMenuItemIndexDown: Integer;
    FMouseOnIconBar: Boolean;
    FDonotHideMenu: Boolean;
    FUpdateCount: Integer;
    FHintIndex: Integer;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure OnMenuItemsVisibleChanged(Sender: TObject);
    function GetStyler: TCustomAdvMenuStyler;
    procedure ButtonClick(Index: Integer);
    procedure MenuItemClick(Index: Integer);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure DrawIconBar(Canvas: TCanvas);
    procedure DrawButtonBar(Canvas: TCanvas);
    procedure DrawButton(Canvas: TCanvas; Index: Integer);
    procedure DrawItemsBackGround(Canvas: TCanvas);
    procedure DrawItems(Canvas: TCanvas);
    procedure DrawItem(Canvas: TCanvas; Index: Integer);

    procedure InvalidateButton(Index: Integer);

    function ButtonAtPos(X, Y: Integer): Integer;
    function ItemAtPos(X, Y: Integer): Integer;
    procedure SelectItem(Index: Integer);
    procedure HideMenuWindow;

    property OnShouldHide: TNotifyEvent read FOnShouldHide write FOnShouldHide;
    property ShowBorder: Boolean read FShowBorder write FShowBorder;

    function ButtonBarVisible: Boolean;
    function GetTotalButtonsWidth: Integer;
    function GetButtonBarHeight: Integer;
    function GetMaxItemWidth(var ItemWidth: Integer): TStickyMenuItem;
    function GetTotalItemsHeight: Integer;
    function GetButtonBarRect: TRect;
    function GetButtonRect(Index: Integer): TRect;
    function GetClientRectEx: TRect;
    function GetItemHeight: Integer;
    function GetItemsRect: TRect;
    function GetItemRect(Index: Integer; var ItemWithCtrlRect: TRect): TRect; overload;
    function GetItemRect(Index: Integer): TRect; overload;
    procedure InitializeAndUpdate;

    function FindMenuItemFromAccel(Accel: Word): TStickyMenuItem;
    function FindButtonFromAccel(Accel: Word): TButtonBarItem;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    function ButtonBar: TButtonBar;
    function MenuItems: TStickyMenuItems;
    property Styler: TCustomAdvMenuStyler read GetStyler;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetIconBarRect: TRect;
    procedure InvalidateMenuItem(Index: Integer);
    property MenuItemIndexHot: integer read FMenuItemIndexHot;
    property AdvStickyPopupMenu: TAdvStickyPopupMenu read FAdvStickyPopupMenu write FAdvStickyPopupMenu;
  end;

  TAdvStickyMenuWindow = class(TCustomForm)
  private
    FHideOnDeActivate: Boolean;
    FOwner: TComponent;
    FShowBorder: Boolean;
    FHideTimer: TTimer;
    FBorderColor: TColor;
    FAdvStickyMenuPanel: TAdvStickyMenuPanel;
    FOnHideQuery: TOnHideQuery;
    FAdvStickyPopupMenu: TAdvStickyPopupMenu;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure HideTimerOnTime(Sender: TObject);
  protected
    procedure Loaded; override;
    procedure Paint; override;
    function GetParentWnd: HWnd;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure UpdateSize;

    property HideOnDeActivate: Boolean read FHideOnDeActivate write FHideOnDeActivate;
    property ShowBorder: Boolean read FShowBorder write FShowBorder;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property AdvStickyMenuPanel: TAdvStickyMenuPanel read FAdvStickyMenuPanel write FAdvStickyMenuPanel;
    property AdvStickyPopupMenu: TAdvStickyPopupMenu read FAdvStickyPopupMenu write FAdvStickyPopupMenu;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property OnHideQuery: TOnHideQuery read FOnHideQuery write FOnHideQuery;
  published
end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvStickyPopupMenu = class(TComponent)
  private
    FOwner: TComponent;
    FStyler: TCustomAdvMenuStyler;
    FInternalStyler: TCustomAdvMenuStyler;
    FCurrentStyler: TCustomAdvMenuStyler;
    FButtonImages: TCustomImageList;
    FMenuItems: TStickyMenuItems;
    FButtonBar: TButtonBar;
    FStickyMenuPanel: TAdvStickyMenuPanel;
    FStickyMenuWindow: TAdvStickyMenuWindow;
    FItemImages: TCustomImageList;
    FItemOffSetX: Integer;
    FItemOffSetY: Integer;
    FItemMarginX: Integer;
    FItemMarginY: Integer;
    FIsVisible: boolean;
    FOnRadioClick: TItemClickEvent;
    FOnCheckClick: TCheckClickEvent;
    FOnButtonClick: TItemClickEvent;
    FOnItemClick: TItemClickEvent;
    FDisplayRelative: TDisplayRelative;
    FOnMenuHide: TNotifyEvent;
    FOnMenuShow: TNotifyEvent;
    FShowButtonBar: Boolean;
    FOnMenuHideQuery: TOnHideQuery;
    procedure OnStickyMenuWindowHide(Sender: TObject);
    procedure OnStickyMenuWindowClose(Sender: TObject; var Action: TCloseAction);
    procedure OnStickyMenuWindowHideQuery(Sender: TObject; var CanHide: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetButtonImages(const Value: TCustomImageList);
    procedure SetStyler(const Value: TCustomAdvMenuStyler);
    procedure SetMenuItems(const Value: TStickyMenuItems);
    procedure SetButtonBar(const Value: TButtonBar);
    procedure SetItemImages(const Value: TCustomImageList);
    procedure SetShowButtonBar(const Value: Boolean);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure InitializeMenu;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function GetVersionNr: integer;
    procedure ShowMenu(X, Y: Integer);
    procedure HideMenu;
    property IsVisible: boolean read FIsVisible write FIsVisible;
    property MenuPanel: TAdvStickyMenuPanel read FStickyMenuPanel;
  published
    property ButtonBar: TButtonBar read FButtonBar write SetButtonBar;
    property DisplayRelative: TDisplayRelative read FDisplayRelative write FDisplayRelative default drScreen;
    property ButtonImages: TCustomImageList read FButtonImages write SetButtonImages;
    property ItemImages: TCustomImageList read FItemImages write SetItemImages;
    property MenuItems: TStickyMenuItems read FMenuItems write SetMenuItems;
    property ShowButtonBar: Boolean read FShowButtonBar write SetShowButtonBar default true;
    property Styler: TCustomAdvMenuStyler read FStyler write SetStyler;
    property Version: string read GetVersion write SetVersion;
    property OnMenuHideQuery: TOnHideQuery read FOnMenuHideQuery write FOnMenuHideQuery;
    property OnButtonClick: TItemClickEvent read FOnButtonClick write FOnButtonClick;
    property OnItemClick: TItemClickEvent read FOnItemClick write FOnItemClick;
    property OnCheckClick: TCheckClickEvent read FOnCheckClick write FOnCheckClick;
    property OnRadioClick: TItemClickEvent read FOnRadioClick write FOnRadioClick;
    property OnMenuShow: TNotifyEvent read FOnMenuShow write FOnMenuShow;
    property OnMenuHide: TNotifyEvent read FOnMenuHide write FOnMenuHide;
  end;

implementation

//------------------------------------------------------------------------------

function GetTextSize(Canvas: TCanvas; WinCtrl: TWinControl; Text: string; font: TFont): TSize;
var
  ACanvas: TCanvas;
  R: TRect;
begin
  Result.cx := 0;
  Result.cy := 0;
  if (not Assigned(WinCtrl) and not Assigned(Canvas)) or (Text = '') then
    Exit;

  ACanvas := Canvas;
  if not Assigned(ACanvas) then
  begin
    ACanvas := TCanvas.Create;
    ACanvas.Handle := GetWindowDC(WinCtrl.Handle);
  end;
  ACanvas.Font.Assign(font);

  R := Rect(0, 0, 1000, 200);
  DrawText(ACanvas.Handle,PChar(Text),Length(Text), R, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
  Result.cx := R.Right - R.Left;
  Result.cy := R.Bottom - R.Top;

  if not Assigned(Canvas) then
    ACanvas.Free;
end;

//----------------------------------------------------------------- DrawGradient

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if Direction then
    R.Right := R.Right - 1
  else
    R.Bottom := R.Bottom - 1;

  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      else
        Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawGradientBackGround(Canvas: TCanvas; FromColor, ToColor, ColorMirror, ColorMirrorTo, BorderColor: TColor; Steps: Integer; R: TRect; Direction: TGradientDirection; RoundRadius: Integer);
var
  rgn: HRGN;
  R1, R2: TRect;
begin
  if not Assigned(Canvas) then
    Exit;

  rgn := 0;
  if (RoundRadius > 0) then
  begin
    rgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, RoundRadius, RoundRadius);
    SelectClipRgn(Canvas.Handle, rgn);
  end;

  if (ColorMirror <> clNone) and (ColorMirrorTo <> clNone) then
  begin
    R1 := R;
    R1.Bottom := R.Top + (R.Bottom - R.Top) div 2;
    R2 := R;
    R2.Top := R1.Bottom;

    DrawGradient(Canvas, FromColor, ToColor, Steps, R1, Direction = gdHorizontal);
    DrawGradient(Canvas, ColorMirror, ColorMirrorTo, Steps, R2, Direction = gdHorizontal);
  end
  else
  begin
    if (FromColor <> clNone) and (ToColor <> clNone) then
      DrawGradient(Canvas, FromColor, ToColor, Steps, R, Direction = gdHorizontal)
    else if (FromColor <> clNone) then
    begin
      Canvas.Brush.Color := FromColor;
      Canvas.Pen.Color := FromColor;
      Canvas.FillRect(R);
    end;
  end;

  if (BorderColor <> clNone) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := BorderColor;
    if (RoundRadius > 0) then
      Canvas.RoundRect(R.Left, R.Top, R.Right - 1, R.Bottom - 1, RoundRadius, RoundRadius)
    else
      Canvas.Rectangle(R);
  end;

  if (RoundRadius > 0) and (rgn <> 0) then
  begin
    SelectClipRgn(Canvas.Handle, 0);
    DeleteObject(rgn);
  end;
end;

//------------------------------------------------------------------------------

procedure DrawContent(Canvas: TCanvas; Caption: string; R: TRect; Images: TCustomImageList; ImageIndex, OffsetX: Integer; Enabled, DrwImg: Boolean);
var
  bmp: TBitmap;
begin
  if not Assigned(Canvas) then
    Exit;

  R.Left := R.Left + OffSetX;
  if Assigned(Images) and (ImageIndex >= 0) then
  begin
    if DrwImg then
    begin
      if not Enabled then // fix strange disabled image paint issue
      begin
        bmp := TBitmap.Create;
        bmp.Width := Images.Width;
        bmp.Height := Images.Height;
        bmp.Canvas.Brush.Color := clFuchsia;
        bmp.Canvas.Pen.Color := clFuchsia;
        bmp.Canvas.Rectangle(0, 0, bmp.Width, bmp.Height);
        Images.Draw(bmp.Canvas, 0, 0, ImageIndex, Enabled);
        bmp.Transparent := true;
        bmp.TransparentMode := tmAuto;
        Canvas.Draw(R.Left, R.Top + (R.Bottom - R.Top - Images.Height) div 2, bmp);
        bmp.Free;
      end
      else
        Images.Draw(Canvas, R.Left, R.Top + (R.Bottom - R.Top - Images.Height) div 2, ImageIndex, Enabled);
    end;
    R.Left := R.Left + Images.Width + IMG_GAP;
  end;

  if (Caption <> '') then
  begin
    R.Left := R.Left + 2;
    Canvas.Brush.Style := bsClear;
    if not Enabled then
      Canvas.Font.Color := clGray;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R, DT_SINGLELINE or DT_VCENTER or DT_LEFT);
  end;
end;

//------------------------------------------------------------------------------

function IsSeparatorItem(Item: TStickyMenuItem): Boolean;
begin
  Result := False;
  if not Assigned(Item) then
    Exit;

  Result := (Item.Style = isSplitter); //(Item.Caption = '-');
end;

//------------------------------------------------------------------------------

function IsWinXP: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  Result := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));
end;

//------------------------------------------------------------------------------

function BlendColor(Col1,Col2:TColor; BlendFactor:Integer): TColor;
var
  r1,g1,b1: Integer;
  r2,g2,b2: Integer;

begin
  if BlendFactor >= 100 then
  begin
    Result := Col1;
    Exit;
  end;
  if BlendFactor <= 0 then
  begin
    Result := Col2;
    Exit;
  end;

  Col1 := Longint(ColorToRGB(Col1));
  r1 := GetRValue(Col1);
  g1 := GetGValue(Col1);
  b1 := GetBValue(Col1);

  Col2 := Longint(ColorToRGB(Col2));
  r2 := GetRValue(Col2);
  g2 := GetGValue(Col2);
  b2 := GetBValue(Col2);

  r1 := Round( BlendFactor/100 * r1 + (1 - BlendFactor/100) * r2);
  g1 := Round( BlendFactor/100 * g1 + (1 - BlendFactor/100) * g2);
  b1 := Round( BlendFactor/100 * b1 + (1 - BlendFactor/100) * b2);

  Result := RGB(r1,g1,b1);
end;

//------------------------------------------------------------------------------

procedure Draw3DLine(Canvas: TCanvas; FromPoint, ToPoint: TPoint; Embossed: Boolean; VerticalLine: Boolean = true);
begin
  with Canvas do
  begin
    if Embossed then
      Pen.Color := clWhite
    else
      Pen.Color := clBtnShadow;

    if VerticalLine then
    begin
      MoveTo(FromPoint.X - 1, FromPoint.Y - 1);
      LineTo(ToPoint.X - 1, ToPoint.Y);
      LineTo(ToPoint.X + 1, ToPoint.Y);
    end
    else
    begin
      MoveTo(FromPoint.X - 1, FromPoint.Y + 1);
      LineTo(FromPoint.X - 1, FromPoint.Y - 1);
      LineTo(ToPoint.X + 1, ToPoint.Y - 1);
    end;

    if Embossed then
      Pen.Color := clBtnShadow
    else
      Pen.Color := clWhite;

    if VerticalLine then
    begin
      MoveTo(ToPoint.X + 1, ToPoint.Y);
      LineTo(ToPoint.X + 1, FromPoint.Y);
      LineTo(ToPoint.X - 1, FromPoint.Y);
    end
    else
    begin
      MoveTo(ToPoint.X + 1, ToPoint.Y - 1);
      LineTo(ToPoint.X + 1, ToPoint.Y + 1);
      LineTo(FromPoint.X, FromPoint.Y + 1);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawCheck(WinCtrl: TWinControl; Canvas: TCanvas; R:TRect; State, Enabled: Boolean; ControlStyle: TAdvControlStyle);
var
  DrawState: Integer;
  DrawRect: TRect;
  BMP: TBitmap;
  HTheme: THandle;
begin
  //if ControlLook.NoDisabledCheckRadioLook or ControlLook.CheckAlwaysActive then
    //Enabled := true;

  case ControlStyle of
  csClassic, csFlat:
    begin
      if State then
        DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED
      else
        DrawState := DFCS_BUTTONCHECK;

      if ControlStyle = csFlat then
        DrawState := DrawState or DFCS_FLAT;

      if not Enabled then
        DrawState := DrawState or DFCS_INACTIVE;

      DrawRect.Left := R.Left + (R.Right - R.Left - CHECK_SIZE) div 2;
      DrawRect.Top:= R.Top + (R.Bottom - R.Top - CHECK_SIZE) div 2;
      DrawRect.Right := DrawRect.Left + CHECK_SIZE;
      DrawRect.Bottom := DrawRect.Top + CHECK_SIZE;

      (*if UseRightToLeftAlignment then
      begin
        DRect := DrawRect;

        if not FNoRTLOrientation then
        begin
          DrawRect.Left := ClientWidth - DrawRect.Left;
          DrawRect.Right := ClientWidth - DrawRect.Right;
        end
        else
        begin
          DrawRect.Left := DrawRect.Left - XYOffset.X;
          DrawRect.Right := DrawRect.Right + XYOffset.X;
        end;

        Hold := DrawRect.Left;
        DrawRect.Left := DrawRect.Right;
        DrawRect.Right := Hold;
        {$IFDEF DELPHI6_LVL}
        ChangeGridOrientation(False);
        {$ELSE}
        SetGridOrientation(False);
        {$ENDIF}
      end;
      *)

      DrawFrameControl(Canvas.Handle,DrawRect,DFC_BUTTON,DrawState);

      (*if UseRightToLeftAlignment then
      begin
        {$IFDEF DELPHI6_LVL}
        ChangeGridOrientation(True);
        {$ELSE}
        SetGridOrientation(True);
        {$ENDIF}
        DrawRect := DRect;
      end;
      *)
    end;
  csTMS:
    begin
      Bmp := TBitmap.Create;
      if State then
      begin
        if Enabled then
          Bmp.LoadFromResourceName(hinstance,'ASGCHK01')
        else
          Bmp.LoadFromResourceName(hinstance,'ASGCHK03');
      end
      else
      begin
        if Enabled then
          Bmp.LoadFromResourceName(hinstance,'ASGCHK02')
        else
          Bmp.LoadFromResourceName(hinstance,'ASGCHK04');
      end;

      Bmp.Transparent := True;
      Bmp.TransparentMode := tmAuto;

      (*if UseRightToLeftAlignment then
      begin
        if not FNoRTLOrientation then
        begin
          R.Left := ClientWidth - R.Left;
          R.Right := ClientWidth - R.Right;
        end
        else
        begin
          R.Left := R.Left - XYOffset.X;
          R.Right := R.Right + XYOffset.X;
        end;

        Hold := R.Left;
        R.Left := R.Right;
        R.Right := Hold;

        {$IFDEF DELPHI6_LVL}
        ChangeGridOrientation(False);
        {$ELSE}
        SetGridOrientation(False);
        {$ENDIF}
      end;
      *)
      Canvas.Draw(R.Left,R.Top,bmp);
      Bmp.free;

      (*if UseRightToLeftAlignment then
      begin
        {$IFDEF DELPHI6_LVL}
        ChangeGridOrientation(True);
        {$ELSE}
        SetGridOrientation(True);
        {$ENDIF}
      end;
      *)
    end;
  csGlyph:
    begin
      (*if UseRightToLeftAlignment then
      begin
        if not FNoRTLOrientation then
        begin
          R.Left := ClientWidth - R.Left;
          R.Right := ClientWidth - R.Right;
        end
        else
        begin
          R.Left := R.Left - XYOffset.X;
          R.Right := R.Right + XYOffset.X;
        end;

        Hold := R.Left;
        R.Left := R.Right;
        R.Right := Hold;

        {$IFDEF DELPHI6_LVL}
        ChangeGridOrientation(False);
        {$ELSE}
        SetGridOrientation(False);
        {$ENDIF}
      end;

      if State and not ControlLook.CheckedGlyph.Empty then
      begin
        ControlLook.CheckedGlyph.Transparent := True;
        ControlLook.CheckedGlyph.TransparentMode := tmAuto;
        Canvas.Draw(R.Left,R.Top,ControlLook.CheckedGlyph);
      end;

      if not State and not ControlLook.UnCheckedGlyph.Empty then
      begin
        ControlLook.UnCheckedGlyph.Transparent := True;
        ControlLook.UnCheckedGlyph.TransparentMode := tmAuto;
        Canvas.Draw(R.Left,R.Top,ControlLook.UnCheckedGlyph);
      end;

      if UseRightToLeftAlignment then
      begin
        {$IFDEF DELPHI6_LVL}
        ChangeGridOrientation(True);
        {$ELSE}
        SetGridOrientation(True);
        {$ENDIF}
      end;
      *)
    end;
  csTheme:
    begin
      if true{FIsWinXP} then
      begin
        HTheme := OpenThemeData(WinCtrl.Handle,'button');

        r := Rect(R.Left, R.Top, R.Left + CHECK_SIZE, R.Top + CHECK_SIZE);

        OffsetRect(r, 2, 0);

        if State then
        begin
          if Enabled then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL,@r,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDDISABLED,@r,nil);
        end
        else
        begin
          if Enabled then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL,@r,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDDISABLED,@r,nil);
        end;

        CloseThemeData(HTheme);

      end;
    end;
  csWinXP:
    begin
      Bmp := TBitmap.Create;
      if State then
      begin
        if Enabled then
          Bmp.LoadFromResourceName(hinstance,'ASGCHK05')
        else
          Bmp.LoadFromResourceName(hinstance,'ASGCHK07');
      end
      else
      begin
        if Enabled then
          Bmp.LoadFromResourceName(hinstance,'ASGCHK06')
        else
          Bmp.LoadFromResourceName(hinstance,'ASGCHK08');
      end;

      Bmp.Transparent := True;
      Bmp.TransparentMode := tmAuto;

      (*if UseRightToLeftAlignment then
      begin
        if not FNoRTLOrientation then
        begin
          R.Left := ClientWidth - R.Left;
          R.Right := ClientWidth - R.Right;
        end
        else
        begin
          R.Left := R.Left - XYOffset.X;
          R.Right := R.Right + XYOffset.X;
        end;

        Hold := R.Left;
        R.Left := R.Right;
        R.Right := Hold;

        {$IFDEF DELPHI6_LVL}
        ChangeGridOrientation(False);
        {$ELSE}
        SetGridOrientation(False);
        {$ENDIF}
      end;
      *)
      Canvas.Draw(R.Left,R.Top,bmp);
      Bmp.free;

      (*if UseRightToLeftAlignment then
      begin
        {$IFDEF DELPHI6_LVL}
        ChangeGridOrientation(True);
        {$ELSE}
        SetGridOrientation(True);
        {$ENDIF}
      end;
      *)
    end;

  csBorland:
    begin
      if Enabled then
        Canvas.Brush.Color := clBtnFace
      else
        Canvas.Brush.Color := clBtnShadow;

      Canvas.Pen.Color := clBtnFace;
      Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
      Canvas.Pen.Color := clBtnHighLight;
      Canvas.MoveTo(R.Left,R.Bottom);
      Canvas.LineTo(R.Left,R.Top);
      Canvas.LineTo(R.Right,R.Top);
      Canvas.Pen.Color := clBtnShadow;
      Canvas.LineTo(R.Right,R.Bottom);
      Canvas.LineTo(R.Left,R.Bottom);

      (*if UseRightToLeftAlignment then
      begin

        if not FNoRTLOrientation then
        begin
          R.Left := ClientWidth - R.Left;
          R.Right := ClientWidth - R.Right;
        end
        else
        begin
          R.Left := R.Left - XYOffset.X;
          R.Right := R.Right + XYOffset.X;
        end;

        Hold := R.Left;
        R.Left := R.Right;
        R.Right := Hold;

        {$IFDEF DELPHI6_LVL}
        ChangeGridOrientation(False);
        {$ELSE}
        SetGridOrientation(False);
        {$ENDIF}

      end;

      if State then
      begin
        if Enabled then
          Canvas.Pen.Color := FControlLook.Color
        else
          Canvas.Pen.Color := clGray;

        Canvas.Pen.Width := 1;
        Dec(R.Top);
        Dec(R.Bottom);
        Canvas.MoveTo(R.Left + 2,R.Top + FControlLook.CheckSize div 2 + 1);
        Canvas.LineTo(R.Left + 2,R.Bottom - 1);
        Canvas.MoveTo(R.Left + 3,R.Top + FControlLook.CheckSize div 2);
        Canvas.LineTo(R.Left + 3,R.Bottom - 2);
        Canvas.MoveTo(R.Left + 2,R.Bottom - 1);
        Canvas.LineTo(R.Right - 2,R.Top + 3);
        Canvas.MoveTo(R.Left + 3,R.Bottom - 1);
        Canvas.LineTo(R.Right - 1,R.Top + 3);
      end;

      if UseRightToLeftAlignment then
      begin
        {$IFDEF DELPHI6_LVL}
        ChangeGridOrientation(True);
        {$ELSE}
        SetGridOrientation(True);
        {$ENDIF}
      end;
      *)
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawRadio(WinCtrl: TWinControl; Canvas: TCanvas; ControlStyle: TAdvControlStyle; R:TRect; Num, Idx: Integer; dir, dis: Boolean;
   sl: TStrings; Selected:boolean; ACol,ARow: integer; SelectionColor: TColor);
var
  DrawState: Integer;
  DrawRect: TRect;
  DrawNum: Integer;
  DrawOfs,Th: Integer;
  s: string;
  Bmp: TBitmap;
  RadioOn: Boolean;
  HTheme: THandle;
  //OldColor: TColor;

begin
  //if ControlLook.NoDisabledCheckRadioLook or ControlLook.RadioAlwaysActive then
    //dis := false;

  DrawOfs := 0;
  SetBkMode(Canvas.Handle,TRANSPARENT);

  for DrawNum := 1 to Num do
  begin
    RadioOn := False;
    s := '';

    (*if Assigned(sl) then
    begin
      if Selected {and ShowSelection} then
        Canvas.Font.Color := SelectionColor;

      if DrawNum <= sl.Count then
      begin
        s := sl.Strings[DrawNum - 1];
        if (idx = -1) and (s = Cells[ACol,ARow]) then
          RadioOn := True;
      end;
    end;
    *)
    if (DrawNum - 1 = Idx) then
     RadioOn := True;

    case ControlStyle of
    csClassic,csFlat:
      begin
        DrawState := DFCS_BUTTONRADIO;

        if ControlStyle = csFlat then
          DrawState := DrawState or DFCS_FLAT;

        if Dis then
          DrawState := DrawState or DFCS_INACTIVE;

        if RadioOn then
          DrawState := DrawState or DFCS_CHECKED;

        if dir then
        begin
          DrawRect.Left := DrawOfs + R.Left + 2 + (DrawNum-1) * RADIO_SIZE;
          DrawRect.Top := R.Top + (R.Bottom - R.Top - RADIO_SIZE) div 2;

          if s <> '' then
          begin
            Canvas.TextOut(DrawRect.Left + RADIO_SIZE,DrawRect.Top - 2,s);
            DrawOfs := DrawOfs + Canvas.TextWidth(s);
          end;
        end
        else
        begin
          th := Canvas.TextHeight('gh');
          if s <> '' then
          begin
            DrawRect.Left := R.Left + 2;
            DrawRect.Top := R.Top + 2 + (DrawNum - 1) * th;
            Canvas.TextOut(DrawRect.Left + RADIO_SIZE + 4,DrawRect.Top - 2,s);
          end
          else
          begin
            DrawRect.Left := R.Left + (R.Right - R.Left - RADIO_SIZE) div 2;
            DrawRect.Top := R.Top + 2 + (DrawNum - 1) * th;
          end;
        end;

        if ControlStyle = csFlat then
        begin
          DrawRect.Right := DrawRect.Left + RADIO_SIZE + 2;
          DrawRect.Bottom := DrawRect.Top + RADIO_SIZE + 2;
        end
        else
        begin
          DrawRect.Right := DrawRect.Left + RADIO_SIZE;
          DrawRect.Bottom := DrawRect.Top + RADIO_SIZE;
        end;

        DrawFrameControl(Canvas.Handle,DrawRect,DFC_BUTTON,DrawState);
      end;
    csTMS, csWinXP, csGlyph:
      begin
        bmp := TBitmap.Create;

        if dir then
        begin
          DrawRect.Left := DrawOfs + R.Left + 2 + (DrawNum - 1) * 16;
          DrawRect.Top := R.Top + (R.Bottom - R.Top - 16) div 2;

          if s <> '' then
          begin
            Canvas.Textout(DrawRect.Left + 16,DrawRect.Top + 1,s);
            DrawOfs := DrawOfs + Canvas.TextWidth(s);
          end
        end
        else
        begin
          th := Max(16,Canvas.TextHeight('gh'));
          if s <> '' then
          begin
            DrawRect.Left := R.Left + 2;
            DrawRect.Top := R.Top + 2 + (DrawNum - 1) * th;
            Canvas.Textout(DrawRect.Left + 16,Drawrect.Top + 1,s);
          end
          else
          begin
            DrawRect.Left := R.Left + (R.Right - R.Left - 16) div 2;
            DrawRect.Top := R.Top + 2 + (DrawNum - 1) * th;
          end;
        end;

        if RadioOn then
        begin
          case ControlStyle of
          csTMS:
            begin
              if not dis then
                Bmp.LoadFromResourceName(hinstance,'ASGRAD01')
              else
                Bmp.LoadFromResourceName(hinstance,'ASGRAD03');
            end;
          csWinXP:
            begin
              if not dis then
                Bmp.LoadFromResourceName(hinstance,'ASGRAD05')
              else
                Bmp.LoadFromResourceName(hinstance,'ASGRAD07');
            end;
          csGlyph:
            ;//Bmp.Assign(ControlLook.RadioOnGlyph);
          end;
        end
        else
        begin
          case ControlStyle of
          csTMS:
            begin
              if not dis then
                Bmp.LoadFromResourceName(hinstance,'ASGRAD02')
              else
                Bmp.LoadFromResourceName(hinstance,'ASGRAD04');
            end;
          csWinXP:
            begin
              if not dis then
                Bmp.LoadFromResourceName(hinstance,'ASGRAD06')
              else
                Bmp.LoadFromResourceName(hinstance,'ASGRAD08');
            end;
          csGlyph:
            ;//Bmp.Assign(ControlLook.RadioOffGlyph);
          end;
        end;

        Bmp.Transparent := True;
        Bmp.TransparentMode := tmAuto;

        Canvas.Draw(DrawRect.Left,DrawRect.Top,Bmp);
        Bmp.free;
      end;
    csBorland:
      begin
        if dir then
        begin
          DrawRect.Left := DrawOfs + R.Left + 2 + (DrawNum - 1) * 16;
          DrawRect.Top := R.Top + (R.Bottom - R.Top - 16) div 2;

          if s <> '' then
          begin
           Canvas.Textout(DrawRect.Left + 16,DrawRect.Top - 2,s);
           DrawOfs := DrawOfs + Canvas.TextWidth(s);
          end
        end
        else
        begin
          th := Max(16,Canvas.TextHeight('gh'));
          if s <> '' then
          begin
            DrawRect.Left := R.Left + 2;
            DrawRect.Top := R.Top + 2 + (DrawNum - 1) * th;
            Canvas.Textout(DrawRect.Left + 16 + 2,Drawrect.Top - 2,s);
          end
          else
          begin
            DrawRect.Left := R.Left + (R.Right - R.Left - 16) div 2;
            DrawRect.Top := R.Top + 2 + (DrawNum - 1) * th;
          end;
        end;

        //OldColor := Canvas.Brush.Color;

        Canvas.Brush.Color := clBtnFace;

        Canvas.Polygon([Point(DrawRect.Left + 2,DrawRect.Top + 8),
                        Point(DrawRect.Left + 8,DrawRect.Top + 2),
                        Point(DrawRect.Left + 14,DrawRect.Top + 8),
                        Point(DrawRect.Left + 8,DrawRect.Top + 14)]);

        if RadioOn then
          Canvas.Pen.Color := clGray
        else
          Canvas.Pen.Color := clWhite;

        Canvas.MoveTo(DrawRect.Left + 8,DrawRect.Top + 14);
        Canvas.LineTo(DrawRect.Left + 2,DrawRect.Top + 8);
        Canvas.LineTo(DrawRect.Left + 8,DrawRect.Top + 2);

        if RadioOn then
          Canvas.Pen.Color := clWhite
        else
          Canvas.Pen.Color := clGray;

        Canvas.LineTo(DrawRect.Left + 14,DrawRect.Top + 8);
        Canvas.LineTo(DrawRect.Left + 8,DrawRect.Top + 14);

        {Canvas.Brush.Color := ControlLook.Color;
        Canvas.Pen.Color := ControlLook.Color;

        if RadioOn then
        Canvas.Polygon([Point(DrawRect.Left + 6,DrawRect.Top + 8),
                        Point(DrawRect.Left + 8,DrawRect.Top + 6),
                        Point(DrawRect.Left + 10,DrawRect.Top + 8),
                        Point(DrawRect.Left + 8,DrawRect.Top + 10)]);


        Canvas.Brush.Color := OldColor;
        }
      end;
    csTheme:
      begin
        if True{FIsWinXP} then
        begin

          if dir then
          begin
            DrawRect.Left := DrawOfs + R.Left + 2 + (DrawNum - 1) * 16;
            DrawRect.Top := R.Top + (R.Bottom - R.Top - 16) div 2;

            if s <> '' then
            begin
             Canvas.Textout(DrawRect.Left + 16,DrawRect.Top - 2,s);
             DrawOfs := DrawOfs + Canvas.TextWidth(s);
            end
          end
          else
          begin
            th := Max(16,Canvas.TextHeight('gh'));
            if s <> '' then
            begin
              DrawRect.Left := R.Left + 2;
              DrawRect.Top := R.Top + 2 + (DrawNum - 1) * th;
              Canvas.Textout(DrawRect.Left + 16 + 2,Drawrect.Top-2,s);
            end
            else
            begin
              DrawRect.Left := R.Left + (R.Right - R.Left - 16) div 2;
              DrawRect.Top := R.Top + 2 + (DrawNum - 1) * th;
            end;
          end;

          DrawRect.Right := DrawRect.Left + 16;
          DrawRect.Bottom := DrawRect.Top + 16;

          HTheme := OpenThemeData(WinCtrl.Handle,'button');

          if RadioOn then
          begin
            if not dis then
              DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDNORMAL,@DrawRect,nil)
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDDISABLED,@DrawRect,nil)
          end
          else
          begin
            if not dis then
              DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDNORMAL,@DrawRect,nil)
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDDISABLED,@DrawRect,nil)
          end;

          CloseThemeData(HTheme);
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ TStickyMenuItemActionLink }

procedure TStickyMenuItemActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);

  if (AClient is TStickyMenuItem) then
    FClient := AClient as TStickyMenuItem;
end;

//------------------------------------------------------------------------------

function TStickyMenuItemActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and (FClient.Checked = (Action as TCustomAction).Checked);
end;

//------------------------------------------------------------------------------

function TStickyMenuItemActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := (FClient is TStickyMenuItem) and
    (TStickyMenuItem(FClient).GroupIndex = (Action as TCustomAction).GroupIndex);
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItemActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    TStickyMenuItem(FClient).Checked := Value;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItemActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then
    TStickyMenuItem(FClient).GroupIndex := Value;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItemActionLink.SetImageIndex(Value: Integer);
begin
  if IsGroupIndexLinked then
    TStickyMenuItem(FClient).ImageIndex := Value;
end;

//------------------------------------------------------------------------------

function TStickyMenuItemActionLink.IsImageIndexLinked: boolean;
begin
  Result := inherited IsImageIndexLinked and
    (TStickyMenuItem(FClient).ImageIndex = (Action as TCustomAction).ImageIndex);
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItemActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
  begin
    TStickyMenuItem(FClient).Enabled := Value;
    TStickyMenuItem(FClient).InvalidatePanel;
  end;
end;

//------------------------------------------------------------------------------

{ TStickyMenuItem }

procedure TStickyMenuItem.Assign(Source: TPersistent);
begin
  if (Source is TStickyMenuItem) then
  begin
    FHideOnClick := (Source as TStickyMenuItem).HideOnClick;
    FImageIndex := (Source as TStickyMenuItem).ImageIndex;
    FEnabled := (Source as TStickyMenuItem).Enabled;
    FControl := (Source as TStickyMenuItem).Control;
    FStyle := (Source as TStickyMenuItem).Style;
    FTag := (Source as TStickyMenuItem).Tag;
    FItemObject := (Source as TStickyMenuItem).ItemObject;
    FCaption := (Source as TStickyMenuItem).Caption;
    Checked := (Source as TStickyMenuItem).Checked;
    AutoCheck := (Source as TStickyMenuItem).AutoCheck;
    GroupIndex := (Source as TStickyMenuItem).GroupIndex;
    Visible := (Source as TStickyMenuItem).Visible;
    Action := (Source as TStickyMenuItem).Action;
    FName := (Source as TStickyMenuItem).Name;
    FHint := (Source as TStickyMenuItem).Hint;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

constructor TStickyMenuItem.Create(Collection: TCollection);
begin
  inherited;
  FHideOnClick := False;
  FImageIndex := -1;
  FEnabled := True;
  FControl := nil;
  FStyle := isMenuItem;
  FTag := 0;
  FItemObject := nil;
  FCaption := '';
  FAutoCheck := True;
  FChecked := False;
  FGroupIndex := 0;
  FVisible := True;
end;

//------------------------------------------------------------------------------

destructor TStickyMenuItem.Destroy;
begin
  if Assigned(FActionLink) then
  begin
    FActionLink.Free;
    FActionLink := nil;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

function TStickyMenuItem.GetDisplayName: string;
begin
  if Caption = '' then
    Result := 'MenuItem'+IntToStr(Index)
  else
    Result := Caption;
end;
//------------------------------------------------------------------------------

procedure TStickyMenuItem.SetAutoCheck(const Value: Boolean);
begin
  if (FAutoCheck <> Value) then
  begin
    FAutoCheck := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.UpdateExclusive;
var
  i: Integer;
begin
  if (Style = isRadioButton) and Checked then
  begin
    for i := 0 to TStickyMenuItems(Collection).Count - 1 do
    begin
      if (TStickyMenuItems(Collection).Items[i].Style = isRadioButton) and (TStickyMenuItems(Collection).Items[i] <> Self) and (TStickyMenuItems(Collection).Items[i].GroupIndex = GroupIndex) and TStickyMenuItems(Collection).Items[i].Checked then
      begin
        TStickyMenuItems(Collection).Items[i].Checked := False;
        if Assigned(TStickyMenuItems(Collection).FOwner) and Assigned(TStickyMenuItems(Collection).FOwner.FStickyMenuPanel) then
          TStickyMenuItems(Collection).FOwner.FStickyMenuPanel.DrawItem(TStickyMenuItems(Collection).FOwner.FStickyMenuPanel.Canvas, i);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.SetChecked(const Value: Boolean);
begin
  if (FChecked <> Value) then
  begin
    if Value and IsSeparatorItem(Self) then
      Exit;
    FChecked := Value;
    UpdateExclusive;
    if Assigned(TStickyMenuItems(Collection).FOwner) and Assigned(TStickyMenuItems(Collection).FOwner.FStickyMenuPanel) and
       (TStickyMenuItems(Collection).FOwner.FStickyMenuPanel.FUpdateCount <= 0) then
      TStickyMenuItems(Collection).FOwner.FStickyMenuPanel.Invalidate;

  end;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.InvalidatePanel;
begin
  if Assigned(TStickyMenuItems(Collection).FOwner) and Assigned(TStickyMenuItems(Collection).FOwner.FStickyMenuPanel) and
     (TStickyMenuItems(Collection).FOwner.FStickyMenuPanel.FUpdateCount <= 0) then
    TStickyMenuItems(Collection).FOwner.FStickyMenuPanel.Invalidate;
end;

//------------------------------------------------------------------------------

function TStickyMenuItem.GetAction: TBasicAction;
begin
  if ActionLink <> nil then
    Result := ActionLink.Action
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.SetAction(const Value: TBasicAction);
begin
  if not Assigned(TStickyMenuItems(Collection).FOwner) then
    Exit;

  if (Value = nil) then
  begin
    FActionLink.Free;
    FActionLink := nil;
  end
  else
  begin
    if (ActionLink = nil) then
      FActionLink := TStickyMenuItemActionLink.Create(TStickyMenuItems(Collection).FOwner);
    FActionLink.AssignClient(Self);
    ActionLink.Action := Value;
    ActionLink.OnChange := DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
    Value.FreeNotification(TStickyMenuItems(Collection).FOwner);
  end;
  //TStickyMenuItems(Collection).FOwner.UpdateActionState;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.DoActionChange(Sender: TObject);
begin
  if (Sender = Action) then
    ActionChange(Sender, False);
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if (Sender is TCustomAction) then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Caption = '') or (Self.Caption = Self.Name) then
        Self.Caption := Caption;
      if not CheckDefaults or (Self.Enabled = True) then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.Visible = True) then
        Self.Visible := Visible;
      if not CheckDefaults or not Assigned(Self.OnClick) then
        Self.OnClick := OnExecute;
      if not CheckDefaults then
        Self.GroupIndex := GroupIndex;
      if not CheckDefaults then
        Self.ImageIndex := ImageIndex;
    end;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.SetControl(
  const Value: TComponent);
begin
  if (FControl <> Value) then
  begin
    FControl := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.SetEnabled(const Value: boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.SetHideOnClick(const Value: Boolean);
begin
  if (FHideOnClick <> Value) then
  begin
    FHideOnClick := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.SetImageIndex(const Value: Integer);
begin
  if (FImageIndex <> Value) then
    FImageIndex := Value;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.SetItemObject(const Value: TObject);
begin
  if (FItemObject <> Value) then
    FItemObject := Value;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.SetStyle(
  const Value: TStickyMenuItemStyle);
begin
  if (FStyle <> Value) then
    FStyle := Value;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.Click;
begin
  if Assigned(TStickyMenuItems(Collection).FOwner) and Assigned(TStickyMenuItems(Collection).FOwner.FStickyMenuPanel) then
    TStickyMenuItems(Collection).FOwner.FStickyMenuPanel.MenuItemClick(Index);
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.SetGroupIndex(const Value: Integer);
begin
  if (FGroupIndex <> Value) then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItem.SetVisible(const Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    if Assigned(TStickyMenuItems(Collection).OnVisibleChanged) then
      TStickyMenuItems(Collection).FOnVisibleChanged(Self);
  end;
end;

//------------------------------------------------------------------------------

{ TStickyMenuItems }

function TStickyMenuItems.Add: TStickyMenuItem;
begin
  Result := TStickyMenuItem(inherited Add);
end;

//------------------------------------------------------------------------------

constructor TStickyMenuItems.Create(AOwner: TPersistent);
begin
  inherited Create(TStickyMenuItem);
  FOwner := TAdvStickyPopupMenu(AOwner);
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItems.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

//------------------------------------------------------------------------------

function TStickyMenuItems.GetItem(Index: Integer): TStickyMenuItem;
begin
  Result := TStickyMenuItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TStickyMenuItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TStickyMenuItems.Insert(Index: Integer): TStickyMenuItem;
begin
  Result := TStickyMenuItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TStickyMenuItems.SetItem(Index: Integer;
  const Value: TStickyMenuItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TButtonBarItem }

procedure TButtonBarItem.Assign(Source: TPersistent);
begin
  if (Source is TButtonBarItem) then
  begin
    FImageIndex := (Source as TButtonBarItem).ImageIndex;
    FTag := (Source as TButtonBarItem).Tag;
    FCaption := (Source as TButtonBarItem).Caption;
    HIdeOnClick := (Source as TButtonBarItem).HideOnClick;
    FHint := (Source as TButtonBarItem).Hint;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

constructor TButtonBarItem.Create(Collection: TCollection);
begin
  inherited;
  FImageIndex := -1;
  FTag := 0;
  FCaption := '';
  FHideOnClick := False;
  FEnabled := True;
end;

//------------------------------------------------------------------------------

destructor TButtonBarItem.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

function TButtonBarItem.GetDisplayName: string;
begin
  if Caption = '' then
    Result := 'ButtonBarItem'+IntToStr(Index)
  else
    Result := Caption;
end;

//------------------------------------------------------------------------------

function TButtonBarItem.GetLeft: Integer;
var
  R: TRect;
begin
  R := TButtonBar(Collection).FOwner.FStickyMenuPanel.GetClientRectEx;
  Result := R.Left + TButtonBar(Collection).FOwner.FItemMarginX;
  if (Index - 1 >= 0) then
    Result := TButtonBar(Collection).Items[Index - 1].Left + TButtonBar(Collection).Items[Index - 1].Width + BTN_SPACE;
end;

//------------------------------------------------------------------------------

function TButtonBarItem.GetWidth: Integer;
begin
  Result := BTNOFFSET_X * 2 + GetTextSize(TButtonBar(Collection).FOwner.FStickyMenuPanel.Canvas, TButtonBar(Collection).FOwner.FStickyMenuPanel, Caption, TButtonBar(Collection).FOwner.FCurrentStyler.ButtonAppearance.CaptionFont).cx;
  if (ImageIndex >= 0) and Assigned(TButtonBar(Collection).FOwner.ButtonImages) then
  Result := Result + IMG_GAP + TButtonBar(Collection).FOwner.ButtonImages.Width;
end;

//------------------------------------------------------------------------------

procedure TButtonBarItem.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonBarItem.SetEnabled(const Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonBarItem.SetHideOnClick(const Value: Boolean);
begin
  if (FHideOnClick <> Value) then
  begin
    FHideOnClick := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonBarItem.SetImageIndex(const Value: Integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    if Assigned(TButtonBar(Collection).FOwner) and Assigned(TButtonBar(Collection).FOwner.FStickyMenuPanel) then
      TButtonBar(Collection).FOwner.FStickyMenuPanel.DrawButton(TButtonBar(Collection).FOwner.FStickyMenuPanel.Canvas, Index);
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonBarItem.Click;
begin
  if Assigned(TButtonBar(Collection).FOwner) and Assigned(TButtonBar(Collection).FOwner.FStickyMenuPanel) then
    TButtonBar(Collection).FOwner.FStickyMenuPanel.ButtonClick(Index);
end;

//------------------------------------------------------------------------------

{ TButtonBar }

function TButtonBar.Add: TButtonBarItem;
begin
  Result := TButtonBarItem(inherited Add);
end;

//------------------------------------------------------------------------------

constructor TButtonBar.Create(AOwner: TPersistent);
begin
  if not (AOwner is TAdvStickyPopupMenu) then
    raise Exception.Create('Invalid Owner');
  FOwner := TAdvStickyPopupMenu(AOwner);
  inherited Create(TButtonBarItem);
end;

//------------------------------------------------------------------------------

procedure TButtonBar.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

//------------------------------------------------------------------------------

function TButtonBar.GetItem(Index: Integer): TButtonBarItem;
begin
  Result := TButtonBarItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TButtonBar.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TButtonBar.Insert(Index: Integer): TButtonBarItem;
begin
  Result := TButtonBarItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TButtonBar.SetItem(Index: Integer;
  const Value: TButtonBarItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TAdvStickyMenuPanel }

procedure TAdvStickyMenuPanel.CMMouseLeave(var Message: TMessage);
var
  j: Integer;
begin
  if (FButtonIndexHot >= 0) or (FButtonIndexDown >= 0) then
  begin
    j := FButtonIndexHot;
    FButtonIndexDown := -1;
    InvalidateButton(j);
  end;

  if (FMenuItemIndexHot >= 0) or (FMenuItemIndexDown >= 0) then
  begin
    j := FMenuItemIndexHot;
    FMenuItemIndexDown := -1;
    InvalidateMenuItem(j);
  end;

  FMouseOnIconBar := false;
  inherited;

end;

//------------------------------------------------------------------------------

constructor TAdvStickyMenuPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];

  FOwner := AOwner;
  FButtonIndexHot := -1;
  FButtonIndexDown := -1;
  FMenuItemIndexHot := -1;
  FMenuItemIndexDown := -1;
  FDonotHideMenu := False;
  FUpdateCount := 0;
end;

//------------------------------------------------------------------------------

destructor TAdvStickyMenuPanel.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.KeyDown(var Key: Word;
  Shift: TShiftState);
var
  mi: TStickyMenuItem;
begin
  inherited;
  case Key of
  VK_SPACE:
    begin
      if FMenuItemIndexHot > -1 then
      begin
        mi := MenuItems[FMenuItemIndexHot];
        if mi.Style = isCheckBox then
        begin
          mi.Checked := not mi.Checked;
          Invalidate;
        end;
        if mi.Style = isRadioButton then
        begin
          mi.Checked := true;
          Invalidate;
        end;
      end;
    end;
  VK_RETURN:
    begin
      if FMenuItemIndexHot > -1 then
        MenuItemClick(FMenuItemIndexHot);
    end;
  VK_ESCAPE:
    begin
      FAdvStickyPopupMenu.HideMenu;
    end;
  VK_DOWN:
    begin
      inc(FMenuItemIndexHot);
      if FMenuItemIndexHot >= MenuItems.Count - 1 then
        FMenuItemIndexHot := MenuItems.Count - 1;
      Invalidate;
    end;
  VK_UP:
    begin
      dec(FMenuItemIndexHot);
      if FMenuItemIndexHot < 0 then
        FMenuItemIndexHot := 0;
      Invalidate;
    end;
  VK_HOME:
    begin
      FMenuItemIndexHot := 0;
      FMenuItemIndexDown := 0;
      Invalidate;
    end;
  VK_END:
    begin
      FMenuItemIndexHot := MenuItems.Count - 1;
      FMenuItemIndexDown := MenuItems.Count - 1;
      Invalidate;
    end;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
  i, j: Integer;
begin
  inherited;

  p := Point(X, y);
  if PtInRect(GetButtonBarRect, P) then
  begin
    i := ButtonAtPos(X, Y);
    if (i >= 0) then
    begin
      if (ButtonBar[i].Enabled) then
      begin
        FButtonIndexHot := i;
        FButtonIndexDown := i;
        DrawButton(Canvas, FButtonIndexDown);
      end;
    end
    else if (FButtonIndexDown >= 0) then
    begin
      j := FButtonIndexDown;
      FButtonIndexDown := -1;
      InvalidateButton(j);
    end;
  end;

  if PtInRect(GetItemsRect, P) then
  begin
    i := ItemAtPos(X, Y);
    if (i >= 0) then
    begin
      if not IsSeparatorItem(MenuItems[i]) or (not MenuItems[i].Enabled) then
      begin
        FMenuItemIndexDown := i;
        DrawItem(Canvas, FMenuItemIndexDown);
      end;
    end
    else if (FMenuItemIndexDown >= 0) then
    begin
      j := FMenuItemIndexDown;
      FMenuItemIndexDown := -1;
      InvalidateMenuItem(j);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  p: TPoint;
  i, j: Integer;
  OldV: Boolean;
begin
  inherited;

  p := Point(X, Y);
  OldV := FMouseOnIconBar;
  FMouseOnIconBar := PtInRect(GetIconBarRect, P);
  if (OldV <> FMouseOnIconBar) and (FMenuItemIndexHot >= 0) then
    DrawItem(Canvas, FMenuItemIndexHot);

  if PtInRect(GetButtonBarRect, P) then
  begin
    i := ButtonAtPos(X, Y);
    if (i >= 0) then
    begin
      if (i <> FButtonIndexHot) then
      begin
        j := FButtonIndexHot;
        if not ButtonBar[i].Enabled then
        begin
          FButtonIndexHot := -1;
          FButtonIndexDown := -1;
        end
        else
        begin
          FButtonIndexHot := i;
          if (ssLeft in Shift) then
            FButtonIndexDown := i;
          DrawButton(Canvas, FButtonIndexHot);
        end;
        if (j >= 0) then
          InvalidateButton(j);
      end;
    end
    else if (FButtonIndexHot >= 0) or (FButtonIndexDown >= 0) then
    begin
      j := FButtonIndexHot;
      FButtonIndexHot := -1;
      FButtonIndexDown := -1;
      InvalidateButton(j);
    end;
  end
  else if (FButtonIndexHot >= 0) or (FButtonIndexDown >= 0) then
  begin
    j := FButtonIndexHot;
    FButtonIndexHot := -1;
    FButtonIndexDown := -1;
    InvalidateButton(j);
  end;

  if PtInRect(GetItemsRect, P) then
  begin
    i := ItemAtPos(X, Y);

    if i <> FHintIndex then
    begin
      Application.CancelHint;
      FHintIndex := i;
    end;

    if (i >= 0) then
    begin
      if (i <> FMenuItemIndexHot) then
      begin
        j := FMenuItemIndexHot;
        if IsSeparatorItem(MenuItems[i]) or (not MenuItems[i].Enabled) then
        begin
          FMenuItemIndexHot := -1;
          FMenuItemIndexDown := -1;
        end
        else
        begin
          FMenuItemIndexHot := i;
          if (ssLeft in Shift) then
            FMenuItemIndexDown := i;
          DrawItem(Canvas, FMenuItemIndexHot);
        end;
        if (j >= 0) then
          InvalidateMenuItem(j);
      end;
    end
    else if (FMenuItemIndexHot >= 0) or (FMenuItemIndexDown >= 0) then
    begin
      j := FMenuItemIndexHot;
      FMenuItemIndexHot := -1;
      FMenuItemIndexDown := -1;
      InvalidateMenuItem(j);
    end;
  end
  else if (FMenuItemIndexHot >= 0) or (FMenuItemIndexDown >= 0) then
  begin
    j := FMenuItemIndexHot;
    FMenuItemIndexHot := -1;
    FMenuItemIndexDown := -1;
    InvalidateMenuItem(j);
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  j: Integer;
  P: TPoint;
begin
  inherited;

  P := Point(X, Y);
  if (FButtonIndexDown >= 0) then
  begin
    j := FButtonIndexDown;
    FButtonIndexDown := -1;
    DrawButton(Canvas, j);
    if (ButtonBar[j].Enabled) then
      ButtonClick(j);
  end;

  if (FMenuItemIndexDown >= 0) then
  begin
    j := FMenuItemIndexDown;
    FMenuItemIndexDown := -1;
    DrawItem(Canvas, j);
    if (ItemAtPos(X, Y) = j) and (MenuItems[j].Enabled) then
      MenuItemClick(j);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.Paint;
begin
  if (FUpdateCount > 0) then
    Exit;

  inherited;

  DrawButtonBar(Canvas);
  DrawItemsBackGround(Canvas);
  DrawIconBar(Canvas);
  DrawItems(Canvas);

  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clGray;
  Canvas.Rectangle(ClientRect);  
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.InitializeAndUpdate;
var
  bw, bh, iw, h, w, i: Integer;
  Ctrl: TControl;
  R: TRect;
begin
  if not Assigned(FAdvStickyPopupMenu) then
    Exit;

  FButtonIndexHot := -1;
  FButtonIndexDown := -1;
  FMenuItemIndexHot := -1;
  FMenuItemIndexDown := -1;
    
  //-------- ButtonBar's Max size
  bw := GetTotalButtonsWidth;
  bh := GetButtonBarHeight + 2;
  if not ButtonBarVisible then
    bh := 2;

  //-------- Items' Max size
  iw := 0;
  GetMaxItemWidth(iw);

  //-------- Set Width and Height
  w := Max(bw, iw);

  h := bh + GetTotalItemsHeight;
  Width := Max(w, MIN_MENUWIDTH);
  Height := Max(h, MIN_MENUHEIGHT);

  for i:= 0 to MenuItems.Count - 1 do
  begin
    if Assigned(MenuItems[i].Control) and (MenuItems[i].Control is TControl) then
    begin
      Ctrl := TControl(MenuItems[i].Control);
      if (Ctrl.Parent <> Self) then
        Ctrl.Parent := Self;

      if not MenuItems[i].Visible then
      begin
        Ctrl.Left := 0;
        Ctrl.Top := - Ctrl.Height - 5;
        Continue;
      end;

      R := GetItemRect(i);
      //if (MenuItems[i].Style in [isCheckBox, isRadioButton]) then
      R.Left := getIConBarRect.Right;

      Ctrl.Align := alNone;
      Ctrl.Left := R.Left + AdvStickyPopupMenu.FItemOffSetX;
      Ctrl.Top := R.Bottom + 1;
    end;
  end;

  MenuItems.OnVisibleChanged := OnMenuItemsVisibleChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.CMDialogChar(var Message: TCMDialogChar);
var
  AMenuItem: TStickyMenuItem;
  aButton: TButtonBarItem;
begin
  if Enabled and Showing and Assigned(AdvStickyPopupMenu) and Assigned(AdvStickyPopupMenu.FStickyMenuWindow) and (AdvStickyPopupMenu.FStickyMenuWindow.Visible) then
  begin
    AMenuItem := FindMenuItemFromAccel(Message.CharCode);
    if AMenuItem <> nil then
    begin
      AMenuItem.Click;
      Message.Result := 1;
      Exit;
    end;

    aButton := FindButtonFromAccel(Message.CharCode);
    if aButton <> nil then
    begin
      aButton.Click;
      InvalidateButton(aButton.Index);
      Message.Result := 1;
      Exit;
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
end;

procedure TAdvStickyMenuPanel.CMHintShow(var Message: TMessage);
var
  hi: PHintInfo;
  i: integer;
  CanShow: boolean;
begin
  CanShow := false;

  hi := PHintInfo(Message.LParam);

  i := ItemAtPos(hi^.CursorPos.X, hi^.CursorPos.Y);
  FHintIndex := i;
  if (i >= 0) then
  begin
    hi^.HintStr :=  MenuItems[i].Hint;
    CanShow := (hi^.HintStr <> '');
  end;

  i := ButtonAtPos(hi^.CursorPos.X, hi^.CursorPos.Y);
  if (i >= 0) then
  begin
    hi^.HintStr :=  ButtonBar[i].Hint;
    CanShow := (hi^.HintStr <> '');
  end;

  Message.Result := Ord(not CanShow);
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.DrawItem(Canvas: TCanvas; Index: Integer);
var
  R, R1, R2, Rc, R3: TRect;
  BrClr, Clr, ClrTo, ClrMirror, ClrMirrorTo, CaptionClr: TColor;
  CtrlStyle: TAdvControlStyle;
  b,ind: Integer;
  DrwImg: Boolean;
begin
  if not Assigned(FAdvStickyPopupMenu) or (Index < 0) or (Index >= MenuItems.Count) then
    Exit;


  if not MenuItems[Index].Visible then
    Exit;

  if (FUpdateCount > 0) then
    Exit;

  R := GetItemRect(Index);

  if (MenuItems[Index].Style = isControlItem) and Assigned(MenuItems[Index].Control) and (MenuItems[Index].Control is TControl) then
  begin
    R.Top := TControl(MenuItems[Index].Control).Top + AdvStickyPopupMenu.FItemOffSetY + AdvStickyPopupMenu.FItemMarginY + 12;
    DrawContent(Canvas, '', R, AdvStickyPopupMenu.ItemImages, MenuItems[Index].ImageIndex, AdvStickyPopupMenu.FItemOffSetX, MenuItems[Index].Enabled, true);
    Exit;
  end;

  if IsSeparatorItem(MenuItems[Index]) then
  begin
    Canvas.Pen.Color := Styler.Separator.Color;
    if Assigned(MenuItems[Index].OnDrawItem) then
      MenuItems[Index].OnDrawItem(MenuITems[Index], Canvas, R, False)
    else
    begin
      ind := getIConBarRect.Right + 2 ;
      Canvas.MoveTo(ind , R.Top + 2);
      Canvas.LineTo(R.Right, R.Top + 2);
      Canvas.Pen.Color := clWhite;
      Canvas.MoveTo(ind, R.Top + 3);
      Canvas.LineTo(R.Right, R.Top + 3);
    end;
  end
  else
  begin
    R1 := R;
    with Styler.SelectedItem do
    begin
      if (Index = FMenuItemIndexHot) then
      begin
        Clr := Color;
        ClrTo := ColorTo;
        BrClr := BorderColor;
        ClrMirror := ColorMirror;
        ClrMirrorTo := ColorMirrorTo;

        if FMouseOnIconBar and (MenuItems[Index].Style in [isCheckBox, isRadioButton]) then
        begin
          Clr := BlendColor(Clr, clWhite, 50);
          if (ClrTo <> clNone) then
            ClrTo := BlendColor(ClrTo, clWhite, 70);
          BrClr := BlendColor(BrClr, clWhite, 90);

          ClrMirror := BlendColor(ClrMirror, clWhite, 70);
          if (ClrMirrorTo <> clNone) then
            ClrMirrorTo := BlendColor(ClrMirrorTo, clWhite, 90);
        end;
      end
      else
      begin
        Clr := clNone;
        ClrTo := clNone;
        BrClr := clNone;
        ClrMirror := clNone;
        ClrMirrorTo := clNone;
      end;

      if MenuItems[Index].Enabled then
        CaptionClr := Styler.SelectedItem.Font.Color
      else
        CaptionClr := clGray;

      if IsWinXP and IsThemeActive then
        CtrlStyle := csTheme
      else
        CtrlStyle := csClassic;

      Rc := GetIconBarRect;
      R1.Left := Rc.Right + AdvStickyPopupMenu.FItemMarginX;

      DrwImg := not Assigned(MenuItems[Index].OnDrawImage);

      if (MenuItems[Index].Style in [isCheckBox, isRadioButton]) then
      begin
        if Assigned(MenuItems[Index].OnDrawItem) then
          MenuItems[Index].OnDrawItem(MenuItems[Index], Canvas, R1, Index = FMenuItemIndexHot)
        else if (clr <> clNone) then
          DrawGradientBackGround(Canvas, Clr, ClrTo, ClrMirror, ClrMirrorTo, BrClr, 60, R1, GradientDirection, ROUND_RADIUS);

        R2 := R;
        R2.Right := Rc.Right;
        R2.Left := R2.Left + AdvStickyPopupMenu.FItemOffSetX;
        R2.Top := R2.Top + AdvStickyPopupMenu.FItemOffSetY;

        if MenuITems[Index].Checked then
          b := 0
        else
          b := 1;

        if (MenuItems[Index].Style = isCheckBox) then
          DrawCheck(Self, Canvas, R2, MenuITems[Index].Checked, MenuItems[Index].Enabled, CtrlStyle)
        else if (MenuItems[Index].Style = isRadioButton)then
          DrawRadio(Self, Canvas, CtrlStyle, R2, 1, b, true, false, nil, MenuItems[Index].Checked, 0, 0, clBlue);

        Canvas.Font.Assign(Styler.SelectedItem.Font);
        Canvas.Font.Color := CaptionClr;
        if Assigned(MenuItems[Index].OnDrawImage) and Assigned(AdvStickyPopupMenu.ItemImages) and (MenuITems[Index].ImageIndex >= 0) then
        begin
          R3 := R1;
          R3.Right := R3.Left + AdvStickyPopupMenu.FItemOffSetX + AdvStickyPopupMenu.ItemImages.Width + IMG_GAP;
          MenuItems[Index].OnDrawImage(MenuItems[Index], Canvas, R3, Index = FMenuItemIndexHot);
        end;

        if not Assigned(MenuITems[Index].OnDrawItem) then
          DrawContent(Canvas, MenuItems[Index].Caption, R1, AdvStickyPopupMenu.ItemImages, MenuItems[Index].ImageIndex, AdvStickyPopupMenu.FItemOffSetX, MenuItems[Index].Enabled, DrwImg)
        else if not Assigned(MenuITems[Index].OnDrawImage) then
          DrawContent(Canvas, '', R1, AdvStickyPopupMenu.ItemImages, MenuItems[Index].ImageIndex, AdvStickyPopupMenu.FItemOffSetX, MenuItems[Index].Enabled, DrwImg);
      end
      else
      begin
        if Assigned(MenuItems[Index].OnDrawItem) then
          MenuItems[Index].OnDrawItem(MenuItems[Index], Canvas, R, Index = FMenuItemIndexHot)
        else if (clr <> clNone) then
          DrawGradientBackGround(Canvas, Clr, ClrTo, ClrMirror, ClrMirrorTo, BrClr, 60, R, GradientDirection, ROUND_RADIUS);

        if Assigned(MenuItems[Index].OnDrawImage) and Assigned(AdvStickyPopupMenu.ItemImages) and (MenuITems[Index].ImageIndex >= 0) then
        begin
          R3 := R;
          R3.Right := R3.Left + AdvStickyPopupMenu.FItemOffSetX + AdvStickyPopupMenu.ItemImages.Width + IMG_GAP;
          MenuItems[Index].OnDrawImage(MenuItems[Index], Canvas, R3, Index = FMenuItemIndexHot);
        end
        else
          DrawContent(Canvas, '', R, AdvStickyPopupMenu.ItemImages, MenuItems[Index].ImageIndex, AdvStickyPopupMenu.FItemOffSetX, MenuItems[Index].Enabled, DrwImg);

        Canvas.Font.Assign(Styler.SelectedItem.Font);
        Canvas.Font.Color := CaptionClr;
        R1.Left := R1.Left + AdvStickyPopupMenu.FItemOffSetX;
        if not Assigned(MenuITems[Index].OnDrawItem) then
          DrawContent(Canvas, MenuItems[Index].Caption, R1, nil, -1, AdvStickyPopupMenu.FItemOffSetX, MenuItems[Index].Enabled, DrwImg);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.DrawItemsBackGround(Canvas: TCanvas);
var
  R: TRect;
begin
  if not Assigned(FAdvStickyPopupMenu) then
    Exit;

  R := GetItemsRect;
  if (Styler.Background.Color <> clNone) and (Styler.Background.ColorTo <> clNone) then
    DrawGradient(Canvas, Styler.Background.Color, Styler.Background.ColorTo, 80, R,  Styler.Background.GradientDirection = gdHOrizontal)
  else if (Color <> clNone) then
  begin
    Canvas.Brush.Color := Styler.Background.Color;
    Canvas.Pen.Color := Styler.Background.Color;
    Canvas.FillRect(R);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.DrawItems(Canvas: TCanvas);
var
  i: Integer;
begin
  if not Assigned(FAdvStickyPopupMenu) then
    Exit;

  for i:= 0 to FAdvStickyPopupMenu.MenuItems.Count - 1 do
    DrawItem(Canvas, i);
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.GetItemsRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if Assigned(FAdvStickyPopupMenu) then
  begin
    Result := GetClientRectEx;
    Result.Left := Result.Left + 1;
    Result.Right := Result.Right - 1;
    Result.Bottom := Result.Bottom - 1;
    if ButtonBarVisible then
      Result.Top := GetButtonBarRect.Bottom;
  end;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.GetItemHeight: Integer;
begin
  Result := MinItemHeight;
  if not Assigned(FAdvStickyPopupMenu) then
    Exit;
  Result := Max(Result, GetTextSize(Canvas, Self, 'gh', Styler.SelectedItem.Font).cy + AdvStickyPopupMenu.FItemOffSetY * 2);
  if Assigned(AdvStickyPopupMenu.ItemImages) then
    Result := Max(Result, AdvStickyPopupMenu.ItemImages.Height + AdvStickyPopupMenu.FItemOffSetY * 2);
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.GetItemRect(Index: Integer; var ItemWithCtrlRect: TRect): TRect;
var
  i: Integer;
  R: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if Assigned(FAdvStickyPopupMenu) and (Index >= 0) and (Index < MenuItems.Count) then
  begin
    if not MenuItems[Index].Visible then
      Exit;
      
    R := GetItemsRect;
    R.Top := R.Top + AdvStickyPopupMenu.FItemMarginY;
    R.Left := R.Left + AdvStickyPopupMenu.FItemMarginX;
    R.Right := R.Right - AdvStickyPopupMenu.FItemMarginX;
    for i:= 0 to Index do
    begin
      if not MenuItems[i].Visible then
        Continue;
        
      Result := R;
      if IsSeparatorItem(MenuItems[i]) then
        Result.Bottom := R.Top + SEPARATOR_SIZE
      else
        if (MenuItems[i].Style = isControlItem) and Assigned(MenuItems[i].Control) and (MenuItems[i].Control is TControl) then
          Result.Bottom := R.Top
      else
        Result.Bottom := R.Top + GetItemHeight;
      R.Top := Result.Bottom + ITEM_SPACE;
      ItemWithCtrlRect := Result;
      if Assigned(MenuItems[i].Control) and (MenuItems[i].Control is TControl) and (TControl(MenuItems[i].Control).Visible) then
      begin
        ItemWithCtrlRect.Bottom := ItemWithCtrlRect.Bottom + TControl(MenuItems[i].Control).Height + 2;
        R.Top := ItemWithCtrlRect.Bottom + ITEM_SPACE;
      end;
      R.Bottom := R.Top + GetItemHeight;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.GetItemRect(Index: Integer): TRect;
var
  R: TRect;
begin
  Result := GetItemRect(Index, R);
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.GetButtonBarHeight: Integer;
begin
  Result := AdvStickyPopupMenu.FCurrentStyler.SideBar.Size;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.GetTotalButtonsWidth: Integer;
begin
  Result := 0;
  if (ButtonBar.Count > 0) then
    Result := ButtonBar[ButtonBar.Count - 1].Left + ButtonBar[ButtonBar.Count - 1].Width + BTN_SPACE;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.GetMaxItemWidth(
  var ItemWidth: Integer): TStickyMenuItem;
var
  i, w: Integer;
begin
  ItemWidth := 0;
  Result := nil;

  for i:= 0 to MenuItems.Count - 1 do
  begin
    if not MenuItems[i].Visible then
      Continue;
      
    if (MenuItems[i].Style = isControlItem) and Assigned(MenuItems[i].Control) and (MenuItems[i].Control is TControl) then
      w := 0
    else
      w := AdvStickyPopupMenu.FItemOffSetX * 2 + GetTextSize(Canvas, Self, MenuItems[i].Caption, Styler.SelectedItem.Font).cx;
    if (MenuItems[i].Style in [isCheckBox, isRadioButton]) and (MenuItems[i].ImageIndex >= 0) and Assigned(AdvStickyPopupMenu.ItemImages) then
      w := w + IMG_GAP + AdvStickyPopupMenu.ItemImages.Width;

    if Assigned(MenuItems[i].Control) and (MenuItems[i].Control is TControl) then
    begin
      w := Max(w, AdvStickyPopupMenu.FItemOffSetX * 2 + TControl(MenuItems[i].Control).Width);
    end;

    if (w > ItemWidth) then
    begin
      ItemWidth := w;
      Result := MenuItems[i];
    end;
  end;

  ItemWidth := ItemWidth + AdvStickyPopupMenu.FItemMarginX * 2 + 35;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.GetTotalItemsHeight: Integer;
var
  i: Integer;
  R, Rc: TRect;
begin
  Result := 0;
  for i:= 0 to MenuItems.Count - 1 do
  begin
    if not MenuItems[i].Visible then
      Continue;
      
    Rc := Rect(0, 0, 0, 0);
    R := GetItemRect(i, Rc);
    Result := Result + (Rc.Bottom - Rc.Top) + ITEM_SPACE;
  end;
  Result := Result + AdvStickyPopupMenu.FItemMarginY;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.HideMenuWindow;
begin
  if Assigned(FAdvStickyPopupMenu) then
  begin
    FAdvStickyPopupMenu.HideMenu;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.BeginUpdate;
begin
  if not Visible then
    Exit;

  Inc(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.EndUpdate;
begin
  if not Visible then
    Exit;

  if FUpdateCount > 0 then Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    if Assigned(Parent) and (Parent is TAdvStickyMenuWindow) and Parent.Visible then
      TAdvStickyMenuWindow(Parent).UpdateSize;
  end;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.ButtonAtPos(X, Y: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  if not Assigned(FAdvStickyPopupMenu) then
    Exit;

  for i := 0 to AdvStickyPopupMenu.ButtonBar.Count - 1 do
  begin
    if PtInRect(GetButtonRect(i), Point(X, Y)) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.ItemAtPos(X, Y: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  if not Assigned(FAdvStickyPopupMenu) then
    Exit;

  for i := 0 to AdvStickyPopupMenu.MenuItems.Count - 1 do
  begin
    if not AdvStickyPopupMenu.MenuItems[i].Visible then
      Continue;
      
    if PtInRect(GetItemRect(i), Point(X, Y)) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.SelectItem(Index: Integer);
begin
  if not Assigned(FAdvStickyPopupMenu) then
    Exit;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.WMGetDlgCode(var Message: TMessage);
begin
  inherited;
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS + DLGC_WANTTAB;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.DrawIconBar(Canvas: TCanvas);
var
  R: TRect;
begin
  if not Assigned(FAdvStickyPopupMenu) then
    Exit;

  R := GetIconBarRect;
  with Styler.IconBar do
  begin
    if (Color <> clNone) and (ColorTo <> clNone) then
      DrawGradient(Canvas, Color, ColorTo, 80, R, GradientDirection = gdHOrizontal)
    else if (Color <> clNone) then
    begin
      Canvas.Brush.Color := Color;
      Canvas.Pen.Color := Color;
      Canvas.FillRect(R);
    end;

    //----- Bottom 3D Line
    Canvas.Pen.Color := clWhite;
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.Pen.Color := Styler.Separator.Color;
    Canvas.MoveTo(R.Right - 1, R.Top);
    Canvas.LineTo(R.Right - 1, R.Bottom);
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.DrawButton(Canvas: TCanvas; Index: Integer);
var
  R: TRect;
  Clr, ClrTo, BrClr: TColor;
begin
  if not Assigned(FAdvStickyPopupMenu) or not ButtonBarVisible or (Index < 0) or (Index >= ButtonBar.Count) then
    Exit;

  R := GetButtonRect(Index);

  with Styler.ButtonAppearance do
  begin
    if (Index = FButtonIndexDown) then
    begin
      Clr := DownColor;
      ClrTo := DownColorTo;
      BrClr := DownBorderColor;
    end
    else if (Index = FButtonIndexHot) then
    begin
      Clr := HoverColor;
      ClrTo := HoverColorTo;
      BrClr := HoverBorderColor;
    end
    else
    begin
      Clr := Color;
      ClrTo := ColorTo;
      BrClr := BorderColor;
    end;

    DrawGradientBackGround(Canvas, Clr, ClrTo, clNone, clNone, BrClr, 60, R, GradientDirection, 2);
    DrawContent(Canvas, ButtonBar[Index].Caption, R, AdvStickyPopupMenu.ButtonImages, ButtonBar[Index].ImageIndex, BTNOFFSET_X, ButtonBar[Index].Enabled, True{DrawImage});
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.DrawButtonBar(Canvas: TCanvas);
var
  i: Integer;
  R: TRect;
begin
  if not Assigned(FAdvStickyPopupMenu) or not ButtonBarVisible then
    Exit;

  R := GetButtonBarRect;
  with Styler.SideBar.Background do
  begin
    if (Color <> clNone) and (ColorTo <> clNone) then
      DrawGradient(Canvas, Color, ColorTo, 80, R, GradientDirection = gdHOrizontal)
    else if (Color <> clNone) then
    begin
      Canvas.Brush.Color := Color;
      Canvas.Pen.Color := Color;
      Canvas.FillRect(R);
    end;

    //----- Bottom 3D Line
    Canvas.Pen.Color := Styler.Separator.Color;
    Canvas.MoveTo(R.Left, R.Bottom - 2);
    Canvas.LineTo(R.Right, R.Bottom - 2);
  end;

  for i:= 0 to AdvStickyPopupMenu.ButtonBar.Count - 1 do
    DrawButton(Canvas, i);
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.ButtonBarVisible: Boolean;
begin
  Result := AdvStickyPopupMenu.ShowButtonBar;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.GetButtonBarRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if ButtonBarVisible then
  begin
    Result := GetClientRectEx;
    Result.Bottom := Result.Top + GetButtonBarHeight;
    InflateRect(Result, -1, -1)
  end;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.GetButtonRect(Index: Integer): TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if ButtonBarVisible and (Index >= 0) and (Index < AdvStickyPopupMenu.ButtonBar.Count) then
  begin
    Result := GetButtonBarRect;
    Result.Left := AdvStickyPopupMenu.ButtonBar[Index].Left;
    Result.Right := Result.Left + AdvStickyPopupMenu.ButtonBar[Index].Width;
    Result.Top := Result.Top + 2;
    Result.Bottom := Result.Bottom - 2;
  end;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.GetClientRectEx: TRect;
begin
  Result := ClientRect;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.GetStyler: TCustomAdvMenuStyler;
begin
  Result := AdvStickyPopupMenu.FCurrentStyler;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.ButtonBar: TButtonBar;
begin
  Result := AdvStickyPopupMenu.ButtonBar;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.MenuItems: TStickyMenuItems;
begin
  Result := AdvStickyPopupMenu.MenuItems;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.GetIconBarRect: TRect;
begin
  Result := GetClientRectEx;
  Result.Left := Result.Left + 1;
  Result.Right := Result.Left + Styler.IconBar.Size;
  Result.Top := GetItemsRect.Top;
  Result.Bottom := Result.Bottom - 1;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.InvalidateButton(Index: Integer);
var
  R: TRect;
begin
  if (Index >= 0) and (Index < ButtonBar.Count) then
  begin
    R := GetButtonRect(Index);
    InvalidateRect(Handle, @R, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.InvalidateMenuItem(Index: Integer);
var
  R: TRect;
begin
  if (Index >= 0) and (Index < MenuItems.Count) then
  begin
    R := GetItemRect(Index);
    InvalidateRect(Handle, @R, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.ButtonClick(Index: Integer);
begin
  if (Index < 0) or (Index >= ButtonBar.Count) then
    Exit;

  FDonotHideMenu := not ButtonBar[Index].HideOnClick;

  if Assigned(ButtonBar[Index].OnClick) then
    ButtonBar[Index].OnClick(ButtonBar[Index])
  else if Assigned(AdvStickyPopupMenu.FOnButtonClick) then
    AdvStickyPopupMenu.FOnButtonClick(AdvStickyPopupMenu, Index);

  FDonotHideMenu := False;
  if ButtonBar[Index].HideOnClick then
    HideMenuWindow;
end;

//------------------------------------------------------------------------------

{$IFNDEF DELPHI_UNICODE}
function DelegatesEqual(A, B: Pointer): Boolean;
begin
  Result := A = B;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.MenuItemClick(Index: Integer);
begin
  if (Index < 0) or (Index >= MenuItems.Count) then
    Exit;

  FDonotHideMenu := not MenuItems[Index].HideOnClick;
  if (MenuItems[Index].Style = isCheckBox) then
  begin
    if (MenuItems[Index].AutoCheck) then
    begin
      MenuItems[Index].Checked := not MenuItems[Index].Checked;
      DrawItem(Canvas, Index);
    end;
    if Assigned(AdvStickyPopupMenu.OnCheckClick) then
      AdvStickyPopupMenu.OnCheckClick(AdvStickyPopupMenu, Index, MenuItems[Index].Checked);
  end
  else if (MenuItems[Index].Style = isRadioButton) then
  begin
    if (MenuItems[Index].AutoCheck) then
    begin
      MenuItems[Index].Checked := True;
      DrawItem(Canvas, Index);
    end;
    if Assigned(AdvStickyPopupMenu.OnRadioClick) then
      AdvStickyPopupMenu.OnRadioClick(AdvStickyPopupMenu, Index);
  end;

  if Assigned(MenuItems[Index].OnClick) and (MenuItems[Index].Action <> nil) and not DelegatesEqual(@MenuItems[Index].OnClick, @MenuItems[Index].Action.OnExecute) then
    MenuItems[Index].OnClick(MenuItems[Index])
  else if not (csDesigning in ComponentState) and (MenuItems[Index].ActionLink <> nil) then
    MenuItems[Index].ActionLink.Execute(Self)
  else if Assigned(MenuItems[Index].OnClick) then
    MenuItems[Index].OnClick(MenuItems[Index])
  else if Assigned(AdvStickyPopupMenu.FOnItemClick) then
    AdvStickyPopupMenu.FOnItemClick(AdvStickyPopupMenu, Index);

  FDonotHideMenu := False;
  if MenuItems[Index].HideOnClick then
    HideMenuWindow;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.FindButtonFromAccel(
  Accel: Word): TButtonBarItem;
var
  I: Integer;
begin
  for I := 0 to ButtonBar.Count - 1 do
  begin
    Result := ButtonBar[I];
    if Result.Enabled and IsAccel(Accel, Result.Caption) then
      Exit;
  end;
  Result := nil;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuPanel.FindMenuItemFromAccel(
  Accel: Word): TStickyMenuItem;
var
  I: Integer;
begin
  for I := 0 to MenuITems.Count - 1 do
  begin
    Result := MenuITems[I];
    if Result.Enabled and Result.Visible and IsAccel(Accel, Result.Caption) then
      Exit;
  end;
  Result := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuPanel.OnMenuItemsVisibleChanged(Sender: TObject);
begin
  if Assigned(Parent) and (Parent is TAdvStickyMenuWindow) and Parent.Visible and (FUpdateCount <= 0) then
  begin
    TAdvStickyMenuWindow(Parent).UpdateSize;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvStickyMenuWindow }

constructor TAdvStickyMenuWindow.Create(AOwner: TComponent);
begin
  inherited;
  FHideOnDeActivate := true;
  FHideTimer := TTimer.Create(self);
  FHideTimer.Interval := 1;
  FHideTimer.Enabled := false;
  FHideTimer.OnTimer := HideTimerOnTime;
  Font.Name := 'Tahoma';
end;

//------------------------------------------------------------------------------

constructor TAdvStickyMenuWindow.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  FOwner := AOwner;
  FHideOnDeActivate := true;
  FHideTimer := TTimer.Create(self);
  FHideTimer.Interval := 1;
  FHideTimer.Enabled := false;
  FHideTimer.OnTimer := HideTimerOnTime;
  FBorderColor := clNone;
  Font.Name := 'Tahoma';
  DoubleBuffered := True;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited CreateParams(Params);
  //Params.Style := Params.Style - WS_BORDER;
  {
  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST; }

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
     ((Win32MajorVersion > 5) or
      ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

//------------------------------------------------------------------------------

destructor TAdvStickyMenuWindow.Destroy;
begin
  if Assigned(FHideTimer) then
  begin
    FHideTimer.Enabled := false;
    FHideTimer.Free;
    FHideTimer := nil;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvStickyMenuWindow.GetParentWnd: HWnd;
var
  Last, P: HWnd;
begin
  P := {GetParent}((Owner as TWinControl).Handle);
  Last := P;
  while P <> 0 do
  begin
    Last := P;
    P := GetParent(P);
  end;
  Result := Last;
end;

//------------------------------------------------------------------------------

function IsAnyChildCtrlHasFocus(WinCtrl: TWinControl; ActiveHandle: HWND): Boolean;
var
  i: Integer;
  h: Hwnd;
  //a: array[0..255] of char;
begin
  Result := False;
  if not Assigned(WinCtrl) then
    Exit;

  h := GetFocus;
  if ActiveHandle <> 0 then
    h := ActiveHandle;
  i := 1;
  while (h <> 0) do
  begin
    if (h = WinCtrl.Handle) then
    begin
      Result := True;
      Break;
    end;
    h := GetParent(h);
    inc(i);
    if (i > 80) then
      Break;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.HideTimerOnTime(Sender: TObject);
begin
  FHideTimer.Enabled := false;
  if IsAnyChildCtrlHasFocus(Self, 0) then
    Exit;

  Hide;
  FAdvStickyPopupMenu.FIsVisible := false;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.Paint;
var
  R: TRect;
begin
  inherited;

  if ShowBorder then
  begin
    R := ClientRect;
    Canvas.Pen.Color := BorderColor;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.WMActivate(var Message: TWMActivate);
var
  CanHide: Boolean;
  {rgn: THandle;
  R: TRect;}
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;


  if Message.Active = integer(False) then
  begin
    if HideOnDeactivate and Visible and Assigned(AdvStickyMenuPanel) and not AdvStickyMenuPanel.FDonotHideMenu then
    begin
      CanHide := True;
      if Assigned(OnHideQuery) then
        OnHideQuery(Self, CanHide);
      if CanHide then
      begin
        //Hide;
        FHideTimer.Enabled := true;
        //FAdvStickyPopupMenu.FIsVisible := false;
      end;
    end;
  end
  else
  begin
    if Assigned(AdvStickyMenuPanel) then
    begin
      SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);
    end;

    {R := ClientRect;
    rgn := CreateRoundRectRgn(0,0,R.Right-R.Left,R.Bottom-R.Top, 4, 4);
    if rgn > 0 then
    begin
      try
        SetWindowRgn(Handle,rgn,true);
      finally
        DeleteObject(rgn);
      end;
    end;}
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
  if Assigned(AdvStickyMenuPanel) then
    AdvStickyMenuPanel.KeyDown(Message.CharCode, []);
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.UpdateSize;
var
  x, y: Integer;
begin
  if Assigned(AdvStickyMenuPanel) then
  begin
    x := 0;
    y := 0;

    AdvStickyMenuPanel.InitializeAndUpdate;
    AdvStickyMenuPanel.Left := x;
    AdvStickyMenuPanel.Top := y;

    Width := AdvStickyMenuPanel.Width + x * 2;
    Height := AdvStickyMenuPanel.Height + y * 2;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.CMDialogChar(var Message: TCMDialogChar);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyMenuWindow.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS + DLGC_WANTTAB;
end;

//------------------------------------------------------------------------------

{ TAdvStickyPopupMenu }

constructor TAdvStickyPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := AOwner;
  FStickyMenuPanel:= nil;
  FStickyMenuWindow := nil;
  FInternalStyler := TCustomAdvMenuStyler.Create(self);
  FInternalStyler.Name := 'InternalStyler';
  FStyler := nil;
  FCurrentStyler := FInternalStyler;
  {$IFDEF DELPHI6_LVL}
  FInternalStyler.SetSubComponent(True);
  {$ENDIF}

  FItemMarginX := 2;
  FItemMarginY := 3;
  FItemOffSetX := 3;
  FItemOffSetY := 3;

  FMenuItems := TStickyMenuItems.Create(Self);
  FButtonBar := TButtonBar.Create(Self);
  FDisplayRelative := drScreen;
  FShowButtonBar := True;
end;

//------------------------------------------------------------------------------

destructor TAdvStickyPopupMenu.Destroy;
begin
  FMenuItems.Free;
  FButtonBar.Free;
  FInternalStyler.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.BeginUpdate;
begin
  if not FIsVisible or not Assigned(FStickyMenuPanel) then
    Exit;

  FStickyMenuPanel.BeginUpdate;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.EndUpdate;
begin
  if not FIsVisible or not Assigned(FStickyMenuPanel) then
    Exit;

  FStickyMenuPanel.EndUpdate;
end;

//------------------------------------------------------------------------------

function TAdvStickyPopupMenu.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvStickyPopupMenu.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited;
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if (AComponent = ItemImages) then
      ItemImages := nil;

    if (AComponent = FButtonImages) then
      ButtonImages := nil;

    if (AComponent = Styler) then
      Styler := nil;

    for i := 0 to MenuItems.Count - 1 do
    begin
      if (MenuItems[i].Control = AComponent) then
        MenuItems[i].Control := nil;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.SetButtonImages(const Value: TCustomImageList);
begin
  FButtonImages := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.SetMenuItems(const Value: TStickyMenuItems);
begin
  FMenuItems.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.SetStyler(const Value: TCustomAdvMenuStyler);
begin
  if (FStyler <> Value) or (Value = nil) then
  begin
    FStyler := Value;

    if (FStyler = nil) then
      FCurrentStyler := FInternalStyler
    else
      FCurrentStyler := FStyler;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    InitializeMenu;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.InitializeMenu;
begin
  if (FStickyMenuWindow = nil) then
  begin
    FStickyMenuWindow := TAdvStickyMenuWindow.CreateNew(FOwner);
    FStickyMenuWindow.BorderIcons := [];
    FStickyMenuWindow.BorderStyle := bsNone;
    FStickyMenuWindow.Ctl3D := false;
    FStickyMenuWindow.FormStyle := fsStayOnTop;
    FStickyMenuWindow.Visible := False;
    FStickyMenuWindow.AutoScroll := False;
    FStickyMenuWindow.BorderWidth := 0;
    FStickyMenuWindow.OnHide := OnStickyMenuWindowHide;
    FStickyMenuWindow.OnClose := OnStickyMenuWindowClose;
    FStickyMenuWindow.OnHideQuery := OnStickyMenuWindowHideQuery;
    FStickyMenuWindow.ShowBorder := False;
    FStickyMenuWindow.AdvStickyPopupMenu := Self;
  end;

  if (FStickyMenuPanel = nil) then
  begin
    FStickyMenuPanel := TAdvStickyMenuPanel.Create(FStickyMenuWindow);
    FStickyMenuPanel.Parent := FStickyMenuWindow;
    FStickyMenuPanel.ShowHint := true;
  end;
  FStickyMenuPanel.AdvStickyPopupMenu := Self;

  FStickyMenuWindow.AdvStickyMenuPanel := FStickyMenuPanel;
  FStickyMenuWindow.UpdateSize;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.ShowMenu(X, Y: Integer);
var
  P: TPoint;
  sw: Integer;
  mon: TMonitor;
begin
  InitializeMenu;

  if Assigned(FStickyMenuWindow) then
  begin
    if Assigned(FOnMenuShow) then
      FOnMenuShow(Self);

    P := Point(X, Y);

    if (DisplayRelative = drForm) then
    begin
      if Assigned(FOwner) and (FOwner is TForm) then
        P := TForm(FOwner).ClientToScreen(P)
      else if Assigned(Application.MainForm) then
        P := Application.MainForm.ClientToScreen(P);
    end;

    mon := Screen.MonitorFromPoint(P);
    if Assigned(mon) then
      sw := mon.Width
    else
      sw := Screen.Width;

    if (P.X + FStickyMenuWindow.Width > sw) and (Screen.MonitorCount = 1) then
      P.X := sw - FStickyMenuWindow.Width;

    FStickyMenuWindow.Left := P.X;

    if P.Y + FStickyMenuWindow.Height > Screen.Height then
      P.Y := Screen.Height - FStickyMenuWindow.Height;

    FStickyMenuWindow.Top := P.Y;

    FStickyMenuWindow.Visible := True;

    FIsVisible := true;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.HideMenu;
var
  CanHide: Boolean;
begin
  CanHide := True;
  if Assigned(FOnMenuHideQuery) then
    FOnMenuHideQuery(Self, CanHide);

  if not CanHide then
    Exit;

  if Assigned(FStickyMenuWindow) then
    FStickyMenuWindow.Hide;

  FIsVisible := false;    
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.SetButtonBar(const Value: TButtonBar);
begin
  FButtonBar.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.OnStickyMenuWindowClose(Sender: TObject; var Action: TCloseAction);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.OnStickyMenuWindowHide(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
  begin
    if Assigned(FOnMenuHide) then
      FOnMenuHide(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.SetItemImages(const Value: TCustomImageList);
begin
  if (FItemImages <> Value) then
  begin
    FItemImages := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.SetShowButtonBar(const Value: Boolean);
begin
  if (FShowButtonBar <> Value) then
  begin
    FShowButtonBar := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvStickyPopupMenu.OnStickyMenuWindowHideQuery(Sender: TObject;
  var CanHide: Boolean);
begin
  if Assigned(FOnMenuHideQuery) then
    FOnMenuHideQuery(Self, CanHide);
end;

//------------------------------------------------------------------------------

end.
