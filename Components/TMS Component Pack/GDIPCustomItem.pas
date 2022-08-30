{*************************************************************************}
{ TCustomItem Base Class                                                  }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010 - 2015                                      }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit GDIPCustomItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Forms, Messages, SysUtils, Controls, Classes, Graphics, GDIPFill, AdvGDIP,
  GDIPPictureContainer, ImgList, Math, ActnList, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  IMAJ_VER = 1; // Major version nr.
  IMIN_VER = 0; // Minor version nr.
  IREL_VER = 0; // Release nr.
  IBLD_VER = 0; // Build nr.

type
  TCustomItem = class;

  TWinControlAccess = class(TWinControl);

  TPictureContainerListItem = class(TCollectionItem)
  private
    FName: String;
    FPicture: TAdvGDIPPicture;
  public
    property Name: String read FName write FName;
    property Picture: TAdvGDIPPicture read FPicture write FPicture;
  end;

  TPictureContainerList = class(TCollection)
  public
    function Add: TPictureContainerListItem;
  end;

  IPictureContainerSupport = interface
  ['{AF45F542-068A-46A9-90C2-BF39003D3A46}']
  procedure FillPictureNames(Proc: TGetStrProc);
  procedure GetPictures(APictureList: TPictureContainerList);
  end;

  IGDIPBase = interface
  ['{02295790-46AF-4227-AEF8-7C22AC0CD8BA}']
  procedure ReadItemState(Reader: TReader; Item: TCustomItem);
  procedure SetItemParentComponent(AParent: TComponent; Item: TCustomItem);
  end;

  IGDIPGlobalCheck = interface
  ['{BA9B81B1-5DCF-4265-97EA-BF480A4DFF3A}']
  procedure GlobalCheck(Item: TCustomItem);
  end;

  IGDIPAnchor = interface
  ['{CBFF4B08-283E-4EC3-9E5E-FF013733C8C7}']
  procedure Anchor(Anchor: String);
  end;

  IGDIPExpand = interface
  ['{B61DA022-0C88-4D88-9117-E579D0B8FC3A}']
  procedure Expand(Item: TCustomItem; Expand: Boolean);
  end;

  {$IFNDEF DELPHI2006_LVL}
  TMargins = class(TPersistent)
  private
    FRight: integer;
    FBottom: integer;
    FOnChange: TNotifyEvent;
    FTop: integer;
    FLeft: integer;
    procedure SetBottom(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetRight(const Value: integer);
    procedure SetTop(const Value: integer);
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TCustomItem);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Bottom: integer read FBottom write SetBottom default 3;
    property Left: integer read FLeft write SetLeft default 3;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Right: integer read FRight write SetRight default 3;
    property Top: integer read FTop write SetTop default 3;
  end;
  {$ENDIF}

  TItemActionLink = class(TActionLink)
  protected
    FClient: TCustomItem;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHelpContextLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsShortCutLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetCaption(const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetHint(const Value: String); override;
    procedure SetGroupIndex(Value: Integer); override;
  end;
  TItemActionLinkClass = class of TItemActionLink;

  TItemLocation = (tlTopLeft, tlTopCenter, tlTopRight, tlCenterLeft, tlCenterCenter,
    tlCenterRight, tlBottomLeft, tlBottomCenter, tlBottomRight, tlCustom);

  TItemLineLocation = (llLeft, llTop, llRight, llBottom);

  TItemLineLocations = set of TItemLineLocation;

  TItemState = (isNormal, isDown, isHovered, isSelected);

  TItemStates = set of TItemState;

  TStatusState  = (ssNormal, ssDown, ssHovered);

  TItemIndexEvent = procedure(Sender: TObject; Item: TCustomItem; OldIndex, NewIndex: integer) of object;

  TItemEvent = procedure(Sender: TObject; Item: TCustomItem) of object;

  TItemSelectEvent = procedure(Sender: TObject; Item: TCustomItem; var Allow: Boolean) of object;

  TItemCheckEvent = procedure(Sender: TObject; Item: TCustomItem; Checked: Boolean) of object;

  TItemStatusPosition = (spItemRectangle, spItemText);

  TItemDrawEvent = procedure(Sender: TObject; AGraphics: TGPGraphics; Item: TCustomItem; ARect: TGPRectF) of object;

  TItemHintEvent = procedure(Sender: TObject; Item: TCustomItem; var Hint: String) of object;

  TItemInteractionType = (itNone, itDefault, itExpander, itButton, itStatus, itGraphic);

  TItemLevelStatus = (hsWithinMainLevel, hsAll);

  TItemInteraction = record
    InteractionItem: TCustomItem;
    InteractionType: TItemInteractionType;
  end;

  TItemAppearance = record
    DrawHTMLCache: Boolean;
    IsMetroStyle: Boolean;
    Disabled, Normal, Down, Hover, Selected: TGDIPFill;
    ButtonDisabled, ButtonNormal, ButtonDown, ButtonHover, ButtonSelected: TGDIPFill;
    DisabledFont, NormalFont, DownFont, HoverFont, SelectedFont: TFont;
    Focus: Boolean;
    FocusedItem: Integer;
    PictureContainer: TGDIPPictureContainer;
    ImageList: TCustomImageList;
  end;

  TItemStatus = class(TPersistent)
  private
    FStatusRect: TGPRectF;
    FOwner: TCustomItem;
    FOffsetTop: integer;
    FOffsetLeft: integer;
    FVisible: Boolean;
    FCaption: String;
    FAppearance: TGDIPStatus;
    FOnChange: TNotifyEvent;
    FPosition: TItemStatusPosition;
    FHint: String;
    procedure SetCaption(const Value: String);
    procedure SetOffsetLeft(const Value: integer);
    procedure SetOffsetTop(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetAppearance(const Value: TGDIPStatus);
    procedure SetPosition(const Value: TItemStatusPosition);
    procedure SetHint(const Value: String);
  protected
    procedure Changed;
    procedure AppearanceChanged(Sender: TObject);
  public
    constructor Create(AOwner: TCustomItem);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default false;
    property Caption: String read FCaption write SetCaption;
    property OffsetLeft: integer read FOffsetLeft write SetOffsetLeft default -6;
    property OffsetTop: integer read FOffsetTop write SetOffsetTop default 14;
    property Appearance: TGDIPStatus read FAppearance write SetAppearance;
    property Position: TItemStatusPosition read FPosition write SetPosition default spItemText;
    property Hint: String read FHint write SetHint;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCustomItemClass = class of TCustomItem;

  TItemShortCutHintPos = (ishpLeft, ishpTop, ishpRight, ishpBottom, ishpCenter, ishpAuto,
                    ishpTopLeft, ishpTopRight, ishpAboveTop, ishpAboveTopLeft,
                    ishpAboveTopRight, ishpBottomLeft, ishpBottomRight, ishpBelowBottom,
                    ishpBelowBottomLeft, ishpBelowBottomRight, ishpBelowBottomCenter);

  TOnItemGetShortCutHintPos = procedure(Sender: TObject; var ShortCutHintPosition: TItemShortCutHintPos) of object;

  TShortCutHintWindow = class(THintWindow)
  private
    FColor: TColor;
    FColorTo: TColor;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
  protected
    procedure Resize; override;
    procedure Paint; override;
    procedure CreateParams(var Params:TCreateParams);override;
  published
    property Color: TColor read FColor write FColor;
    property ColorTo: TColor read FColorTo write FColorTo;
  end;

  TCustomItem = class(TComponent)
  private
    FShortCutHint: TShortCutHintWindow;
    FControlFocused: Boolean;
    FUpdateCount: integer;
    FOwner: TComponent;
    FEnabled: Boolean;
    FWidth: integer;
    FVisible: Boolean;
    FHeight: integer;
    FOnChange: TNotifyEvent;
    FX: integer;
    FY: integer;
    FState: TItemState;
    FOnItemSelect: TItemSelectEvent;
    FItemObject: TObject;
    FTag: Integer;
    FIndex: Integer;
    FStatus: TItemStatus;
    FLevel: Integer;
    FGlow: TGlowMode;
    FStatusState: TStatusState;
    FOnItemEndDraw: TItemDrawEvent;
    FOnItemStartDraw: TItemDrawEvent;
    FOnInternalItemStartDraw: TItemDrawEvent;
    FOnInternalItemEndDraw: TItemDrawEvent;
    FOnInternalItemSelect: TItemSelectEvent;
    FHint: String;
    FControl: TWinControl;
    FControlMargin: TMargins;
    FControlStretched: Boolean;
    FControlLocation: TItemLocation;
    FReadOnly: Boolean;
    FRowIndex: integer;
    FColumnIndex: integer;
    FOnRefresh: TNotifyEvent;
    FOnInternalRefresh: TNotifyEvent;
    FOnInternalIndexChange: TItemIndexEvent;
    FGlowGradientColor: TColor;
    FGlowRadialColor: TColor;
    FOnItemHint: TItemHintEvent;
    FOnItemStatusHint: TItemHintEvent;
    FOnInternalItemStatusHint: TItemHintEvent;
    FOnInternalItemHint: TItemHintEvent;
    FOnGlobalCheckChanged: TItemCheckEvent;
    FOnItemClick: TItemEvent;
    FOnItemRightClick: TItemEvent;
    FOnInternalItemClick: TItemEvent;
    FOnItemDblClick: TItemEvent;
    FOnInternalItemDblClick: TItemEvent;
    FSelectable: Boolean;
    FOnInternalItemDeSelect: TItemSelectEvent;
    FOnItemDeSelect: TItemSelectEvent;
    FItemFocused: Boolean;
    FTabStop: Boolean;
    FOnGetShortCutHintPos: TOnItemGetShortCutHintPos;
    FShortCutHintPos: TItemShortCutHintPos;
    FShortCutHintText: string;
    FOnInternalFocus: TItemEvent;
    FOnInternalDestroy: TItemEvent;
    FActionLink: TItemActionLink;
    FColumnSpan: Integer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetHeight(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: integer);
    procedure SetX(const Value: integer);
    procedure SetY(const Value: integer);
    procedure SetState(const Value: TItemState);
    procedure SetStatus(const Value: TItemStatus);
    procedure SetStatusState(const Value: TStatusState);
    procedure SetHint(const Value: String);
    procedure SetControlLocation(const Value: TItemLocation);
    procedure SetControlMargin(const Value: TMargins);
    procedure SetControlStretched(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetIndex(const Value: Integer);
    procedure SetControl(const Value: TWinControl);
    function GetVersion: String;
    procedure SetSelectable(const Value: Boolean);
    procedure SetItemFocused(const Value: Boolean);
    procedure SetTabStop(const Value: Boolean);
    procedure SetShortCutHintPos(const Value: TItemShortCutHintPos);
    procedure SetShortCutHintText(const Value: string);
    function GetAction: TBasicAction;
    procedure SetAction(const Value: TBasicAction);
    procedure DoActionChange(Sender: TObject);
    procedure SetColumnSpan(const Value: Integer);
  protected
    function GetActionLinkClass: TItemActionLinkClass; dynamic;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
    property ActionLink: TItemActionLink read FActionLink write FActionLink;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure ReadState(Reader: TReader); override;
    function GetOwner: TPersistent; override;
    procedure DrawStatus(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); virtual;
    procedure DrawFocus(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); virtual;
    procedure Changed; virtual;
    procedure RefreshObject;
    procedure StatusChanged(Sender: TObject);
    procedure ControlMarginChanged(Sender: TObject);
    function GetFill(AItemAppearance: TItemAppearance): TGDIPFill;
    function GetFont(AItemAppearance: TItemAppearance): TFont; overload;
    procedure DoInternalIndexChange(Sender: TObject; Item: TCustomItem; OldIndex, NewIndex: integer); virtual;
    procedure DoRefresh(Sender: TObject); virtual;
    procedure DoInternalRefresh(Sender: TObject); virtual;
    procedure DoChange(Sender: TObject); virtual;
    procedure DoControlExit(Sender: TObject);
    procedure DoItemHint(Sender: TObject; Item: TCustomItem; var Hint: String); virtual;
    procedure DoItemStatusHint(Sender: TObject; Item: TCustomItem; var StatusHint: String); virtual;
    procedure DoItemSelect(Sender: TObject; Item: TCustomItem; var Allow: Boolean); virtual;
    procedure DoItemDeSelect(Sender: TObject; Item: TCustomItem; var Allow: Boolean); virtual;
    procedure DoItemClick(Sender: TObject; Item: TCustomItem); virtual;
    procedure DoItemRightClick(Sender: TObject; Item: TCustomItem); virtual;
    procedure DoItemDblClick(Sender: TObject; Item: TCustomItem); virtual;
    procedure DoItemStartDraw(Sender: TObject; AGraphics: TGPGraphics; Item: TCustomItem; ARect: TGPRectF); virtual;
    procedure DoItemEndDraw(Sender: TObject; AGraphics: TGPGraphics; Item: TCustomItem; ARect: TGPRectF); virtual;
    procedure DoInternalChange(Sender: TObject); virtual;
    procedure DoInternalFocus(Item: TCustomItem);
    procedure DoInternalItemSelect(Sender: TObject; Item: TCustomItem; var Allow: Boolean); virtual;
    procedure DoInternalItemDeSelect(Sender: TObject; Item: TCustomItem; var Allow: Boolean); virtual;
    procedure DoInternalItemClick(Sender: TObject; Item: TCustomItem); virtual;
    procedure DoInternalItemDblClick(Sender: TObject; Item: TCustomItem); virtual;
    procedure DoInternalItemStartDraw(Sender: TObject; AGraphics: TGPGraphics; Item: TCustomItem; ARect: TGPRectF); virtual;
    procedure DoInternalItemEndDraw(Sender: TObject; AGraphics: TGPGraphics; Item: TCustomItem; ARect: TGPRectF); virtual;
    procedure DoInternalItemHint(Sender: TObject; Item: TCustomItem; var Hint: String); virtual;
    procedure DoInternalItemStatusHint(Sender: TObject; Item: TCustomItem; var StatusHint: String); virtual;
    procedure DoInternalItemDestroy(Sender: TObject; Item: TCustomItem);
    procedure DoGlobalCheckChanged(Sender: TObject; Item: TCustomItem; Checked: Boolean); virtual;
    function GetVersionNr: integer; virtual;
    property Action: TBasicAction read GetAction write SetAction;
    function ActionIsCaptionLinked: Boolean; virtual;
    function ActionIsCheckedLinked: Boolean; virtual;
    function ActionIsEnabledLinked: Boolean; virtual;
    function ActionIsHelpContextLinked: Boolean; virtual;
    function ActionIsHintLinked: Boolean; virtual;
    function ActionIsGroupIndexLinked: Boolean; virtual;
    function ActionIsImageIndexLinked: Boolean; virtual;
    function ActionIsShortCutLinked: Boolean; virtual;
    function ActionIsVisibleLinked: Boolean; virtual;
    function ActionIsOnExecuteLinked: Boolean; virtual;
    procedure ActionSetChecked(Value: Boolean); virtual;
    procedure ActionSetCaption(const Value: string); virtual;
    procedure ActionSetEnabled(Value: Boolean); virtual;
    procedure ActionSetImageIndex(Value: Integer); virtual;
    procedure ActionSetVisible(Value: Boolean); virtual;
    procedure ActionSetOnExecute(Value: TNotifyEvent); virtual;
    procedure ActionSetHint(const Value: String); virtual;
    procedure ActionSetGroupIndex(Value: Integer); virtual;
    function UseButtonCaptionForAction: Boolean; virtual;
    procedure Loaded; override;
  public
    //must override
    function CreateNewItem(AOwner: TComponent): TCustomItem; virtual;
    function GetClassType: TComponentClass; virtual;
    class function CustomClassName: String; virtual;
    class function Display: Boolean; virtual;
    procedure Draw(g: TGPGraphics; ItemAppearance: TItemAppearance); virtual; //+Draw events
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); virtual; //+Draw events
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsSection: boolean; virtual;
    procedure UseCaption(ACaption: String); virtual;
    function GetCustomControlOwner(AOwner: TComponent): TWinControl;
    procedure ApplyIndex(AIndex: integer);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearItemState; virtual;
    procedure AssignEvents(Item: TCustomItem); virtual;
    procedure DoCMDialogChar(var Message: TCMDialogChar); virtual;
    procedure DoCMMouseLeave(var Message: TMessage); virtual;
    procedure DoCMHintShow(var Message: TMessage; Interaction: TItemInteraction); virtual;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); virtual;
    procedure DoDblClick(Sender: TObject; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); virtual;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); virtual;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); virtual;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    function IsItemAtXY(pX, pY: integer): Boolean; virtual;
    function IsStatusAtXY(pX, pY: integer): Boolean; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure SetParentComponent(AParent: TComponent); override;
    function GetItemInteraction(pX, pY: integer): TItemInteractionType; virtual;
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    property ItemOwner: TComponent read FOwner write FOwner;
    property StatusState: TStatusState read FStatusState write SetStatusState;
    property State: TItemState read FState write SetState default isNormal;
    property OnGlobalCheckChanged: TItemCheckEvent read FOnGlobalCheckChanged write FOnGlobalCheckChanged;
    property OnInternalFocus: TItemEvent read FOnInternalFocus write FOnInternalFocus;
    property OnInternalIndexChange: TItemIndexEvent read FOnInternalIndexChange write FOnInternalIndexChange;
    property OnInternalRefresh: TNotifyEvent read FOnInternalRefresh write FOnInternalRefresh;
    property OnInternalChange: TNotifyEvent read FOnChange write FOnChange;
    property OnInternalItemSelect: TItemSelectEvent read FOnInternalItemSelect write FOnInternalItemSelect;
    property OnInternalItemDeSelect: TItemSelectEvent read FOnInternalItemDeSelect write FOnInternalItemDeSelect;
    property OnInternalDestroy: TItemEvent read FOnInternalDestroy write FOnInternalDestroy;
    property OnInternalItemClick: TItemEvent read FOnInternalItemClick write FOnInternalItemClick;
    property OnInternalItemDblClick: TItemEvent read FOnInternalItemDblClick write FOnInternalItemDblClick;
    property OnInternalItemStartDraw: TItemDrawEvent read FOnInternalItemStartDraw write FOnInternalItemStartDraw;
    property OnInternalItemEndDraw: TItemDrawEvent read FOnInternalItemEndDraw write FOnInternalItemEndDraw;
    property OnInternalItemHint: TItemHintEvent read FOnInternalItemHint write FOnInternalItemHint;
    property OnInternalItemStatusHint: TItemHintEvent read FOnInternalItemStatusHint write FOnInternalItemStatusHint;
    property OnGetShortCutHintPos: TOnItemGetShortCutHintPos read FOnGetShortCutHintPos write FOnGetShortCutHintPos;
    property RowIndex: integer read FRowIndex write FRowIndex default -1;
    property ColumnIndex: integer read FColumnIndex write FColumnIndex default -1;
    procedure FillPictureNames(Proc: TGetStrProc);
    function GetPictures: TPictureContainerList;
    procedure InitDesignTime; virtual;
    procedure CancelTab; virtual;
    function ProcessTab(Backwards: Boolean): Boolean; virtual;
    procedure ResetTab; virtual;
    property ItemFocused: Boolean read FItemFocused write SetItemFocused default true;
    function IsFocusable: Boolean; virtual;
    function FirstTab(Backwards: Boolean): Boolean; virtual;
    procedure ShowShortCutHint;
    procedure HideShortCutHint;
    procedure DoAction;
  published
    property ColumnSpan: Integer read FColumnSpan write SetColumnSpan default 1;
    property X: integer read FX write SetX default 0;
    property Y: integer read FY write SetY default 0;
    property Height: integer read FHeight write SetHeight default 30;
    property Width: integer read FWidth write SetWidth default 75;
    property Visible: Boolean read FVisible write SetVisible default true;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property ItemObject: TObject read FItemObject write FItemObject;
    property Tag: Integer read FTag write FTag;
    property Level: Integer read FLevel write FLevel;
    property Index: Integer read FIndex write SetIndex;
    property Status: TItemStatus read FStatus write SetStatus;
    property Hint: String read FHint write SetHint;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
    property OnItemHint: TItemHintEvent read FOnItemHint write FOnItemHint;
    property OnItemStatusHint: TItemHintEvent read FOnItemStatusHint write FOnItemStatusHint;
    property OnItemSelect: TItemSelectEvent read FOnItemSelect write FOnItemSelect;
    property OnItemClick: TItemEvent read FOnItemClick write FOnItemClick;
    property OnItemDblClick: TItemEvent read FOnItemDblClick write FOnItemDblClick;
    property OnItemRightClick: TItemEvent read FOnItemRightClick write FOnItemRightClick;
    property OnItemStartDraw: TItemDrawEvent read FOnItemStartDraw write FOnItemStartDraw;
    property OnItemEndDraw: TItemDrawEvent read FOnItemEndDraw write FOnItemEndDraw;
    property OnItemDeSelect: TItemSelectEvent read FOnItemDeSelect write FOnItemDeSelect;
    property Control: TWinControl read FControl write SetControl;
    property ControlMargin: TMargins read FControlMargin write SetControlMargin;
    property ControlLocation: TItemLocation read FControlLocation write SetControlLocation default tlCenterLeft;
    property ControlStretched: Boolean read FControlStretched write SetControlStretched default false;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;
    property Selectable: Boolean read FSelectable write SetSelectable default true;
    property TabStop: Boolean read FTabStop write SetTabStop default true;
    property Version: String read GetVersion;
    property ShortCutHint: string read FShortCutHintText write SetShortCutHintText;
    property ShortCutHintPos: TItemShortCutHintPos read FShortCutHintPos write SetShortCutHintPos default ishpTop;
  end;

function PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
procedure GetObjectLocation(var pX, pY: Double; rectangle: TGPRectF; objectwidth, objectheight: Double; location: TItemLocation);
procedure LoadFromRes(resname: string; picture: TAdvGDIPPicture);
function RectanglesInterSect(r1, r2: TGPRectF): Boolean;
procedure RegisterPolyItem(AItem: TCustomItemClass);

procedure Register;

implementation

procedure Register;
begin
  RegisterClass(TCustomItem);
end;

procedure RegisterPolyItem(AItem: TCustomItemClass);
begin
  RegisterNoIcon([AItem]);
  RegisterClass(AItem);
end;

{$R CustomItemResources}

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
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


function RectanglesInterSect(r1, r2: TGPRectF): Boolean;
var
  x, y, w, h: Double;
begin
  x := max(r1.X, r2.X);
  y := max(r1.Y, r2.Y);
  w := min(r1.X + r1.Width, r2.X + r2.Width);
  h := min(r1.Y + r1.Height, r2.Y + r2.Height);

  result := ((w > x) and (h > y));
end;

procedure LoadFromRes(resname: string; picture: TAdvGDIPPicture);
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(HInstance, resname, RT_RCDATA);
  picture.LoadFromStream(rs);
  rs.Free;
end;

function PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
begin
  result := ((pt.X >= r.X) and (pt.X <= r.X + r.Width)) and
     ((pt.Y >= r.Y) and (pt.Y <= r.Y + r.Height));
end;

procedure GetObjectLocation(var pX, pY: Double; rectangle: TGPRectF; objectwidth, objectheight: Double; location: TItemLocation);
var
  w, h, tw, th: Double;
begin
  tw := objectwidth;
  th := objectheight;
  w := rectangle.Width;
  h := rectangle.Height;
  case location of
    tlTopLeft:
    begin
      pX := 0;
      pY := 0;
    end;
    tlTopRight:
    begin
      pX := w - tw;
      pY := 0;
    end;
    tlBottomLeft:
    begin
      pX := 0;
      pY := h - th;
    end;
    tlBottomRight:
    begin
      pX := w - tw;
      pY := h - th;
    end;
    tlTopCenter:
    begin
      pX := (w - tw) / 2;
      pY := 0;
    end;
    tlBottomCenter:
    begin
      pX := (w - tw) / 2;
      pY := h - th;
    end;
    tlCenterCenter:
    begin
      pX := (w - tw) / 2;
      pY := (h - th) / 2;
    end;
    tlCenterLeft:
    begin
      pX := 0;
      pY := (h - th) / 2;
    end;
    tlCenterRight:
    begin
      pX := w - tw;
      pY := (h - th) / 2;
    end;
  end;
end;

{ TCustomItem }

procedure TCustomItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
  with TCustomAction(Sender) do
  begin
    if not CheckDefaults or (Self.Hint = '') then
      Self.Hint := Hint;

    if not CheckDefaults or (Self.Visible) then
      Self.Visible := Visible;
  end;
end;

function TCustomItem.ActionIsCaptionLinked: Boolean;
begin
  Result := False;
end;

function TCustomItem.ActionIsCheckedLinked: Boolean;
begin
  Result := False;
end;

function TCustomItem.ActionIsEnabledLinked: Boolean;
begin
  Result := False;
end;

function TCustomItem.ActionIsGroupIndexLinked: Boolean;
begin
  Result := False;
end;

function TCustomItem.ActionIsHelpContextLinked: Boolean;
begin
  Result := False;
end;

function TCustomItem.ActionIsHintLinked: Boolean;
begin
  Result := Hint = (Action as TCustomAction).Hint;
end;

function TCustomItem.ActionIsImageIndexLinked: Boolean;
begin
  Result := False;
end;

function TCustomItem.ActionIsOnExecuteLinked: Boolean;
begin
  Result := False;
end;

function TCustomItem.ActionIsShortCutLinked: Boolean;
begin
  Result := False;
end;

function TCustomItem.ActionIsVisibleLinked: Boolean;
begin
  Result := Visible = (Action as TCustomAction).Visible;
end;

procedure TCustomItem.ActionSetCaption(const Value: string);
begin
//
end;

procedure TCustomItem.ActionSetChecked(Value: Boolean);
begin
//
end;

procedure TCustomItem.ActionSetEnabled(Value: Boolean);
begin
//
end;

procedure TCustomItem.ActionSetGroupIndex(Value: Integer);
begin
//
end;

procedure TCustomItem.ActionSetHint(const Value: String);
begin
  Hint := Value;
end;

procedure TCustomItem.ActionSetImageIndex(Value: Integer);
begin
//
end;

procedure TCustomItem.ActionSetOnExecute(Value: TNotifyEvent);
begin
//
end;

procedure TCustomItem.ActionSetVisible(Value: Boolean);
begin
  Visible := Value;
end;

procedure TCustomItem.ApplyIndex(AIndex: integer);
begin
  FIndex := AIndex;
end;

procedure TCustomItem.Assign(Source: TPersistent);
begin
  if Source is TCustomItem then
  begin
    FX := (Source as TCustomItem).X;
    FY := (Source as TCustomItem).Y;
    FWidth := (Source as TCustomItem).Width;
    FHeight := (Source as TCustomItem).Height;
    FVisible := (Source as TCustomItem).Visible;
    FEnabled := (Source as TCustomItem).Enabled;
    FColumnSpan := (Source as TCustomItem).ColumnSpan;
    FItemObject := (Source as TCustomItem).ItemObject;
    FTag := (Source as TCustomItem).Tag;
    FLevel := (Source as TCustomItem).Level;
    FStatus.Assign((Source as TCustomItem).Status);
    FControlMargin.Assign(((Source as TCustomItem).ControlMargin));
    FControlStretched := (Source as TCustomItem).ControlStretched;
    FControlLocation := (Source as TCustomItem).ControlLocation;
    FSelectable := (Source as TCustomItem).Selectable;
    FReadOnly := (Source as TCustomItem).ReadOnly;
    FHint := (Source as TCustomItem).Hint;
    Changed;
  end;
end;

procedure TCustomItem.AssignEvents(Item: TCustomItem);
begin
  OnChange := Item.OnChange;
  OnRefresh := Item.OnRefresh;
  OnItemHint := Item.OnItemHint;
  OnItemStatusHint := Item.OnItemStatusHint;
  OnItemSelect := Item.OnItemSelect;
  OnItemClick := Item.OnItemClick;
  OnItemRightClick := Item.OnItemRightClick;
  OnItemDblClick := Item.OnItemDblClick;
  OnItemStartDraw := Item.OnItemStartDraw;
  OnItemEndDraw := Item.OnItemEndDraw;
  OnItemDeSelect := Item.OnItemDeSelect;
end;

procedure TCustomItem.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomItem.CancelTab;
begin

end;

procedure TCustomItem.Changed;
begin
  if FUpdateCount = 0 then
  begin
    DoChange(Self);
    DoInternalChange(Self);
  end;
end;

procedure TCustomItem.ClearItemState;
begin
  if State <> isSelected then
    State := isNormal;
  StatusState := ssNormal;
end;

procedure TCustomItem.ControlMarginChanged(Sender: TObject);
begin
  Changed;
end;

constructor TCustomItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FWidth := 125;
  FHeight := 30;
  FX := 0;
  FY := 0;
  FColumnSpan := 1;
  FColumnIndex := -1;
  FRowIndex := -1;
  FEnabled := true;
  FVisible := true;
  FGlow := gmGradient;
  FGlowGradientColor := clWhite;
  FGlowRadialColor := clWhite;
  FStatus := TItemStatus.Create(Self);
  FStatus.OnChange := StatusChanged;
  FControlMargin := TMargins.Create(nil);
  FControlMargin.OnChange := ControlMarginChanged;
  FControlLocation := tlCenterLeft;
  FControlStretched := false;
  FReadOnly := false;
  FSelectable := true;
  FItemFocused := true;
  FTabStop := true;
  FShortCutHintPos := ishpTop;
  if (csDesigning in AOwner.ComponentState) and not (csLoading in AOwner.ComponentState) then
    UseCaption(CustomClassName);
end;

function TCustomItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TCustomItem.Create(AOwner);
end;

class function TCustomItem.CustomClassName: String;
begin
  Result := 'Custom Item';
end;

destructor TCustomItem.Destroy;
begin
  Action := nil;
  FControlMargin.Free;
  FStatus.Free;
  DoInternalItemDestroy(Self, Self);
  inherited;
end;

class function TCustomItem.Display: Boolean;
begin
  Result := True;
end;

procedure TCustomItem.DoAction;
begin
  if Assigned(Action) then
    Action.Execute;
end;

procedure TCustomItem.DoActionChange(Sender: TObject);
begin
  if Sender = Action then ActionChange(Sender, False);
end;

procedure TCustomItem.DoChange(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TCustomItem.DoCMDialogChar(var Message: TCMDialogChar);
begin
//
end;

procedure TCustomItem.DoCMHintShow(var Message: TMessage;
  Interaction: TItemInteraction);
var
  hnt: String;
begin
  if Interaction.InteractionItem = Self then
  begin
    case Interaction.InteractionType of
      itDefault:
      begin
        hnt := Hint;

        with TCMHintShow(Message).HintInfo^ do
        begin
          DoInternalItemHint(Self, Self, hnt);
          DoItemHint(Self, Self, hnt);
          HintStr := hnt;
          ReshowTimeout := 0;
        end;
      end;
      itStatus:
      begin
        hnt := Status.Hint;

        with TCMHintShow(Message).HintInfo^ do
        begin
          DoInternalItemStatusHint(Self, Self, hnt);
          DoItemStatusHint(Self, Self, hnt);
          HintStr := hnt;
          ReshowTimeout := 0;
        end;
      end;
    end;
  end
end;

procedure TCustomItem.DoCMMouseLeave(var Message: TMessage);
begin
  ClearItemState;
end;

procedure TCustomItem.DoControlExit(Sender: TObject);
begin
  if Assigned(ItemOwner) and (ItemOwner is TWinControl) then
  begin
    if (ItemOwner as TWinControl).Visible then
      (ItemOwner as TWinControl).SetFocus;
  end;
end;

procedure TCustomItem.DoDblClick(Sender: TObject; Interaction: TItemInteraction;
  ItemAppearance: TItemAppearance);
begin
  if Interaction.InteractionItem = Self then
  begin
    DoInternalItemDblClick(Sender, Self);
    DoItemDblClick(Sender, Self);
  end;
end;

procedure TCustomItem.DoInternalChange(Sender: TObject);
begin
  if Assigned(OnInternalChange) then
    OnInternalChange(Self);
end;

procedure TCustomItem.DoInternalFocus(Item: TCustomItem);
begin
  if Assigned(OnInternalFocus) then
    OnInternalFocus(Self, Item);
end;

procedure TCustomItem.DoGlobalCheckChanged(Sender: TObject; Item: TCustomItem;
  Checked: Boolean);
begin
  if Assigned(OnGlobalCheckChanged) then
    OnGlobalCheckChanged(Sender, Item, Checked);
end;

procedure TCustomItem.DoInternalIndexChange(Sender: TObject;
  Item: TCustomItem; OldIndex, NewIndex: integer);
begin
  if Assigned(OnInternalIndexChange) then
    OnInternalIndexChange(Sender, Item, OldIndex, NewIndex);
end;

procedure TCustomItem.DoInternalItemClick(Sender: TObject; Item: TCustomItem);
begin
  if Assigned(OnInternalItemClick) then
    OnInternalItemClick(Sender, Item);
end;

procedure TCustomItem.DoInternalItemDblClick(Sender: TObject;
  Item: TCustomItem);
begin
  if Assigned(OnInternalItemDblClick) then
    OnInternalItemDblClick(Sender, Item);
end;

procedure TCustomItem.DoInternalItemDeSelect(Sender: TObject; Item: TCustomItem;
  var Allow: Boolean);
begin
  if Assigned(OnInternalItemDeSelect) then
    OnInternalItemDeSelect(Sender, Item, Allow);
end;

procedure TCustomItem.DoInternalItemDestroy(Sender: TObject; Item: TCustomItem);
begin
  if Assigned(OnInternalDestroy) then
    OnInternalDestroy(Sender, Item);
end;

procedure TCustomItem.DoInternalItemEndDraw(Sender: TObject;
  AGraphics: TGPGraphics; Item: TCustomItem; ARect: TGPRectF);
begin
  if Assigned(OnInternalItemEndDraw) then
    OnInternalItemEndDraw(Sender, AGraphics, Item, ARect);
end;

procedure TCustomItem.DoInternalItemHint(Sender: TObject; Item: TCustomItem;
  var Hint: String);
begin
  if Assigned(OnInternalItemHint) then
    OnInternalItemHint(Sender, Item, Hint);
end;

procedure TCustomItem.DoInternalItemSelect(Sender: TObject;
  Item: TCustomItem; var Allow: Boolean);
begin
  if Assigned(OnInternalItemSelect) then
    OnInternalItemSelect(Sender, Item, Allow);
end;

procedure TCustomItem.DoInternalItemStartDraw(Sender: TObject;
  AGraphics: TGPGraphics; Item: TCustomItem; ARect: TGPRectF);
begin
  if Assigned(OnInternalItemStartDraw) then
    OnInternalItemStartDraw(Sender, AGraphics, Item, ARect);
end;

procedure TCustomItem.DoInternalItemStatusHint(Sender: TObject;
  Item: TCustomItem; var StatusHint: String);
begin
  if Assigned(OnInternalItemStatusHint) then
    OnInternalItemStatusHint(Sender, Item, StatusHint);
end;

procedure TCustomItem.DoInternalRefresh(Sender: TObject);
begin
  if Assigned(OnInternalRefresh) then
    OnInternalRefresh(Self);
end;

procedure TCustomItem.DoItemClick(Sender: TObject; Item: TCustomItem);
begin
  if Assigned(OnItemClick) then
    OnItemClick(Sender, Item);

  ItemFocused := True;

  if Assigned(OnInternalFocus) then
    OnInternalFocus(Sender, Item);
end;

procedure TCustomItem.DoItemRightClick(Sender: TObject; Item: TCustomItem);
begin
  if Assigned(OnItemRightClick) then
    OnItemRightClick(Sender, Item);
end;


procedure TCustomItem.DoItemDblClick(Sender: TObject; Item: TCustomItem);
begin
  if Assigned(OnItemDblClick) then
    OnItemDblClick(Sender, Item);
end;

procedure TCustomItem.DoItemDeSelect(Sender: TObject; Item: TCustomItem;
  var Allow: Boolean);
begin
  if Assigned(OnItemDeSelect) then
    OnItemDeSelect(Sender, Item, Allow);
end;

procedure TCustomItem.DoItemEndDraw(Sender: TObject;
  AGraphics: TGPGraphics; Item: TCustomItem; ARect: TGPRectF);
begin
  if Assigned(OnItemEndDraw) then
    OnItemEndDraw(Sender, AGraphics, Item, ARect);
end;

procedure TCustomItem.DoItemHint(Sender: TObject; Item: TCustomItem;
  var Hint: String);
begin
  if Assigned(OnItemHint) then
    OnItemHint(Sender, Item, Hint);
end;

procedure TCustomItem.DoItemSelect(Sender: TObject;
  Item: TCustomItem; var Allow: Boolean);
begin
  if Assigned(OnItemSelect) then
    OnItemSelect(Sender, Item, Allow);
end;

procedure TCustomItem.DoItemStartDraw(Sender: TObject;
  AGraphics: TGPGraphics; Item: TCustomItem; ARect: TGPRectF);
begin
  if Assigned(OnItemStartDraw) then
    OnItemStartDraw(Sender, AGraphics, Item, ARect);
end;

procedure TCustomItem.DoItemStatusHint(Sender: TObject; Item: TCustomItem;
  var StatusHint: String);
begin
  if Assigned(OnItemStatusHint) then
    OnItemStatusHint(Sender, Item, StatusHint);
end;

procedure TCustomItem.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_SPACE) or (Key = VK_RETURN)) then
  begin
    if State <> isSelected then
    begin
      State := isDown;
    end;
  end;
end;

procedure TCustomItem.DoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_SPACE) or (Key = VK_RETURN)) then
  begin
    if State = isDown then
    begin
      State := isSelected;
    end
    else if State = isSelected then
    begin
      State := isNormal;
    end;

    DoInternalItemClick(Sender, Self);
    DoItemClick(Sender, Self);
  end;
end;

procedure TCustomItem.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
begin
  if Interaction.InteractionItem = Self then
  begin
    case Interaction.InteractionType of
      itDefault:
      begin
        if State <> isSelected then
        begin
          State := isDown;
        end;
      end;
    end;
  end;
end;

procedure TCustomItem.DoMouseMove(Sender: TObject; Shift: TShiftState; pX,
  pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
begin
  if Interaction.InteractionItem = Self then
  begin
    case Interaction.InteractionType of
      itDefault:
      begin
        if (State <> isHovered) and (State <> isSelected) then
          Application.CancelHint;

        ClearItemState;

        if State <> isSelected then
          State := isHovered;
      end;
      itStatus:
      begin
        if (StatusState <> ssHovered) then
          Application.CancelHint;

        ClearItemState;

        StatusState := ssHovered;
      end;
    end;
  end
  else
    ClearItemState;
end;

procedure TCustomItem.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
begin
  if Interaction.InteractionItem = Self then
  begin
    case Interaction.InteractionType of
      itDefault:
      begin
        if State = isDown then
        begin
          State := isSelected;
        end
        else if (State = isSelected) then
        begin
          State := isNormal;
        end;

        DoInternalItemClick(Sender, Self);
        if Button = mbLeft then
          DoItemClick(Sender, Self);
        if Button = mbRight then
          DoItemRightClick(Sender, Self);
      end;
    end;
  end
  else if (Interaction.InteractionItem <> nil) and (Interaction.InteractionType = itDefault) then
  begin
    if (State = isSelected) and Interaction.InteractionItem.Selectable then
      State := isNormal;
  end;
end;

procedure TCustomItem.DoRefresh(Sender: TObject);
begin
  if Assigned(OnRefresh) then
    OnRefresh(Self);
end;

procedure TCustomItem.Draw(g: TGPGraphics; ItemAppearance: TItemAppearance);
begin
  DrawInRect(g, ItemAppearance, MakeRect(x, y, Width, Height));
end;

procedure TCustomItem.DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance;
  R: TGPRectF);
var
  f: TGDIPFill;
begin
  if Visible then
  begin
    DoItemStartDraw(Self, g, Self, r);
    DoInternalItemStartDraw(Self, g, Self, r);

    f := GetFill(ItemAppearance);
    if Assigned(f) then
      f.Fill(g, r);

    DrawStatus(g, r, ItemAppearance);
    if ItemAppearance.Focus and (ItemAppearance.FocusedItem = Index) then
      DrawFocus(g, r, ItemAppearance);

    DoItemEndDraw(Self, g, Self, r);
    DoInternalItemEndDraw(Self, g, Self, r);
  end;
end;

procedure TCustomItem.DrawFocus(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance);
var
  fr: TGPRectF;
  pathfocus: TGPGraphicsPath;
  pfocus: TGPPen;
  f: TGDIPFill;
  smth: Integer;
  r: TGPRectF;
begin
  if ItemFocused then
  begin
    f := GetFill(ItemAppearance);
    if Assigned(f) then
    begin
      r := ARect;
      fr := MakeRect(R.X + 1 + f.BorderWidth, R.Y + 1 + f.BorderWidth, R.Width - 2 - (f.BorderWidth * 2), R.Height - 2 - (f.BorderWidth * 2));
      pathfocus := GDIPFill.CreateRoundRectangle(fr, f.Rounding - f.BorderWidth, f.RoundingType, false);
      smth := g.GetSmoothingMode;
      g.SetSmoothingMode(SmoothingModeDefault);
      pfocus := TGPPen.Create(MakeColor(255, f.FocusColor), 1);
      pfocus.SetDashStyle(DashStyleDot);
      g.DrawPath(pfocus, pathfocus);
      pfocus.Free;
      pathfocus.Free;
      g.SetSmoothingMode(smth);
    end;
  end;
end;

procedure TCustomItem.DrawStatus(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance);
var
  sx, sy: Integer;
  r: TGPRectF;
begin
  if Status.Visible and ((Status.Caption <> '') or not Status.Appearance.Fill.Picture.Empty) then
  begin
    with Status do
    begin
      r := ARect;
      Appearance.CalculateSize(g, Status.Caption);
      sx := Round(r.x + r.Width) + FStatus.OffsetLeft - Status.Appearance.GetWidth;
      sy := Round(r.y) + Status.OffsetTop;
      Appearance.Draw(g, sx, sy, 0, 0, true,Status.Caption);
      FStatusRect := MakeRect(sx, sy, Status.Appearance.GetWidth, Status.Appearance.GetHeight);
    end;
  end;
end;

procedure TCustomItem.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed;
end;

procedure TCustomItem.FillPictureNames(Proc: TGetStrProc);
var
  picsupport: IPictureContainerSupport;
begin
  if Assigned(ItemOwner) then
  begin
    if ItemOwner.GetInterface(IPictureContainerSupport, picsupport) then
      picsupport.FillPictureNames(Proc);
  end;
end;

function TCustomItem.FirstTab(Backwards: Boolean): Boolean;
begin
  Result := False;
end;

function TCustomItem.GetAction: TBasicAction;
begin
  if FActionLink <> nil then
    Result := FActionLink.Action
  else
    Result := nil;
end;

function TCustomItem.GetActionLinkClass: TItemActionLinkClass;
begin
  Result := TItemActionLink;
end;

function TCustomItem.GetClassType: TComponentClass;
begin
  Result := TCustomItem;
end;

function TCustomItem.GetCustomControlOwner(
  AOwner: TComponent): TWinControl;
begin
  Result := nil;
  if AOwner is TWinControl then
  begin
    Result := TWinControl(AOwner);
  end
  else if Assigned(AOwner) then
  begin
    if (AOwner is TCustomItem) then
      Result := GetCustomControlOwner(TCustomItem(AOwner).ItemOwner)
    else
      Result := GetCustomControlOwner(AOwner.Owner);
  end
  else if AOwner = nil then
  begin
    Result := nil;
  end;
end;

function TCustomItem.GetFill(AItemAppearance: TItemAppearance): TGDIPFill;
begin
  Result := nil;
  if Enabled then
  begin
    case State of
      isNormal: result := AItemAppearance.Normal;
      isDown: result := AItemAppearance.Down;
      isHovered: result := AItemAppearance.Hover;
      isSelected: result := AItemAppearance.Selected;
    end;
  end
  else
    result := AItemAppearance.Disabled;
end;

function TCustomItem.GetFont(AItemAppearance: TItemAppearance): TFont;
begin
  Result := nil;
  if Enabled then
  begin
    case State of
      isNormal: result := AItemAppearance.NormalFont;
      isDown: result := AItemAppearance.DownFont;
      isHovered: result := AItemAppearance.HoverFont;
      isSelected: result := AItemAppearance.SelectedFont;
    end;
  end
  else
    result := AItemAppearance.DisabledFont
end;

function TCustomItem.GetItemInteraction(pX, pY: integer): TItemInteractionType;
begin
  Result := itNone;
  if IsStatusAtXY(pX, pY) then
    Result := itStatus
  else if IsItemAtXY(pX, pY) then
    Result := itDefault;
end;

function TCustomItem.GetOwner: TPersistent;
begin
  if FOwner <> nil then
    Result := FOwner
  else
    Result := inherited GetOwner;
end;

function TCustomItem.GetParentComponent: TComponent;
begin
  if ItemOwner <> nil then
    Result := ItemOwner
  else
    Result := inherited GetParentComponent;
end;

function TCustomItem.GetPictures: TPictureContainerList;
var
  picsupport: IPictureContainerSupport;
begin
  Result := nil;
  if Assigned(ItemOwner) then
  begin
    if ItemOwner.GetInterface(IPictureContainerSupport, picsupport) then
    begin
      Result := TPictureContainerList.Create(TPictureContainerListItem);
      picsupport.GetPictures(Result);
    end;
  end;
end;

function TCustomItem.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TCustomItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(IBLD_VER,IREL_VER),MakeWord(IMIN_VER,IMAJ_VER));
end;

function TCustomItem.HasParent: Boolean;
begin
  if ItemOwner <> nil then
    Result := True else
    Result := inherited HasParent;
end;

procedure TCustomItem.HideShortCutHint;
begin
  if Assigned(FShortCutHint) then
  begin
    FShortCutHint.Visible := false;
  end;
end;

procedure TCustomItem.InitDesignTime;
begin
//
end;

function TCustomItem.IsFocusable: Boolean;
begin
  Result := True;
end;

function TCustomItem.IsItemAtXY(pX, pY: integer): Boolean;
begin
  Result := PtInRect(Bounds(X, Y, Width, Height), Point(pX, pY));
end;

function TCustomItem.IsSection: boolean;
begin
  Result := false;
end;

function TCustomItem.IsStatusAtXY(pX, pY: integer): Boolean;
begin
  Result := false;
  if Status.Visible and ((Status.Caption <> '') or not Status.Appearance.Fill.Picture.Empty) then
    Result := PtInGPRect(Status.FStatusRect, Point(pX, pY));
end;

procedure TCustomItem.Loaded;
begin
  inherited;
end;

procedure TCustomItem.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if (AOperation = opRemove) and (AComponent = Control) then
    Control := nil;
end;

function TCustomItem.ProcessTab(Backwards: Boolean): Boolean;
begin
  Result := True;
end;

procedure TCustomItem.ReadState(Reader: TReader);
var
  gdipbaseif: IGDIPBase;
begin
  inherited ReadState(Reader);
  if Reader.Parent.GetInterface(IGDIPBase, gdipbaseif) then
    gdipbaseif.ReadItemState(reader, Self);
end;

procedure TCustomItem.RefreshObject;
begin
  DoRefresh(Self);
  DoInternalRefresh(Self);
end;

procedure TCustomItem.ResetTab;
begin
  ItemFocused := True;
  FControlFocused := False;
end;

function TCustomItem.UseButtonCaptionForAction: Boolean;
begin
  Result := False;
end;

procedure TCustomItem.UseCaption(ACaption: String);
begin
//
end;

procedure TCustomItem.SetAction(const Value: TBasicAction);
begin
  if Value = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
  end
  else
  begin
    if FActionLink = nil then
      FActionLink := GetActionLinkClass.Create(Self);
    FActionLink.Action := Value;
    FActionLink.OnChange := DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
//      Value.FreeNotification(TAdvPreviewSubMenuItems(Collection).FPreviewMenu);
  end;
end;

procedure TCustomItem.SetColumnSpan(const Value: Integer);
begin
  if FColumnSpan <> Value then
  begin
    FColumnSpan := Value;
    Changed;
  end;
end;

procedure TCustomItem.SetControl(const Value: TWinControl);
begin
  if (Value = nil) and Assigned(FControl) then
    FControl.Visible := true;

  FControl := Value;
  if Assigned(FControl) and Assigned(ItemOwner) then
  begin
    if ItemOwner is TWinControl then
    begin
      FControl.Parent := TWinControl(ItemOwner);
      Changed;
    end
    else if ItemOwner is TCustomItem then
    begin
      FControl.Visible := false;
      Changed;
    end;
  end;
end;

procedure TCustomItem.SetControlLocation(const Value: TItemLocation);
begin
  if FControlLocation <> Value then
  begin
    FControlLocation := Value;
    Changed;
  end;
end;

procedure TCustomItem.SetControlMargin(const Value: TMargins);
begin
  if FControlMargin <> Value then
  begin
    FControlMargin.Assign(Value);
    Changed;
  end;
end;

procedure TCustomItem.SetControlStretched(const Value: Boolean);
begin
  if FControlStretched <> Value then
  begin
    FControlStretched := Value;
    Changed;
  end;
end;

procedure TCustomItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TCustomItem.SetHeight(const Value: integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TCustomItem.SetHint(const Value: String);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TCustomItem.SetIndex(const Value: Integer);
var
  old: integer;
begin
  if FIndex <> Value then
  begin
    old := FIndex;
    FIndex := Value;
    DoInternalIndexChange(Self, Self, old, Value);
    Changed;
  end;
end;

procedure TCustomItem.SetItemFocused(const Value: Boolean);
begin
  if FItemFocused <> Value then
  begin
    FItemFocused := Value;
  end;
end;

procedure TCustomItem.SetParentComponent(AParent: TComponent);
var
  gdipbaseif: IGDIPBase;
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    if AParent.GetInterface(IGDIPBase, gdipbaseif) then
      gdipbaseif.SetItemParentComponent(AParent, Self)
  end;
end;

procedure TCustomItem.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Changed;
  end;
end;

procedure TCustomItem.SetSelectable(const Value: Boolean);
begin
  if FSelectable <> Value then
  begin
    FSelectable := Value;
    FState := isNormal;
    Changed;
  end;
end;

procedure TCustomItem.SetShortCutHintPos(const Value: TItemShortCutHintPos);
begin
  if FShortCutHintPos <> Value then
  begin
    FShortCutHintPos := Value;
    Changed;
  end;
end;

procedure TCustomItem.SetShortCutHintText(const Value: string);
begin
  if FShortCutHintText <> Value then
  begin
    FShortCutHintText := Value;
    Changed;
  end;
end;

procedure TCustomItem.SetState(const Value: TItemState);
var
  Allow: Boolean;
begin
  if (FState <> Value) then
  begin
    Allow := true;
    if (Value = isSelected) and Selectable then
    begin
      DoItemSelect(Self, Self, Allow);
      DoInternalItemSelect(Self, Self, Allow);
    end
    else if (FState = isSelected) and Selectable then
    begin
      DoItemDeSelect(Self, Self, Allow);
      DoInternalItemDeSelect(Self, Self, Allow);
    end;

    if ((Selectable and (Value = isSelected)) or (Value <> isSelected)) and Allow then
      FState := Value;

    RefreshObject;
  end;
end;

procedure TCustomItem.SetStatus(const Value: TItemStatus);
begin
  if FStatus <> value then
  begin
    FStatus.Assign(Value);
    Changed;
  end;
end;

procedure TCustomItem.SetStatusState(const Value: TStatusState);
begin
  if FStatusState <> Value then
  begin
    FStatusState := Value;
    RefreshObject;
  end;
end;

procedure TCustomItem.SetTabStop(const Value: Boolean);
begin
  if FTabStop <> Value then
  begin
    FTabStop := Value;
    Changed;
  end;
end;

procedure TCustomItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    if Assigned(Control) then
      Control.Visible := Value;
    Changed;
  end;
end;

procedure TCustomItem.SetWidth(const Value: integer);
begin
  if FWidth <> value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

procedure TCustomItem.SetX(const Value: integer);
begin
  if FX <> value then
  begin
    FX := Value;
    Changed;
  end;
end;

procedure TCustomItem.SetY(const Value: integer);
begin
  if FY <> value then
  begin
    FY := Value;
    Changed;
  end;
end;

procedure TCustomItem.ShowShortCutHint;
var
  pt: TPoint;
  SCHintPos: TItemShortCutHintPos;
  OffsetX: Integer;
  co: TWinControl;
begin
  co := GetCustomControlOwner(ItemOwner);
  if not Assigned(co) then
    Exit;

  if not Assigned(FShortCutHint) then
  begin
    FShortCutHint := TShortCutHintWindow.Create(Self);
    FShortCutHint.Parent := co;
    FShortCutHint.Visible := False;
    FShortCutHint.Color := clWhite;
    FShortCutHint.ColorTo := clWhite;
  end;

  FShortCutHint.Caption := FShortCutHintText;

  pt := co.ClientToScreen(Point(X,Y));

  OffsetX := 6;
  SCHintPos := ShortCutHintPos;

  if Assigned(FOnGetShortCutHintPos) then
    FOnGetShortCutHintPos(Self, SCHintPos);

  if (SCHintPos = ishpAuto) then
    SCHintPos := ishpTop;

  case SCHintPos of
  ishpLeft:
    begin
      //FShortCutHint.Left := pt.X - (FShortCutHint.Width div 2);
      FShortCutHint.Left := pt.X + OffsetX;
      FShortCutHint.Top := pt.Y + (self.Height - FShortCutHint.Height) div 2;
    end;
  ishpTop:
    begin
      FShortCutHint.Left := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y - (FShortCutHint.Height div 2);
    end;
  ishpRight:
    begin
      FShortCutHint.Left := pt.X + self.Width - (FShortCutHint.Width div 2);
      FShortCutHint.Top := pt.Y + (self.Height - FShortCutHint.Height) div 2;
    end;
  ishpBottom:
    begin
      FShortCutHint.Left := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y + self.Height - (FShortCutHint.Height div 2);
    end;
  ishpCenter:
    begin
      FShortCutHint.Left  := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y + (self.Height - FShortCutHint.Height) div 2;
    end;
  ishpTopLeft:
    begin
      FShortCutHint.Left := pt.X + OffsetX;
      FShortCutHint.Top := pt.Y - (FShortCutHint.Height div 2);
    end;
  ishpTopRight:
    begin
      FShortCutHint.Left := pt.X + self.Width - FShortCutHint.Width + 1;
      FShortCutHint.Top := pt.Y - (FShortCutHint.Height div 2);
    end;
  ishpAboveTop:
    begin
      FShortCutHint.Left := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y - FShortCutHint.Height;
    end;
  ishpAboveTopLeft:
    begin
      FShortCutHint.Left := pt.X + OffsetX;
      FShortCutHint.Top := pt.Y - FShortCutHint.Height;
    end;
  ishpAboveTopRight:
    begin
      FShortCutHint.Left := pt.X + self.Width - FShortCutHint.Width + 1;
      FShortCutHint.Top := pt.Y - FShortCutHint.Height;
    end;
  ishpBottomLeft:
    begin
      FShortCutHint.Left := pt.X + OffsetX;
      FShortCutHint.Top := pt.Y + self.Height - (FShortCutHint.Height div 2);
    end;
  ishpBottomRight:
    begin
      FShortCutHint.Left := pt.X + self.Width - FShortCutHint.Width + 1;
      FShortCutHint.Top := pt.Y + self.Height - (FShortCutHint.Height div 2);
    end;
  ishpBelowBottom:
    begin
      FShortCutHint.Left := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y + self.Height;
    end;
  ishpBelowBottomLeft:
    begin
      FShortCutHint.Left := pt.X + OffsetX;
      FShortCutHint.Top := pt.Y + self.Height
    end;
  ishpBelowBottomRight:
    begin
      FShortCutHint.Left := pt.X + self.Width - FShortCutHint.Width + 1;
      FShortCutHint.Top := pt.Y + self.Height
    end;
  ishpBelowBottomCenter:
    begin
      FShortCutHint.Left  := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y + self.Height
    end;
  end;

  FShortCutHint.Visible := true;
end;

procedure TCustomItem.StatusChanged(Sender: TObject);
begin
  Changed;
end;

{ TItemStatus }

procedure TItemStatus.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TItemStatus.Assign(Source: TPersistent);
begin
  if (Source is TItemStatus) then
  begin
    FAppearance.Assign((Source as TItemStatus).Appearance);
    FOffsetTop := (Source as TItemStatus).OffsetTop;
    FOffsetLeft := (Source as TItemStatus).OffsetLeft;
    FVisible := (Source as TItemStatus).Visible;
    FCaption := (Source as TItemStatus).Caption;
    Fhint := (Source as TItemStatus).Hint;
    FPosition := (Source as TItemStatus).Position;
  end;
end;

procedure TItemStatus.Changed;
begin
  FOwner.Changed;
end;

constructor TItemStatus.Create(AOwner: TCustomItem);
begin
  FOwner := AOwner;
  FVisible := False;
  FAppearance := TGDIPStatus.Create;
  FAppearance.OnChange := AppearanceChanged;
  FAppearance.Fill.Color := clRed;
  FAppearance.Fill.GradientType := gtSolid;
  FAppearance.Fill.BorderColor := clGray;
  FAppearance.Font.Color := clWhite;
  FOffsetTop := 14;
  FOffsetLeft := -6;
  if (csDesigning in FOwner.ComponentState) and not (csLoading in FOwner.ComponentState) then
  begin
    FCaption := '0';
    FHint := 'Status Hint';
  end;

  FPosition := spItemText;
end;

destructor TItemStatus.Destroy;
begin
  FAppearance.Free;
  inherited;
end;

procedure TItemStatus.SetAppearance(const Value: TGDIPStatus);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    AppearanceChanged(Self);
  end;
end;

procedure TItemStatus.SetCaption(const Value: String);
begin
  if FCaption <> value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TItemStatus.SetHint(const Value: String);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TItemStatus.SetOffsetLeft(const Value: integer);
begin
  if FOffsetLeft <> value then
  begin
    FOffsetLeft := Value;
    Changed;
  end;
end;

procedure TItemStatus.SetOffsetTop(const Value: integer);
begin
  if FOffsetTop <> value then
  begin
    FOffsetTop := Value;
    Changed;
  end;
end;

procedure TItemStatus.SetPosition(const Value: TItemStatusPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TItemStatus.SetVisible(const Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    Changed;
  end;
end;


{$IFNDEF DELPHI2006_LVL}
{ TMargins }

procedure TMargins.Assign(Source: TPersistent);
begin
  if Source is TMargins then
  begin
    FTop := (Source as TMargins).Top;
    FBottom := (Source as TMargins).Bottom;
    FRight := (Source as TMargins).Right;
    FLeft := (Source as TMargins).Left;
    Changed;
  end;
end;

procedure TMargins.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TMargins.Create(AOwner: TCustomItem);
begin
  FLeft := 3;
  FTop := 3;
  FBottom := 3;
  FRight := 3;
end;

destructor TMargins.Destroy;
begin

  inherited;
end;

procedure TMargins.SetBottom(const Value: integer);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    Changed;
  end;
end;

procedure TMargins.SetLeft(const Value: integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TMargins.SetRight(const Value: integer);
begin
  if FRight <> Value then
  begin
    FRight := Value;
    Changed;
  end;
end;

procedure TMargins.SetTop(const Value: integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    Changed;
  end;
end;

{$ENDIF}

{ TPictureContainerList }

function TPictureContainerList.Add: TPictureContainerListItem;
begin
  Result := TPictureContainerListItem(inherited Add);
end;

{ TShortCutHintWindow }

procedure TShortCutHintWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited;
  Params.Style := Params.Style and not WS_BORDER;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
     ((Win32MajorVersion > 5) or
      ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
        if Params.WindowClass.Style and CS_DROPSHADOW <> 0 then
          Params.WindowClass.Style := Params.WindowClass.Style - CS_DROPSHADOW;
end;

procedure TShortCutHintWindow.Paint;
var
  r: TRect;
begin
  r := ClientRect;
  DrawGradient(Canvas, Color, ColorTo, 16, r, false);
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(self.Font);

  DrawText(Canvas.Handle,PChar(Caption),Length(Caption),r, DT_CENTER or DT_SINGLELINE or DT_VCENTER);

  Canvas.Pen.Color := clGray;
  RoundRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom, 3,3);
end;


procedure TShortCutHintWindow.Resize;
var
  ow: integer;
begin
  inherited;
  ow := Canvas.TextWidth('O') + 8;
  if Width < ow then
    Width := ow;
end;

procedure TShortCutHintWindow.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TItemActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TCustomItem;
end;

function TItemActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and FClient.ActionIsCaptionLinked;
end;

//------------------------------------------------------------------------------

function TItemActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and FClient.ActionIsCheckedLinked;
end;

//------------------------------------------------------------------------------

function TItemActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and FClient.ActionIsEnabledLinked;
end;

//------------------------------------------------------------------------------

function TItemActionLink.IsHelpContextLinked: Boolean;
begin
  Result := inherited IsHelpContextLinked and FClient.ActionIsHelpContextLinked;
end;

//------------------------------------------------------------------------------

function TItemActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and FClient.ActionIsHintLinked;
end;

//------------------------------------------------------------------------------

function TItemActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := inherited IsGroupIndexLinked and FClient.ActionIsGroupIndexLinked;
end;
//------------------------------------------------------------------------------

function TItemActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and FClient.ActionIsImageIndexLinked;
end;

//------------------------------------------------------------------------------

function TItemActionLink.IsShortCutLinked: Boolean;
begin
  Result := inherited IsShortCutLinked and FClient.ActionIsShortCutLinked;
end;

//------------------------------------------------------------------------------

function TItemActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and FClient.ActionIsVisibleLinked;
end;

//------------------------------------------------------------------------------

procedure TItemActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then
    FClient.ActionSetCaption(Value);
end;

procedure TItemActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    FClient.ActionSetChecked(Value);
end;

//------------------------------------------------------------------------------

procedure TItemActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
    FClient.ActionSetEnabled(Value);
end;

procedure TItemActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then
    FClient.ActionSetGroupIndex(Value);
end;

procedure TItemActionLink.SetHint(const Value: String);
begin
  if IsHintLinked then
    FClient.ActionSetHint(Value);
end;

//------------------------------------------------------------------------------

procedure TItemActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    FClient.ActionSetImageIndex(Value);
end;

//------------------------------------------------------------------------------

procedure TItemActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then
    FClient.ActionSetVisible(Value);
end;

//------------------------------------------------------------------------------

end.
