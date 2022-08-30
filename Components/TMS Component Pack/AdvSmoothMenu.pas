{**************************************************************************}
{ TAdvSmoothMenu component                                                 }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2015                                                       }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvSmoothMenu;

interface

{$I TMSDEFS.INC}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  ExtCtrls, Math, GdipFill, GDIPPictureContainer, AdvStyleIF,
  AdvGDIP, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : New : keyboard support
  // v1.0.1.1 : Improved : hover color removed on mouseleave
  // v1.0.1.2 : Fixed : ItemRectangle always calculated with ItemAppearance.Font in hover and selected state
  // v1.0.2.0 : New : SelectedItemIndex property
  //          : New : Exposed Event OnItemChange
  // v1.0.2.1 : Fixed : Issue with component initialization during reparenting
  // v1.1.0.0 : New : Focus indication
  //          : New : property GlowShape
  //          : New : property Enabled to disable an item
  // v1.1.0.1 : Fixed : Issue with caption rectangle with different fonts
  // v1.1.0.2 : Fixed : Issue with Mouse events
  //          : Fixed : Issue with MouseUp not calling after Application.Minimize
  // v1.1.1.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.1.2.0 : New : Built-in support for reduced color set for use with terminal servers
  // v1.1.2.1 : Fixed : Issue with initializing item notes and caption
  // v1.1.3.0 : New : Delphi 2010 Touch Support
  // v1.1.4.0 : Fixed : Issue with Index out of bounds
  //          : New : Property AllowSelectedIndication to allow selected items to be indicated when rotating.
  // v1.1.4.1 : Fixed : Issue with selected index when disabling items at runtime
  // v1.1.4.2 : Fixed : Issue with enabled items count
  // v1.1.5.0 : New : Built-in support for Office 2010 colors
  // v1.1.5.1 : Fixed : Issue with mouseenter / mouseleave and hovered items
  // v1.2.0.0 : New : Metro style support
  // v1.3.0.0 : New : Windows 8, Office 2013 styles added
  // v1.3.0.1 : Fixed : Issue with drawing hovered style after programmatically selecting item
  // v1.4.0.0 : New : Windows 10, Office 2016 styles added

type

  TAdvSmoothMenuLocation = (plTopLeft, plTopCenter, plTopRight, plCenterLeft, plCenterCenter, plCenterRight, plBottomLeft, plBottomCenter, plBottomRight, plCustom);

  TAdvSmoothMenu = class;

  TWinCtrl = class(TWinControl)
  public                 
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothMenuItem = class(TCollectionItem)
  private
    FItemRect, FCaptionRect: TGPRectF;
    FCaption: String;
    FNotesTop: integer;
    FNotesLeft: integer;
    FNotes: String;
    FNotesLocation: TAdvSmoothMenuLocation;
    FPicturePosition: TFillPicturePosition;
    FPicture: TAdvGDIPPicture;
    FpictureWidth: integer;
    FpictureSize: TFillPictureSize;
    FPictureTop: integer;
    FpictureHeight: integer;
    FPictureLeft: integer;
    FObject: TObject;
    FTag: integer;
    FEnabled: Boolean;
    procedure SetCaption(const Value: String);
    procedure SetNotes(const Value: String);
    procedure SetNotesLeft(const Value: integer);
    procedure SetNotesLocation(const Value: TAdvSmoothMenuLocation);
    procedure SetNotesTop(const Value: integer);
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure SetPictureHeight(const Value: integer);
    procedure SetPictureLeft(const Value: integer);
    procedure SetPicturePosition(const Value: TFillPicturePosition);
    procedure SetPictureSize(const Value: TFillPictureSize);
    procedure SetPictureTop(const Value: integer);
    procedure SetPictureWidth(const Value: integer);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure Draw;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ItemObject: TObject read FObject write FObject;    
  published
    property Caption: String read FCaption write SetCaption;
    property Notes: String read FNotes write SetNotes;
    property NotesLocation: TAdvSmoothMenuLocation read FNotesLocation write SetNotesLocation default plTopLeft;
    property NotesLeft: integer read FNotesLeft write SetNotesLeft default 0;
    property NotesTop: integer read FNotesTop write SetNotesTop default 0;
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
    property PicturePosition: TFillPicturePosition read FPicturePosition write SetPicturePosition default ppTopLeft;
    property PictureLeft: integer read FPictureLeft write SetPictureLeft default 0;
    property PictureTop: integer read FPictureTop write SetPictureTop default 0;
    property PictureSize: TFillPictureSize read FpictureSize write SetPictureSize default psOriginal;
    property PictureWidth: integer read FpictureWidth write SetPictureWidth default 50;
    property PictureHeight: integer read FpictureHeight write SetPictureHeight default 50;
    property Tag: integer read FTag write FTag;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
  end;

  TAdvSmoothMenuItems = class(TCollection)
  private
    FOwner: TAdvSmoothMenu;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothMenuItem;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothMenuItem);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothMenu);
    property Items[Index: Integer]: TAdvSmoothMenuItem read GetItem write SetItem; default;
    function Add: TAdvSmoothMenuItem;
    function Insert(Index: Integer): TAdvSmoothMenuItem;
    procedure Delete(Index: Integer);
  end;

  TAdvSmoothMenuSplitter = class(TPersistent)
  private
    FOwner: TAdvSmoothMenu;
    FOnChange: TNotifyEvent;
    FOpacity: Byte;
    FOpacityTo: Byte;
    FColor: TColor;
    FColorTo: TColor;
    FVisible: Boolean;
    FHeight: Integer;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetOpacity(const Value: Byte);
    procedure SetOpacityTo(const Value: Byte);
    procedure SetVisible(const Value: Boolean);
    procedure SetHeight(const Value: Integer);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Color: TColor read FColor write SetColor default clWhite;
    property ColorTo: TColor read FColorTo write SetColorTo default clWhite;
    property Opacity: Byte read FOpacity write SetOpacity default 20;
    property OpacityTo: Byte read FOpacityTo write SetOpacityTo default 255;
    property Visible: Boolean read FVisible write SetVisible default true;
    property Height: Integer read FHeight write SetHeight default 28;
  end;

  TAdvSmoothMenuGlowShape = (gsEllipse, gsSquare, gsDiamond);

  TAdvSmoothMenuItemAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothMenu;
    FOnChange: TNotifyEvent;
    FFont: TFont;
    Fsplitter: TAdvSmoothMenuSplitter;
    FSelectedFont: TFont;
    FHoverFont: TFont;
    FGlowColorHover: TColor;
    FGlowOpacitySelected: Byte;
    FGlowOpacity: Byte;
    FGlowOpacityHover: Byte;
    FGlowColorSelected: TColor;
    FGlowColor: Tcolor;
    FNotesFont: TFont;
    FGlowColorDisabled: TColor;
    FGlowOpacityDisabled: Byte;
    FGlowShape: TAdvSmoothMenuGlowShape;
    procedure SetFont(const Value: TFont);
    procedure SetSplitter(const Value: TAdvSmoothMenuSplitter);
    procedure SetHoverFont(const Value: TFont);
    procedure SetSelectedFont(const Value: TFont);
    procedure SetGlowColor(const Value: TColor);
    procedure SetGlowColorHover(const Value: TColor);
    procedure SetGlowColorSelected(const Value: TColor);
    procedure SetGlowOpacity(const Value: Byte);
    procedure SetGlowOpacityHover(const Value: Byte);
    procedure SetGlowOpacitySelected(const Value: Byte);
    procedure SetNotesFont(const Value: TFont);
    procedure SetGlowColorDisabled(const Value: TColor);
    procedure SetGlowOpacityDisabled(const Value: Byte);
    procedure SetGlowShape(const Value: TAdvSmoothMenuGlowShape);
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure SplitterChanged(Sender: TObject);    
  public
    constructor Create(AOwner: TAdvSmoothMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property HoverFont: TFont read FHoverFont write SetHoverFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property Font: TFont read FFont write SetFont;
    property NotesFont: TFont read FNotesFont write SetNotesFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Splitter: TAdvSmoothMenuSplitter read Fsplitter write SetSplitter;
    property GlowShape: TAdvSmoothMenuGlowShape read FGlowShape write SetGlowShape default gsEllipse;
    property GlowColor: Tcolor read FGlowColor write SetGlowColor default clWhite;
    property GlowOpacity: Byte read FGlowOpacity write SetGlowOpacity default 100;
    property GlowColorSelected: TColor read FGlowColorSelected write SetGlowColorSelected default clWhite;
    property GlowColorDisabled: TColor read FGlowColorDisabled write SetGlowColorDisabled default clNone;
    property GlowOpacityDisabled: Byte read FGlowOpacityDisabled write SetGlowOpacityDisabled default 100;    
    property GlowOpacitySelected: Byte read FGlowOpacitySelected write SetGlowOpacitySelected default 100;
    property GlowColorHover: TColor read FGlowColorHover write SetGlowColorHover default clWhite;
    property GlowOpacityHover: Byte read FGlowOpacityHover write SetGlowOpacityHover default 100;
  end;

  TAdvSmoothMenuAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothMenu;
    FOnChange: TNotifyEvent;
    FFill: TGDIPFill;
    FLineColor: TColor;
    FLineOpacity: Byte;
    FLineVisible: Boolean;
    FIndicatorColor: TColor;
    FIndicatorVisible: Boolean;
    FIndicatorOpacity: Byte;
    FIndicatorSize: Integer;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetLineColor(const Value: TColor);
    procedure SetLineOpacity(const Value: Byte);
    procedure SetLineVisible(const Value: Boolean);
    procedure SetIndicatorColor(const Value: TColor);
    procedure SetIndicatorOpacity(const Value: Byte);
    procedure SetIndicatorVisible(const Value: Boolean);
    procedure SetIndicatorSize(const Value: Integer);
  protected
    procedure Changed;
    procedure BackGroundChanged(Sender: TObject);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TAdvSmoothMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Fill: TGDIPFill read FFill write SetFill;
    property LineColor: TColor read FLineColor write SetLineColor default clWhite;
    property LineOpacity: Byte read FLineOpacity write SetLineOpacity default 255;
    property LineVisible: Boolean read FLineVisible write SetLineVisible default true;
    property IndicatorColor: TColor read FIndicatorColor write SetIndicatorColor default clWhite;
    property IndicatorOpacity: Byte read FIndicatorOpacity write SetIndicatorOpacity default 255;
    property IndicatorVisible: Boolean read FIndicatorVisible write SetIndicatorVisible default true;
    property IndicatorSize: Integer read FIndicatorSize write SetIndicatorSize default 6;
  end;

  TAdvSmoothMenuItemClickEvent = procedure(Sender: TObject; ItemIndex: integer) of object;

  TAdvSmoothMenuItemEnterEvent = procedure(Sender: TObject; ItemIndex: integer) of object;

  TAdvSmoothMenuItemLeaveEvent = procedure(Sender: TObject; ItemIndex: integer) of object;

  TAdvSmoothMenuItemChangeEvent = procedure(Sender: TObject; OldItemIndex, NewItemIndex: integer) of object;

  TAdvSmoothMenuAnimationType = (atNone, atNoOpacity, atFull);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothMenu = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FFocused: Boolean;
    FConstructed, FDesignTime, FMouseInControl, MouseLeave, MouseEnter: Boolean;
    FHoveredItem, FSelectedItem: Integer;
    FAnimating: Boolean;
    FCurrentPos, FPosTo, FCurrentTextPos: Single;
    FCurrentTextOpc: Byte;
    FSmoothTimer: TTimer;
    FRotationTimer: TTimer;
    FItems: TAdvSmoothMenuItems;
    FItemAppearance: TAdvSmoothMenuItemAppearance;
    FTransparent: Boolean;
    FAppearance: TAdvSmoothMenuAppearance;
    FContainer: TGDIPPictureContainer;
    FOnItemLeave: TAdvSmoothMenuItemLeaveEvent;
    FOnItemEnter: TAdvSmoothMenuItemEnterEvent;
    FOnItemClick: TAdvSmoothMenuItemClickEvent;
    FRotation: Boolean;
    FRotationInterval: Integer;
    FAnimationType: TAdvSmoothMenuAnimationType;
    FOnItemChange: TAdvSmoothMenuItemChangeEvent;
    FAllowSelectedIndication: Boolean;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetItems(const Value: TAdvSmoothMenuItems);
    procedure SetItemAppearance(const Value: TAdvSmoothMenuItemAppearance);
    procedure SetTransparent(const Value: Boolean);
    procedure SetAppearance(const Value: TAdvSmoothMenuAppearance);
    procedure SetRotation(const Value: Boolean);
    procedure SetRotationInterval(const Value: Integer);
    procedure SetAnimationType(const Value: TAdvSmoothMenuAnimationType);
    function GetVersion: String;
    procedure SetVersion(const Value: String);
    procedure SetSelectedItemIndex(const Value: integer);
    procedure SetAllowSelectedIndication(const Value: Boolean);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure ItemAppearanceChanged(Sender: TObject);
    procedure AppearanceChanged(Sender: TObject);
    procedure ItemsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure Changed;
    procedure DrawLine;
    procedure DrawBackGround;
    procedure DrawItems;
    procedure DrawIndicator;
    procedure InitItemRects;
    procedure AnimateMenu(Sender: TObject);
    procedure RotateMenu(Sender: TObject);
    procedure DoItemClick(Sender: TObject; ItemIndex: integer);
    procedure DoItemEnter(Sender: TObject; ItemIndex: integer);
    procedure DoItemLeave(Sender: TObject; ItemIndex: integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;    
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Resize; override;
    procedure DrawPicture(g: TGPGraphics; opacity: Byte; picture: TAdvGDIPPicture; location: TFillPicturePosition; w, h: integer; r: TRect; x, y: integer);
    procedure InitPreview;
    procedure CreateWnd; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure ChangeSelectedItem;
    procedure DoRotate;
    function GetVersionNr: integer;
    function GetShadowOffset: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    procedure GetTextPosition(var x, y: integer; rectangle: TGPRectF; objectwidth, objectheight: integer; location: TAdvSmoothMenuLocation);
    function XYToItem(X, Y: integer): integer;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
  published
    property AnimationType: TAdvSmoothMenuAnimationType read FAnimationType write SetAnimationType default atFull;
    property Anchors;
    property Constraints;
    property Rotation: Boolean read FRotation write SetRotation default true;
    property RotationInterval: Integer read FRotationInterval write SetRotationInterval default 5000;
    property Appearance: TAdvSmoothMenuAppearance read FAppearance write SetAppearance;
    property Items: TAdvSmoothMenuItems read FItems write SetItems;
    property ItemAppearance: TAdvSmoothMenuItemAppearance read FItemAppearance write SetItemAppearance;
    property TabStop default true;
    property TabOrder;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property Container: TGDIPPictureContainer read FContainer write FContainer;
    property Version: String read GetVersion write SetVersion;
    property SelectedItemIndex: integer read FSelectedItem write SetSelectedItemIndex;
    property AllowSelectedIndication: Boolean read FAllowSelectedIndication write SetAllowSelectedIndication default false;

    property OnItemClick: TAdvSmoothMenuItemClickEvent read FOnItemClick write FOnItemClick;
    property OnItemEnter: TAdvSmoothMenuItemEnterEvent read FOnItemEnter write FOnItemEnter;
    property OnItemLeave: TAdvSmoothMenuItemLeaveEvent read FOnItemLeave write FOnItemLeave;
    property OnItemChange: TAdvSmoothMenuItemChangeEvent read FOnItemChange write FOnItemChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property Align;
    property ShowHint;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property OnStartDock;
    property OnStartDrag;
    property PopupMenu;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
  end;

implementation

{$IFNDEF DELPHI7_LVL}

const
  CS_DROPSHADOW = $00020000;

{$ENDIF}

procedure DrawFocus(g: TGPGraphics; r: TGPRectF; rn: Integer);
var
  pathfocus: TGPGraphicsPath;
  pfocus: TGPPen;
begin
  pathfocus := GDIPFill.CreateRoundRectangle(r, rn, rtBoth, false);
  pfocus := TGPPen.Create(MakeColor(255, clBlack), 1);
  pfocus.SetDashStyle(DashStyleDot);
  g.DrawPath(pfocus, pathfocus);
  pfocus.Free;
  pathfocus.Free;
end;

procedure TAdvSmoothMenu.DrawPicture(g: TGPGraphics; opacity: Byte; picture: TAdvGDIPPicture; location: TFillPicturePosition; w, h: integer; r: TRect; x, y: integer);
{*
var
  gpimg: TGPImage;
  sta: TStreamAdapter;
  st: TStream;
  ia: TGPImageAttributes;
  cm: TColorMatrix;
  i, k: integer;
  o: Single;
  imgr: TRect;
  *}
begin
//  if csDesigning in ComponentState then
//  begin
    if not picture.Empty then
    begin
      case location of
      ppTopLeft: picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Left + w, r.Top + h));
      ppTopCenter:
      begin
        r.Left := r.Left + ((r.Right - r.Left) - w) div 2;
        picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Left + w, r.Top + h));
      end;
      ppTopRight: picture.GDIPDraw(g, Rect(r.Right - w, r.Top, r.Right, r.Top + h));
      ppBottomLeft: picture.GDIPDraw(g, Rect(r.Left, r.Bottom - h, r.Left + w, r.Bottom));
      ppBottomCenter:
      begin
        r.Left := r.Left + ((r.Right - r.Left) - w) div 2;
        picture.GDIPDraw(g, Rect(r.Left, r.Bottom - h, r.Left + w, r.Bottom));
      end;
      ppBottomRight: picture.GDIPDraw(g, Rect(r.Right - w, r.Bottom - h, r.Right, r.Bottom));
      ppStretched: picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Right, r.Bottom));
      ppCustom: picture.GDIPDraw(g, Bounds(x, y, w, h));
      ppCenterLeft:
        begin
          r.Top := r.Top + ((r.Bottom - r.Top) - h) div 2;
          picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Left + w, r.Top + h));
        end;
      ppCenterRight:
        begin
          r.Top := r.Top + ((r.Bottom - r.Top) - h) div 2;
          picture.GDIPDraw(g, Rect(r.Right - w,r.Top,r.Right, r.Top + h));
        end;
      ppCenterCenter:
        begin
          r.Left := r.Left + ((r.Right - r.Left) - w) div 2;
          r.Top := r.Top + ((r.Bottom - r.Top) - h) div 2;
          picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Left + w, r.Top + h));
        end;
      end;
    end;
//  end;
  {*
  else
  begin
    if (not picture.Empty) then
    begin
      for I := 0 to 4 do
        for K := 0 to 4 do
          cm[i, k] := 0.0;


      o := opacity / 255;
      cm[0, 0] := 1.0;
      cm[1, 1] := 1.0;
      cm[2, 2] := 1.0;
      cm[3, 3] := o; //OPACITY
      cm[4, 4] := 1.0;


      st := TMemoryStream.Create;
      Picture.SaveToStream(st);
      sta := TStreamAdapter.Create(st);
      gpimg := TGPImage.Create(sta);
      ia := TGPImageAttributes.Create;
      ia.SetColorMatrix(cm);

      case location of
      ppTopLeft: imgr := Rect(r.Left,r.Top,r.Left + w, r.Top + h);
      ppTopCenter:
      begin
        r.Left := r.Left + ((r.Right - r.Left) - w) div 2;
        imgr := Rect(r.Left,r.Top,r.Left + w, r.Top + h);
      end;
      ppTopRight: imgr := Rect(r.Right - w, r.Top, r.Right, r.Top + h);
      ppBottomLeft: imgr := Rect(r.Left, r.Bottom - h, r.Left + w, r.Bottom);
      ppBottomCenter:
      begin
        r.Left := r.Left + ((r.Right - r.Left) - w) div 2;
        imgr := Rect(r.Left, r.Bottom - h, r.Left + w, r.Bottom);
      end;
      ppBottomRight: imgr := Rect(r.Right - w, r.Bottom - h, r.Right, r.Bottom);
      ppStretched: imgr := Rect(r.Left,r.Top,r.Right, r.Bottom);
      ppCustom: imgr := Bounds(x, y, w, h);
      ppCenterCenter:
        begin
          r.Left := r.Left + ((r.Right - r.Left) - w) div 2;
          r.Top := r.Top + ((r.Bottom - r.Top) - h) div 2;
          imgr := Rect(r.Left,r.Top,r.Left + w, r.Top + h);
        end;
      ppCenterLeft:
        begin
          r.Top := r.Top + ((r.Bottom - r.Top) - h) div 2;
          imgr := Rect(r.Left,r.Top,r.Left + w, r.Top + h);
        end;
      ppCenterRight:
        begin
          r.Top := r.Top + ((r.Bottom - r.Top) - h) div 2;
          imgr := Rect(r.Right - w,r.Top,r.Right, r.Top + h);
        end;
      end;

      g.DrawImage(gpimg,MakeRect(imgr.Left, imgr.Top, imgr.Right - imgr.Left, imgr.Bottom - imgr.Top), 0, 0, picture.Width, picture.Height, UnitPixel, ia);

      ia.Free;
      st.Free;
      gpimg.Free;
    end;
  end;
  *}
end;

function AnimateDouble(var Start: Single; Stop, Delta, Margin: Single): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(1, Delta);
    if Start < Stop then
      Start := Start + Delta
    else
      Start := Start - Delta;
  end;
end;

{ TAdvSmoothMenuItems }

function TAdvSmoothMenuItems.Add: TAdvSmoothMenuItem;
begin
  Result := TAdvSmoothMenuItem(inherited Add);
end;

constructor TAdvSmoothMenuItems.Create(AOwner: TAdvSmoothMenu);
begin
  inherited Create(TAdvSmoothMenuItem);
  FOwner := AOwner;
end;

procedure TAdvSmoothMenuItems.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothMenuItems.GetItem(Index: Integer): TAdvSmoothMenuItem;
begin
  Result := TAdvSmoothMenuItem(inherited Items[Index]);
end;

function TAdvSmoothMenuItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothMenuItems.Insert(Index: Integer): TAdvSmoothMenuItem;
begin
  Result := TAdvSmoothMenuItem(inherited Insert(Index));
end;

procedure TAdvSmoothMenuItems.SetItem(Index: Integer;
  const Value: TAdvSmoothMenuItem);
begin
  inherited Items[Index] := Value;
end;


{ TAdvSmoothMenu }

procedure TAdvSmoothMenu.AnimateMenu(Sender: TObject);
var
  d, dopc: Single;
  opc: Single;
begin
  if AnimationType <> atNone then
  begin
    d := Abs(FCurrentPos - FPosTo) / 4;
    FAnimating := AnimateDouble(FCurrentPos, FPosTo, d, 1);

    if FAnimating then
    begin
      FCurrentTextPos := d;
      if FAnimationType = atFull then
      begin
        opc := FCurrentTextOpc;
        dopc := Abs(opc - 255) / 4;
        AnimateDouble(opc, 255, dopc, 1);
        FCurrentTextOpc := round(opc);
      end;
      Changed;
    end
    else
    begin
      FcurrentPos := FPosTo;
    end;
  end;
end;

procedure TAdvSmoothMenu.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMenu.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothMenu then
  begin
    FAnimationType := (Source as TAdvSmoothMenu).AnimationType;
    FRotation := (Source as TAdvSmoothMenu).Rotation;
    FItems.Assign((Source as TAdvSmoothMenu).Items);
    FItemAppearance.Assign((Source as TAdvSmoothMenu).ItemAppearance);
    FAppearance.Assign((Source as TAdvSmoothMenu).Appearance);
    FTransparent := (Source as TAdvSmoothMenu).Transparent;
    Changed;
  end;
end;

procedure TAdvSmoothMenu.Changed;
begin
  InitItemRects;
  Invalidate;
end;

procedure TAdvSmoothMenu.ChangeSelectedItem;
var
  v: integer;
  cnt: integer;
  I: Integer;
begin
  v := FSelectedItem;
  cnt := 0;
  for I := 0 to Items.Count - 1 do
    if Items[I].Enabled then
      Inc(cnt);

  if (cnt > 0) and (v >= 0) and (v < Items.Count) then
  begin
    while not Items[v].Enabled do
    begin
      Inc(v);
      if not ((v <= Items.Count - 1) and (v >= 0)) then
        Exit;

      FSelectedItem := v;
      FHoveredItem := v;
    end;
  end
  else
  begin
    FHoveredItem := -1;
    FSelectedItem := -1;
  end;

  if (FSelectedItem >= 0) and (FSelectedItem <= Items.Count - 1) then
  begin
    with Items[FSelectedItem] do
      FCurrentPos := FCaptionRect.X + (FCaptionRect.Width / 2);
  end;
end;

procedure TAdvSmoothMenu.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseInControl := true;
  Changed;
end;

procedure TAdvSmoothMenu.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseInControl := false;
  FRotationTimer.Enabled := true;
  Changed;
end;

constructor TAdvSmoothMenu.Create(AOwner: TComponent);
begin
  FConstructed := false;
  inherited Create(AOwner);
  DoubleBuffered := true;
  FTransparent := false;
  FAnimationType := atFull;
  FRotation := true;
  FAllowSelectedIndication := false;
  FItems := TAdvSmoothMenuItems.Create(Self);
  FItems.OnChange := ItemsChanged;
  FItemAppearance := TAdvSmoothMenuItemAppearance.Create(self);
  FItemAppearance.OnChange := ItemAppearanceChanged;

  FAppearance := TAdvSmoothMenuAppearance.Create(self);
  FAppearance.OnChange := AppearanceChanged;

  FSmoothTimer := TTimer.Create(Self);
  FSmoothTimer.Interval := 10;
  FSmoothTimer.Enabled := not (csDesigning in ComponentState);
  FSmoothTimer.OnTimer := AnimateMenu;

  FRotationInterval := 5000;
  FRotationTimer := TTimer.Create(Self);
  FRotationTimer.Interval := FRotationInterval;
  FRotationTimer.OnTimer := RotateMenu;

  Width := 500;
  Height := 60;
  FCurrentTextPos := 0;
  FCurrentTextOpc := 255;

  TabStop := true;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);    
end;


procedure TAdvSmoothMenu.CreateParams(var Params: TCreateParams);
begin
  { call the create of the params }
  inherited CreateParams(Params);
  ControlStyle := ControlStyle - [csOpaque] + [csAcceptsControls];

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
     ((Win32MajorVersion > 5) or
      ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
        if Params.WindowClass.Style and CS_DROPSHADOW <> 0 then
          Params.WindowClass.Style := Params.WindowClass.Style - CS_DROPSHADOW;
end;

procedure TAdvSmoothMenu.CreateWnd;
begin
  inherited;
  if FConstructed then
    Exit;

  if FDesignTime then
    InitPreview;

  FConstructed := true;
end;

procedure TAdvSmoothMenu.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

destructor TAdvSmoothMenu.Destroy;
begin
  FItems.Free;
  FItemAppearance.Free;
  FAppearance.Free;
  FSmoothTimer.Free;
  inherited;
end;

procedure TAdvSmoothMenu.DoEnter;
begin
  inherited;
  FFocused := true;
  Changed;
end;

procedure TAdvSmoothMenu.DoExit;
begin
  inherited;
  FFocused := false;
  Changed;
end;

procedure TAdvSmoothMenu.DoItemClick(Sender: TObject; ItemIndex: integer);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(Sender, ItemIndex);
end;

procedure TAdvSmoothMenu.DoItemEnter(Sender: TObject; ItemIndex: integer);
begin
  if Assigned(FOnItemEnter) then
    FOnItemEnter(Sender, ItemIndex);
end;

procedure TAdvSmoothMenu.DoItemLeave(Sender: TObject; ItemIndex: integer);
begin
  if Assigned(FOnItemLeave) then
    FOnItemLeave(Sender, ItemIndex);
end;

procedure TAdvSmoothMenu.DoRotate;
var
  i, cnt: integer;
  chk: Boolean;
begin
  cnt := 0;
  for I := 0 to Items.Count - 1 do
    if Items[I].Enabled then
      Inc(cnt);

  if (Items.Count > 1) and (cnt > 1) then
  begin
    if FHoveredItem = Items.Count - 1 then
      FHoveredItem := 0
    else
      Inc(FhoveredItem);

    FHoveredItem := Max(Min(FHoveredItem, Items.Count - 1), 0);

    if not AllowSelectedIndication then
      chk := (FSelectedItem = FHoveredItem) or not Items[FHoveredItem].Enabled
    else
      chk := not Items[FHoveredItem].Enabled;

    while chk do
    begin
      if FHoveredItem = Items.Count - 1 then
        FHoveredItem := 0
      else
        Inc(FhoveredItem);

      if not AllowSelectedIndication then
        chk := (FSelectedItem = FHoveredItem) or not Items[FHoveredItem].Enabled
      else
        chk := not Items[FHoveredItem].Enabled;
    end;

    with Items[FHoveredItem] do
    begin
      FPosTo := FCaptionRect.X + (FCaptionRect.Width / 2);
      if AnimationType = atNone then
        FCurrentPos := FPosTo;

      FCurrentTextPos := FPosTo - FCurrentPos;
      FCurrentTextOpc := 0;
    end;

    InitItemRects;
  end;
end;

procedure TAdvSmoothMenu.DrawBackGround;
var
  g: TGPGraphics;
begin
  if not Transparent then
  begin
    g := TGPGraphics.Create(Canvas.Handle);

    Appearance.Fill.Fill(g, MakeRect(ClientRect.Left, ClientRect.Top, ClientRect.Right - 1, ClientRect.Bottom - 1));

    g.Free;
  end;
end;

procedure TAdvSmoothMenu.DrawIndicator;
var
  g: TGPGraphics;
  path: TGPGraphicsPath;
  y: Single;
  b: TGPSolidBrush;
  pts: array[0..3] of TGPPointF;
  size: integer;
begin
  if Appearance.IndicatorVisible then
  begin
    g := TGPGraphics.Create(Canvas.Handle);

    path := TGPGraphicsPath.Create;
    y := ItemAppearance.Splitter.Height + 2;

    size := Appearance.IndicatorSize;

    pts[0].X := FCurrentPos - size;
    pts[0].Y := y;

    pts[1].X := FCurrentPos;
    pts[1].Y := y - size;

    pts[2].X := FCurrentPos + size;
    pts[2].Y := y;

    path.AddPolygon(PGPPointF(@pts), 3);

    b := TGPSolidBrush.Create(MakeColor(Appearance.IndicatorOpacity, Appearance.IndicatorColor));
    g.FillPath(b, path);

    b.Free;
    path.Free;
    g.Free;
  end;
end;

procedure TAdvSmoothMenu.DrawItems;
var
  i: integer;
begin
  for I := 0 to Items.Count - 1 do
    Items[I].Draw;
end;

procedure TAdvSmoothMenu.DrawLine;
var
  g: TGPGraphics;
  p: TGPPen;
  bw: integer;
begin
  if Appearance.LineVisible then
  begin
    g := TGPGraphics.Create(Canvas.Handle);
    p := TGPPen.Create(MakeColor(Appearance.LineOpacity, Appearance.LineColor), 0.1);

    bw := 0;
    if (Appearance.Fill.BorderColor <> clNone) then
      bw := Appearance.Fill.BorderWidth;

    g.DrawLine(p, bw, ItemAppearance.Splitter.Height + 2 , Width - (2 * bw)  - GetShadowOffset, ItemAppearance.Splitter.Height + 2);
    p.Free;
    g.Free;
  end;
end;

procedure TAdvSmoothMenu.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothMenu.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothMenu.GetShadowOffset: integer;
begin
  Result := 0;
  if not Transparent and (Appearance.Fill.ShadowColor <> clNone) then
    Result := Appearance.Fill.ShadowOffset;
end;

procedure TAdvSmoothMenu.GetTextPosition(var x, y: integer; rectangle: TGPRectF;
  objectwidth, objectheight: integer; location: TAdvSmoothMenuLocation);
var
  w, h, tw, th: integer;
begin
  tw := objectwidth;
  th := objectheight;
  w := Round(rectangle.Width);
  h := Round(rectangle.Height);
  case location of
    plTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    plTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    plBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    plBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    plTopCenter:
    begin
      x := (w - tw) div 2;
      y := 0;
    end;
    plBottomCenter:
    begin
      x := (w - tw) div 2;
      y := h - th;
    end;
    plCenterCenter:
    begin
      x := (w - tw) div 2;
      y := (h - th) div 2;
    end;
    plCenterLeft:
    begin
      x := 0;
      y := (h - th) div 2;
    end;
    plCenterRight:
    begin
      x := w - tw;
      y := (h - th) div 2;
    end;
  end;
end;

function TAdvSmoothMenu.GetThemeID: String;
begin
  Result := ClassName;
end;

function TAdvSmoothMenu.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothMenu.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothMenu.InitItemRects;
var
  I: Integer;
  g: TGPGraphics;
  f: TGPFont;
  ff: TGPFontFamily;
  fs: integer;
  sf: TGPStringFormat;
  sizeRect: TGPRectF;
  itemleft: single;
  its: Single;
  cf: TFont;
  max: single;
begin
  if (Items.Count = 0) or (csDestroying in ComponentState) then
    exit;

  itemleft := 0;
  max := -MAXLONG;
  cf := TFont.Create;
  for I := 0 to Items.Count - 1 do
  begin
    with Items[I] do
    begin
      if Enabled then
      begin
        if (FHoveredItem = Index) and not (FselectedItem = Index) then
          cf.Assign(ItemAppearance.HoverFont)
        else if (FSelectedItem = Index) then
          cf.Assign(ItemAppearance.SelectedFont)
        else
          cf.Assign(ItemAppearance.Font);
      end
      else
        cf.Assign(ItemAppearance.Font);

      g := TGPGraphics.Create(Canvas.Handle);
      ff := TGPFontFamily.Create(cf.Name);
      if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
      begin
        ff.Free;
        ff := TGPFontFamily.Create('Arial');
      end;

      fs := 0;
      if (fsBold in cf.Style) then
        fs := fs + 1;
      if (fsItalic in cf.Style) then
        fs := fs + 2;
      if (fsUnderline in cf.Style) then
        fs := fs + 4;

      its := (Width - 1 - GetShadowOffset) / Items.Count;

      sf := TGPStringFormat.Create;
      f := TGPFont.Create(ff, cf.Size , fs, UnitPoint);

      g.MeasureString(FCaption, Length(FCaption), f, MakeRect(ClientRect.Left, ClientRect.Top, Width - GetShadowOffset, Height - GetShadowOffset),sf, sizeRect);
      
      FItemRect := sizeRect;
      FItemRect.X := itemleft;
      itemleft := itemleft + its;
      FItemRect.Width := its;
      FItemRect.Y := 5;

      //preset caption rect
      FCaptionRect := MakeRect(FItemRect.X + ((FItemRect.Width - sizerect.Width) / 2), FItemRect.Y, sizerect.Width, 0);
      //Take maximum of captionrectangle
      if sizeRect.Height > max then
        max := Sizerect.Height;

      ff.Free;
      f.Free;
      sf.Free;
      g.Free;
    end;
  end;

  cf.Free;

  for I := 0 to Items.Count - 1 do
  begin
    with items[I] do
    begin
      FCaptionRect.Height := max;
      FItemRect.Height := max;
    end;
  end;
end;

procedure TAdvSmoothMenu.InitPreview;
var
  i: integer;
begin
  Items.Clear;
  
  //ItemAppearance.Font.Size := 11;
  //ItemAppearance.Font.Color := clBlack;
  //ItemAppearance.SelectedFont.Size := 11;
  //ItemAppearance.SelectedFont.Color := clWhite;
  //ItemAppearance.HoverFont.Size := 11;
  //ItemAppearance.HoverFont.Color := clSilver;
  //ItemAppearance.GlowColorHover := clDkGray;
  //ItemAppearance.GlowOpacityHover := 255;
  //ItemAppearance.NotesFont.Size := 7;
  //ItemAppearance.NotesFont.Style := ItemAppearance.NotesFont.Style + [fsBold];

  for I := 0 to 5 do
  begin
    with Items.Add do
    begin
      Caption := 'Item ' + IntToStr(Index);
      Notes := 'Item ' + Inttostr(Index) + ' notes';
    end;
  end;
end;

procedure TAdvSmoothMenu.ItemAppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMenu.ItemsChanged(Sender: TObject);
begin
  InitItemRects;
  Changed;
end;

procedure TAdvSmoothMenu.KeyDown(var Key: Word; Shift: TShiftState);
var
  OldItem: integer;
begin
  inherited;
  case Key of
  VK_SPACE:
    begin
      if Assigned(OnItemClick) then
        OnItemClick(Self, FSelectedItem);
    end;
  VK_LEFT:
    begin
      OldItem := FSelectedItem;
      if FSelectedItem >0 then
        dec(FSelectedItem)
      else
        FSelectedItem  := Items.Count - 1;
      FHoveredItem := FSelectedItem;
      if Assigned(OnItemChange) then
        OnItemChange(Self, olditem, FSelectedItem);

      FPosTo := Items[FSelectedItem].FCaptionRect.X + (Items[FSelectedItem].FCaptionRect.Width / 2);
      Invalidate;
    end;
  VK_RIGHT:
    begin
      oldItem := FSelectedItem;
      if FSelectedItem < Items.Count - 1 then
        inc(FSelectedItem)
      else
        FSelectedItem := 0;
      FHoveredItem := FSelectedItem;
      if Assigned(OnItemChange) then
        OnItemChange(Self, olditem, FSelectedItem);      
      FPosTo := Items[FSelectedItem].FCaptionRect.X + (Items[FSelectedItem].FCaptionRect.Width / 2);
      Invalidate;
    end;
  end;

end;

procedure TAdvSmoothMenu.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothMenu.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

procedure TAdvSmoothMenu.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  it: integer;
  item: TAdvSmoothMenuItem;
begin
  inherited;
  it := XYToItem(X, Y);
  if it <> -1 then
  begin
    item := Items[it];
    if FHoveredItem <> item.Index then
    begin
      FCurrentTextPos := (FPosTo - FCurrentPos);
      if AnimationType <> atNoOpacity then
        FcurrentTextOpc := 0;
    end;

    FHoveredItem := item.Index;
    InitItemRects;    

    MouseLeave := true;
    if MouseEnter then
    begin
      DoItemEnter(Self, FHoveredItem);
      MouseEnter := false;
    end;

    FRotationTimer.Enabled := false;
    FPosTo := item.FCaptionRect.X + (item.FCaptionRect.Width / 2);

    if AnimationType = atNone then
    begin
      FCurrentPos := FPosTo;
      FCurrentTextOpc := 255;
      FCurrentTextPos := 0;
    end;

    Cursor := crHandPoint;
    Changed;
  end
  else
  begin
    MouseEnter := true;
    if MouseLeave then
    begin
      DoItemLeave(Self, FHoveredItem);
      MouseLeave := false;
    end;

    FCurrentTextPos := 0;
    FCurrentTextOpc := 255;
    FRotationTimer.Enabled := true;
    Changed;
    Cursor := crArrow;
  end;
end;

procedure TAdvSmoothMenu.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  it, olditem: integer;
begin
  inherited;
  SetFocus;
  it := XYToItem(X, Y);
  if it <> -1 then
  begin
    olditem := FSelectedItem;
    FSelectedItem := it;
    if Assigned(OnItemChange) then
      OnItemChange(Self, olditem, FSelectedItem);
    InitItemRects;
    DoItemClick(self, FSelectedItem);
    Changed;
  end;
end;

procedure TAdvSmoothMenu.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if not (csDestroying in ComponentState) then
  begin
    if (AOperation = opRemove) and (AComponent = FContainer) then
      FContainer := nil;
  end;

  inherited;    
end;

procedure TAdvSmoothMenu.Paint;
var
  g: TGPGraphics;
begin
  inherited;
  DrawBackGround;
  DrawLine;
  if Items.Count > 0 then
  begin
    DrawItems;
    DrawIndicator;
  end;
  if TabStop and FFocused then
  begin
    g := TGPGraphics.Create(Canvas.Handle);
    g.SetSmoothingMode(SmoothingModeDefault);
    DrawFocus(g, MakeRect(0, 0, Width - 1, Height - 1), 0);
    g.Free;
  end;
end;

procedure TAdvSmoothMenu.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothMenu.Resize;
begin
  inherited;
  InitItemRects;
  Changed;
end;

procedure TAdvSmoothMenu.RotateMenu(Sender: TObject);
begin
  if Rotation and not (csDesigning in ComponentState) then
  begin
    DoRotate;
    Changed;
  end;
end;

procedure TAdvSmoothMenu.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothMenu.SetAllowSelectedIndication(const Value: Boolean);
begin
  if FAllowSelectedIndication <> value then
  begin
    FAllowSelectedIndication := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenu.SetAnimationType(
  const Value: TAdvSmoothMenuAnimationType);
begin
  if FAnimationType <> value then
  begin
    FAnimationType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenu.SetAppearance(const Value: TAdvSmoothMenuAppearance);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMenu.SetColorTones(ATones: TColorTones);
begin
  Appearance.Fill.BorderColor := ATones.Background.BorderColor;
  Appearance.Fill.Color := ATones.Background.BrushColor ;
  Appearance.Fill.ColorTo := ATones.Background.BrushColor ;
  Appearance.Fill.ColorMirror := ATones.Background.BrushColor ;
  Appearance.Fill.ColorMirrorTo := ATones.Background.BrushColor ;


  Appearance.IndicatorColor := ATones.Background.TextColor;
  Appearance.LineColor := ATones.Background.TextColor;

  ItemAppearance.GlowColor := clNone;
  ItemAppearance.GlowColorHover := clNone;
  ItemAppearance.GlowColorSelected := clNone;

  ItemAppearance.Font.Color := ATones.Background.TextColor;
  ItemAppearance.HoverFont.Color := ATones.Background.TextColor;
  ItemAppearance.SelectedFont.Color := ATones.Background.TextColor;
  ItemAppearance.NotesFont.Color := ATones.Background.TextColor;
end;

procedure TAdvSmoothMenu.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  // TODO : do color settings here
  case AStyle of
    tsOffice2003Blue:
      begin
        Appearance.Fill.BorderColor := $E3B28D;
        Appearance.Fill.Color := $FDEADA;
        Appearance.Fill.ColorTo := $FADAC4;
        Appearance.Fill.ColorMirror := $FADAC4;
        Appearance.Fill.ColorMirrorTo := $FADAC4;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := clBlack;
        Appearance.LineColor := clBlack;

        ItemAppearance.GlowColor := $933803;
        ItemAppearance.GlowColorHover := $7AE1FE;
        ItemAppearance.GlowColorSelected := $4C9FFD;

        ItemAppearance.Font.Color := clWhite;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clWhite;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
    tsOffice2003Silver:
      begin
        Appearance.Fill.BorderColor := $947C7C;
        Appearance.Fill.Color := $00F7F3F3;
        Appearance.Fill.ColorTo := $00E6D8D8;
        Appearance.Fill.ColorMirror := $00E6D8D8;
        Appearance.Fill.ColorMirrorTo := $00E6D8D8;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := clBlack;
        Appearance.LineColor := clBlack;

        ItemAppearance.GlowColor := $957475;
        ItemAppearance.GlowColorHover := $7AE1FE;
        ItemAppearance.GlowColorSelected := $4C9FFD;

        ItemAppearance.Font.Color := clWhite;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clWhite;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
    tsOffice2003Olive:
      begin
        Appearance.Fill.BorderColor := $588060;
        Appearance.Fill.Color := $CFF0EA;
        Appearance.Fill.ColorTo := $CFF0EA;
        Appearance.Fill.ColorMirror := $CFF0EA;
        Appearance.Fill.ColorMirrorTo := $CFF0EA;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := clBlack;
        Appearance.LineColor := clBlack;

        ItemAppearance.GlowColor := $447A63;
        ItemAppearance.GlowColorHover := $7AE1FE;
        ItemAppearance.GlowColorSelected := $4C9FFD;

        ItemAppearance.Font.Color := clWhite;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clWhite;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
    tsOffice2003Classic:
    begin
        Appearance.Fill.BorderColor := $808080;
        Appearance.Fill.Color := clWhite;
        Appearance.Fill.ColorTo := $C9D1D5;
        Appearance.Fill.ColorMirror := $C9D1D5;
        Appearance.Fill.ColorMirrorTo := clWhite;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := clWhite;
        Appearance.LineColor := clWhite;

        ItemAppearance.GlowColor := $808080;
        ItemAppearance.GlowColorHover := $7AE1FE;
        ItemAppearance.GlowColorSelected := $4C9FFD;

        ItemAppearance.Font.Color := clWhite;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clWhite;
        ItemAppearance.NotesFont.Color := clBlack;
    end;
    tsOffice2007Luna:
      begin
        Appearance.Fill.BorderColor := $E3B28D;
        Appearance.Fill.Color := $FAF1E9;
        Appearance.Fill.ColorTo := $EDD8C7;
        Appearance.Fill.ColorMirror := $EDD8C7;
        Appearance.Fill.ColorMirrorTo := $FFF2E7;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := clWhite;
        Appearance.LineColor := clWhite;

        ItemAppearance.GlowColor := $FFD2AF;
        ItemAppearance.GlowColorHover := $006CCAFD;
        ItemAppearance.GlowColorSelected := $4C9FFD;

        ItemAppearance.GlowOpacity := 150;
        ItemAppearance.GlowOpacityHover := 210;
        ItemAppearance.GlowOpacitySelected := 200;

        ItemAppearance.Font.Color := clBlack;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clWhite;
        ItemAppearance.NotesFont.Color := $723708;
      end;
    tsOffice2007Obsidian:
      begin
        Appearance.Fill.Color := $CFC6C1;
        Appearance.Fill.ColorTo := $C5BBB4;
        Appearance.Fill.ColorMirror := $C5BBB4;
        Appearance.Fill.ColorMirrorTo := $ECECE5;
        Appearance.Fill.BorderColor := clBlack;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := clBlack;
        Appearance.LineColor := clBlack;

        ItemAppearance.GlowColor := $5C534C;//$C9C2BD;
        ItemAppearance.GlowColorHover := $7AE1FE;
        ItemAppearance.GlowColorSelected := $4C9FFD;

        ItemAppearance.Font.Color := clWhite;
        ItemAppearance.HoverFont.Color := clWhite;
        ItemAppearance.SelectedFont.Color := clWhite;
        ItemAppearance.NotesFont.Color := $433C37;
      end;
    tsWindowsXP:
      begin
        Appearance.Fill.BorderColor := clBlack;
        Appearance.Fill.Color := clBtnFace;
        Appearance.Fill.ColorTo := clBtnFace;
        Appearance.Fill.ColorMirror := clBtnFace;
        Appearance.Fill.ColorMirrorTo := clBtnFace;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := clBlack;
        Appearance.LineColor := clBlack;

        ItemAppearance.GlowColor := $B9D8DC;
        ItemAppearance.GlowColorHover := $EFD3C6;
        ItemAppearance.GlowColorSelected := clInactiveCaption;

        ItemAppearance.Font.Color := clBlack;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clBlack;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
    tsWhidbey:
      begin
        Appearance.Fill.BorderColor := $962D00;
        Appearance.Fill.Color := clWhite;
        Appearance.Fill.ColorTo := $D9E9EC;
        Appearance.Fill.ColorMirror := $D9E9EC;
        Appearance.Fill.ColorMirrorTo := clWhite;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := clBlack;
        Appearance.LineColor := clBlack;

        ItemAppearance.GlowColor := $808080;
        ItemAppearance.GlowColorHover := $7AE1FE;
        ItemAppearance.GlowColorSelected := $4C9FFD;

        ItemAppearance.Font.Color := clWhite;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clWhite;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        Appearance.Fill.BorderColor := $74706F;
        Appearance.Fill.Color := $F6F1EE;
        Appearance.Fill.ColorTo := $E7DCD5;
        Appearance.Fill.ColorMirror := $E7DCD5;
        Appearance.Fill.ColorMirrorTo := $F4F4EE;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := clWhite;
        Appearance.LineColor := clWhite;

        ItemAppearance.GlowColor := $E8E0DB;
        ItemAppearance.GlowColorHover := $7AE1FE;
        ItemAppearance.GlowColorSelected := $4C9FFD;

        ItemAppearance.Font.Color := clBlack;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clWhite;
        ItemAppearance.NotesFont.Color := $723708;
      end;
    tsWindowsVista:
      begin
        Appearance.Fill.BorderColor := $FEDF9A;;
        Appearance.Fill.Color := $FFFFFF;
        Appearance.Fill.ColorTo := $FFFFFF;
        Appearance.Fill.ColorMirror := $FFFFFF;
        Appearance.Fill.ColorMirrorTo := $FFFFFF;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := clBlack;
        Appearance.LineColor := clBlack;

        ItemAppearance.GlowColor := $FBEDD3;
        ItemAppearance.GlowColorHover := $FBEDD3;
        ItemAppearance.GlowColorSelected := $F7DAA2;

        ItemAppearance.Font.Color := clBlack;
        ItemAppearance.HoverFont.Color := $FEDF9A;
        ItemAppearance.SelectedFont.Color := clBlack;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
    tsWindows7:
      begin
        Appearance.Fill.BorderColor := $CEA27D;
        Appearance.Fill.Color := $FFFFFF;
        Appearance.Fill.ColorTo := $FFFFFF;
        Appearance.Fill.ColorMirror := $FFFFFF;
        Appearance.Fill.ColorMirrorTo := $FFFFFF;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := clBlack;
        Appearance.LineColor := clBlack;

        ItemAppearance.GlowColor := $CEA27D;
        ItemAppearance.GlowColorHover := $FDFBFA;
        ItemAppearance.GlowColorSelected := $FCEBDC;

        ItemAppearance.Font.Color := clBlack;
        ItemAppearance.HoverFont.Color := $FEDF9A;
        ItemAppearance.SelectedFont.Color := clBlack;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
      tsTerminal:
      begin
        Appearance.Fill.BorderColor := clGray;
        Appearance.Fill.Color := clBtnFace;
        Appearance.Fill.ColorTo := clBtnFace;
        Appearance.Fill.ColorMirror := clNone;
        Appearance.Fill.ColorMirrorTo := clNone;


        Appearance.IndicatorColor := clBlack;
        Appearance.LineColor := clBlack;

        ItemAppearance.GlowColor := clSilver;
        ItemAppearance.GlowColorHover := clGray;
        ItemAppearance.GlowColorSelected := clHighLight;

        ItemAppearance.Font.Color := clBlack;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clWhite;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
      tsOffice2010Blue:
      begin
        Appearance.Fill.BorderColor := $C7B29F;
        Appearance.Fill.Color := $FDF6EF;
        Appearance.Fill.ColorTo := $F0DAC7;
        Appearance.Fill.ColorMirror := clNone;
        Appearance.Fill.ColorMirrorTo := clNone;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := $5B391E;
        Appearance.LineColor := $5B391E;

        ItemAppearance.GlowColor := clWhite;
        ItemAppearance.GlowColorHover := RGB(253, 227, 138);
        ItemAppearance.GlowColorSelected := RGB(254, 225, 69);

        ItemAppearance.Font.Color := clBlack;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clBlack;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
      tsOffice2010Silver:
      begin
        Appearance.Fill.BorderColor := $D2CDC8;
        Appearance.Fill.Color := $FFFFFF;
        Appearance.Fill.ColorTo := $EDE5E0;
        Appearance.Fill.ColorMirror := clNone;
        Appearance.Fill.ColorMirrorTo := clNone;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := $7C6D66;
        Appearance.LineColor := $7C6D66;

        ItemAppearance.GlowColor := clWhite;
        ItemAppearance.GlowColorHover := RGB(253, 227, 138);
        ItemAppearance.GlowColorSelected := RGB(254, 225, 69);

        ItemAppearance.Font.Color := clBlack;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clBlack;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
      tsOffice2010Black:
      begin
        Appearance.Fill.BorderColor := $6D6D6D;
        Appearance.Fill.Color := $BFBFBF;
        Appearance.Fill.ColorTo := $919191;
        Appearance.Fill.ColorMirror := clNone;
        Appearance.Fill.ColorMirrorTo := clNone;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := $D7D7D6;
        Appearance.LineColor := $D7D7D6;

        ItemAppearance.GlowColor := clWhite;
        ItemAppearance.GlowColorHover := RGB(253, 227, 138);
        ItemAppearance.GlowColorSelected := RGB(254, 225, 69);

        ItemAppearance.Font.Color := clBlack;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clBlack;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
    tsWindows8, tsWindows10:
      begin
        Appearance.Fill.BorderColor := $DCDBDA;
        Appearance.Fill.Color := clWhite;
        Appearance.Fill.ColorTo := clWhite;;
        Appearance.Fill.ColorMirror := clNone;
        Appearance.Fill.ColorMirrorTo := clNone;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := $DCDBDA;
        Appearance.LineColor := $DCDBDA;

        ItemAppearance.GlowColor := $F7F6F5;
        ItemAppearance.GlowColorHover := $F7EFE8;
        ItemAppearance.GlowColorSelected := $F7E0C9;

        ItemAppearance.Font.Color := clBlack;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clBlack;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
    tsOffice2013White:
      begin
        Appearance.Fill.BorderColor := $D4D4D4;
        Appearance.Fill.Color := clWhite;
        Appearance.Fill.ColorTo := clWhite;;
        Appearance.Fill.ColorMirror := clNone;
        Appearance.Fill.ColorMirrorTo := clNone;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := $D4D4D4;
        Appearance.LineColor := $D4D4D4;

        ItemAppearance.GlowColor := $F7F6F5;
        ItemAppearance.GlowColorHover := $F7EFE8;
        ItemAppearance.GlowColorSelected := $F7E0C9;

        ItemAppearance.Font.Color := clBlack;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clBlack;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
    tsOffice2013LightGray:
      begin
        Appearance.Fill.BorderColor := $C6C6C6;
        Appearance.Fill.Color := clWhite;
        Appearance.Fill.ColorTo := clWhite;;
        Appearance.Fill.ColorMirror := clNone;
        Appearance.Fill.ColorMirrorTo := clNone;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := $C6C6C6;
        Appearance.LineColor := $C6C6C6;

        ItemAppearance.GlowColor := $F7F6F5;
        ItemAppearance.GlowColorHover := $F7EFE8;
        ItemAppearance.GlowColorSelected := $F7E0C9;

        ItemAppearance.Font.Color := clBlack;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clBlack;
        ItemAppearance.NotesFont.Color := clBlack;
      end;

    tsOffice2013Gray:
      begin
        Appearance.Fill.BorderColor := $ABABAB;
        Appearance.Fill.Color := clWhite;
        Appearance.Fill.ColorTo := clWhite;;
        Appearance.Fill.ColorMirror := clNone;
        Appearance.Fill.ColorMirrorTo := clNone;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := $ABABAB;
        Appearance.LineColor := $ABABAB;

        ItemAppearance.GlowColor := $F7F6F5;
        ItemAppearance.GlowColorHover := $F7EFE8;
        ItemAppearance.GlowColorSelected := $F7E0C9;

        ItemAppearance.Font.Color := clBlack;
        ItemAppearance.HoverFont.Color := clBlack;
        ItemAppearance.SelectedFont.Color := clBlack;
        ItemAppearance.NotesFont.Color := clBlack;
      end;
    tsOffice2016White:
      begin

        Appearance.Fill.BorderColor := $D4D4D4;
        Appearance.Fill.Color := clWhite;
        Appearance.Fill.ColorTo := clWhite;
        Appearance.Fill.ColorMirror := clWhite;
        Appearance.Fill.ColorMirrorTo := clWhite;
        Appearance.Fill.GradientMirrorType := gtVertical;


        Appearance.IndicatorColor := $D4D4D4;
        Appearance.LineColor := $D4D4D4;

        ItemAppearance.GlowColor := clWhite;
        ItemAppearance.GlowColorHover := $F2E1D5;
        ItemAppearance.GlowColorSelected := $E3BDA3;

        ItemAppearance.Font.Color := $505050;
        ItemAppearance.HoverFont.Color := $505050;
        ItemAppearance.SelectedFont.Color := $505050;
        ItemAppearance.NotesFont.Color := $505050;
      end;
    tsOffice2016Gray:
      begin
        Appearance.Fill.BorderColor := $444444;
        Appearance.Fill.Color := $B2B2B2;
        Appearance.Fill.ColorTo := $B2B2B2;
        Appearance.Fill.ColorMirror := $B2B2B2;
        Appearance.Fill.ColorMirrorTo := $B2B2B2;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := $444444;
        Appearance.LineColor := $444444;

        ItemAppearance.GlowColor := $B2B2B2;
        ItemAppearance.GlowColorHover := $F2E1D5;
        ItemAppearance.GlowColorSelected := $E3BDA3;

        ItemAppearance.Font.Color := $424242;
        ItemAppearance.HoverFont.Color := $424242;
        ItemAppearance.SelectedFont.Color := $424242;
        ItemAppearance.NotesFont.Color := $424242;
      end;

    tsOffice2016Black:
      begin
        Appearance.Fill.BorderColor := $4E4E4E;
        Appearance.Fill.Color := $363636;
        Appearance.Fill.ColorTo := $363636;
        Appearance.Fill.ColorMirror := $363636;
        Appearance.Fill.ColorMirrorTo := $363636;
        Appearance.Fill.GradientMirrorType := gtVertical;

        Appearance.IndicatorColor := $ABABAB;
        Appearance.LineColor := $ABABAB;

        ItemAppearance.GlowColor := $363636;
        ItemAppearance.GlowColorHover := $6A6A6A;
        ItemAppearance.GlowColorSelected := $444444;

        ItemAppearance.Font.Color := $A6A6A6;
        ItemAppearance.HoverFont.Color := $FFFFFF;
        ItemAppearance.SelectedFont.Color := $FFFFFF;
        ItemAppearance.NotesFont.Color := $A6A6A6;
      end;

  end;
end;

procedure TAdvSmoothMenu.SetItemAppearance(
  const Value: TAdvSmoothMenuItemAppearance);
begin
  if FItemAppearance <> value then
  begin
    FItemAppearance := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenu.SetItems(const Value: TAdvSmoothMenuItems);
begin
  if FItems <> value then
  begin
    FItems := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenu.SetRotation(const Value: Boolean);
begin
  if FRotation <> value then
  begin
    FRotation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenu.SetRotationInterval(const Value: Integer);
begin
  if FRotationInterval <> value then
  begin
    FRotationInterval := Value;
    if FRotationTimer <> nil then    
      FRotationTimer.Interval := value;
    Changed;
  end;
end;

procedure TAdvSmoothMenu.SetSelectedItemIndex(const Value: integer);
begin
  if FSelectedItem <> Value then
  begin
    FSelectedItem := Value;
    Changed;
  end;

  FHoveredItem := -1;
  ChangeSelectedItem;
  FPosTo := FCurrentPos;
end;

procedure TAdvSmoothMenu.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenu.SetVersion(const Value: String);
begin

end;

procedure TAdvSmoothMenu.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TAdvSmoothMenu.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end;

procedure TAdvSmoothMenu.WMPaint(var Message: TWMPaint);
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

procedure TAdvSmoothMenu.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

function TAdvSmoothMenu.XYToItem(X, Y: integer): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do
  begin
    with Items[I] do
    begin
      if PtInRect(Bounds(Round(FItemRect.X), Round(FItemRect.Y), Round(FItemRect.Width), Round(FItemRect.Height)), Point(X, Y)) and Enabled then
      begin
        Result := i;
        break;
      end;
    end;
  end;
end;

{ TAdvSmoothMenuItem }

procedure TAdvSmoothMenuItem.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothMenuItem then
  begin
    FTag := (Source as TAdvSmoothMenuItem).Tag;
    FCaption := (Source as TAdvSmoothMenuItem).Caption;
    FNotes := (Source as TAdvSmoothMenuItem).Notes;
    FNotesLocation := (Source as TAdvSmoothMenuItem).NotesLocation;
    FNotesLeft := (Source as TAdvSmoothMenuItem).NotesLeft;
    FNotesTop := (Source as TAdvSmoothMenuItem).NotesTop;
    FPicturePosition := (Source as TAdvSmoothMenuItem).PicturePosition;
    FPicture.Assign((Source as TAdvSmoothMenuItem).Picture);
    FPictureLeft := (Source as TAdvSmoothMenuItem).PictureLeft;
    FPictureTop := (Source as TAdvSmoothMenuItem).PictureTop;
    FPictureSize := (Source as TAdvSmoothMenuItem).PictureSize;
    FPictureWidth := (Source as TAdvSmoothMenuItem).PictureWidth;
    FPictureHeight := (Source as TAdvSmoothMenuItem).PictureHeight;
    FEnabled := (Source as TAdvSmoothMenuItem).Enabled;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItem.Changed;
begin
  (Collection as TAdvSmoothMenuItems).FOwner.ItemsChanged(self);
end;

constructor TAdvSmoothMenuItem.Create(Collection: TCollection);
begin
  inherited;
  FNotesTop := 0;
  FNotesLeft := 0;
  FNotesLocation := plTopLeft;
  FPicturePosition := ppTopLeft;
  FPicture := TAdvGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FpictureWidth := 50;
  FpictureHeight := 50;
  FPictureLeft := 0;
  FPictureTop := 0;
  FpictureSize := psOriginal;
  FEnabled := true;

  with (Collection as TAdvSmoothMenuItems).FOwner do
  begin
    InitItemRects;
    FCurrentPos := Items[0].FCaptionRect.X + (Items[0].FCaptionRect.Width / 2);
    FPosTo := FCurrentPos;
    Changed;
  end;
end;

destructor TAdvSmoothMenuItem.Destroy;
begin
  FPicture.Free;
  (Collection as TAdvSmoothMenuItems).FOwner.InitItemRects;
  Changed;  
  inherited;
end;

procedure TAdvSmoothMenuItem.Draw;
var
  path: TGPGraphicsPath;
  bSplit: TGPLinearGradientBrush;
  rsplit: TGPRectF;
  c, cto: TGPColor;
  g: TGPGraphics;
  ff: TGPFontFamily;
  f: TGPFont;
  fs: integer;
  sf: TGPStringFormat;
  b: TGPSolidBrush;
  glowb: TGPPathGradientBrush;
  cf: TFont;                 
  colors : array[0..0] of TGPColor;
  count: integer;
  glowr, ir, sizer: TGPRectF;
  go: Byte;
  gc: TColor;
  picr: TRect;
  x, y, pw, ph: integer;
  doNotes: Boolean;
  pt: TGPPointF;
  pts: array[0..3] of TGPPointF;
begin
  with (Collection as TAdvSmoothMenuItems).FOwner do
  begin
    glowb := nil;
    path := nil;
    
    g := TGPGraphics.Create(Canvas.Handle);
    g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);

    cf := TFont.Create;
    if Self.Enabled then
    begin
      //Preset
      if (FHoveredItem = Index) and not (FselectedItem = Index) then
      begin
        cf.Assign(ItemAppearance.HoverFont);
        go := ItemAppearance.GlowOpacityHover;
        gc := ItemAppearance.GlowColorHover;
      end
      else if (FSelectedItem = Index) then
      begin
        cf.Assign(ItemAppearance.SelectedFont);
        go := ItemAppearance.GlowOpacitySelected;
        gc := ItemAppearance.GlowColorSelected;
      end
      else
      begin
        cf.Assign(ItemAppearance.Font);
        go := ItemAppearance.GlowOpacity;
        gc := ItemAppearance.GlowColor;
      end;
      /////
    end
    else
    begin
      cf.Assign(ItemAppearance.Font);
      cf.Color := clGray;
      go := ItemAppearance.GlowOpacityDisabled;
      gc := ItemAppearance.GlowColorDisabled;
    end;

    //glow
    if (go > 0) and (gc <> clNone) then
    begin
      glowr := FItemRect;
      path := TGPGraphicsPath.Create;
      case ItemAppearance.GlowShape of
        gsEllipse: path.AddEllipse(glowr);
        gsSquare: path.AddRectangle(glowr);
        gsDiamond:
        begin
          pts[0] := MakePoint(glowr.X + glowr.Width / 2, glowr.Y);
          pts[1] := MakePoint(glowr.X + glowr.Width, glowr.Y + glowr.Height / 2);
          pts[2] := MakePoint(glowr.X + glowr.Width / 2, glowr.Y + glowr.Height);
          pts[3] := MakePoint(glowr.X, glowr.Y + glowr.Height / 2);
          path.AddPolygon(PGPPointF(@pts), 4);
        end;
      end;


      glowb := TGPPathGradientBrush.Create(path);
      glowb.SetCenterPoint(MakePoint(glowr.X + (glowr.Width / 2), glowr.Y + (glowr.Height / 2)));
      glowb.SetCenterColor(MakeColor(go, gc));
      colors[0] := MakeColor(0, clWhite);
      count := 1;
      glowb.SetSurroundColors(@colors, count);

      g.FillRectangle(glowb, glowr);
    end;
    //////

    if FCaption <> '' then
    begin
      //draw with font
      ff := TGPFontFamily.Create(cf.Name);
      if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
      begin
        ff.Free;
        ff := TGPFontFamily.Create('Arial');
      end;

      fs := 0;
      if (fsBold in  cf.Style) then
        fs := fs + 1;
      if (fsItalic in cf.Style) then
        fs := fs + 2;
      if (fsUnderline in cf.Style) then
        fs := fs + 4;

      sf := TGPStringFormat.Create;
      f := TGPFont.Create(ff, cf.Size , fs, UnitPoint);
      b := TGPSolidBrush.Create(MakeColor(255, cf.Color));

      g.DrawString(FCaption, Length(FCaption), f, FCaptionRect, sf, b);

      b.Free;
      f.Free;
      sf.free;
      ff.Free;
    end;

    cf.Free;

    doNotes := false;
    if (FSelectedItem = Index) and (FHoveredItem = -1) then
      doNotes := true
    else if FHoveredItem = Index then
      doNotes := true;

    ir := MakeRect(ClientRect.Left + 2, ItemAppearance.Splitter.Height + 4, ClientRect.Right,
      ClientRect.Bottom - ItemAppearance.Splitter.Height - 4);

    pw := 0;
    ph := 0;
    if doNotes then
    begin
      //Picture
      if not picture.Empty then
      begin
        picture.GetImageSizes;
        case FPictureSize of
          psOriginal:
          begin
            pw := Picture.Width;
            ph := Picture.Height;
          end;
          psCustom:
          begin
            pw := FPictureWidth;
            ph := FPictureHeight;
          end;
        end;
      end;

      picr := Bounds(ClientRect.Left + Round(FCurrentTextPos), ItemAppearance.Splitter.Height + 4, Round(ir.Width), Round(ir.Height));

      if FPicturePosition <> ppCustom then
        DrawPicture(g, FCurrentTextOpc, Picture, FPicturePosition, pw, ph, picr, picr.Left, picr.Top)
      else
        DrawPicture(g, FCurrentTextOpc, Picture, FPicturePosition, pw, ph, picr, Round(FcurrentTextPos) + PictureLeft, PictureTop);
      ///
    end;

    //Notes
    if (Notes <> '') and doNotes then
    begin
      with ItemAppearance do
      begin
        ff := TGPFontFamily.Create(FNotesFont.Name);
        if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
        begin
          ff.Free;
          ff := TGPFontFamily.Create('Arial');
        end;

        fs := 0;
        if (fsBold in FNotesFont.Style) then
          fs := fs + 1;
        if (fsItalic in FNotesFont.Style) then
          fs := fs + 2;
        if (fsUnderline in FNotesFont.Style) then
          fs := fs + 4;

        sf := TGPStringFormat.Create;
        f := TGPFont.Create(ff, FNotesFont.Size, fs, UnitPoint);
        g.MeasureString(Notes, Length(Notes), f, ir, sf, sizer);

        ir.Width := ir.Width - sizer.Width - pw;
        ir.Height := ir.Height - sizer.Height;
        if NotesLocation <> plCustom then
          GetTextPosition(x, y, ir, 0, 0, NotesLocation)
        else
        begin
          x := NotesLeft;
          y := NotesTop;
        end;

        pt := MakePoint(ir.X + x + pw + FCurrentTextPos, ir.Y + y);

        b := TGPSolidBrush.Create(MakeColor(FCurrentTextOpc, NotesFont.Color));
        g.DrawString(Notes, length(Notes), f, pt, sf, b);
        b.Free;

        ff.Free;
        sf.Free;
        f.free;
      end;
    end;
    ///

    //Splitters
    with ItemAppearance.Splitter do
    begin
      if Visible and (Index < (Collection as TAdvSmoothMenuItems).Count - 1) and ((Collection as TAdvSmoothMenuItems).Count > 1) then
      begin
        c := MakeColor(FOpacity, FColor);
        cto := MakeColor(FopacityTo, FColorTo);

        rsplit := MakeRect(FItemRect.X + FItemRect.Width, FItemRect.Y + (FItemRect.Height / 2) - (Height / 2), 1, Height / 2);
        bsplit := TGPLinearGradientBrush.Create(MakeRect(rSplit.X - 1, rSplit.Y - 1, rSplit.Width + 2, rSplit.Height + 2), c, cto, LinearGradientModeVertical);
        g.FillRectangle(bsplit, rsplit);
        bSplit.Free;

        rsplit := MakeRect(FItemRect.X + FItemRect.Width, FItemRect.Y + (FItemRect.Height / 2), 1, Height / 2);
        bsplit := TGPLinearGradientBrush.Create(MakeRect(rSplit.X - 1, rSplit.Y - 1, rSplit.Width + 2, rSplit.Height + 2), cto, c, LinearGradientModeVertical);
        g.FillRectangle(bsplit, rsplit);
        bSplit.Free;
      end;
    end;
    ///


    if go > 0 then
    begin
      glowb.Free;
      path.Free;
    end;

    g.Free;
  end;

end;

procedure TAdvSmoothMenuItem.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMenuItem.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMenuItem.SetCaption(const Value: String);
begin
  if FCaption <> value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> value then
  begin
    FEnabled := Value;
    (Collection as TAdvSmoothMenuItems).FOwner.ChangeSelectedItem;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItem.SetNotes(const Value: String);
begin
  if FNotes <> value then
  begin
    FNotes := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItem.SetNotesLeft(const Value: integer);
begin
  if FNotesLeft <> value then
  begin
    FNotesLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItem.SetNotesLocation(
  const Value: TAdvSmoothMenuLocation);
begin
  if FNotesLocation <> value then
  begin
    FNotesLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItem.SetNotesTop(const Value: integer);
begin
  if FNotesTop <> value then
  begin
    FNotesTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItem.SetPicture(const Value: TAdvGDIPPicture);
begin
  if FPicture <> value then
  begin
    FPicture.Assign(value);
    Changed;
  end;
end;

procedure TAdvSmoothMenuItem.SetPictureHeight(const Value: integer);
begin
  if FpictureHeight <> value then
  begin
    FpictureHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItem.SetPictureLeft(const Value: integer);
begin
  if FPictureLeft <> value then
  begin
    FPictureLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItem.SetPicturePosition(const Value: TFillPicturePosition);
begin
  if FPicturePosition <> value then
  begin
    FPicturePosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItem.SetPictureSize(const Value: TFillPictureSize);
begin
  if FPictureSize <> value then
  begin
    FpictureSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItem.SetPictureTop(const Value: integer);
begin
  if FPictureTop <> value then
  begin
    FPictureTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItem.SetPictureWidth(const Value: integer);
begin
  if FpictureWidth <> value then
  begin
    FpictureWidth := Value;
    Changed;
  end;
end;

{ TAdvSmoothMenuItemAppearance }

procedure TAdvSmoothMenuItemAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothMenuItemAppearance then
  begin
    FFont.Assign((Source as TAdvSmoothMenuItemAppearance).Font);
    FSelectedFont.Assign((Source as TAdvSmoothMenuItemAppearance).SelectedFont);
    FHoverFont.Assign((Source as TAdvSmoothMenuItemAppearance).HoverFont);
    FSplitter.Assign((Source as TAdvSmoothMenuItemAppearance).Splitter);
    FGlowColor := (Source as TAdvSmoothMenuItemAppearance).GlowColor;
    FGlowColorSelected := (Source as TAdvSmoothMenuItemAppearance).GlowColorSelected;
    FGlowColorHover := (Source as TAdvSmoothMenuItemAppearance).GlowColorHover;
    FGlowOpacity := (Source as TAdvSmoothMenuItemAppearance).GlowOpacity;
    FGlowOpacitySelected := (Source as TAdvSmoothMenuItemAppearance).GlowOpacitySelected;
    FGlowOpacityHover := (Source as TAdvSmoothMenuItemAppearance).GlowOpacityHover;
    FGlowColorDisabled := (Source as TAdvSmoothMenuItemAppearance).GlowColorDisabled;
    FGlowOpacityDisabled := (Source as TAdvSmoothMenuItemAppearance).GlowOpacityDisabled;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothMenuItemAppearance.Create(AOwner: TAdvSmoothMenu);
begin
  FOwner := AOwner;
  FFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}
  FFont.OnChange := FontChanged;
  FNotesFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FNotesFont.Name := 'Tahoma';
  {$ENDIF}
  FNotesFont.OnChange := FontChanged;
  FSelectedFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FSelectedFont.Name := 'Tahoma';
  {$ENDIF}
  FSelectedFont.OnChange := FontChanged;
  FHoverFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FHoverFont.Name := 'Tahoma';
  {$ENDIF}
  FHoverFont.OnChange := FontChanged;
  FSplitter := TAdvSmoothMenuSplitter.Create(FOwner);
  FSplitter.OnChange := SplitterChanged;

  FGlowColor := clWhite;
  FGlowColorHover := clWhite;
  FGlowColorSelected := clWhite;
  FGlowColorDisabled := clNone;
  FGlowOpacity := 100;
  FGlowOpacityHover := 100;
  FGlowOpacitySelected := 100;
  FGlowOpacityDisabled := 100;
end;

destructor TAdvSmoothMenuItemAppearance.Destroy;
begin
  FFont.Free;
  FNotesFont.Free;
  FSelectedFont.Free;
  FHoverFont.Free;
  Fsplitter.Free;
  inherited;
end;

procedure TAdvSmoothMenuItemAppearance.FontChanged(Sender: TObject);
begin
  FOwner.InitItemRects;
  Changed;
end;

procedure TAdvSmoothMenuItemAppearance.SetFont(const Value: TFont);
begin
  if FFont <> value then
  begin
    FFont.Assign(Value);
    FOwner.InitItemRects;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SetGlowColor(const Value: TColor);
begin
  if FGlowColor <> value then
  begin
    FGlowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SetGlowColorDisabled(
  const Value: TColor);
begin
  if FGlowColorDisabled <> value then
  begin
    FGlowColorDisabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SetGlowColorHover(const Value: TColor);
begin
  if FGlowColorHover <> value then
  begin
    FGlowColorHover := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SetGlowColorSelected(const Value: TColor);
begin
  if FGlowColorSelected <> value then
  begin
    FGlowColorSelected := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SetGlowOpacity(const Value: Byte);
begin
  if FGlowOpacity <> value then
  begin
    FGlowOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SetGlowOpacityDisabled(
  const Value: Byte);
begin
  if FGlowOpacityDisabled <> value then
  begin
    FGlowOpacityDisabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SetGlowOpacityHover(const Value: Byte);
begin
  if FGlowOpacityHover <> value then
  begin
    FGlowOpacityHover := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SetGlowOpacitySelected(
  const Value: Byte);
begin
  if FGlowOpacitySelected <> value then
  begin
    FGlowOpacitySelected := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SetGlowShape(
  const Value: TAdvSmoothMenuGlowShape);
begin
  if FGlowShape <> value then
  begin
    FGlowShape := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SetHoverFont(const Value: TFont);
begin
  if FHoverFont <> value then
  begin
    FHoverFont.Assign(value);
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SetNotesFont(const Value: TFont);
begin
  if FNotesFont <> value then
  begin
    FNotesFont.Assign(value);
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SetSelectedFont(const Value: TFont);
begin
  if FSelectedFont <> value then
  begin
    FSelectedFont.Assign(value);
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SetSplitter(
  const Value: TAdvSmoothMenuSplitter);
begin
  if Fsplitter <> value then
  begin
    Fsplitter.Assign(value);
    Changed;
  end;
end;

procedure TAdvSmoothMenuItemAppearance.SplitterChanged(Sender: TObject);
begin
  Changed;
end;

{ TAdvSmoothMenuAppearance }

procedure TAdvSmoothMenuAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothMenuAppearance then
  begin
    FFill.Assign((Source as TAdvSmoothMenuAppearance).Fill);
    FLineOpacity := (Source as TAdvSmoothMenuAppearance).LineOpacity;
    FLineColor := (Source as TAdvSmoothMenuAppearance).LineColor;
    FLineVisible := (Source as TAdvSmoothMenuAppearance).LineVisible;
    FIndicatorColor := (Source as TAdvSmoothMenuAppearance).IndicatorColor;
    FIndicatorOpacity := (Source as TAdvSmoothMenuAppearance).IndicatorOpacity;
    FIndicatorVisible := (Source as TAdvSmoothMenuAppearance).IndicatorVisible;
    FIndicatorSize := (Source as TAdvSmoothMenuAppearance).IndicatorSize;
    Changed;
  end;
end;

procedure TAdvSmoothMenuAppearance.BackGroundChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMenuAppearance.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothMenuAppearance.Create(AOwner: TAdvSmoothMenu);
begin
  FOwner := AOwner;
  FFill := TGDIPFill.Create;
  FFill.OnChange := BackGroundChanged;
  FLineColor := clWhite;
  FLineOpacity := 255;
  FLineVisible := true;
  FIndicatorColor := clWhite;
  FIndicatorVisible := true;
  FIndicatorOpacity := 255;
  FIndicatorSize := 6;
end;

destructor TAdvSmoothMenuAppearance.Destroy;
begin
  FFill.Free;
  inherited;
end;

procedure TAdvSmoothMenuAppearance.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuAppearance.SetIndicatorColor(const Value: TColor);
begin
  if FIndicatorColor <> value then
  begin
    FIndicatorColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuAppearance.SetIndicatorOpacity(const Value: Byte);
begin
  if FIndicatorOpacity <> value then
  begin
    FIndicatorOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuAppearance.SetIndicatorSize(const Value: Integer);
begin
  if FIndicatorSize <> value then
  begin
    FIndicatorSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuAppearance.SetIndicatorVisible(const Value: Boolean);
begin
  if FIndicatorVisible <> value then
  begin
    FIndicatorVisible := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuAppearance.SetLineColor(const Value: TColor);
begin
  if FLineColor <> value then
  begin
    FLineColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuAppearance.SetLineOpacity(const Value: Byte);
begin
  if FLineOpacity <> value then
  begin
    FLineOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuAppearance.SetLineVisible(const Value: Boolean);
begin
  if FLineVisible <> value then
  begin
    FLineVisible := Value;
    Changed;
  end;
end;

{ TAdvSmoothMenuSplitter }

procedure TAdvSmoothMenuSplitter.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothMenuSplitter then
  begin
    FColor := (Source as TAdvSmoothMenuSplitter).Color;
    FColorTo := (Source as TAdvSmoothMenuSplitter).ColorTo;
    FOpacity := (Source as TAdvSmoothMenuSplitter).Opacity;
    FOpacityTo := (Source as TAdvSmoothMenuSplitter).OpacityTo;
    FVisible := (Source as TAdvSmoothMenuSplitter).Visible;
    FHeight := (Source as TAdvSmoothMenuSplitter).Height;
    Changed;
  end;
end;

procedure TAdvSmoothMenuSplitter.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothMenuSplitter.Create(AOwner: TAdvSmoothMenu);
begin
  FOwner := AOwner;
  FColor := clWhite;
  FColorTo := clWhite;
  FOpacity := 20;
  FOpacityTo := 255;
  FVisible := true;
  FHeight := 28;
end;

destructor TAdvSmoothMenuSplitter.Destroy;
begin
  inherited;
end;

procedure TAdvSmoothMenuSplitter.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuSplitter.SetColorTo(const Value: TColor);
begin
  if FColorTo <> value then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuSplitter.SetHeight(const Value: Integer);
begin
  if FHeight <> value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuSplitter.SetOpacity(const Value: Byte);
begin
  if FOpacity <> value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuSplitter.SetOpacityTo(const Value: Byte);
begin
  if FOpacityTo <> Value then
  begin
    FOpacityTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMenuSplitter.SetVisible(const Value: Boolean);
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
