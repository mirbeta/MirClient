 {**************************************************************************}
{ TAdvSmoothPanel component                                                }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2010 - 2015                                                }
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

unit AdvSmoothPanel;

{$I TMSDEFS.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Forms,
  Comobj, Activex, Math, AdvStyleIF, ImgList, AdvHintInfo,
  GDIPPictureContainer, GDIPFill, IniFiles, AdvGDIP, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  WM_USERPAINT = WM_USER + 102;

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Improved : html rendering
  //          : Fixed : position of line for bottom caption
  // v1.0.0.2 : Fixed : memory leak
  // v1.0.0.3 : Fixed : Issue with component initialization during reparenting
  // v1.0.0.4 : New : Exposed Properties TabOrder and TabStop
  // v1.0.0.5 : Fixed issue with background and Border color = clNone
  // v1.0.1.0 : New : OnMouseLeave and OnMouseEnter events for Delphi 7
  //          : Improved : Cursor
  // v1.0.2.0 : New : OnDraw Event to draw on the panel canvas
  //          : Fixed : WMWindowPosChanged invalidate
  // v1.0.2.1 : Fixed : issue with cursor on controls inside panel
  // v1.0.2.2 : Fixed : Issue with cursor changing on controls
  // v1.0.2.3 : Fixed : Issue with client aligned panel repaint
  // v1.0.3.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.0.4.0 : New : Built-in support for reduced color set for use with terminal servers
  // v1.0.4.1 : Fixed : issue with redrawing child controls on resizing panel
  // v1.0.4.2 : Fixed : issue with drawing child controls on first display
  // v1.0.4.3 : Fixed : issue with expand collaps in advsmoothexpanderpanel
  // v1.0.5.0 : New : Delphi 2010 Touch Support
  //          : Fixed : issue with HTML when collapsing
  // v1.0.6.0 : New : Exposed property Padding
  // v1.0.6.1 : Improved : Cached drawing and Transparent property to control performance
  // v1.0.7.0 : New : Property TextRendering to control text quality with transparency of control
  // v1.0.7.1 : Fixed : Issue with GDIPFill default values
  // v1.0.8.0 : New : Built-in support for Office 2010 colors
  // v1.0.9.0 : New : PictureAlignment in Button
  // v1.0.9.1 : Improved : Only draw buttons in view
  // v1.0.9.2 : Fixed : Border issue
  // v1.0.9.3 : Fixed : Issue with default color start and color end
  // v1.0.9.4 : Fixed : Issue with Cache bitmap memory leak
  // v1.0.9.5 : New : Gesture support from Delphi 2010
  // v1.0.9.6 : Fixed : Issue with rendering html
  // v1.1.0.0 : New : CanMove property to move panel at runtime
  //          : New : Officehint support
  //          : Fixed : Issue with assigning fill.
  // v1.2.0.0 : New : Metro style support
  // v1.2.1.0 : New : OnEndMoveSize event
  // v1.2.1.1 : Improved : Virtual method DrawExpander
  // v1.2.1.2 : Fixed : Issue with painting caption background and fill rounding
  // v1.3.0.0 : New : Windows 8, Office 2013 styles added
  // v1.3.0.1 : Fixed : Do not set size of the shadow as part of the style
  // v1.3.0.2 : Fixed : Issue with roundingtype of caption background
  // v1.3.0.3 : Fixed : Issue with invalid floating point operation
  // v1.3.0.4 : Fixed : Issue with caption background height
  // v1.4.0.0 : New : runtime resizing functionality
  //          : New : Anchor hints
  // v1.4.0.1 : Fixed : Issue with regular hints
  // v1.5.0.0 : New : Windows 10, Office 2016 styles added
  //          : New : Caption.TextWordWrapping and Caption.HTMLWordWrapping properties added
  // v1.5.1.0 : New : Caption.TextAlignment for separate alignment of text in combination with Caption.Location property


type
  TAdvSmoothPanelLocation = (plTopLeft, plTopCenter, plTopRight, plCenterLeft, plCenterCenter, plCenterRight, plBottomLeft, plBottomCenter, plBottomRight, plCustom);

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothPanelTextRenderingHint = (tAntiAlias, tAntiAliasGridFit, tClearType);

  TAdvSmoothPanel = class;

  TAdvSmoothPanelCaption = class(TPersistent)
  private
    FCache: TGPBitmap;
    FOwner: TAdvSmoothPanel;
    FGradientType: TAdvGradientType;
    FLocation: TAdvSmoothPanelLocation;
    FPicture: TAdvGDIPPicture;
    FEndColor: TColor;
    FHatchStyle: THatchStyle;
    FFont: TFont;
    FStartOpacity: Byte;
    FText: String;
    FTop: integer;
    FLeft: integer;
    FEndOpacity: Byte;
    FStartColor: TColor;
    FOnChange: TNotifyEvent;
    FHTMLText: string;
    FHTMLLocation: TAdvSmoothPanelLocation;
    FHTMLTop: integer;
    FHTMLLeft: integer;
    FHTMLShadowColor: TColor;
    FHTMLURLColor: TColor;
    FHTMLShadowOffset: integer;
    FHTMLFont: TFont;
    FLineColor: TColor;
    FLine: Boolean;
    FTextRendering: TAdvSmoothPanelTextRenderingHint;
    FBackgroundColor: TColor;
    FBackgroundColorTo: TColor;
    FTextWordWrapping: Boolean;
    FHTMLWordWrapping: Boolean;
    FTextAlignment: TAlignment;
    procedure SetEndColor(const Value: TColor);
    procedure SetEndOpacity(const Value: Byte);
    procedure SetFont(const Value: TFont);
    procedure SetGradientType(const Value: TAdvGradientType);
    procedure SetHatchStyle(const Value: THatchStyle);
    procedure SetLeft(const Value: integer);
    procedure SetLocation(const Value: TAdvSmoothPanelLocation);
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure SetStartColor(const Value: TColor);
    procedure SetStartOpacity(const Value: Byte);
    procedure SetText(const Value: String);
    procedure SetTop(const Value: integer);
    procedure SetHTMLText(const Value: string);
    procedure SetHTMLLocation(const Value: TAdvSmoothPanelLocation);
    procedure SetHTMLLeft(const Value: integer);
    procedure SetHTMLTop(const Value: integer);
    procedure SetHTMLShadowColor(const Value: TColor);
    procedure SetHTMLShadowOffset(const Value: integer);
    procedure SetHTMLURLColor(const Value: TColor);
    procedure SetHTMLFont(const Value: TFont);
    procedure SetLine(const Value: Boolean);
    procedure SetTextRendering(const Value: TAdvSmoothPanelTextRenderingHint);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBackgroundColorTo(const Value: TColor);
    procedure SetTextWordWrapping(const Value: Boolean);
    procedure SetHTMLWordWrapping(const Value: Boolean);
    procedure SetTextAlignment(const Value: TAlignment);
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure SetLineColor(const Value: TColor); virtual;
  public
    constructor Create(AOwner: TAdvSmoothPanel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToFile(ini: TIniFile; Section: String);
    procedure LoadFromfile(ini: TIniFile; Section: String);
  published
    property Text: String read FText write SetText;
    property Location: TAdvSmoothPanelLocation read FLocation write SetLocation default plTopLeft;
    property HatchStyle: THatchStyle read FHatchStyle write SetHatchStyle default HatchStyleHorizontal;
    property HTMLFont: TFont read FHTMLFont write SetHTMLFont;
    property HTMLText: string read FHTMLText write SetHTMLText;
    property HTMLLocation: TAdvSmoothPanelLocation read FHTMLLocation write SetHTMLLocation default plCenterCenter;
    property HTMLLeft: integer read FHTMLLeft write SetHTMLLeft default 0;
    property HTMLTop: integer read FHTMLTop write SetHTMLTop default 0;
    property HTMLURLColor: TColor read FHTMLURLColor write SetHTMLURLColor default clBlue;
    property HTMLShadowColor: TColor read FHTMLShadowColor write SetHTMLShadowColor default clGray;
    property HTMLShadowOffset: integer read FHTMLShadowOffset write SetHTMLShadowOffset default 5;
    property Font: TFont read FFont write SetFont;
    property Left: integer read FLeft write SetLeft default 0;
    property Top: integer read FTop write SetTop default 0;
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clNone;
    property BackgroundColorTo: TColor read FBackgroundColorTo write SetBackgroundColorTo default clNone;
    property ColorStart: TColor read FStartColor write SetStartColor default $00B0721C;
    property ColorEnd: TColor read FEndColor write SetEndColor default $009F661A;
    property OpacityStart: Byte read FStartOpacity write SetStartOpacity default 255;
    property OpacityEnd: Byte read FEndOpacity write SetEndOpacity default 255;
    property GradientType: TAdvGradientType read FGradientType write SetGradientType default gtVertical;
    property LineColor: TColor read FLineColor write SetLineColor default $00B0721C;
    property Line: Boolean read FLine write SetLine default true;
    property TextRendering: TAdvSmoothPanelTextRenderingHint read FTextRendering write SetTextRendering default tAntiAliasGridFit;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property TextWordWrapping: Boolean read FTextWordWrapping write SetTextWordWrapping default True;
    property TextAlignment: TAlignment read FTextAlignment write SetTextAlignment default taLeftJustify;
    property HTMLWordWrapping: Boolean read FHTMLWordWrapping write SetHTMLWordWrapping default True;
  end;

  TAdvSmoothPanelAnchorClickEvent = procedure(Sender: TObject; Anchor: String) of object;

  TAdvSmoothPanelDrawEvent = procedure(Sender: TObject; Canvas: TCanvas; Rect: TRect) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothPanel = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FOldPos: TPoint;
    FMouseDown: Boolean;
    refreshcache: Boolean;
    FCache: TGPBitmap;
    FoldCursor: TCursor;
    FDesignTime, FOnAnchor: Boolean;
    FHtmlr: TRect;
    FCaption: TAdvSmoothPanelCaption;
    FOnAnchorClick: TAdvSmoothPanelAnchorClickEvent;
    FContainer: TGDIPPictureContainer;
    FImages: TCustomImageList;
    FFill: TGDIPFill;
    FConstructed: boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnDraw: TAdvSmoothPanelDrawEvent;
    FTransparent: Boolean;
    FCanMove: Boolean;
    FOfficeHint: TAdvHintInfo;
    FOnEndMoveSize: TNotifyEvent;
    FResizable: Boolean;
    FResizeHandle: Boolean;
    FResizeHandleSize: Integer;
    FResizeHandleColor: TColor;
    FResizeHandleOpacity: Byte;
    function GetVersion: String;
    procedure SetVersion(const Value: String);
    procedure SetCaption(const Value: TAdvSmoothPanelCaption);
    procedure SetFill(const Value: TGDIPFill);
    procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    function GetCursorEx: TCursor;
    procedure SetCursorEx(const Value: TCursor);
    function GetDrawingCanvas: TCanvas;
    procedure SetTransparent(const Value: Boolean);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetResizable(const Value: Boolean);
    procedure SetResizeHandle(const Value: Boolean);
    procedure SetResizeHandleColor(const Value: TColor);
    procedure SetResizeHandleSize(const Value: Integer);
    procedure SetResizeHandleOpacity(const Value: Byte);
  protected
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
    procedure CaptionChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure DoAnchorClick(Sender: TObject; Anchor: String);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure GetRoundPath(var gp: TGPGraphicsPath; r: TGPRectF; depth: Integer);
    procedure GetPosition(var x, y: integer; rectangle: TGPRectF; objectwidth, objectheight: integer; location: TAdvSmoothPanelLocation);
    procedure InitPreview;
    procedure GDIPPaint(g: TGPGraphics); overload; virtual;
    {$IFDEF DIRECT2D}
    procedure GDIPPaint(g: TDirect2DCanvas); overload; virtual;
    {$ENDIF}
    function GetVersionNr: integer; virtual;
    function GetShadowSize: integer;
    function InMoveArea(pt: TPoint): boolean; virtual;
    function GetResizeRect: TGPRectF;
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    function GetAnchorAt(X, Y: integer; Focus: Boolean = False): String;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    property ACanvas: TCanvas read GetDrawingCanvas;
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeId: String;
  published
    property CanMove: Boolean read FCanMove write FCanMove default False;
    property Resizable: Boolean read FResizable write SetResizable default False;
    property ResizeHandle: Boolean read FResizeHandle write SetResizeHandle default True;
    property ResizeHandleColor: TColor read FResizeHandleColor write SetResizeHandleColor default clBlack;
    property ResizeHandleOpacity: Byte read FResizeHandleOpacity write SetResizeHandleOpacity default 255;
    property ResizeHandleSize: Integer read FResizeHandleSize write SetResizeHandleSize default 17;
    property Transparent: Boolean read FTransparent write SetTransparent default true;
    property Caption: TAdvSmoothPanelCaption read FCaption write SetCaption;
    property Fill: TGDIPFill read FFill write SetFill;
    property Images: TCustomImageList read FImages write FImages;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property PictureContainer: TGDIPPictureContainer read FContainer write FContainer;
    property Version: String read GetVersion write SetVersion;
    property OnAnchorClick: TAdvSmoothPanelAnchorClickEvent read FOnAnchorClick write FOnAnchorClick;
    property OnDraw: TAdvSmoothPanelDrawEvent read FOnDraw write FOnDraw;
    property Cursor: TCursor read GetCursorEx write SetCursorEx;

    procedure Resize; override;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;

    property Align;
    property Anchors;
    property Constraints;
    property DragMode;
    property DragCursor;
    property DragKind;
    property OnCanResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property PopupMenu;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property OnClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property Visible;
    property ShowHint;
    property OnEndMoveSize: TNotifyEvent read FOnEndMoveSize write FOnEndMoveSize;
    property TabOrder;
    property TabStop;
    {$IFDEF DELPHI2006_LVL}
    property Padding;
    {$ENDIF}
   {$IFDEF DELPHI_TOUCH}
     property OnGesture;
     property Touch;
   {$ENDIF}
  end;

implementation

uses
  CommCtrl, ShellApi;

{$IFNDEF DELPHI7_LVL}

const
  CS_DROPSHADOW = $00020000;

{$ENDIF}

{$i GDIPHTMLEngine.pas}

function PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
begin
  Result := ((pt.X >= r.X) and (pt.X <= r.X + r.Width)) and
     ((pt.Y >= r.Y) and (pt.Y <= r.Y + r.Height));
end;

procedure TAdvSmoothPanel.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothPanel) then
  begin
    FCaption := (Source as TAdvSmoothPanel).Caption;
    FFill.Assign((Source as TAdvSmoothPanel).Fill);
    FTransparent := (Source as TAdvSmoothPanel).Transparent;
    Changed;
  end;
end;

procedure TAdvSmoothPanel.CaptionChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothPanel.Changed;
begin
  RefreshCache := true;
  Invalidate;
end;

procedure TAdvSmoothPanel.CMHintShow(var Message: TMessage);
begin
  with TCMHintShow(Message).HintInfo^ do
  begin
    HintStr := GetAnchorAt(cursorPos.x,cursorpos.y, True);
    if HintStr = '' then
      HintStr := Hint;
    ReshowTimeout := 0;
  end;
end;

procedure TAdvSmoothPanel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TAdvSmoothPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

constructor TAdvSmoothPanel.Create(AOwner: TComponent);
begin
  FConstructed := false;
  inherited;
  FTransparent := true;
  refreshcache := true;
  FResizable := False;
  FResizeHandleSize := 17;
  FResizeHandleOpacity := 255;
  FResizeHandleColor := clBlack;
  FResizeHandle := True;
  FCanMove := False;
  ControlStyle := ControlStyle + [csAcceptsControls];

  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  DoubleBuffered := true;
  Cursor := crDefault;
  Width := 256;
  Height := 128;
  FCaption := TAdvSmoothPanelCaption.Create(self);
  FCaption.OnChange := CaptionChanged;
  FOfficeHint := TAdvHintInfo.Create;
  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);
end;

procedure TAdvSmoothPanel.CreateParams(var Params: TCreateParams);
begin
  { call the create of the params }
  inherited CreateParams(Params);

  with Params do
  begin
    Style := Style and not WS_BORDER;
      //ExStyle := ExStyle or WS_EX_CLIENTEDGE;
  end;

  //with Params do
  //  WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
{
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
     ((Win32MajorVersion > 5) or
      ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
        if Params.WindowClass.Style and CS_DROPSHADOW <> 0 then
          Params.WindowClass.Style := Params.WindowClass.Style - CS_DROPSHADOW;
}
end;

procedure TAdvSmoothPanel.CreateWnd;
begin
  inherited;
  if FConstructed then
    Exit;
    
  if FDesignTime then
    InitPreview;

  FConstructed := true;    
end;

procedure TAdvSmoothPanel.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

destructor TAdvSmoothPanel.Destroy;
begin
  if Assigned(FCache) then
    FCache.Free;

  FFill.Free;
  FCaption.Free;
  FOfficeHint.Free;
  inherited;
end;

procedure TAdvSmoothPanel.DoAnchorClick(Sender: TObject; Anchor: String);
begin
  if Assigned(FOnAnchorClick) then
    FOnAnchorClick(Sender, Anchor);
end;

function Choose(A,B: integer; choice: boolean): integer;
begin
  if choice then
    Result := B
  else
    Result := A;
end;

procedure TAdvSmoothPanel.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothPanel.Paint;
var
  g, gbmp: TGPGraphics;
  {$IFDEF DIRECT2D}
  ca: TDirect2DCanvas;
  {$ENDIF}
  chk: Boolean;
begin
  if ((Self.Width > 1) and (Self.Height > 1)) then
  begin
    {$IFDEF DIRECT2D}
    chk := TDirect2DCanvas.Supported;
    {$ENDIF}
    {$IFNDEF DIRECT2D}
    chk := False;
    {$ENDIF}

    if chk then
    begin
      {$IFDEF DIRECT2D}
      ca := TDirect2DCanvas.Create(Canvas, Bounds(0, 0, Width, Height));

      ca.RenderTarget.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
      case Caption.TextRendering of
        tAntiAlias: ca.RenderTarget.SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_DEFAULT);
        tAntiAliasGridFit: ca.RenderTarget.SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_ALIASED);
        tClearType: ca.RenderTarget.SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE);
      end;

      ca.BeginDraw;
      GDIPPaint(ca);
      ca.EndDraw;
      ca.Free;
      {$ENDIF}
    end
    else
    begin
      if refreshcache then
      begin
        refreshcache := false;
        if Assigned(FCache) then
          FCache.Free;

        FCache := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
        gbmp := TGPGraphics.Create(FCache);
        gbmp.SetSmoothingMode(SmoothingModeAntiAlias);
        case Caption.TextRendering of
          tAntiAlias: gbmp.SetTextRenderingHint(TextRenderingHintAntiAlias);
          tAntiAliasGridFit: gbmp.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
          tClearType: gbmp.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
        end;
        GDIPPaint(gbmp);
        gbmp.Free;
      end;

      g := TGPGraphics.Create(Canvas.Handle);
      g.SetSmoothingMode(SmoothingModeAntiAlias);
      g.DrawImage(FCache, 0, 0);
      g.free;
    end;
  end;

  if Assigned(FOnDraw) then
    FOnDraw(Self, Canvas, ClientRect);

end;

//fix for StreamAdapter with PNG
type
  TFixedStreamAdapter = class(TStreamAdapter)
  public
    {$IFDEF DELPHIXE8_LVL}
    function Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult; override; stdcall;
    {$ELSE}
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult; override; stdcall;
    {$ENDIF}
  end;

{$IFDEF DELPHIXE8_LVL}
function TFixedStreamAdapter.Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult;
{$ELSE}
function TFixedStreamAdapter.Stat(out statstg: TStatStg; grfStatFlag: Integer): HResult;
{$ENDIF}
begin
  Result := inherited Stat(statstg, grfStatFlag);
  statstg.pwcsName := nil;
end;
//fix for StreamAdapter with PNG

procedure TAdvSmoothPanel.GDIPPaint(g: TGPGraphics);
var
  r,tr: TGPRectF;
  th, tw, x, y: Integer;
  htmlr, hr: TRect;
  a, s, k: String;
  XSize, YSize, l, m: integer;
  ff: TGPFontFamily;
  f: TGPFont;
  p: TGPPen;
  fs, pw: integer;
  sf: TGPStringFormat;
  sizeRect: TGPRectF;
  xs, ys, texth, textw: Single;
  textb: TGPBrush;
  start, stop: TGPColor;
  gppointf: TGPPointF;
  gpimg: TGPImage;
  st: TStream;
  sta: TFixedStreamAdapter;
  ch: integer;
  AFill: TGDIPFIll;
  rs: TGPRectF;
begin
  ////Panel Background///
  if (Fill.BorderColor = clNone) or (Fill.BorderWidth = 0) then
    r := Fill.Fill(g, MakeRect(-1, -1, Width+1, Height+1))
  else
    r := Fill.Fill(g, MakeRect(0, 0, Width - 1, Height - 1));



  ///////////////////////
  ///
  texth := 0;
  ////Panel Caption///
  if Caption.Text <> '' then
  begin
    ff := TGPFontFamily.Create(Caption.Font.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    fs := 0;
    if (fsBold in Caption.Font.Style) then
      fs := fs + 1;
    if (fsItalic in Caption.Font.Style) then
      fs := fs + 2;
    if (fsUnderline in Caption.Font.Style) then
      fs := fs + 4;

    tr := r;
    pw := 0;

    if Assigned(Fill.Picture) and not Fill.Picture.Empty then
    begin
      case Fill.PicturePosition of
      ppTopLeft:
        if (Caption.Location in [plTopLeft, plTopRight]) then
        begin
          tr.X := tr.X + Choose(Fill.PictureWidth, Fill.Picture.Width, Fill.PictureSize = psOriginal);
          pw := Choose(Fill.PictureWidth, Fill.Picture.Width, Fill.PictureSize = psOriginal);
        end;
      ppTopRight:
        if (Caption.Location in [plTopRight, plTopLeft]) then
        begin
          tr.Width := tr.Width - Choose(Fill.PictureWidth, Fill.Picture.Width, Fill.PictureSize = psOriginal);
        end;
      ppBottomLeft:
        if (Caption.Location in [plBottomLeft, plBottomRight]) then
        begin
          tr.X := tr.X + Choose(Fill.PictureWidth, Fill.Picture.Width, Fill.PictureSize = psOriginal);
          pw := Choose(Fill.PictureWidth, Fill.Picture.Width, Fill.PictureSize = psOriginal)
        end;
      ppBottomRight:
        if (Caption.Location in [plBottomRight, plBottomLeft]) then
          tr.Width := tr.Width - Choose(Fill.PictureWidth, Fill.Picture.Width, Fill.PictureSize = psOriginal);
      end;
    end;

    sf := TGPStringFormat.Create;
    f := TGPFont.Create(ff, Caption.Font.Size , fs, UnitPoint);
    if not Caption.TextWordWrapping then
      sf.SetFormatFlags(StringFormatFlagsNoWrap);

    try
      sizerect := MakeRect(0, 0, 0, 0);
      g.MeasureString(Caption.Text, Length(Caption.Text), f, MakeRect(tr.x, tr.y, tr.Width - pw, tr.Height), sf, sizerect);
    except
    end;

    tw := Round(sizerect.Width);
    th := Round(sizerect.Height);

    if Caption.Location <> plCustom then
      GetPosition(x, y, MakeRect(tr.X, tr.Y, tr.Width, tr.Height), tw, th, Caption.Location)
    else
    begin
      x := Caption.Left;
      y := Caption.Top;
    end;

    xs := x;
    ys := y;
    textw := sizeRect.Width;
    texth := sizeRect.Height;
    textb := nil;

    if Caption.BackgroundColor <> clNone then
    begin
      AFill := TGDIPFill.Create;
      try
        AFill.Color := Caption.BackgroundColor;
        AFill.ColorTo := Caption.BackgroundColorTo;
        AFill.ColorMirror := clNone;
        AFill.ColorMirrorTo := clNone;
        AFill.Rounding := Fill.Rounding;
        AFill.RoundingType := rtNone;
        case Caption.Location of
          plTopLeft, plTopCenter, plTopRight, plCustom:
          begin
            if Fill.RoundingType in [rtTop, rtBoth] then
              AFill.RoundingType := rtTop;
          end;
          plBottomLeft, plBottomCenter, plBottomRight:
          begin
            if Fill.RoundingType in [rtBottom, rtBoth] then
              AFill.RoundingType := rtBottom;
          end;
        end;
        AFill.BorderWidth := 0;
        if Caption.Location = plCustom then
          AFill.Fill(g, MakeRect(0, 0, tr.Width - pw, Texth + y))
        else
          AFill.Fill(g, MakeRect(0, y - 1, tr.Width - pw, Texth + 1));

      finally
        AFill.Free;
      end;
    end;

    start := MakeColor(Caption.OpacityStart, Caption.ColorStart);
    if Caption.ColorEnd = clNone then
      stop := start
    else
      stop := MakeColor(Caption.OpacityEnd, Caption.ColorEnd);

    gppointf := MakePoint(xs, ys);
    sizeRect.X := gppointf.X;
    sizeRect.Y := gppointf.Y;

    case Caption.GradientType of
      gtSolid: textb := TGPSolidBrush.Create(start);
      gtVertical: textb := TGPLinearGradientBrush.Create(MakePoint(xs, ys), MakePoint(xs, ys + texth), start, stop);
      gtHorizontal: textb := TGPLinearGradientBrush.Create(MakePoint(xs, ys), MakePoint(xs + textw, ys), start, stop);
      gtForwardDiagonal: textb := TGPLinearGradientBrush.Create(MakePoint(xs, ys), MakePoint(xs + textw, ys + texth), start, stop);
      gtBackwardDiagonal: textb := TGPLinearGradientBrush.Create(MakePoint(xs, ys + texth), MakePoint(xs + textw, ys), stop, start);
      gtHatch: textb := TGPHatchBrush.Create(Caption.HatchStyle, start, stop);
      gtTexture:
      begin
        if not Caption.Picture.Empty then
        begin
          st := TMemoryStream.Create;
          Caption.Picture.SaveToStream(st);
          sta := TFixedStreamAdapter.Create(st);
          gpimg := TGPImage.Create(sta);
          textb := TGPTextureBrush.Create(gpimg, WrapModeTile);
          g.DrawString(Caption.Text, Length(Caption.Text), f, MakeRect(gppointf.X, gppointf.Y, Round(tr.Width - 2 - pw), Height - 1 - GetShadowSize), sf, textb);
          st.free;
          gpimg.free;
        end;
      end;
      gtNone: ;
    end;

    case Caption.TextAlignment of
      taRightJustify: sf.SetAlignment(StringAlignmentFar);
      taCenter: sf.SetAlignment(StringAlignmentCenter);
    end;

    if Caption.GradientType <> gtTexture then
      g.DrawString(Caption.Text, Length(Caption.Text), f, MakeRect(gppointf.X, gppointf.Y, textw, texth), sf, textb);

    if textb <> nil then
      textb.free;

    if (Caption.LineColor <> clNone) and Caption.Line then
    begin
      //g.SetSmoothingMode(SmoothingModeNone);
      p := TGPPen.Create(ColorToARGB(Caption.LineColor), 1);

      if Caption.Location in [plBottomCenter, plBottomLeft, plBottomRight] then
        g.DrawLine(p, Round(tr.X + 2), gpPointf.Y - 2, Round(tr.Width - 2), gpPointf.Y - 2)
      else
        g.DrawLine(p, Round(tr.X + 2), gpPointf.Y + texth, Round(tr.Width - 2), gpPointf.Y + texth);
      p.Free;

      //g.SetSmoothingMode(SmoothingModeAntiAlias);
    end;

    sf.Free;
    ff.Free;
    f.Free;
  end;

  ////////////////////
  //Panel HTML Text//
  ////////////////////
  if Caption.HTMLText <> '' then
  begin
    ch := Round(texth);
    if Caption.HTMLLocation = plCustom then
      htmlr := Rect(0, ch, Width - GetShadowSize - (2 * Fill.BorderWidth) - Caption.HTMLLeft, 10000)
    else
      htmlr := Rect(Caption.HTMLLeft, ch, Width - GetShadowSize - (Fill.BorderWidth * 2), 10000);

    //HTMLDrawEx(Canvas,Caption.HTMLText,htmlr,nil, 0,0,-1,-1,Caption.HTMLShadowOffset,False,true,false,false,
    //  False,False,true,1.0,Caption.HTMLURLColor,clNone,clNone,Caption.HTMLShadowColor,a,s,k,XSize,YSize,l,m,hr,nil, FContainer,2);

    HTMLDrawGDIP(g, Caption.HTMLFont, Caption.HTMLText, htmlr, FImages, 0, 0, -1, -1, Caption.HTMLShadowOffset, False, True, False, False,
      False, False, Caption.HTMLWordWrapping, 1.0, Caption.HTMLURLColor, clNone, clNone, Caption.HTMLShadowColor, a, s, k, XSize, YSize, l, m, hr, nil, FContainer, 2);

    r.Height := r.Height - ch;

    if Caption.HTMLLocation <> plCustom then
      GetPosition(x, y, r, XSize, YSize, Caption.HTMLLocation)
    else
    begin
      x := Caption.HTMLLeft;
      y := Caption.HTMLTop;
      if (x + XSize > Width - GetShadowSize - Fill.BorderWidth * 2) then
        XSize := Width - GetShadowSize - Fill.BorderWidth * 2 - X;
    end;

    if Caption.HTMLLocation <> plCustom then
      htmlr := Bounds(x, y + ch, XSize, YSize)
    else
      htmlr := Bounds(x, y, Width - GetShadowSize - 2 * Fill.BorderWidth - Caption.HTMLLeft , YSize);


//    if YSize < Height then
    begin
      HTMLDrawGDIP(g, Caption.HTMLFont, Caption.HTMLText,htmlr, FImages, 0, 0, -1, -1,Caption.HTMLShadowOffset, False, False, False, False,
        False, False, Caption.HTMLWordWrapping,1.0,Caption.HTMLURLColor,clNone,clNone,Caption.HTMLShadowColor,a,s,k,XSize,YSize,l,m,hr,nil, FContainer,2);
    end;

    Fhtmlr := htmlr;
  end;

  if Resizable and ResizeHandle then
  begin
    rs := GetResizeRect;
    p := TGPPen.Create(MakeColor(ResizeHandleOpacity, ResizeHandleColor));
    g.DrawLine(p, rs.X + (rs.Width / 4 * 3), rs.Y + rs.Height, rs.X + rs.Width, rs.Y + (rs.Height / 4 * 3));
    g.DrawLine(p, rs.X + (rs.Width / 2), rs.Y + rs.Height, rs.X + rs.Width, rs.Y + (rs.Height / 2));
    g.DrawLine(p, rs.X + (rs.Width / 4), rs.Y + rs.Height, rs.X + rs.Width, rs.Y + (rs.Height / 4));
    p.Free;
  end;
end;

{$IFDEF DIRECT2D}
procedure TAdvSmoothPanel.GDIPPaint(g: TDirect2DCanvas);
var
  r,tr: TD2DRectF;
  th, tw, x, y: Integer;
  sizeRect: TD2DRectF;
  xs, ys, texth, textw: Single;
  pw: integer;
  start, stop: TD2D1ColorF;
  txtformat: IDWriteTextFormat;
  GrBrush2D: TDirect2DBrush;
  solGrgBrush2D: ID2D1SolidColorBrush;
  linBrush2D: ID2D1LinearGradientBrush;
  linBrush2DProp: TD2D1LinearGradientBrushProperties;
  linGrgBrush2DStop: ID2D1GradientStopCollection;
  linGrgBrush2DStops: array[0..1] of TD2D1GradientStop;
  mode: LinearGradientMode;
  txtLayout: IDWriteTextLayout;
  txtmet: DWRITE_TEXT_METRICS;
  gppointf: TD2DPoint2f;
  bmp: ID2D1Bitmap;
  bmporig: TBitmap;
  bmpBrushProps: TD2D1BitmapBrushProperties;
  bmpBrush: ID2D1BitmapBrush;
  penGrBrush2D: ID2D1Brush;
begin
  if (Fill.BorderColor = clNone) or (Fill.BorderWidth = 0) then
    r := Fill.Fill2D(g, D2D1RectF(-1, -1, Width+1, Height+1))
  else
    r := Fill.Fill2D(g, D2D1RectF(0, 0, Width - 1, Height - 1));

  if Caption.Text <> '' then
  begin
    txtformat := Create2DTextFormat(Caption.Font, taLeftJustify, taVerticalCenter, True, False);

    tr := r;

    pw := 0;

    if Assigned(Fill.Picture) and not Fill.Picture.Empty then
    begin
      case Fill.PicturePosition of
      ppTopLeft:
        if (Caption.Location in [plTopLeft, plTopRight]) then
        begin
          tr.left := tr.left + Choose(Fill.PictureWidth, Fill.Picture.Width, Fill.PictureSize = psOriginal);
          pw := Choose(Fill.PictureWidth, Fill.Picture.Width, Fill.PictureSize = psOriginal);
        end;
      ppTopRight:
        if (Caption.Location in [plTopRight, plTopLeft]) then
        begin
          tr.right := tr.right - Choose(Fill.PictureWidth, Fill.Picture.Width, Fill.PictureSize = psOriginal);
        end;
      ppBottomLeft:
        if (Caption.Location in [plBottomLeft, plBottomRight]) then
        begin
          tr.left := tr.left + Choose(Fill.PictureWidth, Fill.Picture.Width, Fill.PictureSize = psOriginal);
          pw := Choose(Fill.PictureWidth, Fill.Picture.Width, Fill.PictureSize = psOriginal)
        end;
      ppBottomRight:
        if (Caption.Location in [plBottomRight, plBottomLeft]) then
          tr.right := tr.right - Choose(Fill.PictureWidth, Fill.Picture.Width, Fill.PictureSize = psOriginal);
      end;
    end;

    DWriteFactory.CreateTextLayout(pChar(Caption.Text), Length(Caption.Text), txtformat, Max(0, tr.right - pw), Max(0, tr.bottom - tr.top), txtLayout);
    txtLayout.GetMetrics(txtmet);

    sizerect := D2D1RectF(0, 0, txtmet.widthIncludingTrailingWhitespace + 6, txtmet.height);

    tw := Round(sizerect.Right - sizerect.Left);
    th := Round(sizerect.Bottom - sizerect.Top);

    if Caption.Location <> plCustom then
      GetPosition(x, y, MakeRect(tr.left, tr.top, tr.right - tr.left, tr.bottom - tr.top), tw, th, Caption.Location)
    else
    begin
      x := Caption.Left;
      y := Caption.Top;
    end;

    xs := x;
    ys := y;
    textw := tw;
    texth := th;

    start := D2D1ColorF(Caption.ColorStart, Caption.OpacityStart / 255);
    if Caption.ColorEnd = clNone then
      stop := start
    else
      stop := D2D1ColorF(Caption.ColorEnd, Caption.OpacityEnd / 255);

    gppointf := D2D1PointF(xs, ys);
    sizeRect.left := gppointf.X;
    sizeRect.right := sizerect.right + gppointf.x;
    sizeRect.top := gppointf.Y;
    sizeRect.bottom := sizerect.Bottom + gppointf.y;

    GrBrush2D := TDirect2DBrush.Create(g);

    case Caption.GradientType of
      gtSolid:
      begin
        g.RenderTarget.CreateSolidColorBrush(start, nil, solGrgBrush2D);
      GrBrush2D.Handle := solGrgBrush2D;
      end;
      gtVertical, gtHorizontal, gtForwardDiagonal, gtBackwardDiagonal:
      begin
         mode := LinearGradientModeVertical;
         if Caption.GradientType = gtHorizontal then
            mode := LinearGradientModeHorizontal;
         if Caption.GradientType = gtForwardDiagonal then
            mode := LinearGradientModeForwardDiagonal;
         if Caption.GradientType = gtBackwardDiagonal then
            mode := LinearGradientModeBackwardDiagonal;


         case mode of
           LinearGradientModeHorizontal:
           begin
             linBrush2DProp.startPoint := D2D1PointF(xs, ys);
             linBrush2DProp.endPoint := D2D1PointF(xs + textw, ys);
           end;
           LinearGradientModeVertical:
           begin
             linBrush2DProp.startPoint := D2D1PointF(xs, ys);
             linBrush2DProp.endPoint := D2D1PointF(xs, ys + texth);
           end;
           LinearGradientModeForwardDiagonal:
           begin
             linBrush2DProp.startPoint := D2D1PointF(xs, ys);
             linBrush2DProp.endPoint := D2D1PointF(xs + textw, ys + texth);
           end;
           LinearGradientModeBackwardDiagonal:
           begin
             linBrush2DProp.startPoint := D2D1PointF(xs, ys + texth);
             linBrush2DProp.endPoint := D2D1PointF(xs + textw, ys);
           end;
         end;

         linGrgBrush2DStops[0].position := 0;
         linGrgBrush2DStops[1].position := 1;

         if mode = LinearGradientModeBackwardDiagonal then
         begin
           linGrgBrush2DStops[0].color := stop;
           linGrgBrush2DStops[1].color := start;
         end
         else
         begin
           linGrgBrush2DStops[0].color := start;
           linGrgBrush2DStops[1].color := stop;
         end;

         g.RenderTarget.CreateGradientStopCollection(@linGrgBrush2DStops, Length(linGrgBrush2DStops), D2D1_GAMMA_2_2, D2D1_EXTEND_MODE_WRAP, linGrgBrush2DStop);
         g.RenderTarget.CreateLinearGradientBrush(linBrush2DProp, nil, linGrgBrush2DStop, linBrush2D);

         GrBrush2D.Handle := linBrush2D;
      end;

      gtTexture:
      begin
        if not Caption.Picture.Empty then
        begin
          bmporig := TBitmap.Create;
          bmporig.Width := self.Caption.Picture.Width;
          bmporig.Height := self.Caption.Picture.Height;
          self.Caption.Picture.Draw(bmporig.Canvas, Bounds(0, 0, bmpOrig.Width, bmporig.Height));
          bmp := g.CreateBitmap(bmporig);
          bmpBrushProps := defaultBitmapbrushProperties;
          g.RenderTarget.CreateBitmapBrush(bmp, @bmpBrushProps, nil, bmpBrush);
          GrBrush2D.Handle := bmpBrush;
          bmporig.Free;
        end;
      end;
    end;

    g.RenderTarget.DrawText(pChar(Caption.Text), Length(Caption.Text), txtformat, sizeRect, GrBrush2D.Handle);

    GrBrush2D.Free;

    if (Caption.LineColor <> clNone) and Caption.Line then
    begin

      penGrBrush2D := g.CreateBrush(Caption.LineColor);

      if Caption.Location in [plBottomCenter, plBottomLeft, plBottomRight] then
        g.RenderTarget.DrawLine(D2D1PointF(tr.left + 2, gpPointf.Y - 2), D2D1PointF(tr.right - 2, gpPointf.Y - 2), penGrBrush2D)
      else
        g.RenderTarget.DrawLine(D2D1PointF(tr.left + 2, gpPointf.Y + texth), D2D1PointF(tr.right - 2, gpPointf.Y + texth), penGrBrush2D);
    end;

  end;

  {
  ////////////////////
  //Panel HTML Text//
  ////////////////////
  if Caption.HTMLText <> '' then
  begin
    ch := Round(texth);
    if Caption.HTMLLocation = plCustom then
      htmlr := Rect(0, ch, Width - GetShadowSize - (2 * Fill.BorderWidth) - Caption.HTMLLeft, 10000)
    else
      htmlr := Rect(Caption.HTMLLeft, ch, Width - GetShadowSize - (Fill.BorderWidth * 2), 10000);

    //HTMLDrawEx(Canvas,Caption.HTMLText,htmlr,nil, 0,0,-1,-1,Caption.HTMLShadowOffset,False,true,false,false,
    //  False,False,true,1.0,Caption.HTMLURLColor,clNone,clNone,Caption.HTMLShadowColor,a,s,k,XSize,YSize,l,m,hr,nil, FContainer,2);

    HTMLDrawGDIP(g, Caption.HTMLFont, Caption.HTMLText, htmlr, FImages, 0, 0, -1, -1, Caption.HTMLShadowOffset, False, True, False, False,
      False, False, True, 1.0, Caption.HTMLURLColor, clNone, clNone, Caption.HTMLShadowColor, a, s, k, XSize, YSize, l, m, hr, nil, FContainer, 2);

    r.Height := r.Height - ch;
    if Caption.HTMLLocation <> plCustom then
      GetPosition(x, y, r, XSize, YSize, Caption.HTMLLocation)
    else
    begin
      x := Caption.HTMLLeft;
      y := Caption.HTMLTop;
      if (x + XSize > Width - GetShadowSize - Fill.BorderWidth * 2) then
        XSize := Width - GetShadowSize - Fill.BorderWidth * 2 - X;
    end;

    if Caption.HTMLLocation <> plCustom then
      htmlr := Bounds(x, y + ch, XSize, YSize)
    else
      htmlr := Bounds(x, y, Width - GetShadowSize - 2 * Fill.BorderWidth - Caption.HTMLLeft , YSize);
//        htmlr := Bounds(x, y, XSize, YSize);


    //HTMLDrawEx(Canvas,Caption.HTMLText,htmlr,nil, 0,0,-1,-1,Caption.HTMLShadowOffset,False,false,false,false,
    //  False,False,true,1.0,Caption.HTMLURLColor,clNone,clNone,Caption.HTMLShadowColor,a,s,k,XSize,YSize,l,m,hr,nil, FContainer,2);

    if YSize < Height then
    begin
      HTMLDrawGDIP(g, Caption.HTMLFont, Caption.HTMLText,htmlr, FImages, 0, 0, -1, -1,Caption.HTMLShadowOffset, False, False, False, False,
        False, False, True,1.0,Caption.HTMLURLColor,clNone,clNone,Caption.HTMLShadowColor,a,s,k,XSize,YSize,l,m,hr,nil, FContainer,2);
    end;

    Fhtmlr := htmlr;
  end;
  }
end;
{$ENDIF}

function TAdvSmoothPanel.GetAnchorAt(X, Y: integer; Focus: Boolean = False): String;
var
  a, s, k: String;
  XSize, YSize: integer;
  l, m: integer;
  hr: TRect;
  g: TGPGraphics;
begin
  if PtInRect(FHtmlr, Point(X, Y)) then
  begin

    g := TGPGraphics.Create(Canvas.Handle);

    HTMLDrawGDIP(g,Caption.HTMLFont, Caption.HTMLText,Fhtmlr,FImages, X,Y,-1,-1,Caption.HTMLShadowOffset,true,false,false,false,
      False,False,Caption.HTMLWordWrapping,1.0,Caption.HTMLURLColor,clNone,clNone,Caption.HTMLShadowColor,a,s,k,XSize,YSize,l,m,hr,nil, FContainer,2);

    g.Free;
  end;

  if Focus then
    Result := k
  else
    Result :=  a;
end;

function TAdvSmoothPanel.GetDrawingCanvas: TCanvas;
begin
  Result := Self.Canvas;
end;

function TAdvSmoothPanel.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothPanel.GetCursorEx: TCursor;
begin
  Result := inherited Cursor;
end;

procedure TAdvSmoothPanel.GetPosition(var x, y: integer; rectangle: TGPRectF; objectwidth, objectheight: integer; location: TAdvSmoothPanelLocation);
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
      x := Round(rectangle.X);
      y := Round(rectangle.Y);
    end;
    plTopRight:
    begin
      x := Max(Round(rectangle.X), w - tw);
      y := Round(rectangle.Y);
    end;
    plBottomLeft:
    begin
      x := Round(rectangle.X);
      y := h - th;
    end;
    plBottomRight:
    begin
      x := Max(Round(rectangle.X), w - tw);
      y := h - th;
    end;
    plTopCenter:
    begin
      x := Max(Round(rectangle.X), w - tw) div 2;
      y := Round(rectangle.Y);
    end;
    plBottomCenter:
    begin
      x := Max(Round(rectangle.X), w - tw) div 2;
      y := h - th;
    end;
    plCenterCenter:
    begin
      x := Max(Round(rectangle.X), w - tw) div 2;
      y := (h - th) div 2;
    end;
    plCenterLeft:
    begin
      x := Round(rectangle.x);
      y := (h - th) div 2;
    end;
    plCenterRight:
    begin
      x := Max(Round(rectangle.X), w - tw);;
      y := (h - th) div 2;
    end;
  end;
end;

function TAdvSmoothPanel.GetResizeRect: TGPRectF;
begin
  Result.X := Width - Fill.ShadowOffset - ResizeHandleSize  - Fill.Rounding / 3;
  Result.Y := Height - Fill.ShadowOffset - ResizeHandleSize - Fill.Rounding / 3;
  if Fill.Rounding = 0 then
  begin
    Result.X := Result.X - 3;
    Result.Y := Result.Y - 3;
  end;
  result.Width := ResizeHandleSize;
  result.Height := ResizeHandleSize;
end;

procedure TAdvSmoothPanel.GetRoundPath(var gp: TGPGraphicsPath; r: TGPRectF; depth: Integer);
begin
  gp.AddArc(r.X, r.Y, depth, depth, 180, 90);
  gp.AddArc(((r.X + r.Width) - depth), r.Y, depth, depth, 270, 90);
  gp.AddArc(((r.X + r.Width) - depth), ((r.Y + r.Height) - depth), depth, depth, 0, 90);
  gp.AddArc(r.X, ((r.Y + r.Height) - depth), depth, depth, 90, 90);
  gp.AddLine(r.X, ((r.Y + r.Height) - depth), r.X, (r.Y + (depth / 2)));
end;

function TAdvSmoothPanel.GetShadowSize: integer;
begin
  Result := 0;
  if Fill.ShadowColor <> clNone then
    Result := Fill.ShadowOffset;
end;

function TAdvSmoothPanel.GetThemeId: String;
begin
  Result := ClassName;
end;

function TAdvSmoothPanel.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothPanel.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothPanel.InitPreview;
begin
  (*
  FFill.GradientType := gtBackwardDiagonal;
  FFill.BorderColor := clBlack;
  *)
  FFill.ShadowOffset := 10;
  FFill.Rounding := 10;
  FCaption.Text := self.Name;
end;

procedure TAdvSmoothPanel.LoadFromTheme(FileName: String);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(FileName);
  FFill.LoadFromFile(ini, GetThemeId + '.Fill');
  FCaption.LoadFromFile(ini, GetThemeId + '.Caption');
  ini.free;
end;

procedure TAdvSmoothPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  a: String;
begin
  inherited;
  if FOnAnchor then
  begin
    a := GetAnchorAt(X, Y);
    if a <> '' then
      DoAnchorClick(Self, a);
  end;

  if FResizable then
  begin
    FMouseDown := PtInGPRect(GetResizeRect, Point(X, Y));
    if FMouseDown then
    begin
      SetCapture(Self.Handle);
      FOldPos := Point(X, Y);
    end;
  end;
end;

procedure TAdvSmoothPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  a: String;
begin
  inherited;
  if FMouseDown then
  begin
    Width := Width + (X - FoldPos.X);
    Height := Height + (Y - FoldPos.Y);
    FOldPos := Point(X, Y);
  end
  else
  begin
    a := GetAnchorAt(X, Y);
    FOnAnchor := a <> '';
    if FOnAnchor then
      inherited Cursor := crHandPoint
    else
    begin
      if Resizable then
      begin
        if PtInGPRect(GetResizeRect, Point(X, Y)) then
          inherited Cursor := crSizeNWSE
        else
          inherited Cursor := FoldCursor;
      end
      else
        inherited Cursor := FoldCursor;
    end;
  end;
end;

procedure TAdvSmoothPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if FMouseDown then
  begin
    FMouseDown := False;
    ReleaseCapture;
  end;
  inherited Cursor := FoldCursor;
end;

procedure TAdvSmoothPanel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if not (csDestroying in ComponentState) then
  begin
    if (AOperation = opRemove) and (AComponent = FContainer) then
      FContainer := nil;

    if (AOperation = opRemove) and (AComponent = FImages) then
      FImages := nil;
  end;
  inherited;    
end;

procedure TAdvSmoothPanel.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothPanel.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothPanel.Resize;
begin
  inherited;
  Changed;
end;

procedure TAdvSmoothPanel.SaveToTheme(FileName: String);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(FileName);
  FFill.SaveToFile(ini, GetThemeId + '.Fill');
  FCaption.SaveToFile(ini, GetThemeId + '.Caption');
  ini.free;
end;

procedure TAdvSmoothPanel.SetCaption(const Value: TAdvSmoothPanelCaption);
begin
  if FCaption <> value then
  begin
    FCaption.Assign(Value);
    CaptionChanged(Self);
  end;
end;

function HTMLToRgb(color: tcolor): tcolor;
var
  r,g,b: integer;
begin
  r := (Color and $0000FF);
  g := (Color and $00FF00);
  b := (Color and $FF0000) shr 16;
  Result := b or g or (r shl 16);
end;

procedure TAdvSmoothPanel.SetColorTones(ATones: TColorTones);
begin
  Caption.BackgroundColor := ATones.Selected.BrushColor;
  Caption.BackgroundColorTo := ATones.Selected.BrushColor;

  Caption.ColorStart := ATones.Selected.TextColor;
  Caption.ColorEnd := ATones.Selected.TextColor;
  Caption.LineColor := ATones.Background.BorderColor;
  Caption.HTMLFont.Color :=  ATones.Background.TextColor;
  Caption.Font.Name := GetMetroFont;

  Font.Name := GetMetroFont;
  Font.Color := ATones.Background.TextColor;

  Fill.Color :=  ATones.Background.BrushColor;
  Fill.ColorTo :=  ATones.Background.BrushColor;
  Fill.ColorMirror :=  ATones.Background.BrushColor;
  Fill.ColorMirrorTo := ATones.Background.BrushColor;
  Fill.BorderColor := ATones.Background.BorderColor;
  Fill.Rounding := 0;
  Fill.ShadowColor := clNone;
end;

procedure TAdvSmoothPanel.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  Fill.GradientType := gtVertical;
  Fill.Glow := gmNone;

  Fill.Rounding := 10;
  Fill.ShadowColor := clGray;

  // do not force the size of the shadow as part of the style
  // Fill.ShadowOffset := 10;

  if Astyle <> tsCustom then
  begin
    Caption.BackgroundColor := clNone;
    Caption.BackgroundColorTo := clNone;
  end;

  case AStyle of
    tsOffice2003Blue:
      begin
        Caption.ColorStart := clBlack;
        Caption.ColorEnd := clBlack;
        Caption.LineColor := clBlack;

        Fill.Color := $00FDEADA;
        Fill.ColorTo := $00E4AE88;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $DFD2C5;
      end;
    tsOffice2003Silver:
      begin
        Caption.ColorStart := clBlack;
        Caption.ColorEnd := clBlack;
        Caption.LineColor := clBlack;
        
        Fill.Color := $00F7F3F3;
        Fill.ColorTo := $00E6D8D8;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $00927476;
      end;
    tsOffice2003Olive:
      begin
        Caption.ColorStart := clBlack;
        Caption.ColorEnd := clBlack;
        Caption.LineColor := clBlack;

        Fill.Color := $00CFF0EA;
        Fill.ColorTo := $008CC0B1;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $006B7760;

      end;
    tsOffice2003Classic:
      begin
        Caption.ColorStart := clBlack;
        Caption.ColorEnd := clBlack;
        Caption.LineColor := clBlack;

        Fill.Color := clWhite;
        Fill.ColorTo := $00C9D1D5;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clGray;
      end;
    tsOffice2007Luna:
      begin
        Caption.ColorStart := $00B0721C;
        Caption.ColorEnd := $009F661A;
        Caption.LineColor := $00B0721C;

        Fill.BorderColor := $E3B28D;
        Fill.Color := $FAF1E9;
        Fill.ColorTo := $EDD8C7;
        Fill.ColorMirror := $EDD8C7;
        Fill.ColorMirrorTo := $FFF2E7;
        Fill.GradientMirrorType := gtVertical;
        Fill.GradientType := gtVertical;
        //Fill.Color := $00FAF1E9;
        //Fill.ColorTo := $00EDD8C7;
        //Fill.BorderColor := $C2C2C2;

      end;
    tsOffice2007Obsidian:
      begin
        Caption.ColorStart := clWhite;
        Caption.ColorEnd := clWhite;
        Caption.LineColor := clWhite;

        Fill.Color := $CFC6C1;
        Fill.ColorTo := $C5BBB4;
        Fill.ColorMirror := $C5BBB4;
        Fill.ColorMirrorTo := $ECECE5;
        Fill.BorderColor := clBlack;
        Fill.GradientMirrorType := gtVertical;


        //Fill.Color := $006E6E6D;
        //Fill.ColorTo := $00CFC6C1;
        //Fill.BorderColor := $00B4B0AE;
      end;
    tsWindowsXP:
      begin
        Caption.ColorStart := clBlack;
        Caption.ColorEnd := clBlack;
        Caption.LineColor := clBlack;
        
        Fill.Color := clBtnFace;
        Fill.ColorTo := clBtnFace;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clBlack;
      end;
    tsWhidbey:
      begin
        Caption.ColorStart := clBlack;
        Caption.ColorEnd := clBlack;
        Caption.LineColor := clBlack;

        Fill.Color := clWhite;
        Fill.ColorTo := $00D9E9EC;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $00828F92;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        Caption.ColorStart := $00B0721C;
        Caption.ColorEnd := $009F661A;
        Caption.LineColor := $00B0721C;

        //Fill.Color := $00F6F1EE;
        //Fill.ColorTo := $00E7DCD5;
        //Fill.BorderColor := $00C1BFBD;

        Fill.BorderColor := $74706F;
        Fill.Color := $F6F1EE;
        Fill.ColorTo := $E7DCD5;
        Fill.ColorMirror := $E7DCD5;
        Fill.ColorMirrorTo := $F4F4EE;
        Fill.GradientMirrorType := gtVertical;

      end;
    tsWindowsVista:
      begin
        Caption.ColorStart := clBlack;
        Caption.ColorEnd := clBlack;
        Caption.LineColor := $FDDE99;

        Fill.Color := $FDF8F1;
        Fill.ColorTo := $FCEFD5;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $FDDE99;

      end;
    tsWindows7:
      begin
        Caption.ColorStart := clBlack;
        Caption.ColorEnd := clBlack;
        Caption.LineColor := $CEA27D;

        Fill.Color := $FCEBDC;
        Fill.ColorTo := $FCDBC1;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $CEA27D;

      end;

    tsTerminal:
      begin
        Caption.ColorStart := clBlack;
        Caption.ColorEnd := clBlack;
        Caption.LineColor := clBlack;

        Fill.Color := clBtnFace;
        Fill.ColorTo := clBtnFace;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clBlack;
      end;
         tsOffice2010Blue:
      begin
        Caption.ColorStart := $5B391E;
        Caption.ColorEnd := $5B391E;
        Caption.LineColor := $5B391E;

        Fill.Glow := gmRadial;

        Fill.Color := $FDF6EF;
        Fill.ColorTo := $F0DAC7;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $C7B29F;
      end;
         tsOffice2010Silver:
      begin
        Caption.ColorStart := $5B391E;
        Caption.ColorEnd := $5B391E;
        Caption.LineColor := $5B391E;

        Fill.Glow := gmRadial;

        Fill.Color := $FFFFFF;
        Fill.ColorTo := $EDE5E0;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $D2CDC8;
      end;
         tsOffice2010Black:
      begin
        Caption.ColorStart := clWhite;
        Caption.ColorEnd := clWhite;
        Caption.LineColor := clWhite;

        Fill.Glow := gmRadial;

        Fill.Color := $BFBFBF;
        Fill.ColorTo := $919191;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $6D6D6D;
      end;
    tsWindows8, tsWindows10:
      begin
        Caption.ColorStart := clBlack;
        Caption.ColorEnd := clBlack;
        Caption.LineColor := $E4E3E2;

        Fill.Glow := gmRadial;
        Fill.Rounding := 0;
        Fill.ShadowOffset := 0;

        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $E4E3E2;
      end;
    tsOffice2013White:
      begin
         Caption.ColorStart := clBlack;
        Caption.ColorEnd := clBlack;
        Caption.LineColor := $D4D4D4;

        Fill.Glow := gmRadial;
        Fill.Rounding := 0;
        Fill.ShadowOffset := 0;


        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $D4D4D4;
      end;
    tsOffice2013LightGray:
      begin
         Caption.ColorStart := clBlack;
        Caption.ColorEnd := clBlack;
        Caption.LineColor := $C6C6C6;

        Fill.Glow := gmRadial;
        Fill.Rounding := 0;
        Fill.ShadowOffset := 0;

        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $C6C6C6;
      end;
    tsOffice2013Gray:
      begin
        Caption.ColorStart := clBlack;
        Caption.ColorEnd := clBlack;
        Caption.LineColor := $ABABAB;

        Fill.Glow := gmRadial;
        Fill.Rounding := 0;
        Fill.ShadowOffset := 0;

        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $ABABAB;
      end;
   tsOffice2016White:
      begin
        Caption.ColorStart := $505050;
        Caption.ColorEnd := $505050;
        Caption.LineColor := $D4D4D4;

        Fill.Glow := gmRadial;
        Fill.Rounding := 0;
        Fill.ShadowOffset := 0;


        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $D4D4D4;
      end;
    tsOffice2016Gray:
      begin
        Caption.ColorStart := $424242;
        Caption.ColorEnd := $424242;
        Caption.LineColor := $444444;

        Fill.Glow := gmNone;
        Fill.Rounding := 0;
        Fill.ShadowOffset := 0;

        Fill.Color := $B2B2B2;
        Fill.ColorTo := $B2B2B2;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $444444;
      end;
    tsOffice2016Black:
      begin
        Caption.ColorStart := $A6A6A6;
        Caption.ColorEnd := $A6A6A6;
        Caption.LineColor := $444444;

        Fill.Glow := gmNone;
        Fill.Rounding := 0;
        Fill.ShadowOffset := 0;

        Fill.Color := $363636;
        Fill.ColorTo := $363636;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $444444;
      end;

  end;

end;

procedure TAdvSmoothPanel.SetCursorEx(const Value: TCursor);
begin
  inherited Cursor := Value;
  FoldCursor := Value;
end;

procedure TAdvSmoothPanel.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothPanel.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

procedure TAdvSmoothPanel.SetResizable(const Value: Boolean);
begin
  if FResizable <> Value then
  begin
    FResizable := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanel.SetResizeHandle(const Value: Boolean);
begin
  if FResizeHandle <> Value then
  begin
    FResizeHandle := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanel.SetResizeHandleColor(const Value: TColor);
begin
  if FResizeHandleColor <> Value then
  begin
    FResizeHandleColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanel.SetResizeHandleOpacity(const Value: Byte);
begin
  if FResizeHandleOpacity <> Value then
  begin
    FResizeHandleOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanel.SetResizeHandleSize(const Value: Integer);
begin
  if FResizeHandleSize <> Value then
  begin
    FResizeHandleSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanel.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanel.SetVersion(const Value: String);
begin

end;

procedure TAdvSmoothPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
{
var
  DC: HDC;
  i: Integer;
  p: TPoint;
}
begin
  {
  if Assigned(Parent) and (Fill.ShadowOffset > 0) then
  begin
    DC := Message.DC;
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
  end;
  }

  if not Transparent then
  begin
    inherited;
    Exit;
  end;


  {$IFDEF DELPHI2006_LVL}
  inherited;
  {$ENDIF}
  {$IFNDEF DELPHI2006_LVL}
  message.Result := 1;
  {$ENDIF}
end;

procedure TAdvSmoothPanel.WMExitSizeMove(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnEndMoveSize) then
    FOnEndMoveSize(Self);
end;

function TAdvSmoothPanel.InMoveArea(pt: TPoint): boolean;
var
  r: TRect;
begin
  r := ClientRect;
  Result := (pt.y < 20) and (pt.x < r.right) and (pt.X > r.Left);
end;

procedure TAdvSmoothPanel.WMNCHitTest(var Msg: TWMNCHitTest);
var
  pt: TPoint;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  pt := ScreenToClient(point(msg.xpos,msg.ypos));

  if InMoveArea(pt) and (Msg.Result = htClient) and FCanMove then
  begin
    MouseMove([],pt.X,pt.Y);
    Msg.Result := htCaption;
  end;
end;

procedure TAdvSmoothPanel.WMPaint(var Message: TWMPaint);
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

procedure TAdvSmoothPanel.WMSize(var Message: TWMSize);
begin
  inherited;
  Invalidate;
end;

procedure TAdvSmoothPanel.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  if (csDesigning in ComponentState) then
    Invalidate
  else if (Fill.ShadowColor <> clNone) and (Fill.ShadowOffset > 0) then
    Invalidate;
  inherited;
end;

procedure TAdvSmoothPanel.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

{ TAdvSmoothPanelCaption }

procedure TAdvSmoothPanelCaption.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothPanelCaption then
  begin
    FText := (Source as TAdvSmoothPanelCaption).Text;
    FLocation := (Source as TAdvSmoothPanelCaption).Location;
    FLeft := (Source as TAdvSmoothPanelCaption).Left;
    FTop := (Source as TAdvSmoothPanelCaption).Top;
    FStartColor := (Source as TAdvSmoothPanelCaption).ColorStart;
    FEndColor := (Source as TAdvSmoothPanelCaption).ColorEnd;
    FGradientType := (Source as TAdvSmoothPanelCaption).GradientType;
    FHatchStyle := (Source as TAdvSmoothPanelCaption).HatchStyle;
    FStartOpacity := (Source as TAdvSmoothPanelCaption).OpacityStart;
    FEndOpacity := (Source as TAdvSmoothPanelCaption).OpacityEnd;
    FPicture.Assign((Source as TAdvSmoothPanelCaption).Picture);
    FHTMLText := (Source as TAdvSmoothPanelCaption).HTMLText;
    FHTMLLocation := (Source as TAdvSmoothPanelCaption).HTMLLocation;
    FHTMLLeft := (Source as TAdvSmoothPanelCaption).HTMLLeft;
    FHTMLTop := (Source as TAdvSmoothPanelCaption).HTMLTop;
    FHTMLURLColor := (Source as TAdvSmoothPanelCaption).HTMLURLColor;
    FHTMLShadowColor := (Source as TAdvSmoothPanelCaption).HTMLShadowColor;
    FHTMLShadowOffset := (Source as TAdvSmoothPanelCaption).HTMLShadowOffset;
    FHTMLFont.Assign((Source as TAdvSmoothPanelCaption).HTMLFont);
    FFont.Assign((Source as TAdvSmoothPanelCaption).Font);
    FLine := (Source as TAdvSmoothPanelCaption).Line;
    FTextWordWrapping := (Source as TAdvSmoothPanelCaption).TextWordWrapping;
    FHTMLWordWrapping := (Source as TAdvSmoothPanelCaption).HTMLWordWrapping;
    FTextAlignment := (Source as TAdvSmoothPanelCaption).TextAlignment;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.Changed;
begin
  FOwner.CaptionChanged(Self);
end;

constructor TAdvSmoothPanelCaption.Create(AOwner: TAdvSmoothPanel);
begin
  FOwner := AOwner;
  FTextWordWrapping := True;
  FTextAlignment := taLeftJustify;
  FHTMLWordWrapping := True;
  FHTMLLocation := plCenterCenter;
  FHTMLTop := 0;
  FHTMLLeft := 0;
  FHTMLShadowColor := clGray;
  FHTMLURLColor := clBlue;
  FHTMLShadowOffset := 5;
  FLocation := plTopLeft;
  FFont := TFont.Create;
  {$IFNDEF DEPLHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}
  FFont.OnChange := FontChanged;
  FStartColor := $00B0721C;
  FEndColor := $009F661A;
  FStartOpacity := 255;
  FEndOpacity := 255;
  FFont.Size := 12;
  FLine := true;
  FGradientType := gtVertical;
  FHatchStyle := HatchStyleHorizontal;
  FPicture := TAdvGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FLineColor := $00B0721C;
  FHTMLFont := TFont.Create;
  {$IFNDEF DEPLHI9_LVL}
  FHTMLFont.Name := 'Tahoma';
  {$ENDIF}
  FHTMLFont.OnChange := FontChanged;
  FTextRendering := tAntiAliasGridFit;
  FBackgroundColor := clNone;
  FBackgroundColorTo := clNone;
end;


destructor TAdvSmoothPanelCaption.Destroy;
begin
  if Assigned(FCache) then
    FCache.Free;
  FPicture.Free;
  FFont.Free;
  FHTMLFont.Free;
  inherited;
end;

procedure TAdvSmoothPanelCaption.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothPanelCaption.LoadFromfile(ini: TIniFile; Section: String);
begin

end;

procedure TAdvSmoothPanelCaption.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothPanelCaption.SaveToFile(ini: TIniFile; Section: String);
begin
//  ini.WriteInteger(Section, 'HatchStyle', Integer(HatchStyle));
//    property HTMLFont: TFont read FHTMLFont write SetHTMLFont;
//
//
//
//
//    property HTMLURLColor: TColor read FHTMLURLColor write SetHTMLURLColor default clBlue;
//    property HTMLShadowColor: TColor read FHTMLShadowColor write SetHTMLShadowColor default clGray;
//    property HTMLShadowOffset: integer read FHTMLShadowOffset write SetHTMLShadowOffset default 5;
//    property Font: TFont read FFont write SetFont;
//
//
//    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
//    property ColorStart: TColor read FStartColor write SetStartColor default clWhite;
//    property ColorEnd: TColor read FEndColor write SetEndColor default clSilver;
//    property OpacityStart: Byte read FStartOpacity write SetStartOpacity default 255;
//    property OpacityEnd: Byte read FEndOpacity write SetEndOpacity default 255;
//    property GradientType: TAdvGradientType read FGradientType write SetGradientType default gtVertical;
//    property LineColor: TColor read FLineColor write SetLineColor default $00B0721C;
//    property Line: Boolean read FLine write SetLine default true;
end;

procedure TAdvSmoothPanelCaption.SetBackgroundColor(const Value: TColor);
begin
  if (FBackgroundColor <> Value) then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetBackgroundColorTo(const Value: TColor);
begin
  if (FBackgroundColorTo <> Value) then
  begin
    FBackgroundColorTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetEndColor(const Value: TColor);
begin
  if FEndColor <> value then
  begin
    FEndColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetEndOpacity(const Value: Byte);
begin
  if FEndOpacity <> value then
  begin
    FEndOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetFont(const Value: TFont);
begin
  if FFont <> value then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetGradientType(const Value: TAdvGradientType);
begin
  if FGradientType <> value then
  begin
    FGradientType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetHatchStyle(const Value: THatchStyle);
begin
  if FHatchStyle <> value then
  begin
    FHatchStyle := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetHTMLFont(const Value: TFont);
begin
  if FHTMLFont <> value then
  begin
    FHTMLFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetHTMLLeft(const Value: integer);
begin
  if FHTMLLeft <> value then
  begin
    FHTMLLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetHTMLLocation(
  const Value: TAdvSmoothPanelLocation);
begin
  if FHTMLLocation <> value then
  begin
    FHTMLLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetHTMLShadowColor(const Value: TColor);
begin
  if FHTMLShadowColor <> value then
  begin
    FHTMLShadowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetHTMLShadowOffset(const Value: integer);
begin
  if FHTMLShadowOffset <> value then
  begin
    FHTMLShadowOffset := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetHTMLText(const Value: string);
begin
  if HTMLText <> value then
  begin
    FHTMLText := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetHTMLTop(const Value: integer);
begin
  if FHTMLTop <> value then
  begin
    FHTMLTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetHTMLURLColor(const Value: TColor);
begin
  if FHTMLURLColor <> value then
  begin
    FHTMLURLColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetHTMLWordWrapping(const Value: Boolean);
begin
  if FHTMLWordWrapping <> Value then
  begin
    FHTMLWordWrapping := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetLeft(const Value: integer);
begin
  if FLeft <> value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetLine(const Value: Boolean);
begin
  if FLine <> value then
  begin
    FLine := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetLineColor(const Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetLocation(
  const Value: TAdvSmoothPanelLocation);
begin
  if FLocation <> value then
  begin
    FLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetPicture(const Value: TAdvGDIPPicture);
begin
  if FPicture <> value then
  begin
    FPicture.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetStartColor(const Value: TColor);
begin
  if FStartColor <> value then
  begin
    FStartColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetStartOpacity(const Value: Byte);
begin
  if FStartOpacity <> value then
  begin
    FStartOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetTextAlignment(const Value: TAlignment);
begin
  if FTextAlignment <> Value then
  begin
    FTextAlignment := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetTextRendering(
  const Value: TAdvSmoothPanelTextRenderingHint);
begin
  if FTextRendering <> Value then
  begin
    FTextRendering := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetTextWordWrapping(const Value: Boolean);
begin
  if FTextWordWrapping <> Value then
  begin
    FTextWordWrapping := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPanelCaption.SetTop(const Value: integer);
begin
  if FTop <> value then
  begin
    FTop := Value;
    Changed;
  end;
end;

end.
