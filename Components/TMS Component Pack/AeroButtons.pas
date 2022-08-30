{***************************************************************************}
{ AeroButtons component                                                     }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010 - 2012                                        }
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

unit AeroButtons;

{$I TMSDEFS.INC}

{$ImportedData ON}

interface

uses
  Classes, Windows, Controls, Forms, Graphics, Buttons, Messages, StdCtrls, ImgList,
  ActnList, SysUtils, CommCtrl, AdvGDIP, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Improved : Accel char display

type
  TImagePos = (ipLeft, ipRight, ipTop, ipBottom);

  //--- TAeroButton
  {$IFDEF DELPHI_UNICODE}
  TAeroButton = class(TCustomButton)
  {$ELSE}
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAeroButton = class(TButton)
  {$ENDIF}
  private
    IsFocused: Boolean;
    FCanvas: TCanvas;
    FGlyph: TObject;
    FMouseInControl: Boolean;
    FImagePos: TImagePos;
    FImages: TCustomImageList;
    FSpacing: Integer;
    FImageIndex: TImageIndex;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure GlyphChanged(Sender: TObject);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImagePos(const Value: TImagePos);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetSpacing(const Value: Integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CNCtlColorBtn(var Message: TWMCtlColorBtn); message CN_CTLCOLORBTN;
    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: Integer; virtual;
  published
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property ImagePos: TImagePos read FImagePos write SetImagePos default ipLeft;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Version: string read GetVersion write SetVersion;
  {$IFDEF DELPHI_UNICODE}
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Cancel;
    property Caption;
    property Constraints;
    property Default;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ModalResult;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  {$ENDIF}
  end;

  //---

  //--- TAeroBitBtn
  TAeroBitBtn = class;

  TAeroBitBtnActionLink = class(TControlActionLink)
  protected
    FClient: TAeroBitBtn;
    FImageIndex: Integer;
    procedure AssignClient(AClient: TObject); override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetImageIndex(Value: Integer); override;
  public
    constructor Create(AClient: TObject); override;
  end;

  {$IFDEF DELPHI_UNICODE}
  TAeroBitBtn = class(TCustomButton)
  {$ELSE}
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAeroBitBtn = class(TButton)
  {$ENDIF}
  private
    FCanvas: TCanvas;
    FGlyph: TObject;
    FStyle: TButtonStyle;
    FKind: TBitBtnKind;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    IsFocused: Boolean;
    FModifiedGlyph: Boolean;
    FMouseInControl: Boolean;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure SetGlyph(Value: TBitmap);
    function GetGlyph: TBitmap;
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure GlyphChanged(Sender: TObject);
    function IsCustom: Boolean;
    function IsCustomCaption: Boolean;
    procedure SetStyle(Value: TButtonStyle);
    procedure SetKind(Value: TBitBtnKind);
    function GetKind: TBitBtnKind;
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CNCtlColorBtn(var Message: TWMCtlColorBtn); message CN_CTLCOLORBTN;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetPalette: HPALETTE; override;
    procedure SetButtonStyle(ADefault: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    function GetVersionNr: Integer; virtual;
  published
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Cancel stored IsCustom;
    property Caption stored IsCustomCaption;
    property Constraints;
    property Default stored IsCustom;
    property Enabled;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustom;
    property Kind: TBitBtnKind read GetKind write SetKind default bkCustom;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property ModalResult stored IsCustom;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs stored IsCustom default 1;
    property ParentShowHint;
    property ParentBiDiMode;
    property ShowHint;
    property Style: TButtonStyle read FStyle write SetStyle default bsAutoDetect;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property TabOrder;
    property TabStop;
    property Version: string read GetVersion write SetVersion;
    property Visible;
  {$IFDEF DELPHI_UNICODE}
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property ParentFont;
    property PopupMenu;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  {$ENDIF}
    property OnEnter;
    property OnExit;
  end;

  //--- TAeroSpeedButton

  TAeroSpeedButton = class;

  TAeroSpeedButtonActionLink = class(TControlActionLink)
  protected
    FClient: TAeroSpeedButton;
    FImageIndex: Integer;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
  public
    constructor Create(AClient: TObject); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAeroSpeedButton = class(TGraphicControl)
  private
    FGroupIndex: Integer;
    FGlyph: TObject;
    FDown: Boolean;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FTransparent: Boolean;
    FMargin: Integer;
    FFlat: Boolean;
    FMouseInControl: Boolean;
    procedure GlyphChanged(Sender: TObject);
    procedure UpdateExclusive;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetDown(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetTransparent(Value: Boolean);
    procedure SetMargin(Value: Integer);
    procedure UpdateTracking;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    FState: TButtonState;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetPalette: HPALETTE; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property MouseInControl: Boolean read FMouseInControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Action;
    property Align;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Caption;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Font;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Visible;
    property OnClick;
    property OnDblClick;
{$IFDEF DELPHI2007_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


{$IFNDEF DELPHI2007_LVL}
{$EXTERNALSYM DwmIsCompositionEnabled}
function DwmIsCompositionEnabled(var pfEnabled: BOOL): HResult; stdcall;
{$ENDIF}

implementation

uses
  Consts, Themes, uxTheme, ActiveX
{$IFDEF DELPHI2007_LVL}
  , DwmAPI
{$ENDIF}
{$IFNDEF DELPHI2007_LVL}
  , AdvDWM
{$ENDIF}
  ;

{$IFDEF DELPHIXE2_LVL}
function ThemeServices: TCustomStyleServices;
begin
  Result := StyleServices;
end;

function ThemeServicesThemesEnabled: boolean;
begin
  Result := StyleServices.Enabled;
end;

function ThemeServicesContentRect(DC:HDC; Details: TThemedElementDetails; const BoundingRect: TRect): TRect;
begin
  StyleServices.GetElementContentRect(DC,Details,BoundingRect,Result);
end;
{$ENDIF}

{$IFNDEF DELPHIXE2_LVL}
function ThemeServicesThemesEnabled: boolean;
begin
  Result := ThemeServices.ThemesEnabled;
end;

function ThemeServicesContentRect(DC:HDC; Details: TThemedElementDetails; const BoundingRect: TRect): TRect;
begin
  Result := ThemeServices.ContentRect(DC, Details, BoundingRect);
end;
{$ENDIF}

//------------------------------------------------------------------------------

function HideAccelFlag(Control: TControl): Integer;
begin
  //ask the top level window about its UI state
  while Assigned(Control.Parent) do begin
    Control := Control.Parent;
  end;
  if (Control.Perform(WM_QUERYUISTATE, 0, 0) and UISF_HIDEACCEL)=UISF_HIDEACCEL then begin
    Result := DT_HIDEPREFIX;
  end else begin
    Result := 0;
  end;
end;

//------------------------------------------------------------------------------

function DarkenColor(Color: TColor; Perc: integer): TColor;
var
  r,g,b: longint;
  l: longint;
begin
  l := ColorToRGB(Color);
  r := ((l AND $FF0000) shr 16) and $FF;
  g := ((l AND $FF00) shr 8) and $FF;
  b := (l AND $FF);

  r := Round(r * (100 - Perc)/100);
  g := Round(g * (100 - Perc)/100);
  b := Round(b * (100 - Perc)/100);

  Result := (r shl 16) or (g shl 8) or b;
end;

//------------------------------------------------------------------------------

function DrawGDIPText(DC: HDC; Alignment: TAlignment; r: TRect; Caption:string; WideCaption: widestring; AFont: TFont; Enabled: Boolean; RealDraw: Boolean; AntiAlias: TAntiAlias; OutLineClr: TColor; IsOnGlass: Boolean = False): TRect;
var
  graphics : TGPGraphics;
  w,h: Integer;
  fontFamily: TGPFontFamily;
  font: TGPFont;
  rectf: TGPRectF;
  stringFormat: TGPStringFormat;
  solidBrush, SB2: TGPSolidBrush;
  x1,y1,x2,y2: single;
  fs: integer;
  sizerect: TGPRectF;
  szRect: TRect;
  GPath: TGPGraphicsPath;
begin
  Result := Rect(0, 0, 0, 0);
  if (Caption <> '') or (WideCaption <> '') then
  begin
    graphics := TGPGraphics.Create(DC);
    fontFamily:= TGPFontFamily.Create(AFont.Name);

    if (fontFamily.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      fontFamily.Free;
      fontFamily := TGPFontFamily.Create('Arial');
    end;


    fs := 0;

    if (fsBold in AFont.Style) then
      fs := fs + 1;

    if (fsItalic in AFont.Style) then
      fs := fs + 2;

    if (fsUnderline in AFont.Style) then
      fs := fs + 4;

    font := TGPFont.Create(fontFamily, AFont.Size , fs, UnitPoint);

    if not (IsOnGlass and RealDraw) then
      graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    w := R.Right - R.Left;
    h := R.Bottom - R.Top;

    x1 := r.Left;
    y1 := r.Top;
    x2 := w;
    y2 := h;

    rectf := MakeRect(x1,y1,x2,y2);

    stringFormat := TGPStringFormat.Create;

    if Enabled then
      solidBrush := TGPSolidBrush.Create(ColorToARGB(AFont.Color))
    else
      solidBrush := TGPSolidBrush.Create(ColorToARGB(clGray));

    case Alignment of
      taLeftJustify: stringFormat.SetAlignment(StringAlignmentNear);
      taCenter:
      begin
        // Center-justify each line of text.
        stringFormat.SetAlignment(StringAlignmentCenter);
      end;
      taRightJustify: stringFormat.SetAlignment(StringAlignmentFar);
    end;

    // Center the block of text (top to bottom) in the rectangle.

    stringFormat.SetLineAlignment(StringAlignmentCenter);
    stringFormat.SetHotkeyPrefix(HotkeyPrefixShow);
    stringFormat.SetTrimming(StringTrimmingNone);


    case AntiAlias of
    aaClearType:graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    aaAntiAlias:graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    end;

    // graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);

    if (AntiAlias = aaNone) then
    begin
      szRect.Left := round(rectf.X);
      szRect.Top := round(rectf.Y);

      szRect.Right := szRect.Left + 2;

      if (Caption <> '') then
        szRect.Bottom := DrawText(DC, PChar(Caption),Length(Caption), szrect, DT_CALCRECT or DT_LEFT or DT_WORDBREAK)
      else
        szRect.Bottom := DrawTextW(DC, PWideChar(WideCaption),Length(WideCaption), szrect, DT_CALCRECT or DT_LEFT or DT_WORDBREAK);

      sizeRect.X := szRect.Left;
      sizeRect.Y := szRect.Top;
      sizeRect.Width := szRect.Right - szRect.Left;
      sizeRect.Height := szRect.Bottom - szRect.Top;
    end
    else
    begin
      FillChar(sizerect,SizeOf(sizerect),0);

      if (Caption <> '') then
        graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect)
      else
        graphics.MeasureString(WideCaption, Length(WideCaption), font, rectf, stringFormat, sizerect)
    end;

    Result := Rect(round(sizerect.X), Round(sizerect.Y), Round(sizerect.X + sizerect.Width), Round(sizerect.Y + sizerect.Height));
    rectf := MakeRect(x1,y1,x2,y2);

    if RealDraw then
    begin
      if IsOnGlass then
      begin
        graphics.SetSmoothingMode(SmoothingModeHighQuality);

        GPath := TGPGraphicsPath.Create();
        GPath.AddString(Caption, Length(Caption), fontFamily, FontStyleBold, AFont.Size + 3, MakeRect(x1, y1 + 1,x2,y2), stringFormat);
        SB2 := TGPSolidBrush.Create(ColorToARGB(DarkenColor(OutLineClr, 35)));
        graphics.FillPath(SB2, GPath);
        SB2.Free;
        GPath.Free;

        GPath := TGPGraphicsPath.Create();
        GPath.AddString(Caption, Length(Caption), fontFamily, FontStyleBold{FontStyleRegular}, AFont.Size + 3, rectf, stringFormat);

        graphics.FillPath(solidbrush, GPath);

        GPath.Free;
      end
      else
      begin
        if (Caption <> '') then
          graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush)
        else
          graphics.DrawString(WideCaption, Length(WideCaption), font, rectf, stringFormat, solidBrush)
      end;
    end;
    stringformat.Free;
    solidBrush.Free;
    font.Free;
    fontfamily.Free;
    graphics.Free;
  end;
end;

//------------------------------------------------------------------------------

{ TAeroBitBtn data }
var
  BitBtnResNames: array[TBitBtnKind] of PChar = (
    nil, 'BBOK', 'BBCANCEL', 'BBHELP', 'BBYES', 'BBNO', 'BBCLOSE',
    'BBABORT', 'BBRETRY', 'BBIGNORE', 'BBALL');
  BitBtnCaptions: array[TBitBtnKind] of Pointer;

  BitBtnModalResults: array[TBitBtnKind] of TModalResult = (
    0, mrOk, mrCancel, 0, mrYes, mrNo, 0, mrAbort, mrRetry, mrIgnore,
    mrAll);

  BitBtnGlyphs: array[TBitBtnKind] of TBitmap;

//------------------------------------------------------------------------------

type

  TGlyphList = class(TImageList)
  private
    Used: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;

  TGlyphCache = class
  private
    GlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(List: TGlyphList);
    function Empty: Boolean;
  end;

  TButtonGlyph = class
  private
    FOriginal: TBitmap;
    FGlyphList: TGlyphList;
    FIndexs: array[TButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    FUseGDIP: Boolean;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure Invalidate;
    function CreateButtonGlyph(State: TButtonState): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState; Transparent: Boolean; Images: TCustomImageList; ImageIndex: Integer);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState; Flags: Longint);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
      BiDiFlags: Longint; Images: TCustomImageList; ImageIndex: Integer);
  protected
    property UseGDIP: Boolean read FUseGDIP write FUseGDIP default False;
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean; BiDiFlags: Longint; Images: TCustomImageList; ImageIndex: Integer): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

//------------------------------------------------------------------------------

function GetBitBtnGlyph(Kind: TBitBtnKind; Designing: boolean): TBitmap;
begin
  if BitBtnGlyphs[Kind] = nil then
  begin
    BitBtnGlyphs[Kind] := TBitmap.Create;
    BitBtnGlyphs[Kind].LoadFromResourceName(HInstance, BitBtnResNames[Kind]);
  end;
  Result := BitBtnGlyphs[Kind];
end;

//------------------------------------------------------------------------------

procedure DrawGDIPImage(gr: TGPGraphics; Canvas: TCanvas; P: TPoint; bmp: TGraphic; Transparent: Boolean = False);
var
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  graphics: TGPGraphics;
  ImageAttributes: TGPImageAttributes;
  r, g, b: byte;
  GPBmp: TGPBitmap;
  Aclr: TGPColor;
  hr: HResult;
begin
  if (not Assigned(gr) and not Assigned(Canvas)) then
    Exit;
    
  graphics := gr;
  if not Assigned(graphics) then
  begin
    graphics := TGPGraphics.Create(Canvas.Handle);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
  end;

  ms := TMemoryStream.Create;
  bmp.SaveToStream(ms);
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for image');
  end;

  pstm := nil;
  pcbWrite := 0;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if hr = S_OK then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
       Img := TGPImage.Create(pstm);

      if Transparent and (Img.GetFormat <> ifPNG) then
      begin
        GPBmp := TGPBitmap.Create(pstm);
        GPBmp.GetPixel(0, 0, AClr);
        GPBmp.Free;

        r := ADVGDIP.GetRed(AClr);
        g := ADVGDIP.GetGreen(AClr);
        b := ADVGDIP.GetBlue(AClr);

        ImageAttributes := TGPImageAttributes.Create;
        ImageAttributes.SetColorKey(MakeColor(r, g, b), MakeColor(r, g, b), ColorAdjustTypeDefault);
        graphics.DrawImage(Img, MakeRect(P.X, P.Y, Img.GetWidth, Img.Getheight),  // destination rectangle
         0, 0,        // upper-left corner of source rectangle
         Img.GetWidth,       // width of source rectangle
         Img.GetHeight,      // height of source rectangle
         UnitPixel,
         ImageAttributes);
        //graphics.DrawImage(Img, P.X, P.y);
        ImageAttributes.Free;
      end
      else
        graphics.DrawImage(Img, P.X, P.y);

      Img.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);

  ms.Free;

  if not Assigned(gr) then
    graphics.Free;
end;

//------------------------------------------------------------------------------

{ TAeroBitBtnActionLink }

procedure TAeroBitBtnActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TAeroBitBtn;
end;

//------------------------------------------------------------------------------

constructor TAeroBitBtnActionLink.Create(AClient: TObject);
begin
  inherited;
  FImageIndex := -1;
end;

//------------------------------------------------------------------------------

function TAeroBitBtnActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FImageIndex = (Action as TCustomAction).ImageIndex);
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtnActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
  begin
    FImageIndex := Value;
    with (Action as TCustomAction) do
      { Copy image from action's imagelist }
      if (ActionList <> nil) and (ActionList.Images <> nil) then
        if (FImageIndex >= 0) and (FImageIndex < ActionList.Images.Count) then
          FClient.CopyImage(ActionList.Images, Value)
        else
          FClient.Glyph := nil;
    FClient.GlyphChanged(nil);
  end;
end;

//------------------------------------------------------------------------------

{ TAeroBitBtn }

constructor TAeroBitBtn.Create(AOwner: TComponent);
begin
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  inherited Create(AOwner);
  FCanvas := TCanvas.Create;
  FStyle := bsAutoDetect;
  FKind := bkCustom;
  FLayout := blGlyphLeft;
  FSpacing := 4;
  FMargin := -1;
  ControlStyle := ControlStyle + [csReflector];
  DoubleBuffered := True;
end;

//------------------------------------------------------------------------------

destructor TAeroBitBtn.Destroy;
begin
  inherited Destroy;
  TButtonGlyph(FGlyph).Free;
  FCanvas.Free;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.CreateHandle;
var
  State: TButtonState;
begin
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  inherited CreateHandle;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;

//------------------------------------------------------------------------------
    
procedure TAeroBitBtn.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or BS_OWNERDRAW;
end;

//------------------------------------------------------------------------------
    
procedure TAeroBitBtn.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> IsFocused then
  begin
    IsFocused := ADefault;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.Click;
var
  Form: TCustomForm;
  Control: TWinControl;
begin
  case FKind of
    bkClose:
      begin
        Form := GetParentForm(Self);
        if Form <> nil then Form.Close
        else inherited Click;
      end;
    bkHelp:
      begin
        Control := Self;
        while (Control <> nil) and (Control.HelpContext = 0) do
          Control := Control.Parent;
        if Control <> nil then Application.HelpContext(Control.HelpContext)
        else inherited Click;
      end;
    else
      inherited Click;
  end;
end;

//------------------------------------------------------------------------------
    
procedure TAeroBitBtn.CNMeasureItem(var Message: TWMMeasureItem);
begin  
  with Message.MeasureItemStruct^ do
  begin
    itemWidth := Width;
    itemHeight := Height;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.DrawItem(const DrawItemStruct: TDrawItemStruct);
const
  WordBreakFlag: array[Boolean] of Integer = (0, DT_WORDBREAK);
var
  IsDown, IsDefault: Boolean;
  State: TButtonState;
  R: TRect;
  Flags: Longint;
  Details: TThemedElementDetails;
  Button: TThemedButton;
  Offset: TPoint;
  PaintOnGlass: Boolean;
  LForm: TCustomForm;
begin
  FCanvas.Handle := DrawItemStruct.hDC;
  R := ClientRect;

  with DrawItemStruct do
  begin
    FCanvas.Handle := hDC;
    FCanvas.Font := Self.Font;
    IsDown := itemState and ODS_SELECTED <> 0;
    IsDefault := itemState and ODS_FOCUS <> 0;

    if not Enabled then State := bsDisabled
    else if IsDown then State := bsDown
    else State := bsUp;
  end;

  if ThemeControl(Self) then
  begin
    if not Enabled then
      Button := tbPushButtonDisabled
    else
      if IsDown then
        Button := tbPushButtonPressed
      else
        if FMouseInControl then
          Button := tbPushButtonHot
        else
          if IsFocused or IsDefault then
            Button := tbPushButtonDefaulted
          else
            Button := tbPushButtonNormal;

    Details := ThemeServices.GetElementDetails(Button);
    // Parent background.
    PaintOnGlass := ThemeServicesThemesEnabled and DwmCompositionEnabled and
      not (csDesigning in ComponentState);
    if PaintOnGlass then
    begin
      LForm := GetParentForm(Self);
      PaintOnGlass := (LForm <> nil) and LForm.GlassFrame.FrameExtended and
        LForm.GlassFrame.IntersectsControl(Self);
    end;

    if not PaintOnGlass then
      ThemeServices.DrawParentBackground(Handle, DrawItemStruct.hDC, @Details, True)
    else
    begin
      InflateRect(R, -2, -2);
      FillRect(DrawItemStruct.hDC, R, GetStockObject(BLACK_BRUSH));
    end;
    // Button shape.
    ThemeServices.DrawElement(DrawItemStruct.hDC, Details, DrawItemStruct.rcItem);
    R := ThemeServicesContentRect(FCanvas.Handle, Details, DrawItemStruct.rcItem);

    if Button = tbPushButtonPressed then
      Offset := Point(1, 0)
    else
      Offset := Point(0, 0);
    if PaintOnGlass then
      State := TButtonState(Integer(State) or 4); // Hack to pass PaintOnGlass into TButtonGlyph

    TButtonGlyph(FGlyph).Draw(FCanvas, R, Offset, Caption, FLayout, FMargin, FSpacing, State, False,
      DrawTextBiDiModeFlags(0) or WordBreakFlag[WordWrap] or HideAccelFlag(Self), nil, -1);

    if IsFocused and IsDefault then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);
    end;
  end
  else
  begin
    R := ClientRect;

    Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if IsDown then Flags := Flags or DFCS_PUSHED;
    if DrawItemStruct.itemState and ODS_DISABLED <> 0 then
      Flags := Flags or DFCS_INACTIVE;

    { DrawFrameControl doesn't allow for drawing a button as the
        default button, so it must be done here. }
    if IsFocused or IsDefault then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Style := bsClear;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

      { DrawFrameControl must draw within this border }
      InflateRect(R, -1, -1);
    end;

    { DrawFrameControl does not draw a pressed button correctly }
    if IsDown then
    begin
      FCanvas.Pen.Color := clBtnShadow;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Color := clBtnFace;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      InflateRect(R, -1, -1);
    end
    else
      DrawFrameControl(DrawItemStruct.hDC, R, DFC_BUTTON, Flags);

    if IsFocused then
    begin
      R := ClientRect;
      InflateRect(R, -1, -1);
    end;

    FCanvas.Font := Self.Font;
    if IsDown then
      OffsetRect(R, 1, 1);

    TButtonGlyph(FGlyph).Draw(FCanvas, R, Point(0,0), Caption, FLayout, FMargin,
      FSpacing, State, False, DrawTextBiDiModeFlags(0) or WordBreakFlag[WordWrap] or HideAccelFlag(Self), nil, -1);

    if IsFocused and IsDefault then
    begin
      R := ClientRect;
      InflateRect(R, -4, -4);
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);
    end;
  end;

  FCanvas.Handle := 0;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  PaintBuffer: HPAINTBUFFER;
begin
  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    if DwmCompositionEnabled then
    begin
      DC := BeginPaint(Handle, PS);
      try                  
        PaintBuffer := BeginBufferedPaint(DC, PS.rcPaint, BPBF_COMPOSITED, nil, MemDC);
        Perform(WM_ERASEBKGND, MemDC, MemDC);
        Perform(WM_PRINTCLIENT, MemDC, PRF_CLIENT);
  //      if not (Self is TCustomForm) then
  //        BufferedPaintMakeOpaque(PaintBuffer, @PS.rcPaint);
        EndBufferedPaint(PaintBuffer, True);
      finally
        EndPaint(Handle, PS);
      end;
    end
    else
    begin
      DC := BeginPaint(Handle, PS);
      MemBitmap := CreateCompatibleBitmap(DC, PS.rcPaint.Right - PS.rcPaint.Left,
        PS.rcPaint.Bottom - PS.rcPaint.Top);
      try
        MemDC := CreateCompatibleDC(DC);
        OldBitmap := SelectObject(MemDC, MemBitmap);
        try
          SetWindowOrgEx(MemDC, PS.rcPaint.Left, PS.rcPaint.Top, nil);
          Perform(WM_ERASEBKGND, MemDC, MemDC);
          Message.DC := MemDC;
          WMPaint(Message);
          Message.DC := 0;
          BitBlt(DC, PS.rcPaint.Left, PS.rcPaint.Top,
            PS.rcPaint.Right - PS.rcPaint.Left,
            PS.rcPaint.Bottom - PS.rcPaint.Top,
            MemDC,
            PS.rcPaint.Left, PS.rcPaint.Top,
            SRCCOPY);
        finally
          SelectObject(MemDC, OldBitmap);
        end;
      finally
        EndPaint(Handle, PS);
        DeleteDC(MemDC);
        DeleteObject(MemBitmap);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if ThemeServicesThemesEnabled then
    Message.Result := 1
  else
    DefaultHandler(Message);
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.CNCtlColorBtn(var Message: TWMCtlColorBtn);
begin
  with ThemeServices do
    if ThemeServicesThemesEnabled then
    begin
      if (Parent <> nil) and Parent.DoubleBuffered then
        PerformEraseBackground(Self, Message.ChildDC)
      else
        DrawParentBackground(Handle, Message.ChildDC, nil, False);
      { Return an empty brush to prevent Windows from overpainting we just have created. }
      Message.Result := GetStockObject(NULL_BRUSH);
    end
    else
      inherited;
end;

//------------------------------------------------------------------------------

function TAeroBitBtn.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.SetGlyph(Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value as TBitmap;
  FModifiedGlyph := True;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAeroBitBtn.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAeroBitBtn.IsCustom: Boolean;
begin
  Result := Kind = bkCustom;
end;

//------------------------------------------------------------------------------
    
procedure TAeroBitBtn.SetStyle(Value: TButtonStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAeroBitBtn.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAeroBitBtn.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.SetKind(Value: TBitBtnKind);
begin
  if Value <> FKind then
  begin
    if Value <> bkCustom then
    begin
      Default := Value in [bkOK, bkYes];
      Cancel := Value in [bkCancel, bkNo];

      if ((csLoading in ComponentState) and (Caption = '')) or
        (not (csLoading in ComponentState)) then
      begin
        if BitBtnCaptions[Value] <> nil then
          Caption := LoadResString(BitBtnCaptions[Value]);
      end;

      ModalResult := BitBtnModalResults[Value];
      TButtonGlyph(FGlyph).Glyph := GetBitBtnGlyph(Value, (csDesigning in ComponentState));
      NumGlyphs := 2;
      FModifiedGlyph := False;
    end;
    FKind := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAeroBitBtn.IsCustomCaption: Boolean;
begin
  Result := AnsiCompareStr(Caption, LoadResString(BitBtnCaptions[FKind])) <> 0;
end;

//------------------------------------------------------------------------------
    
function TAeroBitBtn.GetKind: TBitBtnKind;
begin
  if FKind <> bkCustom then
    if ((FKind in [bkOK, bkYes]) xor Default) or
      ((FKind in [bkCancel, bkNo]) xor Cancel) or
      (ModalResult <> BitBtnModalResults[FKind]) or
      FModifiedGlyph then
      FKind := bkCustom;
  Result := FKind;
end;

//------------------------------------------------------------------------------
    
procedure TAeroBitBtn.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
    
function TAeroBitBtn.GetNumGlyphs: TNumGlyphs;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then Value := 1
  else if Value > 4 then Value := 4;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then
  begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= - 1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.CopyImage(ImageList: TCustomImageList; Index: Integer);
begin
  with Glyph do
  begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;//! for lack of a better color
      Canvas.FillRect(Rect(0,0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if CheckDefaults or (TAeroBitBtnActionLink(ActionLink).FImageIndex <> -1) or
         (TAeroBitBtnActionLink(ActionLink).FImageIndex <> ImageIndex) then
      begin
        TAeroBitBtnActionLink(ActionLink).FImageIndex := ImageIndex;
        if ImageIndex <> -1 then
          Glyph := nil;
      end;
      { Copy image from action's imagelist }
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;
end;

//------------------------------------------------------------------------------

procedure DestroyLocals; far;
var
  I: TBitBtnKind;
begin
  for I := Low(TBitBtnKind) to High(TBitBtnKind) do
    BitBtnGlyphs[I].Free;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if ThemeServicesThemesEnabled and not FMouseInControl and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroBitBtn.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if ThemeServicesThemesEnabled and FMouseInControl then
  begin
    FMouseInControl := False;
    Repaint;
  end;
end;

//------------------------------------------------------------------------------

function TAeroBitBtn.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAeroBitBtnActionLink;
end;

//------------------------------------------------------------------------------

{ TGlyphList }

constructor TGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  Used := TBits.Create;
end;

//------------------------------------------------------------------------------

destructor TGlyphList.Destroy;
begin
  Used.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TGlyphList.AllocateIndex: Integer;
begin
  Result := Used.OpenBit;
  if Result >= Used.Size then
  begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;

//------------------------------------------------------------------------------

function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

//------------------------------------------------------------------------------

procedure TGlyphList.Delete(Index: Integer);
begin
  if Used[Index] then
  begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;

//------------------------------------------------------------------------------

{ TGlyphCache }

constructor TGlyphCache.Create;
begin
  inherited Create;
  GlyphLists := TList.Create;
end;

//------------------------------------------------------------------------------

destructor TGlyphCache.Destroy;
begin
  GlyphLists.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := GlyphLists.Count - 1 downto 0 do
  begin
    Result := GlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := TGlyphList.CreateSize(AWidth, AHeight);
  GlyphLists.Add(Result);
end;

//------------------------------------------------------------------------------

procedure TGlyphCache.ReturnList(List: TGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then
  begin
    GlyphLists.Remove(List);
    List.Free;
  end;
end;

//------------------------------------------------------------------------------

function TGlyphCache.Empty: Boolean;
begin
  Result := GlyphLists.Count = 0;
end;

var
  GlyphCache: TGlyphCache = nil;
  ButtonCount: Integer = 0;

  
//------------------------------------------------------------------------------

{ TButtonGlyph }

constructor TButtonGlyph.Create;
var
  I: TButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  FUseGDIP := False;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
end;

//------------------------------------------------------------------------------

destructor TButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if FIndexs[I] <> -1 then FGlyphList.Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
  FGlyphList := nil;
end;

//------------------------------------------------------------------------------

procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonGlyph.SetGlyph(Value: TBitmap);
var
  Glyphs: Integer;
begin
  Invalidate;
  FOriginal.Assign(Value);
  if (Value <> nil) and (Value.Height > 0) then
  begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
    GlyphChanged(Glyph);
  end;
end;

//------------------------------------------------------------------------------

function TButtonGlyph.CreateButtonGlyph(State: TButtonState): Integer;
const
  ROP_DSPDxax = $00E20746;
var
  TmpImage, DDB, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TButtonState;
  DestDC: HDC;
begin
  if (State = bsDown) and (NumGlyphs < 3) then State := bsUp;
  Result := FIndexs[State];
  if Result <> -1 then Exit;
  if (FOriginal.Width or FOriginal.Height) = 0 then Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
    I := State;
    if Ord(I) >= NumGlyphs then I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      bsUp, bsDown,
      bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          if FOriginal.TransparentMode = tmFixed then
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, FTransparentColor)
          else
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
      bsDisabled:
        begin
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(FOriginal);
            DDB.HandleType := bmDDB;
            if NumGlyphs > 1 then
            with TmpImage.Canvas do
            begin    { Change white & gray to clBtnHighlight and clBtnShadow }
              CopyRect(IRect, DDB.Canvas, ORect);
              MonoBmp.Monochrome := True;
              MonoBmp.Width := IWidth;
              MonoBmp.Height := IHeight;

              { Convert white to clBtnHighlight }
              DDB.Canvas.Brush.Color := clWhite;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnHighlight;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    
              { Convert gray to clBtnShadow }
              DDB.Canvas.Brush.Color := clGray;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnShadow;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    
              { Convert transparent color to clBtnFace }
              DDB.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnFace;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
            end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
                HandleType := bmDDB;
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := clBtnFace;
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

//------------------------------------------------------------------------------

procedure TButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean; Images: TCustomImageList; ImageIndex: Integer);
var
  Index: Integer;
  Details: TThemedElementDetails;
  R: TRect;
  Button: TThemedButton;
  //MemDC: HDC;
  //PaintBuffer: HPAINTBUFFER;
  PaintOnGlass: Boolean;
  bmp: TBitmap;
begin
  if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
  begin
    R.TopLeft := GlyphPos;
    R.Right := R.Left + Images.Width;
    R.Bottom := R.Top + Images.Height;

    bmp := TBitmap.Create;
    bmp.Width := R.Right - R.Left;
    bmp.Height := R.Bottom - R.Top;
    Images.Draw(bmp.Canvas, 0, 0, ImageIndex);
    DrawGDIPImage(nil, Canvas, Point(R.Left, R.Top), bmp, True);
    bmp.Free;

    Exit;
  end
  else
  begin
    if FOriginal = nil then Exit;
    if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  end;
  PaintOnGlass := Integer(State) and 4 = 4; // Hack to extract PaintOnGlass from caller
  if PaintOnGlass then
    State := TButtonState(Integer(State) and 3); // Reset State after extracting PaintOnGlass
  Index := CreateButtonGlyph(State);
  with GlyphPos do
  begin
    if ThemeServicesThemesEnabled then
    begin
      R.TopLeft := GlyphPos;
      R.Right := R.Left + FOriginal.Width div FNumGlyphs;
      R.Bottom := R.Top + FOriginal.Height;
      case State of
        bsDisabled:
          Button := tbPushButtonDisabled;
        bsDown,
        bsExclusive:
          Button := tbPushButtonPressed;
      else
        // bsUp
        Button := tbPushButtonNormal;
      end;
      Details := ThemeServices.GetElementDetails(Button);

      if PaintOnGlass then
      begin
        bmp := TBitmap.Create;
        bmp.Width := R.Right - R.Left;
        bmp.Height := R.Bottom - R.Top;
        ThemeServices.DrawIcon(bmp.Canvas.Handle, Details, Rect(0, 0, bmp.Width, bmp.Height), FGlyphList.Handle, Index);
        DrawGDIPImage(nil, Canvas, Point(R.Left, R.Top), bmp, True);
        bmp.Free;

        {
        PaintBuffer := BeginBufferedPaint(Canvas.Handle, R, BPBF_TOPDOWNDIB, nil, MemDC);
        try
          ThemeServices.DrawIcon(MemDC, Details, R, FGlyphList.Handle, Index);
          BufferedPaintMakeOpaque(PaintBuffer, @R);
        finally
          EndBufferedPaint(PaintBuffer, True);
        end;
        }
      end
      else
        ThemeServices.DrawIcon(Canvas.Handle, Details, R, FGlyphList.Handle, Index);
    end
    else
      if Transparent or (State = bsExclusive) then
      begin
        ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
          clNone, clNone, ILD_Transparent)
      end
      else
        ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
          ColorToRGB(clBtnFace), clNone, ILD_Normal);
  end;
end;

//------------------------------------------------------------------------------

procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; Flags: LongInt);
var
  PaintOnGlass: Boolean;

  procedure DoDrawText(DC: HDC; const Text: string; TextLen: Integer;
    var TextRect: TRect; TextFlags: Cardinal);
  var
    Options: TDTTOpts;
  begin
    if PaintOnGlass then
    begin
      FillChar(Options, SizeOf(Options), 0);
      Options.dwSize := SizeOf(Options);
      Options.dwFlags := DTT_TEXTCOLOR or DTT_COMPOSITED;
      Options.crText := ColorToRGB(Canvas.Font.Color);

      with ThemeServices.GetElementDetails(tbPushButtonNormal) do
        DrawThemeTextEx(ThemeServices.Theme[teButton], DC, Part, State,
          PWideChar(WideString(Text)), TextLen, TextFlags, @TextRect, Options);
    end
    else
     Windows.DrawText(DC, PChar(Text), TextLen, TextRect, TextFlags);
  end;

  procedure DoGDIPDrawText(DC: HDC; const Text: string; TextRect: TRect; TextFlags: Cardinal);
  begin
    TextRect.Right := TextRect.Right + 50;
    TextRect.Left := TextRect.Left - 2;
    DrawGDIPText(DC, taLeftJustify, TextRect, Text, '', Canvas.Font, True, True, aaAntiAlias, clNone, False);
  end;

begin
  PaintOnGlass := Integer(State) and 4 = 4; // Hack to extract PaintOnGlass from caller
  if PaintOnGlass then
    State := TButtonState(Integer(State) and 3); // Reset State after extracting PaintOnGlass
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DoDrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DoDrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or Flags);
    end
    else if UseGDIP then
      DoGDIPDrawText(Handle, PChar(Caption), TextBounds, DT_CENTER or DT_VCENTER or Flags)
    else
      DoDrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or Flags);
  end;
end;
    
procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
  BiDiFlags: LongInt; Images: TCustomImageList; ImageIndex: Integer);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight
    else 
      if Layout = blGlyphRight then Layout := blGlyphLeft;
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
    GlyphSize := Point(Images.Width, Images.Height)
  else if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height)
  else
    GlyphSize := Point(0, 0);
    
  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds,
      DT_CALCRECT or BiDiFlags);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;
    
  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;
    
  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;
    
  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing < 0 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing < 0 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;
    
  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;
    
  { fixup the result variables }
  with GlyphPos do
  begin
    Inc(X, Client.Left {+ Offset.X});
    Inc(Y, Client.Top {+ Offset.Y});
  end;

  { Themed text is not shifted, but gets a different color. }
  if ThemeServicesThemesEnabled then
    OffsetRect(TextBounds, TextPos.X + Client.Left, TextPos.Y + Client.Top)
  else
    OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.Y);

  //Inc(TextBounds.Left, Offset.X);
 // Inc(TextBounds.Top, Offset.Y);
end;

//------------------------------------------------------------------------------

function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
  BiDiFlags: LongInt; Images: TCustomImageList; ImageIndex: Integer): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result, BiDiFlags, Images, ImageIndex);
  DrawButtonGlyph(Canvas, GlyphPos, State, Transparent, Images, ImageIndex);
  DrawButtonText(Canvas, Caption, Result, State, BiDiFlags);
end;

//------------------------------------------------------------------------------

{ TAeroButton }

constructor TAeroButton.Create(AOwner: TComponent);
begin
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  inherited Create(AOwner);
  FCanvas := TCanvas.Create;
  FImageIndex := -1;
  FImagePos := ipLeft;
  FSpacing := 4;
  ControlStyle := ControlStyle + [csReflector];
  DoubleBuffered := True;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.CreateHandle;
var
  State: TButtonState;
begin
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  inherited CreateHandle;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;

//------------------------------------------------------------------------------

destructor TAeroButton.Destroy;
begin
  inherited;
  TButtonGlyph(FGlyph).Free;
  FCanvas.Free;
end;

//------------------------------------------------------------------------------


procedure TAeroButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
const
  WordBreakFlag: array[Boolean] of Integer = (0, DT_WORDBREAK);
var
  IsDown, IsDefault: Boolean;
  State: TButtonState;
  R: TRect;
  Flags: Longint;
  Details: TThemedElementDetails;
  Button: TThemedButton;
  Offset: TPoint;
  PaintOnGlass: Boolean;
  LForm: TCustomForm;
  bl: TButtonLayout;
begin
  FCanvas.Handle := DrawItemStruct.hDC;
  R := ClientRect;

  with DrawItemStruct do
  begin
    FCanvas.Handle := hDC;
    FCanvas.Font := Self.Font;
    IsDown := itemState and ODS_SELECTED <> 0;
    IsDefault := itemState and ODS_FOCUS <> 0;

    if not Enabled then State := bsDisabled
    else if IsDown then State := bsDown
    else State := bsUp;
  end;

  bl := blGlyphLeft;
  if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
  begin
    case ImagePos of
      ipRight: bl := blGlyphRight;
      ipTop: bl := blGlyphTop;
      ipBottom: bl := blGlyphBottom;
    end;
  end;

  if ThemeControl(Self) then
  begin
    if not Enabled then
      Button := tbPushButtonDisabled
    else
      if IsDown then
        Button := tbPushButtonPressed
      else
        if FMouseInControl then
          Button := tbPushButtonHot
        else
          if IsFocused or IsDefault then
            Button := tbPushButtonDefaulted
          else
            Button := tbPushButtonNormal;

    Details := ThemeServices.GetElementDetails(Button);
    // Parent background.
    PaintOnGlass := ThemeServicesThemesEnabled and DwmCompositionEnabled and
      not (csDesigning in ComponentState);
    if PaintOnGlass then
    begin
      LForm := GetParentForm(Self);
      PaintOnGlass := (LForm <> nil) and LForm.GlassFrame.FrameExtended and
        LForm.GlassFrame.IntersectsControl(Self);
    end;

    if not PaintOnGlass then
      ThemeServices.DrawParentBackground(Handle, DrawItemStruct.hDC, @Details, True)
    else
    begin
      InflateRect(R, -2, -2);
      FillRect(DrawItemStruct.hDC, R, GetStockObject(BLACK_BRUSH));
    end;
    // Button shape.
    ThemeServices.DrawElement(DrawItemStruct.hDC, Details, DrawItemStruct.rcItem);
    R := ThemeServicesContentRect(FCanvas.Handle, Details, DrawItemStruct.rcItem);

    if Button = tbPushButtonPressed then
      Offset := Point(1, 0)
    else
      Offset := Point(0, 0);
    if PaintOnGlass then
      State := TButtonState(Integer(State) or 4); // Hack to pass PaintOnGlass into TButtonGlyph

    //TButtonGlyph(FGlyph).Draw(FCanvas, R, Offset, Caption, blGlyphLeft, -1, 4, State, False,
      //DrawTextBiDiModeFlags(0) or WordBreakFlag[WordWrap]);

    TButtonGlyph(FGlyph).Draw(FCanvas, R, Offset, Caption, bl, -1, Spacing, State, False,
      DrawTextBiDiModeFlags(0) or WordBreakFlag[WordWrap] or HideAccelFlag(Self), Images, ImageIndex);

    if IsFocused and IsDefault then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);
    end;
  end
  else
  begin
    R := ClientRect;

    Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if IsDown then Flags := Flags or DFCS_PUSHED;
    if DrawItemStruct.itemState and ODS_DISABLED <> 0 then
      Flags := Flags or DFCS_INACTIVE;

    { DrawFrameControl doesn't allow for drawing a button as the
        default button, so it must be done here. }
    if IsFocused or IsDefault then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Style := bsClear;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

      { DrawFrameControl must draw within this border }
      InflateRect(R, -1, -1);
    end;

    { DrawFrameControl does not draw a pressed button correctly }
    if IsDown then
    begin
      FCanvas.Pen.Color := clBtnShadow;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Color := clBtnFace;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      InflateRect(R, -1, -1);
    end
    else
      DrawFrameControl(DrawItemStruct.hDC, R, DFC_BUTTON, Flags);

    if IsFocused then
    begin
      R := ClientRect;
      InflateRect(R, -1, -1);
    end;

    FCanvas.Font := Self.Font;
    if IsDown then
      OffsetRect(R, 1, 1);
    TButtonGlyph(FGlyph).Draw(FCanvas, R, Point(0,0), Caption, blGlyphLeft, -1,
      4, State, False, DrawTextBiDiModeFlags(0) or WordBreakFlag[WordWrap] or HideAccelFlag(Self), Images, ImageIndex);

    if IsFocused and IsDefault then
    begin
      R := ClientRect;
      InflateRect(R, -4, -4);
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);
    end;
  end;

  FCanvas.Handle := 0;
end;

//------------------------------------------------------------------------------

function TAeroButton.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAeroButton.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAeroButton.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAeroButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> IsFocused then
  begin
    IsFocused := ADefault;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.SetImageIndex(const Value: TImageIndex);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.SetImagePos(const Value: TImagePos);
begin
  if (FImagePos <> Value) then
  begin
    FImagePos := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.SetImages(const Value: TCustomImageList);
begin
  if (FImages <> Value) then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.SetSpacing(const Value: Integer);
begin
  if (FSpacing <> Value) then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
end;

//------------------------------------------------------------------------------

procedure TAeroButton.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    itemWidth := Width;
    itemHeight := Height;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or BS_OWNERDRAW;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if ThemeServicesThemesEnabled then
    Message.Result := 1
  else
    DefaultHandler(Message);
end;

//------------------------------------------------------------------------------

procedure TAeroButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;

//------------------------------------------------------------------------------

procedure TAeroButton.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if ThemeServicesThemesEnabled and not FMouseInControl and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if ThemeServicesThemesEnabled and FMouseInControl then
  begin
    FMouseInControl := False;
    Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.CNCtlColorBtn(var Message: TWMCtlColorBtn);
begin
  with ThemeServices do
    if ThemeServicesThemesEnabled then
    begin
      if (Parent <> nil) and Parent.DoubleBuffered then
        PerformEraseBackground(Self, Message.ChildDC)
      else
        DrawParentBackground(Handle, Message.ChildDC, nil, False);
      { Return an empty brush to prevent Windows from overpainting we just have created. }
      Message.Result := GetStockObject(NULL_BRUSH);
    end
    else
      inherited;
end;

//------------------------------------------------------------------------------

procedure TAeroButton.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  PaintBuffer: HPAINTBUFFER;
begin
  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    if DwmCompositionEnabled then
    begin
      DC := BeginPaint(Handle, PS);
      try                  
        PaintBuffer := BeginBufferedPaint(DC, PS.rcPaint, BPBF_COMPOSITED, nil, MemDC);
        Perform(WM_ERASEBKGND, MemDC, MemDC);
        Perform(WM_PRINTCLIENT, MemDC, PRF_CLIENT);
  //      if not (Self is TCustomForm) then
  //        BufferedPaintMakeOpaque(PaintBuffer, @PS.rcPaint);
        EndBufferedPaint(PaintBuffer, True);
      finally
        EndPaint(Handle, PS);
      end;
    end
    else
    begin
      DC := BeginPaint(Handle, PS);
      MemBitmap := CreateCompatibleBitmap(DC, PS.rcPaint.Right - PS.rcPaint.Left,
        PS.rcPaint.Bottom - PS.rcPaint.Top);
      try
        MemDC := CreateCompatibleDC(DC);
        OldBitmap := SelectObject(MemDC, MemBitmap);
        try
          SetWindowOrgEx(MemDC, PS.rcPaint.Left, PS.rcPaint.Top, nil);
          Perform(WM_ERASEBKGND, MemDC, MemDC);
          Message.DC := MemDC;
          WMPaint(Message);
          Message.DC := 0;
          BitBlt(DC, PS.rcPaint.Left, PS.rcPaint.Top,
            PS.rcPaint.Right - PS.rcPaint.Left,
            PS.rcPaint.Bottom - PS.rcPaint.Top,
            MemDC,
            PS.rcPaint.Left, PS.rcPaint.Top,
            SRCCOPY);
        finally
          SelectObject(MemDC, OldBitmap);
        end;
      finally
        EndPaint(Handle, PS);
        DeleteDC(MemDC);
        DeleteObject(MemBitmap);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ TAeroSpeedButtonActionLink }

procedure TAeroSpeedButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TAeroSpeedButton;
end;

//------------------------------------------------------------------------------

constructor TAeroSpeedButtonActionLink.Create(AClient: TObject);
begin
  inherited Create(AClient);
  FImageIndex := -1;
end;

//------------------------------------------------------------------------------

function TAeroSpeedButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and (FClient.GroupIndex <> 0) and
    FClient.AllowAllUp and (FClient.Down = (Action as TCustomAction).Checked);
end;

//------------------------------------------------------------------------------

function TAeroSpeedButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := (FClient is TAeroSpeedButton) and
    (TAeroSpeedButton(FClient).GroupIndex = (Action as TCustomAction).GroupIndex);
end;

//------------------------------------------------------------------------------

function TAeroSpeedButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FImageIndex = (Action as TCustomAction).ImageIndex);
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then TAeroSpeedButton(FClient).Down := Value;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then TAeroSpeedButton(FClient).GroupIndex := Value;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
  begin
    FImageIndex := Value;
    with (Action as TCustomAction) do
      { Copy image from action's imagelist }
      if (ActionList <> nil) and (ActionList.Images <> nil) then
        if (FImageIndex >= 0) and (FImageIndex < ActionList.Images.Count) then
          FClient.CopyImage(ActionList.Images, Value)
        else
          FClient.Glyph := nil;
  end;
end;

//------------------------------------------------------------------------------

{ TAeroSpeedButton }

constructor TAeroSpeedButton.Create(AOwner: TComponent);
begin
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  inherited Create(AOwner);
  FFlat := True;
  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;
  Color := clBtnFace;
  FSpacing := 4;
  FMargin := -1;
  FLayout := blGlyphLeft;
  FTransparent := True;
  Inc(ButtonCount);
end;

//------------------------------------------------------------------------------

destructor TAeroSpeedButton.Destroy;
begin
  Dec(ButtonCount);
  inherited Destroy;
  TButtonGlyph(FGlyph).Free;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.Paint;
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  DrawFlags: Integer;
  Offset: TPoint;
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
  Details: TThemedElementDetails;
  PaintOnGlass: Boolean;
  LForm: TCustomForm;
begin
  if not Enabled then
  begin
    FState := bsDisabled;
    FDragging := False;
  end
  else if FState = bsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;
  Canvas.Font := Self.Font;

  if ThemeControl(Self) then
  begin
    PaintOnGlass := ThemeServicesThemesEnabled and DwmCompositionEnabled and
      not (csDesigning in ComponentState);
    if PaintOnGlass then
    begin
      LForm := GetParentForm(Self);
      PaintOnGlass := (LForm <> nil) and LForm.GlassFrame.FrameExtended and
        LForm.GlassFrame.IntersectsControl(Self);
    end;

    if not PaintOnGlass then
      if Transparent then
        ThemeServices.DrawParentBackground(0, Canvas.Handle, nil, False)
      else
        PerformEraseBackground(Self, Canvas.Handle)
    else
      FillRect(Canvas.Handle, ClientRect, GetStockObject(BLACK_BRUSH));

    if not Enabled then
      Button := tbPushButtonDisabled
    else
      if FState in [bsDown, bsExclusive] then
        Button := tbPushButtonPressed
      else
        if MouseInControl then
          Button := tbPushButtonHot
        else
          Button := tbPushButtonNormal;

    ToolButton := ttbToolbarDontCare;
    if FFlat then
    begin
      case Button of
        tbPushButtonDisabled:
          Toolbutton := ttbButtonDisabled;
        tbPushButtonPressed:
          Toolbutton := ttbButtonPressed;
        tbPushButtonHot:
          Toolbutton := ttbButtonHot;
        tbPushButtonNormal:
          Toolbutton := ttbButtonNormal;
      end;
    end;

    PaintRect := ClientRect;
    if ToolButton = ttbToolbarDontCare then
    begin
      Details := ThemeServices.GetElementDetails(Button);
      ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
      PaintRect := ThemeServicesContentRect(Canvas.Handle, Details, PaintRect);
    end
    else
    begin
      Details := ThemeServices.GetElementDetails(ToolButton);
      ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
      PaintRect := ThemeServicesContentRect(Canvas.Handle, Details, PaintRect);
    end;

    if Button = tbPushButtonPressed then
    begin
      // A pressed speed button has a white text. This applies however only to flat buttons.
      //if ToolButton <> ttbToolbarDontCare then
        //Canvas.Font.Color := clHighlightText;
      Offset := Point(1, 0);
    end
    else
      Offset := Point(0, 0);
    if PaintOnGlass then
    begin
      FState := TButtonState(Integer(FState) or 4); // Hack to pass PaintOnGlass into TButtonGlyph
      if Assigned(Parent) then
        TButtonGlyph(FGlyph).UseGDIP := not Parent.DoubleBuffered;
    end;
    TButtonGlyph(FGlyph).Draw(Canvas, PaintRect, Offset, Caption, FLayout, FMargin, FSpacing, FState, Transparent,
      DrawTextBiDiModeFlags(0) or HideAccelFlag(Self), nil, -1);
  end
  else
  begin
    PaintRect := Rect(0, 0, Width, Height);
    if not FFlat then
    begin
      DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
      if FState in [bsDown, bsExclusive] then
        DrawFlags := DrawFlags or DFCS_PUSHED;
      DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
    end
    else
    begin
      if (FState in [bsDown, bsExclusive]) or
        (FMouseInControl and (FState <> bsDisabled)) or
        (csDesigning in ComponentState) then
        DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]],
          FillStyles[Transparent] or BF_RECT)
      else if not Transparent then
      begin
        Canvas.Brush.Color := Color;
        Canvas.FillRect(PaintRect);
      end;
      InflateRect(PaintRect, -1, -1);
    end;
    if FState in [bsDown, bsExclusive] then
    begin
      if (FState = bsExclusive) and (not FFlat or not FMouseInControl) then
      begin
        Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
        Canvas.FillRect(PaintRect);
      end;
      Offset.X := 1;
      Offset.Y := 1;
    end
    else
    begin
      Offset.X := 0;
      Offset.Y := 0;
    end;
    TButtonGlyph(FGlyph).Draw(Canvas, PaintRect, Offset, Caption, FLayout, FMargin,
      FSpacing, FState, Transparent, DrawTextBiDiModeFlags(0) or HideAccelFlag(Self), nil, -1);
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.UpdateTracking;
var
  P: TPoint;
begin
  if FFlat then
  begin
    if Enabled then
    begin
      GetCursorPos(P);
      FMouseInControl := not (FindDragTarget(P, True) = Self);
      if FMouseInControl then
        Perform(CM_MOUSELEAVE, 0, 0)
      else
        Perform(CM_MOUSEENTER, 0, 0);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.Loaded;
var
  State: TButtonState;
begin
  inherited Loaded;
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if not FDown then
    begin
      FState := bsDown;
      Invalidate;
    end;
    FDragging := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if not FDown then NewState := bsUp
    else NewState := bsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then NewState := bsExclusive else NewState := bsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end
  else if not FMouseInControl then
    UpdateTracking;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      { Redraw face in-case mouse is captured }
      FState := bsUp;
      FMouseInControl := False;
      if DoClick and not (FState in [bsExclusive, bsDown]) then
        Invalidate;
    end
    else
      if DoClick then
      begin
        SetDown(not FDown);
        if FDown then Repaint;
      end
      else
      begin
        if FDown then FState := bsExclusive;
        Repaint;
      end;
    if DoClick then Click;
    UpdateTracking;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.Click;
begin
  inherited Click;
end;

//------------------------------------------------------------------------------

function TAeroSpeedButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

//------------------------------------------------------------------------------

function TAeroSpeedButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAeroSpeedButtonActionLink;
end;

//------------------------------------------------------------------------------

function TAeroSpeedButton.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.SetGlyph(Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAeroSpeedButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then Value := 1
  else if Value > 4 then Value := 4;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then
  begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := LParam(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = bsUp then Invalidate;
      FState := bsExclusive
    end
    else
    begin
      FState := bsUp;
      Repaint;
    end;
    if Value then UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    if Value then
      ControlStyle := ControlStyle - [csOpaque] else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  if FDown then DblClick;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.CMEnabledChanged(var Message: TMessage);
const
  NewState: array[Boolean] of TButtonState = (bsDisabled, bsUp);
begin
  TButtonGlyph(FGlyph).CreateButtonGlyph(NewState[Enabled]);
  UpdateTracking;
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TAeroSpeedButton;
begin
  if integer(Message.WParam) = FGroupIndex then
  begin
    Sender := TAeroSpeedButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        if (Action is TCustomAction) then
          TCustomAction(Action).Checked := False;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled and Visible and
      (Parent <> nil) and Parent.Showing then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.CMSysColorChange(var Message: TMessage);
begin
  with TButtonGlyph(FGlyph) do
  begin
    Invalidate;
    CreateButtonGlyph(FState);
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.CMMouseEnter(var Message: TMessage);
var
  NeedRepaint: Boolean;
begin
  inherited;
  { Don't draw a border if DragMode <> dmAutomatic since this button is meant to
    be used as a dock client. }
  NeedRepaint := FFlat and not FMouseInControl and Enabled and (DragMode <> dmAutomatic) and (GetCapture = 0);

  { Windows XP introduced hot states also for non-flat buttons. }
  if (NeedRepaint or ThemeServicesThemesEnabled) and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    if Enabled then
      Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.CMMouseLeave(var Message: TMessage);
var
  NeedRepaint: Boolean;
begin
  inherited;
  NeedRepaint := FFlat and FMouseInControl and Enabled and not FDragging;
  { Windows XP introduced hot states also for non-flat buttons. }
  if NeedRepaint or ThemeServicesThemesEnabled then
  begin
    FMouseInControl := False;
    if Enabled then
      Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.CopyImage(ImageList: TCustomImageList; Index: Integer);
begin
  with Glyph do
  begin
    Width := ImageList.Width;
    Height := ImageList.Height;
    Canvas.Brush.Color := clFuchsia;//! for lack of a better color
    Canvas.FillRect(Rect(0,0, Width, Height));
    ImageList.Draw(Canvas, 0, 0, Index);
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if CheckDefaults or (TAeroSpeedButtonActionLink(ActionLink).FImageIndex <> -1) or
         (TAeroSpeedButtonActionLink(ActionLink).FImageIndex <> ImageIndex) then
      begin
        TAeroSpeedButtonActionLink(ActionLink).FImageIndex := ImageIndex;
        if ImageIndex <> -1 then
          Glyph := nil;
      end;
      if CheckDefaults or (Self.GroupIndex = 0) then
        Self.GroupIndex := GroupIndex;
      { Copy image from action's imagelist }
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;
end;

//------------------------------------------------------------------------------


initialization
  BitBtnCaptions[bkCustom] := nil;
  BitBtnCaptions[bkOK] := @SOKButton;
  BitBtnCaptions[bkCancel] := @SCancelButton;
  BitBtnCaptions[bkHelp] := @SHelpButton;
  BitBtnCaptions[bkYes] := @SYesButton;
  BitBtnCaptions[bkNo] := @SNoButton;
  BitBtnCaptions[bkClose] := @SCloseButton;
  BitBtnCaptions[bkAbort] := @SAbortButton;
  BitBtnCaptions[bkRetry] := @SRetryButton;
  BitBtnCaptions[bkIgnore] := @SIgnoreButton;
  BitBtnCaptions[bkAll] := @SAllButton;
end.
