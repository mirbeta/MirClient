{*************************************************************************}
{ TADVTOOLBUTON component                                                 }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2002 - 2015                                      }
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

{$I TMSDEFS.INC}

unit AdvToolBtn;

interface

uses
  Classes, Windows, Graphics, Buttons, Controls, Messages, Forms, Dialogs, ExtCtrls,
  Menus, AdvStyleIF, ImgList, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 6; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // 1.2.1.0 : Added Whidbey appearance style
  // 1.3.0.0 : New Style interface added
  //           New Office 2007 Luna & Obsidian styles
  // 1.3.1.0 : Exposed Align property
  // 1.3.1.1 : Fixed issue with OnDblClick
  // 1.3.2.0 : New support for Office 2007 silver style added
  // 1.3.2.1 : Fixed issue with programmatically setting down
  // 1.3.3.0 : New : public method ShowDropDownMenu
  // 1.3.4.0 : New : with DropDownButton = false and assigned DropDownMenu, menu shows on click
  // 1.3.5.0 : New : property DropDownSplit property added
  // 1.3.5.1 : Fixed : small issue with BorderDownColor for non gradient buttons
  // 1.4.0.0 : New : Terminal, Vista & Windows 7 styles
  // 1.5.0.0 : New : Support for ImageList images added
  // 1.6.0.0 : New : Built in support for Office 2010 colors
  // 1.6.1.0 : Improved : Designtime border painting for empty buttons for easier design
  // 1.6.1.1 : Fixed : Issue with action imagelist images


type
  TAdvToolButtonStyle = (tasButton, tasCheck);

  TAdvToolButton = class;

  TAdvToolButtonActionLink = class(TControlActionLink)
  protected
    FImageIndex: Integer;
    FClient: TAdvToolButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetCaption(const Value: string); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvToolButton = class(TGraphicControl, ITMSStyle)
  private
    FTMSStyle: TTMSStyle;
    FGroupIndex: Integer;
    FGlyph: TBitmap;
    FDown: Boolean;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FTransparent: Boolean;
    FMargin: Integer;
    FFlat: Boolean;
    FMouseInControl: Boolean;
    FColorTo: TColor;
    FColorHot: TColor;
    FColorHotTo: TColor;
    FColorDown: TColor;
    FColorDownTo: TColor;
    FBorderColor: TColor;
    FBorderDownColor: TColor;
    FBorderHotColor: TColor;
    FGlyphDisabled: TBitmap;
    FGlyphHot: TBitmap;
    FGlyphDown: TBitmap;
    FGlyphShade: TBitmap;
    FShaded: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FColorChecked: TColor;
    FColorCheckedTo: TColor;
    FStyle: TAdvToolButtonStyle;
    FLook: Integer;
    FRounded: Boolean;
    FDropDownButton: boolean;
    FDropDownSplit: boolean;
    FAutoThemeAdapt: Boolean;
    FOnDropDown: TNotifyEvent;
    FDropDownMenu: TPopupMenu;
    FShowCaption: Boolean;
    FImages: TCustomImageList;
    FImageIndex: integer;
    procedure GlyphChanged(Sender: TObject);
    procedure UpdateExclusive;
    procedure SetGlyph(Value: TBitmap);
    procedure SetDown(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure UpdateTracking;
    procedure WMLButtonDblClk(var Message: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetGlyphDisabled(const Value: TBitmap);
    procedure SetGlyphDown(const Value: TBitmap);
    procedure SetGlyphHot(const Value: TBitmap);
    procedure GenerateShade;
    procedure SetShaded(const Value: Boolean);
    procedure SetColorTo(const Value: TColor);
    procedure SetColorChecked(const Value: TColor);
    procedure SetColorCheckedTo(const Value: TColor);    
    procedure SetStyle(const Value: TAdvToolButtonStyle);
    procedure SetRounded(const Value: Boolean);
    procedure SetDropDownButton(const Value: Boolean);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetLook(const Value: Integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetImageIndex(const Value: integer);
  protected
    FState: TButtonState;

    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure Loaded; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function ActionHasImages: boolean;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState; Transparent: Boolean);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState; BiDiFlags: LongInt);
    function DrawButton(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
      BiDiFlags: LongInt): TRect;
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
      Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
      BiDiFlags: LongInt);
    procedure Paint; override;
    property MouseInControl: Boolean read FMouseInControl;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;    
    procedure ThemeAdapt;
    procedure SetAutoThemeAdapt(const Value: Boolean);
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property Look: Integer read FLook write SetLook;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure ShowDropDownMenu;
  published
    property Action;
    property Align;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Anchors;
    property AutoThemeAdapt: Boolean read FAutoThemeAdapt write SetAutoThemeAdapt;
    property BiDiMode;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property BorderDownColor: TColor read FBorderDownColor write FBorderDownColor default clNone;
    property BorderHotColor: TColor read FBorderHotColor write FBorderHotColor default clNone;
    property Color: TColor read GetColor write SetColor default clBtnFace;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property ColorDown: TColor read FColorDown write FColorDown;
    property ColorDownTo: TColor read FColorDownTo write FColorDownTo default clNone;
    property ColorHot: TColor read FColorHot write FColorHot;
    property ColorHotTo: TColor read FColorHotTo write FColorHotTo default clNone;
    property ColorChecked: TColor read FColorChecked write SetColorChecked default clGray;
    property ColorCheckedTo: TColor read FColorCheckedTo write SetColorCheckedTo default clNone;
    property Constraints;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property DropDownButton: Boolean read FDropDownButton write SetDropDownButton default False;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property DropDownSplit: boolean read FDropDownSplit write FDropDownSplit default true;
    property Caption;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Font;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphHot: TBitmap read FGlyphHot write SetGlyphHot;
    property GlyphDown: TBitmap read FGlyphDown write SetGlyphDown;
    property GlyphDisabled: TBitmap read FGlyphDisabled write SetGlyphDisabled;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImageList;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property Rounded: Boolean read FRounded write SetRounded default False;
    property Shaded: Boolean read FShaded write SetShaded default True;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Style: TAdvToolButtonStyle read FStyle write SetStyle default tasButton;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property Version: string read GetVersion write SetVersion;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvRepeatButton = class(TAdvToolButton)
  private
    FRepeatTimer: TTimer;
    FInitRepeatPause: Integer;
    FRepeatPause: Integer;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property InitRepeatPause: Integer read FInitRepeatPause write FInitRepeatPause default 400;
    property RepeatPause: Integer read FRepeatPause write FRepeatPause default 100;
  end;

implementation

uses
  ActnList, SysUtils, ComObj;

const
  // theme changed notifier
  WM_THEMECHANGED = $031A;

type
  XPColorScheme = (xpNone, xpBlue, xpGreen, xpGray);

var
  GetCurrentThemeName: function(pszThemeFileName: PWideChar;
    cchMaxNameChars: Integer;
    pszColorBuff: PWideChar;
    cchMaxColorChars: Integer;
    pszSizeBuff: PWideChar;
    cchMaxSizeChars: Integer): THandle cdecl stdcall;

  IsThemeActive: function: BOOL cdecl stdcall;

function IsWinXP: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  Result := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));
end;

function CurrentXPTheme: XPColorScheme;
var
  FileName, ColorScheme, SizeName: WideString;
  hThemeLib: THandle;
begin
  hThemeLib := 0;
  Result := xpNone;

  if not IsWinXP then
    Exit;

  try
    hThemeLib := LoadLibrary('uxtheme.dll');
    if hThemeLib > 0 then
    begin
      IsThemeActive := GetProcAddress(hThemeLib,'IsThemeActive');

      if Assigned(IsThemeActive) then
        if IsThemeActive then
        begin
          GetCurrentThemeName := GetProcAddress(hThemeLib, 'GetCurrentThemeName');
          if Assigned(GetCurrentThemeName) then
          begin
            SetLength(FileName, 255);
            SetLength(ColorScheme, 255);
            SetLength(SizeName, 255);
            GetCurrentThemeName(PWideChar(FileName), 255,
              PWideChar(ColorScheme), 255, PWideChar(SizeName), 255);
            if(PWideChar(ColorScheme)='NormalColor') then
              Result := xpBlue
            else if(PWideChar(ColorScheme)='HomeStead') then
              Result := xpGreen
            else if(PWideChar(ColorScheme)='Metallic') then
              Result := xpGray
            else
              Result := xpNone;
          end;
        end;
    end;
  finally
    if hThemeLib <> 0 then
      FreeLibrary(hThemeLib);
  end;
end;

procedure DrawGradient(Canvas: TCanvas; FromColor,ToColor, PenColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr,startr,endr: Integer;
  diffg,startg,endg: Integer;
  diffb,startb,endb: Integer;
  rstepr,rstepg,rstepb,rstepw: Real;
  i,stepw: Word;

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
    Brush.Style := bsSolid;  
    for i := 0 to steps-1 do
    begin
      endr := startr + Round(rstepr*i);
      endg := startg + Round(rstepg*i);
      endb := startb + Round(rstepb*i);
      stepw := Round(i*rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw,R.Top,R.Left + stepw + Round(rstepw)+1,R.Bottom)
      else
        Rectangle(R.Left,R.Top + stepw,R.Right,R.Top + stepw + Round(rstepw)+1);
    end;

    if PenColor <> clNone then
    begin
      Pen.Color := PenColor;
      Brush.Style := bsClear;
      Rectangle(R.Left,R.Top,R.Right, R.Bottom);
    end;
  end;

end;
  

constructor TAdvToolButton.Create(AOwner: TComponent);
begin
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChanged;
  FGlyphHot := TBitmap.Create;
  FGlyphDown := TBitmap.Create;
  FGlyphDisabled := TBitmap.Create;
  FGlyphShade := TBitmap.Create;
  inherited Create(AOwner);

  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;
  Color := clBtnFace;
  FColorTo := clNone;
  FColorHot := RGB(199,199,202);
  FColorHotTo := clNone;
  FColorDown := RGB(210,211,216);
  FColorDownTo := clNone;
  FColorChecked := clGray;
  FColorCheckedTo := clNone;
  FBorderColor := clNone;
  FBorderDownColor := clNone;
  FBorderHotColor := clNone;  
  FSpacing := 4;
  FMargin := -1;
  FImageIndex := -1;
  Flat := True;
  FLayout := blGlyphLeft;
  FTransparent := True;
  FShaded := True;
  FShowCaption := True;
  FDropDownSplit := true;
end;

procedure TAdvToolButton.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

destructor TAdvToolButton.Destroy;
begin
  inherited Destroy;
  FGlyph.Free;
  FGlyphHot.Free;
  FGlyphDown.Free;
  FGlyphDisabled.Free;
  FGlyphShade.Free;
end;

procedure TAdvToolButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if CheckDefaults or (Self.GroupIndex = 0) then
        Self.GroupIndex := GroupIndex;
      if (csDesigning in ComponentState) then
      begin
        if ActionHasImages then
          Self.ImageIndex := ImageIndex;

        Self.Down := Checked;
      end;
    end;
end;

function TAdvToolButton.ActionHasImages: boolean;
var
  ActnList: TCustomActionList;
begin
  Result := false;

  if Assigned(Action) then
  begin
    ActnList := (Action as TCustomAction).ActionList;

    if Assigned(ActnList) then
      Result := Assigned(ActnList.Images);
  end;
end;


procedure TAdvToolButton.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean);
var
  SelGlyph: TBitmap;
  imgList: TCustomImageList;
begin
  if FMouseInControl  then
  begin
    if (FState in [ bsDown, bsExclusive]) or FDown then
    begin
      if GlyphDown.Empty then
        SelGlyph := FGlyph
      else
        SelGlyph := GlyphDown;
    end
    else
    begin
      if GlyphHot.Empty or (csDesigning in ComponentState) then
        SelGlyph := FGlyph
      else
        SelGlyph := GlyphHot;
    end;
  end
  else
  begin
    if {(FState in [bsDown, bsExclusive]} FDown then
    begin
      if GlyphDown.Empty then
        SelGlyph := FGlyph
      else
        SelGlyph := GlyphDown;
    end
    else
      SelGlyph := FGlyph;
  end;

  if not Enabled then
  begin
    if FGlyphDisabled.Empty then
      SelGlyph := FGlyph
    else
      SelGlyph := FGlyphDisabled;
  end;

//  Shaded := true;


  if not SelGlyph.Empty then
  begin
    if FMouseInControl and Shaded and Enabled and not (FState = bsDown) then
    begin
      FGlyphShade.TransparentMode := tmAuto;
      FGlyphShade.Transparent := True;
      Canvas.Draw(GlyphPos.X + 2,GlyphPos.Y + 2, FGlyphShade);
    end;

    SelGlyph.TransparentMode := tmAuto;
    SelGlyph.Transparent := True;
    Canvas.Draw(GlyphPos.X,GlyphPos.Y,SelGlyph);
  end
  else
  begin
    if Assigned(Action) and ActionHasImages then
    begin
      imgList := (Action as TCustomAction).ActionList.Images;
    end
    else
      imgList := Images;

    if Assigned(imgList) and (ImageIndex > -1) then
    begin
      imgList.Draw(Canvas,GlyphPos.X,GlyphPos.Y, ImageIndex);
    end;
  end;
end;

procedure TAdvToolButton.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; BiDiFlags: LongInt);
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
    end else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
  end;
end;

procedure TAdvToolButton.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
  BiDiFlags: LongInt);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize, Ofs: TPoint;
  imgList: TCustomImageList;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight
    else
      if Layout = blGlyphRight then Layout := blGlyphLeft;
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if not FGlyph.Empty then
    GlyphSize := Point(FGlyph.Width, FGlyph.Height) else
    GlyphSize := Point(0, 0);

  if Assigned(Action) and ActionHasImages then
    imgList := (Action as TCustomAction).ActionList.Images
  else
    imgList := Images;

  if Assigned(imgList) and (ImageIndex <> -1) and FGlyph.Empty then
    GlyphSize := Point(imgList.Width, imgList.Height);

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
    if Spacing = -1 then
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
    if Spacing = -1 then
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

  Ofs := Offset;

  { fixup the result variables }
  with GlyphPos do
  begin
    Inc(X, Client.Left + Ofs.X);
    Inc(Y, Client.Top + Ofs.Y);
  end;

  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X,
    TextPos.Y + Client.Top + Offset.X);
end;

function TAdvToolButton.DrawButton(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
  BiDiFlags: LongInt): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result, BiDiFlags);

  DrawButtonGlyph(Canvas, GlyphPos, State, Transparent);

  if not (State in [bsDown, bsExclusive]) then
  begin
    if FMouseInControl and Shaded and Enabled and not (State = bsDown) and Flat and not (csDesigning in ComponentState) then
    begin
      if Shaded then
        OffsetRect(Result, +1 , +1)
      else
        OffsetRect(Result, 0, +1);
    end;
  end;

  if ShowCaption then
    DrawButtonText(Canvas, Caption, Result, State, BiDiFlags);
end;


procedure TAdvToolButton.Paint;
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  DrawFlags: Integer;
  Offset: TPoint;
  PColorTo: TColor;
  mid: Integer;
  HasNoCaption: Boolean;
begin
  if not Enabled then
  begin
    FState := bsDisabled;
    FDragging := False;
  end
  else
  begin
    if (FState = bsDisabled) then
      if FDown and (GroupIndex <> 0) then
        FState := bsExclusive
      else
        FState := bsUp;
  end;

  if (Style = tasCheck) and (Down) then
  begin
    FState := bsDown;
  end;

  Canvas.Font := Self.Font;
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
    if ((FState in [bsDown, bsExclusive]) or
      (FMouseInControl and (FState <> bsDisabled)) or
      (csDesigning in ComponentState)) and not Rounded and not Flat and (BorderDownColor = clNone) then
        DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]],
          FillStyles[FTransparent] or BF_RECT);

      if {(csDesigning in ComponentState) or} (FBorderDownColor = clNone) or Rounded then
        InflateRect(PaintRect,-1,-1);

      if FMouseInControl and Enabled and not (csDesigning in ComponentState) then
      begin
        if (FState in [bsDown]) then
        begin
          Canvas.Brush.Color := ColorDown;
          PColorTo := ColorDownTo;
          Canvas.Pen.Color := FBorderDownColor;
        end
        else
        begin
          Canvas.Brush.Color := ColorHot;
          PColorTo := ColorHotTo;
          Canvas.Pen.Color := FBorderHotColor;
        end;

        if (Style = tasCheck) and Down and (FState <> bsDown) then
        begin
          Canvas.Pen.Color := FBorderDownColor;
          Canvas.Pen.Width := 1;
          Canvas.Brush.Color := ColorChecked;
          PColorTo := ColorCheckedTo;
          Canvas.Pen.Color := FBorderDownColor;
        end;

        Canvas.Pen.Width := 1;

        if Rounded then
          Canvas.Pen.Color := clNone;

        if PColorTo <> clNone then
        begin
          DrawGradient(Canvas, Canvas.Brush.Color, PColorTo, Canvas.Pen.Color, 16, PaintRect, False);
        end
        else
        begin
          if Canvas.Pen.Color = clNone then
            Canvas.Pen.Color := Canvas.Brush.Color;
          Canvas.Rectangle(PaintRect.Left,PaintRect.Top,PaintRect.Right,PaintRect.Bottom);
        end;

        if Rounded then
        begin
          InflateRect(PaintRect, +1, +1);
          if (FState in [bsDown]) then
            Canvas.Pen.Color := FBorderDownColor
          else
            Canvas.Pen.Color := FBorderHotColor;
          Canvas.Brush.Style := bsClear;
          Canvas.RoundRect(PaintRect.Left,PaintRect.Top,PaintRect.Right,PaintRect.Bottom,8,8);
        end;

        if FDropDownButton and FDropDownSplit then
        begin
          if FState = bsDown then
            Canvas.Pen.COlor := FBorderDownColor
          else
            Canvas.Pen.COlor := FBorderHotColor;

          Canvas.MoveTo(PaintRect.Right - 12, PaintRect.Top);
          Canvas.LineTo(PaintRect.Right - 12, PaintRect.Bottom);
        end;
      end
      else
      begin
        Canvas.Pen.Width := 1;

        if (Style = tasCheck) and Down then
        begin
          Canvas.Pen.Color := FBorderDownColor;
          Canvas.Pen.Width := 1;
          Canvas.Brush.Color := ColorChecked;

          if ColorCheckedTo <> clNone then
            DrawGradient(Canvas, Canvas.Brush.Color, ColorCheckedTo, Canvas.Pen.Color, 16, PaintRect, False)
          else
          begin
            if FBorderDownColor = clNone then
              Canvas.Pen.Color := Canvas.Brush.Color;

            Canvas.Rectangle(PaintRect.Left,PaintRect.Top,PaintRect.Right,PaintRect.Bottom);
          end;

        end
        else
        begin
          Canvas.Brush.Color := ColorToRGB(Color);
          if ColorTo <> clNone then
          begin
            if Down then
              Canvas.Pen.Color := BorderDownColor
            else
              Canvas.Pen.Color := BorderColor;

            if Rounded then
              Canvas.Pen.Color := clNone;

            DrawGradient(Canvas, Color, ColorTo, Canvas.Pen.Color, 16, PaintRect, False);
          end
          else
          begin
            Canvas.FillRect(PaintRect);
          end;

          if (FBorderColor <> clNone) then
          begin
            if Rounded then
            begin
              InflateRect(PaintRect, +1, +1);
              Canvas.Pen.Color := FBorderColor;
              Canvas.Pen.Width := 1;
              Canvas.RoundRect(PaintRect.Left,PaintRect.Top,PaintRect.Right,PaintRect.Bottom,8,8);
            end
            else
            begin
              Canvas.Pen.Color := FBorderColor;
              Canvas.Brush.Style := bsClear;
              Canvas.Rectangle(PaintRect.Left, PaintRect.Top, PaintRect.Right, PaintRect.Bottom);
            end;
          end;

        end;
      end;

    InflateRect(PaintRect, -1, -1);
  end;


  Offset := Point(0,0);

  if FState in [bsDown, bsExclusive] then
  begin
    if (FState = bsExclusive) and (not FFlat or not FMouseInControl) then
    begin
      Canvas.Brush.Color := ColorChecked;
      PColorTo := ColorCheckedTo;

      if Down then
        Canvas.Pen.Color := BorderDownColor
      else
        Canvas.Pen.Color := BorderColor;

      InflateRect(PaintRect, +1, +1);
      if PColorTo <> clNone then
      begin
        DrawGradient(Canvas, ColorChecked, ColorCheckedTo, Canvas.Pen.Color, 16, PaintRect, False)
      end
      else
      begin
        if Canvas.Pen.Color = clNone then
          Canvas.FillRect(PaintRect)
        else
          Canvas.Rectangle(PaintRect);
      end;
    end;
    if not FFlat and not (csDesigning in ComponentState) then
      Offset := Point(1,1);
  end
  else
  begin
    if FFlat then
    begin
      if FMouseInControl and Enabled and Shaded and not (FState = bsDown) and not (csDesigning in ComponentState) then
        Offset := Point(-1,-1)
    end;
  end;

  if FDropDownButton then
  begin
    mid := PaintRect.Top + (PaintRect.Bottom - PaintRect.Top) div 2;
    Canvas.Brush.Color := Font.Color;
    Canvas.Pen.Color := Font.Color;
    Canvas.Polygon([Point(PaintRect.Right -8, Mid -1),Point(PaintRect.Right - 4, Mid -1),Point(PaintRect.Right - 6, Mid + 1)]);
    PaintRect.Right := PaintRect.Right - 12;
  end;

  if FDown and (GroupIndex <> 0) then
    DrawButton(Canvas, PaintRect, Offset, Caption, FLayout, FMargin,
      FSpacing, bsDown, FTransparent, DrawTextBiDiModeFlags(0))
  else
    DrawButton(Canvas, PaintRect, Offset, Caption, FLayout, FMargin,
      FSpacing, FState, FTransparent, DrawTextBiDiModeFlags(0));

  HasNoCaption := not (ShowCaption and (Caption <> ''));
  if (csDesigning in ComponentState) and (ImageIndex = -1) and HasNoCaption and (Glyph.Empty) then
  begin
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clBlue;
    Canvas.Pen.Width := 1;
    Canvas.Rectangle(0,0,Width-1,Height-1);
  end;
end;

procedure TAdvToolButton.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvToolButton.UpdateTracking;
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
    
procedure TAdvToolButton.Loaded;
begin
  inherited Loaded;

  if FShaded then
    GenerateShade;

  if AutoThemeAdapt then
    ThemeAdapt;
end;

procedure TAdvToolButton.ShowDropDownMenu;
var
  pt: TPoint;
begin
  if Assigned(FDropDownMenu) then
  begin
    pt := Point(Left, Top + Height);
    pt := Parent.ClientToScreen(pt);
    FDropDownMenu.Popup(pt.X,pt.Y);
  end;
end;

procedure TAdvToolButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if (Button = mbLeft) and Enabled then
  begin
    if ((FDropDownButton) and (X > ClientRect.Right - 12))
      or (FDropDownButton and (not FDropDownSplit)) or ((not FDropDownButton) and assigned(FDropDownMenu)) then
    begin
      FState := bsUp;
      FMouseInControl := False;
      Repaint;
      if Assigned(FOnDropDown) then
        FOnDropDown(Self);
      ShowDropDownMenu;
      Exit;
    end;

    if not FDown then
    begin
      FState := bsDown;
      Invalidate;
    end;
    if Style = tasCheck then
    begin
      FState := bsDown;
      Repaint;
    end;

    FDragging := True;
  end;
end;

procedure TAdvToolButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if (not FDown) then NewState := bsUp
    else NewState := bsExclusive;

    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then NewState := bsExclusive else NewState := bsDown;

    if (Style = tasCheck) and FDown then
    begin
      NewState := bsDown;
    end;

    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end
  else if not FMouseInControl then
    UpdateTracking;
end;
    
procedure TAdvToolButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
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
      // Redraw face in-case mouse is captured 
      FState := bsUp;
      FMouseInControl := False;

      if Style = tasCheck then
      begin
        SetDown(not FDown);
        FState := bsUp;
      end;

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
        if FDown then
          FState := bsExclusive;
        Repaint;
      end;
    if DoClick then Click;
    UpdateTracking;
  end;
end;
    
procedure TAdvToolButton.Click;
begin
  inherited Click;
end;
    
function TAdvToolButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAdvToolButtonActionLink;
end;

procedure TAdvToolButton.SetGlyph(Value: TBitmap);
var
  x,y: Integer;
  PxlColor: TColor;
  c: byte;
begin
  FGlyph.Assign(Value);
  //if no disabled glyph is given... add this automatically...
  if FGlyphDisabled.Empty then
  begin
    FGlyphDisabled.Assign(Value);
    for x := 0 to FGlyphDisabled.Width - 1 do
      for y := 0 to FGlyphDisabled.Height - 1 do
      begin
        PxlColor := ColorToRGB(FGlyphDisabled.Canvas.Pixels[x, y]);
        c := Round((((PxlColor shr 16) + ((PxlColor shr 8) and $00FF) +
               (PxlColor and $0000FF)) div 3)) div 2 + 96;
        FGlyphDisabled.Canvas.Pixels[x, y] := RGB(c, c, c);
      end;
  end;
  Invalidate;
end;
    
procedure TAdvToolButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvToolButton.UpdateExclusive;
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

procedure TAdvToolButton.SetDown(Value: Boolean);
begin
  if (FGroupIndex = 0) and (Style = tasButton) then
    Value := False;

  if (Style = tasCheck) then
  begin
    FDown := Value;
    //FState := bsDown;
    Repaint;
    Exit;
  end;

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

procedure TAdvToolButton.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;
    
procedure TAdvToolButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;
    
procedure TAdvToolButton.SetImageIndex(const Value: integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TAdvToolButton.SetImageList(const Value: TCustomImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure TAdvToolButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;
    
procedure TAdvToolButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;
    
procedure TAdvToolButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TAdvToolButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;
    
procedure TAdvToolButton.WMLButtonDblClk(var Message: TWMLButtonDown);
begin
  inherited;
  {if FDown then }DblClick;
end;
    
procedure TAdvToolButton.CMEnabledChanged(var Message: TMessage);
const
  NewState: array[Boolean] of TButtonState = (bsDisabled, bsUp);
begin
  UpdateTracking;
  Repaint;
end;

procedure TAdvToolButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TAdvToolButton;
begin
  if integer(Message.WParam) = FGroupIndex then
  begin
    Sender := TAdvToolButton(Message.LParam);
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

procedure TAdvToolButton.CMDialogChar(var Message: TCMDialogChar);
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
    
procedure TAdvToolButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;
    
procedure TAdvToolButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;
    
procedure TAdvToolButton.CMSysColorChange(var Message: TMessage);
begin
  with TBitmap(FGlyph) do
  begin
    Invalidate;
  end;
end;

procedure TAdvToolButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  { Don't draw a border if DragMode <> dmAutomatic since this button is meant to 
    be used as a dock client. }
  if FFlat and not FMouseInControl and Enabled and (DragMode <> dmAutomatic) 
    and (GetCapture = 0) then
  begin
    FMouseInControl := True;
    Invalidate;
  end;

  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TAdvToolButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if Enabled and FMouseInControl then
  begin
    FMouseInControl := False;
    Invalidate;
  end;

  if FFlat and FMouseInControl and Enabled and not FDragging then
  begin
  end;

  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TAdvToolButton.SetGlyphDisabled(const Value: TBitmap);
begin
  FGlyphDisabled.Assign(Value);
end;

procedure TAdvToolButton.SetGlyphDown(const Value: TBitmap);
begin
  FGlyphDown.Assign(Value);
end;

procedure TAdvToolButton.SetGlyphHot(const Value: TBitmap);
begin
  FGlyphHot.Assign(Value);
end;

procedure TAdvToolButton.GenerateShade;
var
  r: TRect;
  bmp: TBitmap;
begin
  if not FGlyph.Empty then
  begin
    FGlyphShade.Width := FGlyph.Width;
    FGlyphShade.Height := FGlyph.Height;

    r := Rect(0,0,FGlyphShade.Width,FGlyphShade.Height);
    FGlyphShade.Canvas.Brush.Color := ColorToRGB(clBlack);
    FGlyphShade.Canvas.BrushCopy(r,FGlyph,r, FGlyph.Canvas.Pixels[0,FGlyph.Height-1]);
    FGlyphShade.Canvas.CopyMode := cmSrcInvert;
    FGlyphShade.Canvas.CopyRect(r,FGlyph.Canvas,r);

    bmp := TBitmap.Create;
    try
      bmp.Width := FGlyph.Width;
      bmp.Height := FGlyph.Height;
      bmp.Canvas.Brush.Color := ColorToRGB(clGray);
      bmp.Canvas.BrushCopy(r,FGlyphShade,r,ColorToRGB(clBlack));

      FGlyphShade.Canvas.CopyMode := cmSrcCopy;
      FGlyphShade.Canvas.CopyRect(r,bmp.Canvas,r);
    finally
      bmp.Free;
    end;
  end;
end;

procedure TAdvToolButton.SetShowCaption(const Value: Boolean);
begin
  if (FShowCaption <> Value) then
  begin
    FShowCaption := Value;
    Invalidate;
  end;
end;

procedure TAdvToolButton.SetShaded(const Value: Boolean);
begin
  FShaded := Value;

  if FShaded then
    if not (csLoading in ComponentState) then
    begin
      GenerateShade;
    end;
end;

procedure TAdvToolButton.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  Invalidate;
end;

procedure TAdvToolButton.SetColorChecked(const Value: TColor);
begin
  FColorChecked := Value;
  Invalidate;
end;

procedure TAdvToolButton.SetStyle(const Value: TAdvToolButtonStyle);
begin
  FStyle := Value;
  Invalidate;
end;

procedure TAdvToolButton.SetColorCheckedTo(const Value: TColor);
begin
  FColorCheckedTo := Value;
  Invalidate;
end;

procedure TAdvToolButton.SetRounded(const Value: Boolean);
begin
  FRounded := Value;
  Invalidate;
end;

procedure TAdvToolButton.SetDropDownButton(const Value: Boolean);
begin
  FDropDownButton := Value;
  Invalidate;
end;

procedure TAdvToolButton.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;


procedure TAdvToolButton.WndProc(var Message: TMessage);
begin
  // message does not seem to get through always?
  if (Message.Msg = WM_THEMECHANGED) and AutoThemeAdapt then
  begin
    ThemeAdapt;
  end;

  if (Message.Msg = CM_SYSFONTCHANGED) and AutoThemeAdapt then
  begin
    ThemeAdapt;
  end;
  
  inherited;
end;

procedure TAdvToolButton.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvToolButton.ThemeAdapt;
var
  eTheme: XPColorScheme;
begin
  eTheme := CurrentXPTheme();
  case eTheme of
    xpBlue: Look := 2;
    xpGreen: Look := 3;
    xpGray: Look := 4;
  else
    Look := 1;
  end;
end;

procedure TAdvToolButton.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  case AStyle of
    tsOffice2003Blue: Look := 2;
    tsOffice2003Silver: Look := 4;
    tsOffice2003Olive: Look := 3;
    tsOffice2003Classic: Look := 1;
    tsOffice2007Luna: Look := 8;
    tsOffice2007Obsidian: Look := 9;
    tsOffice2007Silver: Look := 10;
    tsWindowsXP: Look := 0;
    tsWhidbey: Look := 7;
    tsWindowsVista: Look := 11;
    tsWindows7: Look := 12;
    tsTerminal: Look := 13;
    tsOffice2010Blue: Look := 14;
    tsOffice2010Silver: Look := 15;
    tsOffice2010Black: Look := 16;
    tsWindows8: Look := 17;
    tsOffice2013White: Look := 18;
    tsOffice2013LightGray: Look := 19;
    tsOffice2013Gray: Look := 20;
    tsWindows10: Look := 21;
    tsOffice2016White: Look := 22;
    tsOffice2016Gray: Look := 23;
    tsOffice2016Black: Look := 24;
  end;
end;


procedure TAdvToolButton.SetLook(const Value: Integer);
begin
  case Value of
  // Windows XP
  0:begin
      self.Color := $EDF1F1;
      self.ColorTo := $DFEBEB;
      self.ColorHot := $FAFCFC;
      self.ColorHotTo := $E5ECED;
      self.ColorDown := $E0E6E7;
      self.ColorDownTo := $D8E0E1;
      self.ColorChecked := $FFFFFF;
      self.ColorCheckedTo := clNone;
      self.BorderDownColor := $AF987A;
      self.BorderHotColor := $C3CECE;
      self.BorderColor := clNone;
      self.Rounded := True;
      self.Flat := True;
    end;
  // Office 2002
  1:begin
      self.Color := clBtnFace;
      self.ColorTo := clNone;
      self.ColorHot := $EED2C1;
      self.ColorHotTo := clNone;
      self.ColorDown := $E2B598;
      self.ColorDownTo := clNone;
      self.ColorChecked := $E8E6E1;
      self.ColorCheckedTo := clNone;
      self.BorderDownColor := $C56A31;
      self.BorderHotColor := $C56A31;
      self.BorderColor := clNone;
      self.Rounded := False;
      self.Flat := True;
    end;
  // XP (Blue)
  2:begin
      self.Color := $FDEADA;
      self.ColorTo := $E4AE88;
      self.ColorHot := $CCF4FF;
      self.ColorHotTo := $91D0FF;
      self.ColorDown := $4E91FE;
      self.ColorDownTo := $8ED3FF;
      self.ColorChecked := $8ED3FF;
      self.ColorCheckedTo := $55ADFF;
      self.BorderDownColor := clBlack;
      self.BorderHotColor := clBlack;
      self.BorderColor := clNone;
      self.Rounded := False;
      self.Flat := True;
    end;
  // XP (Olive)
  3:begin
      self.Color := $CFF0EA;
      self.ColorTo := $8CC0B1;
      self.ColorHot := $CCF4FF;
      self.ColorHotTo := $91D0FF;
      self.ColorDown := $4E91FE;
      self.ColorDownTo := $8ED3FF;
      self.ColorChecked := $8ED3FF;
      self.ColorCheckedTo := $55ADFF;
      self.BorderDownColor := clBlack;
      self.BorderHotColor := clBlack;
      self.BorderColor := clNone;
      self.Rounded := False;
      self.Flat := True;
    end;
  // XP (Silver)
  4:begin
      self.Color := $ECE2E1;
      self.ColorTo := $B39698;
      self.ColorHot := $CCF4FF;
      self.ColorHotTo := $91D0FF;
      self.ColorDown := $4E91FE;
      self.ColorDownTo := $8ED3FF;
      self.ColorChecked := $8ED3FF;
      self.ColorCheckedTo := $55ADFF;
      self.BorderDownColor := clBlack;
      self.BorderHotColor := clBlack;
      self.BorderColor := clNone;
      self.Rounded := False;
      self.Flat := True;
    end;
  // Flat style
  5:begin
      self.Color := clBtnFace;
      self.ColorTo := clNone;
      self.ColorHot := clBtnFace;
      self.ColorHotTo := clNone;
      self.ColorDown := $00D8D3D2;
      self.ColorDownTo := clNone;
      self.ColorChecked := $00CAC7C7;
      self.ColorCheckedTo := clNone;
      self.BorderDownColor := clNone;
      self.BorderHotColor := clNone;
      self.BorderColor := clNone;
      self.Rounded := false;
      self.Flat := True;
    end;
  // Avant garde
  6:begin
      self.Color := $00CAFFFF;
      self.ColorTo := $00A6FFFF;
      self.ColorHot := $00A8F0FD;
      self.ColorHotTo := $007CE9FC;
      self.ColorDown := $004DE0FB;
      self.ColorDownTo := $007AE9FC;
      self.ColorChecked := $00B5E6F2;
      self.ColorCheckedTo := $009CDDED;
      self.BorderDownColor := clGray;
      self.BorderHotColor := clGray;
      self.BorderColor := clNone;
      self.Rounded := false;
      self.Flat := True;
    end;
  // Whidbey
  7:begin
      self.Color := $EBEEEF;
      self.ColorTo := $7E9898;
      self.ColorHot := $DCFFFF;
      self.ColorHotTo := $5BC0F7;
      self.ColorDown := $087FE8;
      self.ColorDownTo := $7CDAF7;
      self.ColorChecked := $8ED3FF;
      self.ColorCheckedTo := $55ADFF;
      self.BorderDownColor := clBlack;
      self.BorderHotColor := clBlack;
      self.BorderColor := clNone;
      self.Rounded := false;
      self.Flat := True;
    end;
  // Office 2007 Luna
  8:begin
      self.Color := $EEDBC8;
      self.ColorTo := $F6DDC9;

      self.ColorHot := $EBFDFF;
      self.ColorHotTo := $ACECFF;

      self.ColorDown := $76AFF1;
      self.ColorDownTo := $4190F3;

      self.ColorChecked := $B5DBFB;
      self.ColorCheckedTo := $78C7FE;

      self.BorderDownColor := $45667B;
      self.BorderHotColor := $99CEDB;
      self.BorderColor := $E0B99B;
      self.Rounded := true;
      self.Flat := True;
    end;
  // Office 2007 Obsidian
  9:begin
      self.Color := $DFDED6;
      self.ColorTo := $E4E2DB;

      self.ColorHot := $EBFDFF;
      self.ColorHotTo := $ACECFF;

      self.ColorDown := $76AFF1;
      self.ColorDownTo := $4190F3;

      self.ColorChecked := $B5DBFB;
      self.ColorCheckedTo := $78C7FE;

      self.BorderDownColor := $45667B;
      self.BorderHotColor := $99CEDB;
      self.BorderColor := $C0BCB2;
      self.Rounded := true;
      self.Flat := True;
    end;
  // Office 2007 Silver
  10:begin
      self.Color := $F3F3F1;
      self.ColorTo := $EEEAE7;

      self.ColorHot := $EBFDFF;
      self.ColorHotTo := $ACECFF;

      self.ColorDown := $76AFF1;
      self.ColorDownTo := $4190F3;

      self.ColorChecked := $B5DBFB;
      self.ColorCheckedTo := $78C7FE;

      self.BorderDownColor := $45667B;
      self.BorderHotColor := $99CEDB;
      self.BorderColor := $CCCAC9;
      self.Rounded := true;
      self.Flat := True;
    end;
    // WindowsVista
  11:begin
      self.Color := $FDF8F1;
      self.ColorTo := $FCEFD5;

      self.ColorHot := $FFFDF9;
      self.ColorHotTo := $FFFAF0;

      self.ColorDown := $FEF9F0;
      self.ColorDownTo := $FDF0D7;

      self.ColorChecked := $FBEDD3;
      self.ColorCheckedTo := $FAE9C6;

      self.BorderDownColor := $FEDF9A;
      self.BorderHotColor := $FCF2DA;
      self.BorderColor := $FDDE99;
      self.Rounded := true;
      self.Flat := True;
    end;
    // Windows7
  12:begin
      self.Color := $FCEBDC;
      self.ColorTo := $FCDBC1;

      self.ColorHot := $FDFBFA;
      self.ColorHotTo := $FDF3EB;

      self.ColorDown := $FCEBDC;
      self.ColorDownTo := $FCDBC1;

      self.ColorChecked := $FDFBFA;
      self.ColorCheckedTo := $FDF3EB;

      self.BorderDownColor := $CEA27D;
      self.BorderHotColor := $FBD6B8;
      self.BorderColor := $CEA27D;
      self.Rounded := true;
      self.Flat := True;
    end;
    // Terminal
  13:begin
      self.Color := clBtnFace;
      self.ColorTo := clBtnFace;

      self.ColorHot := clSilver;
      self.ColorHotTo := clSilver;

      self.ColorDown := clHighLight;
      self.ColorDownTo := clHighLight;

      self.ColorChecked := clWhite;
      self.ColorCheckedTo := clWhite;

      self.BorderDownColor := clGray;
      self.BorderHotColor := clGray;
      self.BorderColor := clGray;
      self.Rounded := false;
      self.Flat := True;
    end;
    // Office2010Blue
  14:begin
      self.Color := $FDF6EF;
      self.ColorTo := $F0DAC7;

      self.ColorHot := $8AE3FD;
      self.ColorHotTo := $D9F9FD;

      self.ColorDown := $6CD0FF;
      self.ColorDownTo := $7BEEFF;

      self.ColorChecked := $6CD0FF;
      self.ColorCheckedTo := $6CD0FF;

      self.BorderDownColor := $308AC2;
      self.BorderHotColor := $58CAF1;
      self.BorderColor := $C7B29F;
      self.Rounded := true;
      self.Flat := True;
    end;
    // Office2010Silver
  15:begin
      self.Color := $FFFFFF;
      self.ColorTo := $EDE5E0;

      self.ColorHot := $8AE3FD;
      self.ColorHotTo := $D9F9FD;

      self.ColorDown := $6CD0FF;
      self.ColorDownTo := $7BEEFF;

      self.ColorChecked := $6CD0FF;
      self.ColorCheckedTo := $6CD0FF;

      self.BorderDownColor := $308AC2;
      self.BorderHotColor := $58CAF1;
      self.BorderColor := $D2CDC8;
      self.Rounded := true;
      self.Flat := True;
    end;
    // Office2010Black
  16:begin
      self.Color := $BFBFBF;
      self.ColorTo := $919191;

      self.ColorHot := $8AE3FD;
      self.ColorHotTo := $D9F9FD;

      self.ColorDown := $6CD0FF;
      self.ColorDownTo := $7BEEFF;

      self.ColorChecked := $6CD0FF;
      self.ColorCheckedTo := $6CD0FF;


      self.BorderDownColor := $308AC2;
      self.BorderHotColor := $58CAF1;
      self.BorderColor := $6D6D6D;
      self.Rounded := true;
      self.Flat := True;
    end;

     // tsWindows8
  17:   begin
        self.Color := $F7F6F5;
        self.ColorTo := clNone;
        self.BorderColor := $E4E3E2;

        self.ColorHot := $F7EFE8;
        self.ColorHotTo := clNone;
        self.BorderHotColor := $F9CEA4;

        self.ColorDown := $F7E0C9;
        self.ColorDownTo := clNone;
        self.BorderDownColor := $E4A262;

        self.ColorChecked := $DAA026;
        self.ColorCheckedTo := clNone;

        self.Rounded := false;
        self.Flat := True;


      end;

      //tsOffice2013White:
  18:    begin
        self.Color := clWhite;
        self.ColorTo := clNone;
        self.BorderColor := $D4D4D4;

        self.ColorHot := $FCF0E4;
        self.ColorHotTo := clNone;
        self.BorderHotColor := $EAB47E;

        self.ColorDown := $FCE2C8;
        self.ColorDownTo := clNone;
        self.BorderDownColor := $E59D56;

        self.ColorChecked := $FF9933;
        self.ColorCheckedTo := clNone;

        self.Rounded := false;
        self.Flat := True;
      end;

      //tsOffice2013LightGray:
  19:   begin
        self.Color := $F6F6F6;
        self.ColorTo := clNone;
        self.BorderColor := $C6C6C6;

        self.ColorHot := $FCF0E4;
        self.ColorHotTo := clNone;
        self.BorderHotColor := $EAB47E;

        self.ColorDown := $FCE2C8;
        self.ColorDownTo := clNone;
        self.BorderDownColor := $E59D56;

        self.ColorChecked := $FF9933;
        self.ColorCheckedTo := clNone;

        self.Rounded := false;
        self.Flat := True;
      end;

      //tsOffice2013Gray:
  20:   begin
        self.Color := $E5E5E5;
        self.ColorTo := clNone;
        self.BorderColor := $ABABAB;

        self.ColorHot := $FCF0E4;
        self.ColorHotTo := clNone;
        self.BorderHotColor := $EAB47E;

        self.ColorDown := $FCE2C8;
        self.ColorDownTo := clNone;
        self.BorderDownColor := $E59D56;

        self.ColorChecked := $FF9933;
        self.ColorCheckedTo := clNone;


        self.Rounded := false;
        self.Flat := True;
      end;

     //tsWindows10:
  21:    begin
        self.Color := $F7F6F5;
        self.ColorTo := clNone;
        self.BorderColor := $E4E3E2;

        self.ColorHot := $F7EFE8;
        self.ColorHotTo := clNone;
        self.BorderHotColor := $F9CEA4;

        self.ColorDown := $F7E0C9;
        self.ColorDownTo := clNone;
        self.BorderDownColor := $E4A262;

        self.ColorChecked := $DAA026;
        self.ColorCheckedTo := clNone;

        self.Rounded := false;
        self.Flat := True;


      end;
     // tsOffice2016White:
  22:   begin

        self.Color := clWhite;
        self.ColorTo := clNone;
        self.BorderColor := $D4D4D4;

        self.ColorHot := $F2E1D5;
        self.ColorHotTo := clNone;
        self.BorderHotColor := $F2E1D5;

        self.ColorDown := $E3BDA3;
        self.ColorDownTo := clNone;
        self.BorderDownColor := $E3BDA3;

        self.ColorChecked := $F2D5C2;
        self.ColorCheckedTo := clNone;

        self.Rounded := false;
        self.Flat := True;
      end;

      //tsOffice2016Gray:
  23:   Begin
        self.Color := $B2B2B2;
        self.ColorTo := clNone;
        self.BorderColor := $444444;

        self.ColorHot := $F2E1D5;
        self.ColorHotTo := clNone;
        self.BorderHotColor := $F2E1D5;

        self.ColorDown := $E3BDA3;
        self.ColorDownTo := clNone;
        self.BorderDownColor := $E3BDA3;

        self.ColorChecked := $F2D5C2;
        self.ColorCheckedTo := clNone;

        self.Rounded := false;
        self.Flat := True;
        End;

      //tsOffice2016Black:
  24:   Begin

        self.Color := $363636;
        self.ColorTo := $363636;
        self.BorderColor := $444444;

        self.ColorHot := $6A6A6A;
        self.ColorHotTo := $6A6A6A;
        self.BorderHotColor := $6A6A6A;

        self.ColorDown := $444444;
        self.ColorDownTo := $444444;
        self.BorderDownColor := $444444;

        self.ColorChecked := $575757;
        self.ColorCheckedTo := $575757;

        self.Rounded := false;
        self.Flat := True;
      End;


  end;
end;

procedure TAdvToolButton.SetAutoThemeAdapt(const Value: Boolean);
begin
  FAutoThemeAdapt := Value;

  if not (csDesigning in ComponentState) then
  begin
    if FAutoThemeAdapt then
      ThemeAdapt;
  end;
end;

function TAdvToolButton.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvToolButton.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvToolButton.SetVersion(const Value: string);
begin

end;

function TAdvToolButton.GetColor: TColor;
begin
  Result := inherited Color;
end;

function TAdvToolButton.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

procedure TAdvToolButton.SetColor(const Value: TColor);
begin
  inherited Color := Value;
end;


procedure TAdvToolButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FDropDownMenu) then
    FDropDownMenu := nil;
end;


{ TAdvToolButtonActionLink }

procedure TAdvToolButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TAdvToolButton;
end;

function TAdvToolButtonActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    (FClient.Caption = (Action as TCustomAction).Caption);
end;

function TAdvToolButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and (FClient.GroupIndex <> 0) and
    FClient.AllowAllUp and (FClient.Down = (Action as TCustomAction).Checked);
end;

function TAdvToolButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := (FClient is TAdvToolButton) and
    (TAdvToolButton(FClient).GroupIndex = (Action as TCustomAction).GroupIndex);
end;

function TAdvToolButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FImageIndex = (Action as TCustomAction).ImageIndex);
end;

procedure TAdvToolButtonActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then
    TAdvToolButton(FClient).Caption := Value;
end;

procedure TAdvToolButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    TAdvToolButton(FClient).Down := Value;
end;

procedure TAdvToolButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then
    TAdvToolButton(FClient).GroupIndex := Value;
end;

procedure TAdvToolButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsGroupIndexLinked then
  begin
    FImageIndex := Value;
    TAdvToolButton(FClient).Invalidate;

    if (csDesigning in FClient.ComponentState) then
    begin
      if TAdvToolButton(FClient).ActionHasImages then
        TAdvToolButton(FClient).ImageIndex := Value;
    end;
  end;
end;

{ TAdvRepeatButton }

constructor TAdvRepeatButton.Create(AOwner: TComponent);
begin
  inherited;
  FInitRepeatPause := 400;
  FRepeatPause := 100;
end;

procedure TAdvRepeatButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);

  if FRepeatTimer = nil then
    FRepeatTimer := TTimer.Create(Self);

  FRepeatTimer.OnTimer := TimerExpired;
  FRepeatTimer.Interval := InitRepeatPause;
  FRepeatTimer.Enabled  := True;
end;

procedure TAdvRepeatButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;

procedure TAdvRepeatButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FState = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;



end.

