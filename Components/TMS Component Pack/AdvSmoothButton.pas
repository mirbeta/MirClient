{**************************************************************************}
{ TAdvSmoothButton component                                               }
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

unit AdvSmoothButton;

interface

{$I TMSDEFS.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, IniFiles,
  Math, AdvStyleIF, GDIPFill, Forms, ExtCtrls, ActnList, AdvGDIP, AdvHintInfo, Types;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.
  WMTABLETDEFBASE = $02C0;
  WMTABLETQUERYSYSTEMGESTURESTATUS = WMTABLETDEFBASE + 12;
  TABLETDISABLEPRESSANDHOLD = $00000001;

  // version history
  // v1.0.0.0 : first release
  // v1.5.0.0 : New: Status Indicator
  //          : Improved: GDI+ Drawing in seperate GDIPFill
  // v1.5.0.1 : Fixed : issue with wordwrapped caption
  // v1.6.0.0 : New : Focus indication
  //          : New : Keyboard support to handle click with SpaceBar and Enter keys
  //          : New : HotKey Support
  // v1.6.0.1 : Fixed : Picture and Text position
  //          : Fixed : Issue with status caption = ''
  // v1.6.0.2 : Improved : Button color gray when Enabled = false
  // v1.6.1.0 : New : Property ShiftDown to change the shift amount of text and image in down state
  //          : Improved : speed of click & dbl click handling
  // v1.6.2.0 : New : Added support for Actions
  //          : Fixed : Issue with Button Status when MouseUp outside button
  // v1.6.3.0 : New :  TextAlignment property
  //          : New : ParentFont property added
  //          : Improved: Text Rectangle drawing
  // v1.6.4.0 : New : Support for Windows Vista & Windows Seven style
  //          : Fixed: Issue with Odd spacing value and text rectangle
  // v1.6.5.0 : New : Built-in support for reduced color set for use with terminal servers
  // v1.6.5.1 : Fixed : issue with text alignment
  //          : Fixed : drawing issue with copy paste buttons at design time
  //          : Fixed : issue with Doublebuffering in WMPaint
  // v1.6.5.2 : Improved : Property ParentShowHint exposed
  // v1.6.5.3 : Fixed : Issue with CMDialogChar and Tabstop
  // v1.6.6.0 : New : Property ModalResult
  // v1.6.6.1 : Fixed : Issue with onclick called twice
  // v1.6.6.2 : New : Property FocusColor
  //          : Improved : Procedure Click exposed
  // v1.6.7.0 : New : Property ShowFocus
  // v1.6.8.0 : New : Built-in support for Office 2010 colors
  // v1.6.9.0 : New : PictureAlignment in Button
  // v1.6.9.1 : Fixed : Issue with background when double-buffered is false
  // v1.6.9.2 : Fixed : Memory leak with cached bitmap
  // v1.6.9.3 : Fixed : Issue with WMEraseBackGround in Delphi 2006 or newer
  // v1.6.9.4 : Fixed : Issue with caption positioning in AdvSmoothButton
  // v1.6.9.5 : Fixed : Issue with size rect of caption in gdipfill
  //          : Improved : show down state for touchscreen operation
  // v1.6.9.6 : Improved : color darkening for down state
  // v1.7.0.0 : New : Picturecontainer and ImageList support
  //          : New : Autosize control to picture size
  //          : New : OfficeHint support added
  // v1.7.0.1 : Fixed : Issue in drawing and calculation text with wordwrapping
  // v1.7.1.0 : New : Property GlowPercentage
  // v1.8.0.0 : New : Metro style
  //          : New : Disabled property
  //          : New : Rounding property
  // v1.8.1.0 : Fixed : Issue with font color
  //          : New : DisabledFontColor property
  // v1.9.0.0 : New : Picture Stretching and Stretch Mode
  //          : New : OnStatusClick
  //          : Fixed : Issue with double-clicking component
  // v1.9.0.1 : Fixed : Issue with setting cursor different from crDefault
  // v2.0.0.0 : New : Windows 8, Office 2013 styles added
  // v2.0.0.1 : Improved : ClickDelay property to turn off visual click delays
  // v2.0.0.2 : Fixed : Issue with simplelayout
  // v2.0.1.0 : New : InitPause, AllowTimer and RepeatInterval added
  // v2.0.1.1 : Fixed : Issue with Picture stretch and mode
  // v2.0.2.0 : New : WordWrapping property
  // v2.0.2.1 : Improved : Touch feedback
  // v2.0.2.2 : Fixed : Issue with use on operating systems older than Windows 7
  // v2.0.2.3 : Improved : Creation of click timer and caching bitmap only when necessary
  // v2.0.2.4 : Fixed : Issue with unregistering touch window in destroywnd
  // v2.1.0.0 : New : Windows 10, Office 2016 styles added
  // v2.1.0.1 : Fixed : Issue with toggling Appearance.SimpleLayoutBorder

type
  TAdvSmoothButton = class;

  TAdvSmoothButtonStatus = class(TPersistent)
  private
    FOwner: TAdvSmoothButton;
    FOffsetTop: integer;
    FOffsetLeft: integer;
    FVisible: Boolean;
    FCaption: String;
    FAppearance: TGDIPStatus;
    FOnChange: TNotifyEvent;
    procedure SetCaption(const Value: String);
    procedure SetOffsetLeft(const Value: integer);
    procedure SetOffsetTop(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetAppearance(const Value: TGDIPStatus);
  protected
    procedure Changed;
    procedure AppearanceChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothButton);
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

{$IFDEF DELPHI6_LVL}
  TAdvSmoothButtonActionLink = class(TControlActionLink)
  protected
    FClient: TAdvSmoothButton;
    procedure AssignClient(AClient: TObject); override;
  end;
{$ENDIF}

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothButton = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    {$IFDEF DELPHIXE_LVL}
    FTouchRegistered: Boolean;
    {$ENDIF}
    FPauseClick: Boolean;
    FClickTimer: TTimer;
    FColorDown: TColor;
    DisableCache: Boolean;
    FMouseInside: Boolean;
    FCache: TGPBitmap;
    FValidCache: Boolean;
    FFocused: Boolean;
    FDesignTime: Boolean;
    FAppearance: TGDIPButton;
    FColor: TColor;
    FDown: boolean;
    FPicture: TAdvGDIPPicture;
    FBevel: boolean;
    FShadow: boolean;
    FDisabledColor: TColor;
    FBevelColor: TColor;
    FButtonStatus: TAdvSmoothButtonStatus;
    FVerticalSpacing: integer;
    FHorizontalSpacing: integer;
    FModalResult: TModalResult;
    FShowFocus: Boolean;
    FAutoSizeToPicture: Boolean;
    FOfficeHint: TAdvHintInfo;
    FDisabledFontColor: TColor;
    FOnStatusClick: TNotifyEvent;
    FDefCursor: TCursor;
    FClickDelay: Boolean;
    FAllowTimer: Boolean;
    FRepeatInterval: Integer;
    FInitPause: Integer;
    procedure SetColor(const Value: TColor);
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure SetBevel(const Value: boolean);
    procedure SetShadow(const Value: boolean);
    procedure SetDisabledColor(const Value: TColor);
    procedure SetBevelColor(const Value: TColor);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure SetButtonStatus(const Value: TAdvSmoothButtonStatus);
    procedure SetHorizontalSpacing(const Value: integer);
    procedure SetVerticalSpacing(const Value: integer);
    procedure SetAppearance(const Value: TGDIPButton);
    procedure InvalidateCache;
    procedure SetModalResult(const Value: TModalResult);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetAutoSizeToPicture(const Value: Boolean);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetDisabledFontColor(const Value: TColor);
    procedure SetAllowTimer(const Value: Boolean);
    procedure SetRepeatInterval(const Value: Integer);
    procedure SetInitPause(const Value: Integer);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure ClickTimer(Sender: TObject);
    procedure UpdateSize;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Changed;
    procedure AppearanceChanged(Sender: TObject);
    procedure AppearanceFontChanged(Sender: TObject);
    procedure AppearanceFontStored(Sender: TObject; var IsStored: boolean);
    procedure PictureChanged(Sender: TObject);
    procedure StatusChanged(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function GetVersionNr: integer;
    procedure DoExit; override;
    procedure DoEnter; override;
    procedure DoClick; virtual;
    procedure Loaded; override;
{$IFDEF DELPHI6_LVL}
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
{$ENDIF}
    procedure CreateTimer;
    procedure DestroyTimer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure SetColorTones(ATones: TColorTones);
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeId: String;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Click; override;
    function XYToIndicator(X, Y: Integer): Boolean;
  published
    property AllowTimer: Boolean read FAllowTimer write SetAllowTimer default False;
    property InitPause: Integer read FInitPause write SetInitPause default 500;
    property RepeatInterval: Integer read FRepeatInterval write SetRepeatInterval default 100;
    property ClickDelay: Boolean read FClickDelay write FClickDelay default True;
    property AutoSizeToPicture: Boolean read FAutoSizeToPicture write SetAutoSizeToPicture default False;
    property Action;
    property Align;
    property Anchors;
    property Appearance: TGDIPButton read FAppearance write SetAppearance;
    property Status: TAdvSmoothButtonStatus read FButtonStatus write SetButtonStatus;
    property Bevel: boolean read FBevel write SetBevel default true;
    property BevelColor: TColor read FBevelColor write SetBevelColor default clWhite;
    property Constraints;
    property Caption;
    property Color: TColor read FColor write SetColor default clGray;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property ParentFont;
    property ParentShowHint;
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
    property PopupMenu;
    property Shadow: boolean read FShadow write SetShadow default false;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default $808080;
    property DisabledFontColor: TColor read FDisabledFontColor write SetDisabledFontColor default clSilver;
    property ShowHint;
    property TabOrder;
    property TabStop default true;
    property Visible;
    property Enabled;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default true;
    property HorizontalSpacing: integer read FHorizontalSpacing write SetHorizontalSpacing default 5;
    property VerticalSpacing: integer read FVerticalSpacing write SetVerticalSpacing default 5;
    property Version: string read GetVersion write SetVersion;
    property ModalResult: TModalResult read FModalResult write SetModalResult default mrNone;

    property OnStatusClick: TNotifyEvent read FOnStatusClick write FOnStatusClick;

    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnClick;
    property OnDblClick;
    property OnContextPopup;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnExit;
    property OnEnter;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
  end;

implementation

function GetWindowsVersion: Double;
var
  OSVersionInfo: TOSVersionInfo;
begin
  Result := 0;
  OSVersionInfo.dwOSVersionInfoSize := sizeof(TOSVersionInfo);
  if GetVersionEx(OSVersionInfo) then
  begin
    Result := OSVersionInfo.dwMajorVersion + OSVersionInfo.dwMinorVersion / 10;
  end;
end;

procedure DrawFocus(g: TGPGraphics; r: TGPRectF; rn: Integer; c: TColor);
var
  pathfocus: TGPGraphicsPath;
  pfocus: TGPPen;
begin
  pathfocus := GDIPFill.CreateRoundRectangle(r, rn, rtBoth, false);
  g.SetSmoothingMode(SmoothingModeDefault);
  pfocus := TGPPen.Create(MakeColor(255, c), 1);
  pfocus.SetDashStyle(DashStyleDot);
  g.DrawPath(pfocus, pathfocus);
  pfocus.Free;
  pathfocus.Free;
end;

{ TAdvSmoothButton }

type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

procedure TAdvSmoothButton.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothButton.AppearanceFontChanged(Sender: TObject);
begin
  ParentFont := false;
end;

procedure TAdvSmoothButton.AppearanceFontStored(Sender: TObject;
  var IsStored: boolean);
begin
  IsStored := not ParentFont;
end;

procedure TAdvSmoothButton.Changed;
begin
  FValidCache := false;
  Invalidate;
end;

procedure TAdvSmoothButton.ClickTimer(Sender: TObject);
begin
  if not AllowTimer or not Assigned(FClickTimer) then
    Exit;

  if not FPauseClick then
  begin
    FPauseClick := True;
    FClickTimer.Interval := RepeatInterval;
  end;

  DoClick;
end;

procedure TAdvSmoothButton.Click;
begin
  inherited;
end;

procedure TAdvSmoothButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      FDown := true;
      Repaint;
      DoClick;
      if ClickDelay then
        Sleep(150);
      FDown := false;
      repaint;
      Result := 1;
    end
    else
      inherited;
end;

procedure TAdvSmoothButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Changed;
end;

procedure TAdvSmoothButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseInside := true;
  Changed;
end;

procedure TAdvSmoothButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseInside := false;
  FDown := false;
  Changed;
end;

procedure TAdvSmoothButton.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if ParentFont then
  begin
    Appearance.OnFontChange := nil;
    Appearance.Font.Assign(Font);
    Appearance.OnFontChange := AppearanceFontChanged;
  end;
end;

procedure TAdvSmoothButton.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Changed;
end;

constructor TAdvSmoothButton.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := true;
  FAllowTimer := False;
  FInitPause := 500;
  FRepeatInterval := 100;
  FClickTimer := nil;
  FClickDelay := True;
  FColorDown := clNone;
  FPicture := TAdvGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FColor := clGray;
  FBevel := true;
  FBevelColor := clWhite;
  FVerticalSpacing := 5;
  FHorizontalSpacing := 5;
  FAutoSizeToPicture := False;
  FDisabledColor := $808080;
  FDisabledFontColor := clSilver;
  FAppearance := TGDIPButton.Create;
  FAppearance.OnChange := AppearanceChanged;
  FAppearance.OnFontChange := AppearanceFontChanged;
  FAppearance.OnIsFontStored := AppearanceFontStored;
  DisableCache := True;
  Width := 120;
  Height := 35;
  DisableCache := False;
  FCache := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
  FButtonStatus := TAdvSmoothButtonStatus.Create(Self);
  FButtonStatus.OnChange := StatusChanged;
  ControlStyle := ControlStyle - [csClickEvents];
  TabStop := true;
  FShowFocus := true;
  FOfficeHint := TAdvHintInfo.Create;
end;

procedure TAdvSmoothButton.CreateTimer;
begin
  if not Assigned(FClickTimer) then
  begin
    FClickTimer := TTimer.Create(Self);
    FClickTimer.Interval := 500;
    FClickTimer.Enabled := False;
    FClickTimer.OnTimer := ClickTimer;
  end;
end;

procedure TAdvSmoothButton.CreateWnd;
begin
  inherited;
  {$IFDEF DELPHIXE_LVL}
  if GetWindowsVersion > 6 then
  begin
    FTouchRegistered := RegisterTouchWindow(Handle, 0);
  end;
  {$ENDIF}
end;

procedure TAdvSmoothButton.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

destructor TAdvSmoothButton.Destroy;
begin
  DestroyTimer;
  FPicture.Free;
  FButtonStatus.Free;
  FAppearance.Free;
  FOfficeHint.Free;
  if FCache <> nil then
    FCache.Free;
  inherited;
end;

procedure TAdvSmoothButton.DestroyTimer;
begin
  if Assigned(FClickTimer) then
  begin
    FClickTimer.Free;
    FClickTimer := nil;
  end;
end;

procedure TAdvSmoothButton.DestroyWnd;
begin
  {$IFDEF DELPHIXE_LVL}
  if GetWindowsVersion > 6 then
  begin
    if FTouchRegistered then
      UnregisterTouchWindow(Handle);
  end;
  {$ENDIF}
  inherited;
end;

{$IFDEF DELPHI6_LVL}
function TAdvSmoothButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAdvSmoothButtonActionLink
end;

procedure TAdvSmoothButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  Changed;
end;
{$ENDIF}

procedure TAdvSmoothButton.DoClick;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.ModalResult := ModalResult;

  inherited Click;
end;

procedure TAdvSmoothButton.DoEnter;
begin
  inherited;
  FFocused := true;
  Changed;
end;

procedure TAdvSmoothButton.DoExit;
begin
  inherited;
  FFocused := false;
  Changed;
end;

function TAdvSmoothButton.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothButton.GetThemeId: String;
begin
  result := ClassName;
end;

function TAdvSmoothButton.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothButton.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not TabStop then
    Exit;
  case Key of
    VK_SPACE, VK_RETURN:
    begin
      //DoClick;
    end;
  end;
end;

procedure TAdvSmoothButton.KeyPress(var Key: Char);
var
  Form: TCustomForm;
begin
  inherited;

  if (Key = #32) or (Key = #13) then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := ModalResult;

    DoClick;
  end;
end;

procedure TAdvSmoothButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

procedure TAdvSmoothButton.Loaded;
begin
  inherited;
  FDefCursor := Cursor;
end;

procedure TAdvSmoothButton.LoadFromTheme(FileName: String);
var
  ini: TInifile;
begin
  ini := TIniFile.Create(FileName);
  FColor := ini.ReadInteger(GetThemeId, 'Color', Color);
  ini.Free;
  Changed;
end;

procedure TAdvSmoothButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FPauseClick := False;

  if Assigned(FClickTimer) then
  begin
    FClickTimer.Enabled := False;
    FClickTimer.Interval := InitPause;
  end;

  if Enabled and not XYToIndicator(X, Y) then
  begin
    FDown := true;

    if Assigned(FClickTimer) then
      FClickTimer.Enabled := AllowTimer;

    if TabStop then
      SetFocus;
    FValidCache := false;
    Repaint;
  end;
end;

procedure TAdvSmoothButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if XYToIndicator(X, Y) then
    Cursor := crHandPoint
  else
    Cursor := FDefCursor;
end;

procedure TAdvSmoothButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i: DWORD;
begin
  inherited;

  if Assigned(FClickTimer) then
    FClickTimer.Enabled := False;

  if XYToIndicator(X, Y) then
  begin
    if Assigned(OnStatusClick) then
      OnStatusClick(Self);
    Exit;
  end;

  if FDown and not FPauseClick then
    DoClick;

  if Enabled and ClickDelay then
  begin
    FDown := true;
    FMouseInside := true;
    FValidCache := false;

    i := GetTickCount;

    Invalidate;
    repeat
      Application.ProcessMessages;
    until (GetTickCount - i > 100);

    FDown := false;
    FValidCache := false;

    i := GetTickCount;

    Invalidate;
    repeat
      Application.ProcessMessages;
    until (GetTickCount - i > 100);
  end;
end;

procedure TAdvSmoothButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if Assigned(FAppearance) then
    FAppearance.DoNotification(Self, AComponent, AOperation);
  inherited;
end;

procedure TAdvSmoothButton.Paint;
var
  g: TGPGraphics;
  x, y, hs, vs: integer;
  c, fc: TColor;
  R: TRect;
  rgn1: HRGN;
  i: integer;
  p: TPoint;
  picidx: Integer;
  picname: String;
begin
//  if FTransparent and not FMouseEnter then
  begin
    // TRANSPARENCY CODE

    R := ClientRect;
    rgn1 :=  CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
    SelectClipRgn(Canvas.Handle, rgn1);

    i := SaveDC(Canvas.Handle);
    p := ClientOrigin;
    Windows.ScreenToClient(Parent.Handle, p);
    p.x := -p.x;
    p.y := -p.y;
    MoveWindowOrg(Canvas.Handle, p.x, p.y);

    SendMessage(Parent.Handle, WM_ERASEBKGND, Canvas.Handle, 0);
    // transparency ?
    SendMessage(Parent.Handle, WM_PAINT, Canvas.Handle, 0);

    if (Parent is TWinCtrl) then
     (Parent as TWinCtrl).PaintCtrls(Canvas.Handle, nil);

    RestoreDC(Canvas.Handle, i);

    SelectClipRgn(Canvas.Handle, 0);
    DeleteObject(rgn1);
  end;

  if not FValidCache then
  begin
    g := TGPGraphics.Create(FCache);
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    g.SetTextRenderingHint(TextRenderingHintAntiAlias);
    g.Clear(MakeColor(0, clWhite));

    hs := 0;
    vs := 0;
    if Status.Visible and ((Status.Caption <> '') or not Status.Appearance.Fill.Picture.Empty) then
    begin
      vs := VerticalSpacing;
      hs := HorizontalSpacing;
    end;

    if Enabled then
    begin
      c := Color;
      fc := Appearance.Font.Color;
    end
    else
    begin
      c := DisabledColor;
      fc := DisabledFontColor;
    end;

    if Enabled then
    begin
      picname := FAppearance.PictureName;
      picidx := FAppearance.ImageIndex;
    end
    else
    begin
      picname := FAppearance.DisabledPictureName;
      picidx := FAppearance.DisabledImageIndex;
    end;

    FAppearance.Draw(g, Caption, 0, 0, Width, Height, vs, hs, c, FColorDown, BevelColor, fc,
      Shadow, FDown and FMouseInside, Bevel, false, false, rtBoth, Picture, 0, 0, true, picidx, picname);

    if TabStop and FFocused and ShowFocus then
      DrawFocus(g, MakeRect(0,0, Width - 1, Height - 1), Appearance.Rounding, Appearance.FocusColor);

    if Status.Visible and ((Status.Caption <> '') or not Status.Appearance.Fill.Picture.Empty) then
    begin
      with Status do
      begin
        Appearance.CalculateSize(g, Status.Caption);
        x := Self.Width + FButtonStatus.OffsetLeft - Status.Appearance.GetWidth;
        y := Status.OffsetTop;
        Appearance.Draw(g, Status.OffsetLeft + x, y, 0, 0, true,Status.Caption);
      end;
    end;

    FValidCache := true;

    g.Free;
  end;

  if FValidCache then
  begin
    g := TGPGraphics.Create(Canvas.Handle);
    g.DrawImage(FCache, 0, 0);
    g.Free;
  end;
end;


procedure TAdvSmoothButton.SetAllowTimer(const Value: Boolean);
begin
  if FAllowTimer <> Value then
  begin
    FAllowTimer := Value;

    if FAllowTimer then
      CreateTimer
    else
      DestroyTimer;

    Changed;
  end;
end;

procedure TAdvSmoothButton.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothButton.InvalidateCache;
begin
  if not HandleAllocated then
    Exit;

  if DisableCache then
    Exit;

  if Assigned(FCache) then
    FCache.Free;

  FCache := TGPBitmap.Create(Width, Height);
  FValidCache := false;
end;

procedure TAdvSmoothButton.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothButton.Resize;
begin
  inherited;
  InvalidateCache;
end;

procedure TAdvSmoothButton.SaveToTheme(FileName: String);
var
  ini: TInifile;
begin
  ini := TIniFile.Create(FileName);
  ini.WriteInteger(GetThemeId, 'Color', Color);
  ini.Free;
  Changed;
end;

procedure TAdvSmoothButton.SetAppearance(const Value: TGDIPButton);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothButton.SetAutoSizeToPicture(const Value: Boolean);
begin
  if FAutoSizeToPicture <> Value then
  begin
    FAutoSizeToPicture := Value;
    UpdateSize;
    Changed;
  end;
end;

procedure TAdvSmoothButton.SetBevel(const Value: boolean);
begin
  if (FBevel <> Value) then
  begin
    FBevel := Value;
    Changed;
  end;
end;

procedure TAdvSmoothButton.SetBevelColor(const Value: TColor);
begin
  if (FBevelColor <> Value) then
  begin
    FBevelColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  InvalidateCache;
end;

procedure TAdvSmoothButton.SetButtonStatus(const Value: TAdvSmoothButtonStatus);
begin
  if FButtonStatus <> value then
  begin
    FButtonStatus.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothButton.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothButton.SetColorTones(ATones: TColorTones);
begin
  Color :=  ATones.Foreground.BrushColor;
  Appearance.Font.Color := ATones.Selected.TextColor;
  Appearance.Font.Name := GetMetroFont;
  Appearance.SimpleLayout := true;
  Repaint;
end;

procedure TAdvSmoothButton.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  FColorDown := clNone;
  Appearance.Rounding := 8;
  Appearance.SimpleLayout := False;
  Appearance.Font.Color := clBlack;
  // TODO : do color settings here
  case astyle of
    tsOffice2003Blue:
      Color := $00E3B28D;
    tsOffice2003Silver:
      Color := $00927476;
    tsOffice2003Olive:
      Color := $447A63; //08CC0B1; 006B7760;
    tsOffice2003Classic:
      Color := $00C9D1D5;
    tsOffice2007Luna:
      Color := $00FDEADA;
    tsOffice2007Obsidian:
      Color := $006E6E6D;
    tsWindowsXP:
      Color := $B9D8DC;
    tsWhidbey:
      Color := $00828F92;
    tsCustom: ;
    tsOffice2007Silver:
      Color := $00E7DCD5;
    tsWindowsVista:
      Color := $FDF8F1;
    tsWindows7:
      Color := $FCEBDC;
    tsTerminal:
      Color := clBtnFace;
    tsOffice2010Blue:
      Color := $F0DAC7;
    tsOffice2010Silver:
      Color := $EDE5E0;
    tsOffice2010Black:
      Color := $919191;
    tsWindows8, tsWindows10:
    begin
      Appearance.SimpleLayout := True;
      Appearance.Rounding := 0;
      Color := $F7F6F5;
      FColorDown := $F7E0C9;
    end;
    tsOffice2013White:
    begin
      Appearance.SimpleLayout := True;
      Appearance.Rounding := 0;
      Color := clWhite;
      FColorDown := $FCE2C8;
    end;
    tsOffice2013LightGray:
    begin
      Appearance.SimpleLayout := True;
      Appearance.Rounding := 0;
      Color := $F6F6F6;
      FColorDown := $FCE2C8;
    end;
    tsOffice2013Gray:
    begin
      Appearance.SimpleLayout := True;
      Appearance.Rounding := 0;
      Color := $E5E5E5;
      FColorDown := $FCE2C8;
    end;
    tsOffice2016White:
    begin
      Appearance.SimpleLayout := True;
      Appearance.Rounding := 0;
      Color := $F0F0F0;
      FColorDown := $E3BDA3;
      Appearance.Font.Color := $505050;
    end;
    tsOffice2016Gray:
    begin
      Appearance.SimpleLayout := True;
      Appearance.Rounding := 0;
      Color := $B2B2B2;
      FColorDown := $E3BDA3;
      Appearance.Font.Color := $424242;
    end;
    tsOffice2016Black:
    begin
      Appearance.SimpleLayout := True;
      Appearance.Rounding := 0;
      Color := $363636;
      FColorDown := $444444;
      Appearance.Font.Color := $A6A6A6;
    end;

  end;
end;

procedure TAdvSmoothButton.SetHorizontalSpacing(const Value: integer);
begin
  if FHorizontalSpacing <> value then
  begin
    FHorizontalSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothButton.SetInitPause(const Value: Integer);
begin
  if FInitPause <> value then
  begin
    FInitPause := Value;
    Changed;
  end;
end;

procedure TAdvSmoothButton.SetModalResult(const Value: TModalResult);
begin
  FModalResult := Value;
end;

procedure TAdvSmoothButton.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

procedure TAdvSmoothButton.SetPicture(const Value: TAdvGDIPPicture);
begin
  FPicture.Assign(Value);
  UpdateSize;
end;

procedure TAdvSmoothButton.SetShadow(const Value: boolean);
begin
  if (FShadow <> Value) then
  begin
    FShadow := Value;
    Changed;
  end;
end;

procedure TAdvSmoothButton.SetDisabledColor(const Value: TColor);
begin
  if (FDisabledColor <> Value) then
  begin
    FDisabledColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothButton.SetDisabledFontColor(const Value: TColor);
begin
  if FDisabledFontColor <> Value then
  begin
    FDisabledFontColor := Value;
    Changed;
  end;
end;


procedure TAdvSmoothButton.SetRepeatInterval(const Value: Integer);
begin
  if FRepeatInterval <> Value then
  begin
    FRepeatInterval := Value;
    Changed;
  end;
end;

procedure TAdvSmoothButton.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Changed;
  end;
end;

procedure TAdvSmoothButton.SetVersion(const Value: string);
begin
end;

procedure TAdvSmoothButton.SetVerticalSpacing(const Value: integer);
begin
  if FVerticalSpacing <> value then
  begin
    FVerticalSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothButton.StatusChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothButton.UpdateSize;
var
  pic: TAdvGDIPPicture;
  bmp: TBitmap;
  picidx: integer;
  picname: String;
begin
  if AutoSizeToPicture then
  begin
    if not (csLoading in ComponentState) and (csDesigning in ComponentState) then
    begin
      if Enabled then
      begin
        picidx := Appearance.ImageIndex;
        picname := Appearance.PictureName;
      end
      else
      begin
        picidx := Appearance.DisabledImageIndex;
        picname := AppearancE.DisabledPictureName;
      end;

      pic := TAdvGDIPPicture.Create;

      pic.Assign(Picture);

      if Assigned(Appearance.PictureContainer) then
        if picname <> '' then
          pic.Assign(Appearance.PictureContainer.FindPicture(picname));


      if Assigned(Appearance.ImageList) then
      begin
        if (picidx >= 0) and (picidx <= Appearance.ImageList.Count - 1) then
        begin
          bmp := TBitmap.Create;
          Appearance.ImageList.GetBitmap(picidx, bmp);
          if not bmp.Empty then
            pic.Assign(bmp);
          bmp.Free;
        end;
      end;

      if not pic.Empty then
      begin
        pic.GetImageSizes;
        Height := pic.Height + 6;
      end;
    end;
  end;
end;

procedure TAdvSmoothButton.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  {$IFDEF DELPHI2006_LVL}
  inherited;
  {$ENDIF}
  {$IFNDEF DELPHI2006_LVL}
  message.Result := 1;
  {$ENDIF}
end;

procedure TAdvSmoothButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TAdvSmoothButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if Assigned(OnDblClick) then
    OnDblClick(Self);
end;

procedure TAdvSmoothButton.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
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
    DC := GetDC(0);
    if DC <> 0 then
    begin
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
end;

procedure TAdvSmoothButton.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WMTABLETQUERYSYSTEMGESTURESTATUS:
    begin
      Message.Result := Message.Result or TABLETDISABLEPRESSANDHOLD;
    end;
  end;
end;

procedure TAdvSmoothButton.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

function TAdvSmoothButton.XYToIndicator(X, Y: Integer): Boolean;
var
  g: TGPGraphics;
  xt, yt: Integer;
begin
  Result := False;
  if Status.Visible and ((Status.Caption <> '') or not Status.Appearance.Fill.Picture.Empty) then
  begin
    with Status do
    begin
      g := TGPGraphics.Create(Canvas.Handle);
      Appearance.CalculateSize(g, Status.Caption);
      xt := Self.Width + FButtonStatus.OffsetLeft - Status.Appearance.GetWidth;
      yt := Status.OffsetTop;
      Result := PtInRect(Bounds(Status.OffsetLeft + xt, yt, Appearance.GetWidth, Appearance.GetHeight), Point(X, Y));
      g.Free;
    end;
  end;
end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

{ TAdvSmoothButtonStatus }


procedure TAdvSmoothButtonStatus.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothButtonStatus.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothButtonStatus) then
  begin
    FAppearance.Assign((Source as TAdvSmoothButtonStatus).Appearance);
    FOffsetTop := (Source as TAdvSmoothButtonStatus).OffsetTop;
    FOffsetLeft := (Source as TAdvSmoothButtonStatus).OffsetLeft;
    FVisible := (Source as TAdvSmoothButtonStatus).Visible;
    FCaption := (Source as TAdvSmoothButtonStatus).Caption;
  end;
end;

procedure TAdvSmoothButtonStatus.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothButtonStatus.Create(AOwner: TAdvSmoothButton);
begin
  FOwner := AOwner;
  FOffsetTop := 0;
  FOffsetLeft := 0;
  FVisible := False;
  FAppearance := TGDIPStatus.Create;
  FAppearance.OnChange := AppearanceChanged;
  if FOwner.FDesigntime then
  begin
    FCaption := '0';
    FAppearance.Fill.Color := clRed;
    FAppearance.Fill.GradientType := gtSolid;
    FAppearance.Fill.BorderColor := clGray;
    FAppearance.Font.Color := clWhite;
  end;
end;

destructor TAdvSmoothButtonStatus.Destroy;
begin
  FAppearance.Free;
  inherited;
end;

procedure TAdvSmoothButtonStatus.SetAppearance(const Value: TGDIPStatus);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    AppearanceChanged(Self);
  end;
end;

procedure TAdvSmoothButtonStatus.SetCaption(const Value: String);
begin
  if FCaption <> value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothButtonStatus.SetOffsetLeft(const Value: integer);
begin
  if FOffsetLeft <> value then
  begin
    FOffsetLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothButtonStatus.SetOffsetTop(const Value: integer);
begin
  if FOffsetTop <> value then
  begin
    FOffsetTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothButtonStatus.SetVisible(const Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    Changed;
  end;
end;


{$IFDEF DELPHI6_LVL}

{ TAdvSmoothButtonActionLink }

procedure TAdvSmoothButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TAdvSmoothButton;
end;

{$ENDIF}

end.
