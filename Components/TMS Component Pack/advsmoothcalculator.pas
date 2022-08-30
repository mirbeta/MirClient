{***********************************************************************}
{ TAdvSmoothCalculator component                                        }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2010 - 2015                                    }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}
unit AdvSmoothCalculator;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, Messages, Graphics, Controls, Buttons, SysUtils, StdCtrls,
  GDIPFill, AdvStyleIF, Types,
  AdvGDIP
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.
  WMTABLETDEFBASE = $02C0;
  WMTABLETQUERYSYSTEMGESTURESTATUS = WMTABLETDEFBASE + 12;
  TABLETDISABLEPRESSANDHOLD = $00000001;


  // Version history
  // v1.0.0.0 : first release
  // v1.0.1.0 : New : Built-in support for Office 2010 colors
  // v1.0.1.1 : Fixed : Issue with DropDown Parent
  //          : Improved : Changing text in edit reflected in calculator
  // v1.0.1.2 : Fixed : Issue with decimal when setting float
  // v1.0.1.3 : Fixed : Issue with numbers and precision = 0
  // v1.1.0.0 : New : AutoPrecision property to automatically determine precision of number
  // v1.1.0.1 : Fixed : Issue with Precision and sequence of numbers
  // v1.2.0.0 : New : Metro Style Support
  // v1.3.0.0 : New : Windows 8, Office 2013 styles added
  // v1.3.0.1 : Fixed : Small memory leak
  // v1.3.0.2 : Improved : touch feedback
  // v1.3.0.3 : Fixed : Issue with use on operating systems older than Windows 7
  // v1.3.0.4 : Fixed : Issue with unregistering touch window in destroywnd
  // v1.4.0.0 : New : Windows 10, Office 2016 styles added

type
  TAdvSmoothCalculator = class;

  TExtraButtons = class(TPersistent)
  private
    FOwner: TAdvSmoothCalculator;
    FExtra1: string;
    FExtra4: string;
    FExtra2: string;
    FExtra3: string;
    FOnChange: TNotifyEvent;
    procedure SetExtra1(const Value: string);
    procedure SetExtra2(const Value: string);
    procedure SetExtra3(const Value: string);
    procedure SetExtra4(const Value: string);
  public
    procedure Changed;
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvSmoothCalculator);
  published
    property Extra1:string read fExtra1 write SetExtra1;
    property Extra2:string read fExtra2 write SetExtra2;
    property Extra3:string read fExtra3 write SetExtra3;
    property Extra4:string read fExtra4 write SetExtra4;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TExtraButtonClickEvent = procedure(Sender:TObject; idx:integer; var value:extended) of object;


  TDisplayAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothCalculator;
    FGradientType: TAdvGradientType;
    FPicture: TAdvGDIPPicture;
    FColorOffOpacity: Byte;
    FColorOff: TColor;
    FEndColor: TColor;
    FHatchStyle: THatchStyle;
    FAngle: integer;
    FStartOpacity: Byte;
    FOnChange: TNotifyEvent;
    FFill: TGDIPFill;
    FEndOpacity: Byte;
    FStartColor: TColor;
    procedure SetAngle(const Value: integer);
    procedure SetColorOff(const Value: TColor);
    procedure SetColorOffOpacity(const Value: Byte);
    procedure SetEndColor(const Value: TColor);
    procedure SetEndOpacity(const Value: Byte);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetGradientType(const Value: TAdvGradientType);
    procedure SetHatchStyle(const Value: THatchStyle);
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure SetStartColor(const Value: TColor);
    procedure SetStartOpacity(const Value: Byte);
  protected
    procedure FillChanged(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure Changed;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    destructor Destroy; override;
  published
    property Fill: TGDIPFill read FFill write SetFill;
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
    property ColorStart: TColor read FStartColor write SetStartColor default $004080FF;
    property ColorEnd: TColor read FEndColor write SetEndColor default clRed;
    property ColorOff: TColor read FColorOff write SetColorOff default clGray;
    property ColorOffOpacity: Byte read FColorOffOpacity write SetColorOffOpacity default 255;
    property OpacityStart: Byte read FStartOpacity write SetStartOpacity default 255;
    property OpacityEnd: Byte read FEndOpacity write SetEndOpacity default 255;
    property GradientType: TAdvGradientType read FGradientType write SetGradientType default gtVertical;
    property HatchStyle: THatchStyle read FHatchStyle write SetHatchStyle default HatchStyleHorizontal;
    property Angle: integer read FAngle write SetAngle default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCalculatorLook = class(TGDIPButton)
  private
    FBevel: boolean;
    FColor: TColor;
    FBevelColor: TColor;
    FOwner: TAdvSmoothCalculator;
    FDisplayAppearance: TDisplayAppearance;
    FFill: TGDIPFill;
    FButtonColorSpecial: TColor;
    FOnChange: TNotifyEvent;
    procedure SetBevel(const Value: boolean);
    procedure SetBevelColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetDisplayAppearance(const Value: TDisplayAppearance);
    procedure SetOwner(const Value: TAdvSmoothCalculator);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetButtonColorSpecial(const Value: TColor);
  protected
    procedure FillChanged(Sender: TObject);
    property Owner: TAdvSmoothCalculator read FOwner write SetOwner;
    procedure Changed; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; override;
    destructor Destroy; override;
  published
    property ButtonBevel: boolean read FBevel write SetBevel default true;
    property ButtonBevelColor: TColor read FBevelColor write SetBevelColor default clWhite;
    property ButtonColorSpecial: TColor read FButtonColorSpecial write SetButtonColorSpecial default clGray;
    property ButtonColor: TColor read FColor write SetColor default clGray;
    property DisplayAppearance: TDisplayAppearance read FDisplayAppearance write SetDisplayAppearance;
    property Fill: TGDIPFill read FFill write SetFill;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TValueChangedEvent = procedure(Sender: TObject; Value: Double) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothCalculator = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    {$IFDEF DELPHIXE_LVL}
    FTouchRegistered: Boolean;
    {$ENDIF}
    FFocused: Boolean;
    FDesignTime: Boolean;
    FCache: TGPBitmap;
    FValidCache: Boolean;
    FFloat: Double;
    FButtonDown: Integer;
    newval:boolean;
    prevval:extended;
    prevop:integer;
    FDecim: Double;
    FCalculatorLook: TCalculatorLook;
    FExtraButtons: TExtraButtons;
    FPrecision: smallint;
    fOnExtraButtonClick: TExtraButtonClickEvent;
    FOnValueChanged: TValueChangedEvent;
    FAutoPrecision: Boolean;
    procedure SetCalculatorLook(const Value: TCalculatorLook);
    procedure SetExtraButtons(const Value: TExtraButtons);
    function GetFloat: Double;
    procedure SetFloat(const Value: Double);
    procedure WMGetMinMaxInfo(var Msg: TMessage); Message WM_GETMINMAXINFO;
    procedure SetPrecision(const Value: SmallInt);
    procedure SetAutoPrecision(const Value: Boolean);
    function GetPrecision: SmallInt;
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure CalculatorChanged(Sender: TObject);
    procedure NumButtonClick(ButtonIdx: Integer);
    procedure DrawBackGround(g: TGPGraphics);
    procedure DrawCalculatorButtons(g: TGPGraphics);
    procedure DrawDisplay(g: TGpGraphics);
    procedure FillChanged(Sender: TObject);
    procedure Changed;
    function EStrToFloat(s: string): extended;
    procedure docalc;
    procedure doplus;
    procedure domin;
    procedure domul;
    procedure dodiv;
    procedure doeq;
    procedure doperc;
    procedure Resize; override;
    procedure DoExit; override;
    procedure DoEnter; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    function XYToButton(X, Y: Integer): Integer;
    function ButtonAtFunction(Key: Integer): Integer;
  published
    property AutoPrecision: Boolean read FAutoPrecision write SetAutoPrecision default False;
    property ExtraButtons: TExtraButtons read FExtraButtons write SetExtraButtons;
    property CalculatorLook: TCalculatorLook read FCalculatorLook write SetCalculatorLook;
    property FloatValue: Double read GetFloat write SetFloat;
    property OnExtraButtonClick: TExtraButtonClickEvent read fOnExtraButtonClick write fOnExtraButtonClick;
    property OnValueChanged: TValueChangedEvent read FOnValueChanged write FOnValueChanged;
    property Precision: SmallInt read GetPrecision write SetPrecision default 0;
    property Align;
    property Color;
    property TabStop;
  end;

implementation

{$I DELPHIXE.INC}

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

function Darker(Color:TColor; Percent:Byte):TColor;
var
  r, g, b:Byte;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := r - muldiv(r, Percent, 100);  //Percent% closer to black
  g := g - muldiv(g, Percent, 100);
  b := b - muldiv(b, Percent, 100);
  result := RGB(r, g, b);
end;

function StripThousandSep(s: string): string;
begin
  while (Pos(ThousandSeparator, s) > 0) do
    Delete(s, Pos(ThousandSeparator, s), 1);
  Result := s;
end;

{ TAdvSmoothCalculator }

procedure TAdvSmoothCalculator.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothCalculator) then
  begin
    FExtraButtons.Assign((Source as TAdvSmoothCalculator).ExtraButtons);
    FCalculatorlook.Assign((Source as TAdvSmoothCalculator).CalculatorLook);
    FloatValue := (Source as TAdvSmoothCalculator).FloatValue;
    Changed;
  end;
end;

procedure TAdvSmoothCalculator.DrawCalculatorButtons(g: TGPGraphics);
var
  i, n: integer;
  FButtonWidth: integer;
  FButtonHeight: integer;
  ex: array[1..4] of string;
  l, t: integer;
  cap: String;
  c: TColor;
  picname: String;
  picidx: Integer;
begin
  n := 5;
  with fExtraButtons do
   begin
    if (fExtra1 <> '') or (fExtra2 <> '') or (fExtra3 <> '') or (fExtra4 <> '') then n := 6;

    c := CalculatorLook.ButtonColor;
    FButtonWidth := ((Width - 4) div n) - 2;
    FButtonHeight := ((Height - 4) div 5) - 2;

    ex[1] := fExtra1;
    ex[2] := fExtra2;
    ex[3] := fExtra3;
    ex[4] := fExtra4;

    if Enabled then
    begin
      picname := CalculatorLook.PictureName;
      picidx := CalculatorLook.ImageIndex;
    end
    else
    begin
      picname := CalculatorLook.DisabledPictureName;
      picidx := CalculatorLook.DisabledImageIndex;
    end;

    for i := 1 to 4 do
    if ex[i]<>'' then
     begin
       CalculatorLook.Draw(g, ex[i]+' ', 2 + 5 * (FButtonWidth + 2), 2 + i * (FButtonHeight + 2) , FbuttonWidth, FButtonHeight,
         0, 0, c, clNone, CalculatorLook.ButtonBevelColor, CalculatorLook.Font.Color, false, FButtonDown = i + 18, calculatorlook.ButtonBevel, false, false, rtBoth, nil, 0, 0, true, picidx, picname);
     end;
   end;

   for i := 0 to 18 do
   begin
    l := 0;
    t := 0;
    case i of
    0,1,4,7:l:=2;
    2,5,8,14:l:=2+(FButtonWidth+2);
    3,6,9,15:l:=2+2*(FButtonWidth+2);
    10,11,12,13:l:=2+3*(FButtonWidth+2);
    16,17,18:l:=2+4*(FButtonWidth+2);
    end;

    case i of
    7,8,9,10:t:=2 + (FButtonHeight + 2);
    4,5,6,11,18:t:=2+2*(FButtonHeight+2);
    1,2,3,12,16:t:=2+3*(FButtonHeight+2);
    0,13,14,15,17:t:=2+4*(FButtonHeight+2);
    end;


    cap := '';
    case i of
    0..9: cap := inttostr(i)+' ';
    10: cap := '+ ';
    11: cap := '- ';
    12: cap := '* ';
    13: cap := '/ ';
    14: cap := '+/- ';
    15: cap := '. ';
    16: cap := 'C ';
    17: cap := '= ';
    18: cap := '% ';
    end;


    if i > 9 then
      c := CalculatorLook.ButtonColorSpecial;

    CalculatorLook.Draw(g, cap, l, t , FbuttonWidth, FButtonHeight, 0, 0, c, clNone, CalculatorLook.ButtonBevelColor, CalculatorLook.Font.Color,
      false, FButtonDown = i, calculatorlook.ButtonBevel, false, false, rtBoth, nil, 0, 0, true, picidx, picname);
   end;
end;

procedure TAdvSmoothCalculator.DrawDisplay(g: TGpGraphics);
var
  r: TGPRectF;
  s: String;
  I: Integer;
  pres: SmallInt;
begin
  r := MakeRect(2, 2, Width - 4, ((Height - 4) div 5) - 2);
  CalculatorLook.DisplayAppearance.Fill.Fill(g, r);
  with CalculatorLook.DisplayAppearance do
  begin
    pres := Precision;
    if (pres > 0) then
    begin
      if (floatvalue < 1) and (Length(Format('%g', [FloatValue])) > 7) then
        s := '0';
      for I := 0 to 5 - pres do
      begin
        s := s+'0';
      end;
      s := s + '.';
      for I := 0 to pres - 1 do
      begin
        s := s+'0';
      end;
    end
    else
      s := '000000';

    DrawLed(g, r, vtNormal, s, '', FloatValue,
      0, ColorOff, ColorOffOpacity, ColorStart, ColorEnd, OpacityStart,
        OpacityEnd, GradientType, Angle, HatchStyle, Picture);
  end;
end;

function TAdvSmoothCalculator.ButtonAtFunction(Key: Integer): Integer;
begin
  Result := -1;
  case Key of
    VK_NUMPAD0, VK_NUMPAD1, VK_NUMPAD2, VK_NUMPAD3,
    VK_NUMPAD4, VK_NUMPAD5, VK_NUMPAD6, VK_NUMPAD7,
    VK_NUMPAD8, VK_NUMPAD9: Result := Key - 96;
    VK_DIVIDE: Result := 13;
    VK_SUBTRACT: Result := 11;
    VK_ADD: Result := 10;
    VK_MULTIPLY: Result := 12;
    VK_RETURN, VK_SPACE: Result := 17;
    VK_DELETE: Result := 16;
    VK_DECIMAL: Result := 15;
  end;
end;

procedure TAdvSmoothCalculator.CalculatorChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothCalculator.Changed;
begin
  FValidCache := false;
  invalidate;
end;

constructor TAdvSmoothCalculator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := true;
  FExtraButtons := TExtraButtons.Create(Self);
  FCalculatorLook := TCalculatorLook.Create;
  FCalculatorlook.Owner := Self;
  FCalculatorLook.OnChange := CalculatorChanged;
  Width := 200;
  Height := 200;
  FButtonDown := -1;
  FPrecision := 0;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);

  TabStop := true;
  FAutoPrecision := False;
end;

procedure TAdvSmoothCalculator.CreateWnd;
begin
  inherited;
  {$IFDEF DELPHIXE_LVL}
  if GetWindowsVersion > 6 then
  begin
    FTouchRegistered := RegisterTouchWindow(Handle, 0);
  end;
  {$ENDIF}
end;

destructor TAdvSmoothCalculator.Destroy;
begin
  if Assigned(FCache) then
    FCache.Free;

  FCalculatorLook.Free;
  FExtraButtons.Free;
  inherited;
end;

procedure TAdvSmoothCalculator.DestroyWnd;
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

procedure TAdvSmoothCalculator.docalc;
var
  e: extended;
begin
  e := FFloat;
  try
    case prevop of
    0:prevval := prevval + e;
    1:prevval := prevval * e;
    2:prevval := prevval - e;
    3:if (e<>0) then prevval:=prevval/e else prevval:=0;
    else
      prevval := FFloat;
    end;
  except
    prevval:=0;
  end;

  FloatValue := prevval;
  newval := true;
end;

procedure TAdvSmoothCalculator.dodiv;
begin
  docalc;
  prevop := 3;
end;

procedure TAdvSmoothCalculator.DoEnter;
begin
  inherited;
  FFocused := true;
  Changed;
end;

procedure TAdvSmoothCalculator.doeq;
begin
  DoCalc;
  prevval := FFloat;
  prevop := -1;
end;

procedure TAdvSmoothCalculator.DoExit;
begin
  inherited;
  FFocused := false;
  Changed;
end;

procedure TAdvSmoothCalculator.domin;
begin
  docalc;
  prevop := 2;
end;

procedure TAdvSmoothCalculator.domul;
begin
  docalc;
  prevop := 1;
end;

procedure TAdvSmoothCalculator.doperc;
var
  e: extended;
begin
  e := FFloat;
  e := prevval * e/100;
  FFloat := e;
end;

procedure TAdvSmoothCalculator.doplus;
begin
  docalc;
  prevop := 0;
end;

procedure TAdvSmoothCalculator.DrawBackGround(g: TGPGraphics);
begin
  FCalculatorLook.FFill.Fill(g, MakeRect(0, 0, width - 1, Height - 1));
end;

procedure TAdvSmoothCalculator.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

 procedure TAdvSmoothCalculator.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothCalculator.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

function TAdvSmoothCalculator.EStrToFloat(s: string): extended;
var
  pres: SmallInt;
begin
  if Pos(ThousandSeparator, s) > 0 then
    s := StripThousandSep(s);

  pres := GetPrecision;

  if (pres > 0) and (Length(s) > pres) then
     if s[Length(s) - pres] = Thousandseparator then
      s[Length(s) - pres] := Decimalseparator;

  try
    TryStrToFloat(s, Result);
  except
    Result := 0;
  end;
end;

procedure TAdvSmoothCalculator.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothCalculator.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

 function TAdvSmoothCalculator.GetFloat: Double;
begin
  Result := FFloat;
end;

function TAdvSmoothCalculator.GetPrecision: SmallInt;
var
  f: Double;
  str, strsub: String;
  ps: integer;
begin
  if AutoPrecision then
  begin
    f := FloatValue;
    str := Format('%g', [f]);
    ps := Pos(DecimalSeparator, str);
    if ps > 0 then
    begin
      strsub := copy(str, ps + 1, Length(str) - ps + 1);
      Result := Length(strsub);
     end
    else
      Result := 0;
  end
  else
    Result := FPrecision;
end;

procedure TAdvSmoothCalculator.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  FButtonDown := ButtonAtFunction(Key);
  if FButtonDown <> -1 then
  begin
    NumButtonClick(FButtonDown);
  end;
  Changed;
end;

procedure TAdvSmoothCalculator.KeyUp(var Key: Word; Shift: TShiftState);
 begin
  inherited;
  FButtonDown := -1;
  Changed;
end;

procedure TAdvSmoothCalculator.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FFocused := true;
  FButtonDown := XYToButton(X, Y);
  Changed;
end;

procedure TAdvSmoothCalculator.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FButtonDown <> -1 then
   begin
    NumButtonClick(FButtonDown);
    FButtonDown := -1;
    Changed;
  end;
end;

procedure TAdvSmoothCalculator.NumButtonClick(ButtonIdx: Integer);
var
  e,n:extended;
  f: double;
begin
  if (ButtonIdx in [10..14,16,17,18,19..22]) then
    FDecim := 0;

  if (ButtonIdx < 10) then
  begin
    f := FloatValue;
    if ((FloatValue = 0 ) and (FDecim = 0)) or (newval) then
    begin
       FFloat := ButtonIdx;
      newval := false;
    end
   else
   begin
     if FDecim > 0 then
     begin
       f := f + ButtonIdx / FDecim;
       FDecim := FDecim * 10;
       FFloat := f;
       Changed;
     end
     else
     begin
       f := f * 10 + ButtonIdx;
       FloatValue := f;
     end;
   end;
 end
 else
  begin
   case ButtonIdx of
   10:begin doplus; prevop:=0; end;
   11:begin domin; prevop:=2; end;
   12:begin domul; prevop:=1; end;
   13:begin dodiv; prevop:=3; end;
   14:FFloat := FFloat * -1;
   15:begin
       FDecim := 10;
       if newval then
         FloatValue := 0;
       newval := false;
      end;
   16:begin
        FFloat := 0;
        prevval := 0;
        prevop := -1;
      end;
   17:doeq;
   18:begin doperc; end;
   19..22:begin
           e := FFloat;
           n := e;
           if Assigned(FOnExtraButtonClick) then
             FOnExtraButtonClick(Self, ButtonIdx - 18,e);
           if (e<>n) then FFloat := e;
          end;
    end;
  end;

  if Assigned(OnValueChanged) then
    OnValueChanged(Self, FloatValue);
end;

procedure TAdvSmoothCalculator.Paint;
var
  g: TGPGraphics;
  p: TGPPen;
begin
  if not FValidCache then
  begin
    if Assigned(FCache) then
      FCache.Free;

    FCache := TGPBitmap.Create(Width, Height);
    g := TGPGraphics.Create(FCache);
    g.SetSmoothingMode(SmoothingModeAntiAlias);
    g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    DrawBackGround(g);
    DrawCalculatorButtons(g);
    DrawDisplay(g);
    if TabStop and FFocused then
    begin
      g.SetSmoothingMode(SmoothingModeDefault);
      p := TGPPen.Create(MakeColor(255, clBlack));
      p.SetDashStyle(DashStyleDot);
      g.DrawRectangle(p, MakeRect(0, 0, Width - 1, Height - 1));
      p.Free;
    end;

    g.Free;
    FValidCache := true;
  end;

  if FValidCache then
  begin
    g := TGPGraphics.Create(Canvas.Handle);
    g.DrawImage(FCache, 0, 0);
    g.free;
  end;
end;

procedure TAdvSmoothCalculator.Resize;
begin
  inherited;
  Changed;
end;

procedure TAdvSmoothCalculator.SetAutoPrecision(const Value: Boolean);
begin
  if FAutoPrecision <> Value then
  begin
    FAutoPrecision := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalculator.SetCalculatorLook(const Value: TCalculatorLook);
begin
  if FCalculatorLook <> value then
  begin
    FCalculatorLook.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCalculator.SetColorTones(ATones: TColorTones);
begin
  CalculatorLook.Fill.Color := ATones.Background.BrushColor;
  CalculatorLook.Fill.ColorTo := ATones.Background.BrushColor;
  CalculatorLook.Fill.BorderColor := ATones.Background.BorderColor;
  CalculatorLook.DisplayAppearance.Fill.Color := ATones.Foreground.BrushColor;
  CalculatorLook.DisplayAppearance.Fill.ColorTo := ATones.Foreground.BrushColor;
  CalculatorLook.ButtonColor := ATones.Foreground.BrushColor;
  CalculatorLook.DisplayAppearance.ColorStart := ATones.Selected.BrushColor;
  CalculatorLook.DisplayAppearance.ColorEnd := ATones.Selected.BrushColor;
  CalculatorLook.Font.Color := ATones.Foreground.TextColor;
  CalculatorLook.ButtonBevel:= false;
  CalculatorLook.ButtonColorSpecial := ATones.Foreground.BrushColor;
  CalculatorLook.SimpleLayout := true;
end;

procedure TAdvSmoothCalculator.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  CalculatorLook.SimpleLayout := False;
  CalculatorLook.DisplayAppearance.Fill.Color := clwhite;
  CalculatorLook.DisplayAppearance.Fill.ColorTo := clWhite;
  CalculatorLook.DisplayAppearance.Fill.Opacity := 70;
  CalculatorLook.DisplayAppearance.Fill.OpacityTo := 120;
  CalculatorLook.DisplayAppearance.ColorOff := clWhite;
  CalculatorLook.DisplayAppearance.ColorOffOpacity := 40;
  case astyle of
    tsOffice2003Blue:
    begin
      with CalculatorLook do
      begin
        ButtonColor := $00E3B28D;
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
        DisplayAppearance.ColorStart := $D68759;
        DisplayAppearance.ColorEnd := $D68759;
        CalculatorLook.Font.Color := $933803;
      end;
    end;
    tsOffice2003Silver:
    begin
      with CalculatorLook do
      begin
        ButtonColor := $00927476;
        Fill.Color := $00E6D8D8;
        Fill.ColorTo := $00E6D8D8;
        DisplayAppearance.ColorStart := $BDA4A5;
        DisplayAppearance.ColorEnd := $BDA4A5;
        CalculatorLook.Font.Color := clWhite;
      end;
    end;
    tsOffice2003Olive:
    begin
      with CalculatorLook do
      begin
        ButtonColor := $447A63;
        Fill.Color := $CFF0EA;
        Fill.ColorTo := $CFF0EA;
        DisplayAppearance.ColorStart := $82C0AF;
        DisplayAppearance.ColorEnd := $82C0AF;
        CalculatorLook.Font.Color := clWhite;
      end;
    end;
    tsOffice2003Classic:
    begin
      with CalculatorLook do
      begin
        Fill.Color := $00F2F2F2;
        Fill.ColorTo := $00F2F2F2;
        ButtonColor := $00C9D1D5;
        DisplayAppearance.ColorStart := $808080;
        DisplayAppearance.ColorEnd := $808080;
        CalculatorLook.Font.Color := $808080;
      end;
    end;
    tsOffice2007Luna:
    begin
      with CalculatorLook do
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
        ButtonColor := $00FDEADA;
        DisplayAppearance.ColorStart := RGB(0, 133, 186);
        DisplayAppearance.ColorEnd := RGB(0, 133, 186);
        CalculatorLook.Font.Color := $723708;
      end;
    end;
    tsOffice2007Obsidian:
    begin
      with CalculatorLook do
      begin
        Fill.Color := $5C534C;
        Fill.ColorTo := $5C534C;
        ButtonColor := $006E6E6D;
        DisplayAppearance.ColorStart := $F2F1F0;
        DisplayAppearance.ColorEnd := $F2F1F0;
        CalculatorLook.Font.Color := $F2F1F0;
      end;
    end;
    tsWindowsXP:
    begin
      with CalculatorLook do
      begin
        Fill.Color := $00B6B6B6;
        Fill.ColorTo := $00B6B6B6;
        ButtonColor := $B9D8DC;
        DisplayAppearance.ColorStart := clBlack;
        DisplayAppearance.ColorEnd := clBlack;
        CalculatorLook.Font.Color := clBlack;
      end;
    end;
    tsWhidbey:
    begin
      with CalculatorLook do
      begin
        Fill.Color := $F5F9FA;
        Fill.ColorTo := $F5F9FA;
        ButtonColor := $00828F92;
        DisplayAppearance.ColorStart := $7E9898;
        DisplayAppearance.ColorEnd := $7E9898;
        CalculatorLook.Font.Color := clBlack;
      end;
    end;
    tsOffice2007Silver:
    begin
      with CalculatorLook do
      begin
        Fill.Color := $00CAC1BA;
        Fill.ColorTo := $00CAC1BA;
        ButtonColor := $00E7DCD5;
        DisplayAppearance.ColorStart := $F8F7F6;
        DisplayAppearance.ColorEnd := $F8F7F6;
        CalculatorLook.Font.Color := clBlack;
      end;
    end;
    tsWindowsVista:
    begin
      with CalculatorLook do
      begin
        Fill.Color := $FDF8F1;
        Fill.ColorTo := $FCEFD5;
        ButtonColor := $FDF8F1;
        DisplayAppearance.ColorStart := $FDDE99;
        DisplayAppearance.ColorEnd := $FDDE99;
        CalculatorLook.Font.Color := clBlack;
      end;
    end;
    tsWindows7:
    begin
      with CalculatorLook do
      begin
        Fill.Color := $FDF8F1;
        Fill.ColorTo := $FCEFD5;
        ButtonColor := $FCEBDC;
        DisplayAppearance.ColorStart := $F9D996;
        DisplayAppearance.ColorEnd := $F9D996;
        CalculatorLook.Font.Color := clBlack;
      end;
    end;
    tsTerminal:
    begin
      with CalculatorLook do
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        ButtonColor := clBtnFace;
        DisplayAppearance.ColorStart := Darker(clBtnFace, 20);
        DisplayAppearance.ColorEnd := Darker(clBtnFace, 20);
        CalculatorLook.Font.Color := clBlack;
        ButtonBevelColor := clSilver;
      end;
    end;
    tsOffice2010Blue:
    begin
      with CalculatorLook do
      begin
        ButtonColor := $F0DAC7;
        Fill.Color := $EAD3BF;
        Fill.ColorTo := clNone;
        DisplayAppearance.ColorStart := $5B391E;
        DisplayAppearance.ColorEnd := $5B391E;
        CalculatorLook.Font.Color := $5B391E;
      end;
    end;
    tsOffice2010Silver:
    begin
      with CalculatorLook do
      begin
        ButtonColor := $EDE5E0;
        Fill.Color := $D4CFCB;
        Fill.ColorTo := clNone;
        DisplayAppearance.ColorStart := $7C6D66;
        DisplayAppearance.ColorEnd := $7C6D66;
        CalculatorLook.Font.Color := $5B391E;
      end;
    end;
    tsOffice2010Black:
    begin
      with CalculatorLook do
      begin
        ButtonColor := $919191;
        Fill.Color := $656565;
        Fill.ColorTo := clNone;
        DisplayAppearance.ColorStart := $CFCFCF;
        DisplayAppearance.ColorEnd := $CFCFCF;
        CalculatorLook.Font.Color := clWhite;
      end;
    end;
    tsWindows8, tsWindows10:
    begin
      with CalculatorLook do
      begin
        ButtonColor := $F7F6F5;
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        DisplayAppearance.ColorStart := $DAA026;
        DisplayAppearance.ColorEnd := $DAA026;
        CalculatorLook.Font.Color := clBlack;
      end;
    end;
    tsOffice2013White:
    begin
      with CalculatorLook do
      begin
        ButtonColor := clWhite;
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        DisplayAppearance.ColorStart := $FF9933;
        DisplayAppearance.ColorEnd := $FF9933;
        CalculatorLook.Font.Color := clBlack;
      end;
    end;
    tsOffice2013LightGray:
    begin
      with CalculatorLook do
      begin
        ButtonColor := $F6F6F6;
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        DisplayAppearance.ColorStart := $FF9933;
        DisplayAppearance.ColorEnd := $FF9933;
        CalculatorLook.Font.Color := clBlack;
      end;
    end;
    tsOffice2013Gray:
    begin
      with CalculatorLook do
      begin
        ButtonColor := $E5E5E5;
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        DisplayAppearance.ColorStart := $FF9933;
        DisplayAppearance.ColorEnd := $FF9933;
        CalculatorLook.Font.Color := clBlack;
      end;
    end;
   tsOffice2016White:
    begin
      with CalculatorLook do
      begin
        ButtonColor := clWhite;
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        DisplayAppearance.ColorStart := $E3BDA3;
        DisplayAppearance.ColorEnd := $E3BDA3;
        CalculatorLook.Font.Color := clBlack;
      end;
    end;
    tsOffice2016Gray:
    begin
      with CalculatorLook do
      begin
        ButtonColor := $B2B2B2;
        Fill.Color := $B2B2B2;
        Fill.ColorTo := clNone;
        DisplayAppearance.ColorStart := $E3BDA3;
        DisplayAppearance.ColorEnd := $E3BDA3;
        CalculatorLook.Font.Color := clBlack;
      end;
    end;
    tsOffice2016Black:
    begin
      with CalculatorLook do
      begin
        ButtonColor := $363636;
        Fill.Color := $363636;
        Fill.ColorTo := clNone;
        DisplayAppearance.ColorStart := $444444;
        DisplayAppearance.ColorEnd := $444444;
        CalculatorLook.Font.Color := $A6A6A6;
      end;
    end;

    end;


  CalculatorLook.ButtonColorSpecial := Darker(CalculatorLook.ButtonColor, 25);
  CalculatorLook.Fill.BorderColor := CalculatorLook.ButtonColorSpecial;
  CalculatorLook.ButtonBevelColor := CalculatorLook.ButtonColorSpecial;
end;

procedure TAdvSmoothCalculator.SetExtraButtons(const Value: TExtraButtons);
begin
  if FExtraButtons <> Value then
  begin
    FExtraButtons.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCalculator.SetFloat(const Value: Double);
begin
  FFloat := Value;
  Changed;
end;

procedure TAdvSmoothCalculator.SetPrecision(const Value: SmallInt);
begin
  if FPrecision <> value then
  begin
    FPrecision := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCalculator.WMGetMinMaxInfo(var Msg: TMessage);
begin
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize := Point(50, 50);
end;

procedure TAdvSmoothCalculator.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WMTABLETQUERYSYSTEMGESTURESTATUS:
    begin
      Message.Result := Message.Result or TABLETDISABLEPRESSANDHOLD;
    end;
  end;
end;

function TAdvSmoothCalculator.XYToButton(X, Y: Integer): Integer;
var
  i, n: integer;
  FButtonWidth: integer;
  FButtonHeight: integer;
  ex: array[1..4] of string;
  l, t: integer;
begin
  Result := -1;
  n := 5;
  with fExtraButtons do
  begin
    if (fExtra1 <> '') or (fExtra2 <> '') or (fExtra3 <> '') or (fExtra4 <> '') then n := 6;

    FButtonWidth := ((Width - 4) div n) - 2;
    FButtonHeight := ((Height - 4) div 5) - 2;

    ex[1] := fExtra1;
    ex[2] := fExtra2;
    ex[3] := fExtra3;
    ex[4] := fExtra4;

    for i := 1 to 4 do
    begin
      if ex[i]<>'' then
      begin
        l := 2 + 5 * (FButtonWidth + 2);
        t := 2 + i * (FButtonHeight + 2);

        if PtInRect(Bounds(l, t, FbuttonWidth, FButtonHeight), Point(X, Y)) then
        begin
          Result := 18 + i;
          Break;
        end;
      end;
    end;

    for i := 0 to 18 do
    begin
      l := 0;
      t := 0;
      case i of
        0,1,4,7:l:=2;
        2,5,8,14:l:=2+(FButtonWidth+2);
        3,6,9,15:l:=2+2*(FButtonWidth+2);
        10,11,12,13:l:=2+3*(FButtonWidth+2);
        16,17,18:l:=2+4*(FButtonWidth+2);
      end;

      case i of
        7,8,9,10:t:=2 + (FButtonHeight + 2);
        4,5,6,11,18:t:=2+2*(FButtonHeight+2);
        1,2,3,12,16:t:=2+3*(FButtonHeight+2);
        0,13,14,15,17:t:=2+4*(FButtonHeight+2);
      end;

      if PtInRect(Bounds(l, t, FbuttonWidth, FButtonHeight), Point(X, Y)) then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

{ TCalculatorLook }

procedure TCalculatorLook.Assign(Source: TPersistent);
begin
  if Source is TCalculatorLook then
  begin
    FBevel := (Source as TCalculatorLook).ButtonBevel;
    FBevelColor := (Source as TCalculatorLook).ButtonBevelColor;
    FButtonColorSpecial := (Source as TCalculatorLook).ButtonColorSpecial;
    FColor := (Source as TCalculatorLook).ButtonColor;
    FDisplayAppearance.Assign((Source as TCalculatorLook).DisplayAppearance);
    FFill.Assign((Source as TCalculatorLook).Fill);
    Changed;
  end
  else
    Inherited;
end;

procedure TCalculatorLook.Changed;
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TCalculatorLook.Create;
begin
  inherited;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FBevel := true;
  FBevelColor := clWhite;
  FButtonColorSpecial := clGray;
  FColor := clGray;
  FDisplayAppearance := TDisplayAppearance.Create;
end;

destructor TCalculatorLook.Destroy;
begin
  FDisplayAppearance.Free;
  FFill.Free;
  inherited;
end;

procedure TCalculatorLook.FillChanged(Sender: TObject);
begin
  if Assigned(FOwner) then
    FOwner.Changed;
end;

procedure TCalculatorLook.SetBevel(const Value: boolean);
begin
  if FBevel <> value then
  begin
    FBevel := Value;
    Changed;
  end;
end;

procedure TCalculatorLook.SetBevelColor(const Value: TColor);
begin
  if FBevelColor <> value then
  begin
    FBevelColor := Value;
    Changed;
  end;
end;

procedure TCalculatorLook.SetButtonColorSpecial(const Value: TColor);
begin
  if FButtonColorSpecial <> value then
  begin
    FButtonColorSpecial := Value;
    Changed;
  end;
end;

procedure TCalculatorLook.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TCalculatorLook.SetDisplayAppearance(const Value: TDisplayAppearance);
begin
  if FDisplayAppearance <> Value then
  begin
    FDisplayAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TCalculatorLook.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TCalculatorLook.SetOwner(const Value: TAdvSmoothCalculator);
begin
  FOwner := Value;
  FDisplayAppearance.FOwner := FOwner;
end;

{ TWinCtrl }

{ TDisplayAppearance }

procedure TDisplayAppearance.Assign(Source: TPersistent);
begin
  if Source is TDisplayAppearance then
  begin
    FFill.Assign((Source as TDisplayAppearance).Fill);
    FPicture.Assign((Source as TDisplayAppearance).Picture);
    FStartColor := (Source as TDisplayAppearance).ColorStart;
    FEndColor := (Source as TDisplayAppearance).ColorEnd;
    FColorOff := (Source as TDisplayAppearance).ColorOff;
    FColorOffOpacity := (Source as TDisplayAppearance).ColorOffOpacity;
    FStartOpacity := (Source as TDisplayAppearance).OpacityStart;
    FEndOpacity := (Source as TDisplayAppearance).OpacityEnd;
    FGradientType := (Source as TDisplayAppearance).GradientType;
    FHatchStyle := (Source as TDisplayAppearance).HatchStyle;
    FAngle := (Source as TDisplayAppearance).Angle;
    Changed;
  end
  else
    inherited;
end;

procedure TDisplayAppearance.Changed;
begin
  if Assigned(FOwner) then
    FOwner.Changed;
end;

constructor TDisplayAppearance.Create;
begin
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  Fpicture :=  TAdvGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FStartColor := $004080FF;
  FEndColor := clRed;
  FColorOff := clGray;
  FColorOffOpacity := 255;
  FStartOpacity := 255;
  FEndOpacity := 255;
  FGradientType := gtVertical;
  FHatchStyle := HatchStyleHorizontal;
  FAngle := 0;
end;

destructor TDisplayAppearance.Destroy;
begin
  FFill.Free;
  FPicture.Free;
  inherited;
end;

procedure TDisplayAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TDisplayAppearance.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TDisplayAppearance.SetAngle(const Value: integer);
begin
  if FAngle <> value then
  begin
    FAngle := Value;
    Changed;
  end;
end;

procedure TDisplayAppearance.SetColorOff(const Value: TColor);
begin
  if FColorOff <> Value then
  begin
    FColorOff := Value;
    Changed;
  end;
end;

procedure TDisplayAppearance.SetColorOffOpacity(const Value: Byte);
begin
  if FColorOffOpacity <> value then
  begin
    FColorOffOpacity := Value;
    Changed;
  end;
end;

procedure TDisplayAppearance.SetEndColor(const Value: TColor);
begin
  if FEndColor <> value then
  begin
    FEndColor := Value;
    Changed;
  end;
end;

procedure TDisplayAppearance.SetEndOpacity(const Value: Byte);
begin
  if FEndOpacity <> value then
  begin
    FEndOpacity := Value;
    Changed;
  end;
end;

procedure TDisplayAppearance.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TDisplayAppearance.SetGradientType(const Value: TAdvGradientType);
begin
  if FGradientType <> value then
  begin
    FGradientType := Value;
    Changed;
  end;
end;

procedure TDisplayAppearance.SetHatchStyle(const Value: THatchStyle);
begin
  if FHatchStyle <> value then
  begin
    FHatchStyle := Value;
    Changed;
  end;
end;

procedure TDisplayAppearance.SetPicture(const Value: TAdvGDIPPicture);
begin
  if FPicture <> Value then
  begin
    FPicture.Assign(value);
    Changed;
  end;
end;

procedure TDisplayAppearance.SetStartColor(const Value: TColor);
begin
  if FStartColor <> Value then
  begin
    FStartColor := Value;
    Changed;
  end;
end;

procedure TDisplayAppearance.SetStartOpacity(const Value: Byte);
begin
  if FStartOpacity <> Value then
  begin
    FStartOpacity := Value;
    Changed;
  end;
end;


{ TExtraButtons }

procedure TExtraButtons.Assign(Source: TPersistent);
begin
  if Source is TExtraButtons then
  begin
     FExtra1 := (Source as TExtraButtons).Extra1;
     FExtra2 := (Source as TExtraButtons).Extra2;
     FExtra3 := (Source as TExtraButtons).Extra3;
     FExtra4 := (Source as TExtraButtons).Extra4;
     Changed;
  end
  else
    inherited;
end;

procedure TExtraButtons.Changed;
begin
  FOwner.Changed;
end;

constructor TExtraButtons.Create(AOwner: TAdvSmoothCalculator);
begin
  FOwner := AOwner;
end;

procedure TExtraButtons.SetExtra1(const Value: string);
begin
  fExtra1 := Value;
  FOwner.Changed;
end;

procedure TExtraButtons.SetExtra2(const Value: string);
begin
  fExtra2 := Value;
  FOwner.Changed;
end;

procedure TExtraButtons.SetExtra3(const Value: string);
begin
  fExtra3 := Value;
  FOwner.Changed;
end;

procedure TExtraButtons.SetExtra4(const Value: string);
begin
  fExtra4 := Value;
  FOwner.Changed;
end;

end.
