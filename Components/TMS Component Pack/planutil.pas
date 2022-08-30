{***********************************************************************}
{ TPLANNER utility functions                                            }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 1999-2015                                      }
{            Email: info@tmssoftware.com                                }
{            Web: http://www.tmssoftware.com                            }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The complete     }
{ source code remains property of the author and may not be distributed,}
{ published, given or sold in any form as such. No parts of the source  }
{ code can be included in any other component or application without    }
{ written authorization of the author.                                  }
{***********************************************************************}

{$I TMSDEFS.INC}

unit planutil;

interface

uses
  Classes, Windows, Controls, Graphics, SysUtils, PlanHTML
  {$IFDEF DELPHI6_LVL}
  , StrUtils, Types
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type
  TArrowDirection = (adUp,adDown,adRight,adLeft);

  TVAlignment = (vtaCenter, vtaTop, vtaBottom);

  TDateTimeObject = class(TObject)
  private
    FDT: TDateTime;
    property DT: TDateTime read FDT write FDT;
  end;

  TDateTimeList = class(TList)
  private
    function GetDT(index: Integer): TDateTime;
    procedure SetDT(index: Integer; const Value: TDateTime);
  public
    property Items[index: Integer]: TDateTime read GetDT write SetDT; default;
    procedure Add(Value: TDateTime);
    procedure Insert(Index: Integer; Value: TDateTime);
    procedure Delete(Index: Integer);
    procedure Clear; override;
    destructor Destroy; override;
  end;

  TGaugeOrientation = (goHorizontal, goVertical);

  TGaugeSettings = record
    Level0Color: TColor;
    Level0ColorTo : TColor;
    Level1Color : TColor;
    Level1ColorTo : TColor;
    Level2Color : TColor;
    Level2ColorTo : TColor;
    Level3Color : TColor;
    Level3ColorTo : TColor;
    Level1Perc : Integer;
    Level2Perc : Integer;
    BorderColor : TColor;
    ShowBorder : Boolean;
    Stacked : Boolean;
    ShowPercentage : Boolean;
    Font : TFont;
    CompletionSmooth : Boolean;
    ShowGradient : Boolean;
    Steps : Integer;
    Position : Integer;
    BackgroundColor : TColor;
    Orientation : TGaugeOrientation;
    CompletionFormat: string;
  end;


  TRecurrencyDialogLanguage = class(TPersistent)
  private
    FCaption: string;
    FExceptions: string;
    FSettings: string;
    FRange: string;
    FRecurrencyPattern: string;
    FPatternDetails: string;
    FRangeFor: string;
    FRangeOccurences: string;
    FRangeInfinite: string;
    FRangeUntil: string;
    FFreqWeekly: string;
    FFreqDaily: string;
    FFreqHourly: string;
    FFreqYearly: string;
    FFreqNone: string;
    FFreqMonthly: string;
    FButtonRemove: string;
    FButtonAdd: string;
    FButtonClear: string;
    FButtonCancel: string;
    FButtonOK: string;
    FEveryDay: string;
    FEveryWeekDay: string;

    FDayFriday: string;
    FDayThursday: string;
    FDayMonday: string;
    FDayTuesday: string;
    FDaySaturday: string;
    FDaySunday: string;
    FDayWednesday: string;

    FMonthJanuary: string;
    FMonthFebruary: string;
    FMonthMarch: string;
    FMonthApril: string;
    FMonthMay: string;
    FMonthJune: string;
    FMonthJuly: string;
    FMonthAugust: string;
    FMonthSeptember: string;
    FMonthOctober: string;
    FMonthNovember: string;
    FMonthDecember: string;

    FEveryMonthDay: string;
    FEveryYearDay: string;
    FEvery: string;
    FEveryThird: string;
    FEveryFirst: string;
    FEveryFourth: string;
    FEverySecond: string;
    FDayWeekend: string;
    FDayWeekday: string;
    FInterval: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write FCaption;
    property Settings: string read FSettings write FSettings;
    property Exceptions: string read FExceptions write FExceptions;
    property RecurrencyPattern: string read FRecurrencyPattern write FRecurrencyPattern;
    property PatternDetails: string read FPatternDetails write FPatternDetails;
    property Range: string read FRange write FRange;
    property RangeInfinite: string read FRangeInfinite write FRangeInfinite;
    property RangeUntil: string read FRangeUntil write FRangeUntil;
    property RangeFor: string read FRangeFor write FRangeFor;
    property RangeOccurences: string read FRangeOccurences write FRangeOccurences;
    property FreqNone: string read FFreqNone write FFreqNone;
    property FreqHourly: string read FFreqHourly write FFreqHourly;
    property FreqDaily: string read FFreqDaily write FFreqDaily;
    property FreqWeekly: string read FFreqWeekly write FFreqWeekly;
    property FreqMonthly: string read FFreqMonthly write FFreqMonthly;
    property FreqYearly: string read FFreqYearly write FFreqYearly;
    property ButtonAdd: string read FButtonAdd write FButtonAdd;
    property ButtonClear: string read FButtonClear write FButtonClear;
    property ButtonRemove: string read FButtonRemove write FButtonRemove;
    property ButtonOK: string read FButtonOK write FButtonOK;
    property ButtonCancel: string read FButtonCancel write FButtonCancel;
    property Every: string read FEvery write FEvery;
    property EveryDay: string read FEveryDay write FEveryDay;
    property EveryWeekDay: string read FEveryWeekDay write FEveryWeekDay;
    property EveryMonthDay: string read FEveryMonthDay write FEveryMonthDay;
    property EveryYearDay: string read FEveryYearDay write FEveryYearDay;
    property EveryFirst: string read FEveryFirst write FEveryFirst;
    property EverySecond: string read FEverySecond write FEverySecond;
    property EveryThird: string read FEveryThird write FEveryThird;
    property EveryFourth: string read FEveryFourth write FEveryFourth;
    property Interval: string read FInterval write FInterval;

    property DayMonday: string read FDayMonday write FDayMonday;
    property DayTuesday: string read FDayTuesday write FDayTuesday;
    property DayWednesday: string read FDayWednesday write FDayWednesday;
    property DayThursday: string read FDayThursday write FDayThursday;
    property DayFriday: string read FDayFriday write FDayFriday;
    property DaySaturday: string read FDaySaturday write FDaySaturday;
    property DaySunday: string read FDaySunday write FDaySunday;
    property DayWeekday: string read FDayWeekday write FDayWeekday;
    property DayWeekend: string read FDayWeekend write FDayWeekend;

    property MonthJanuary: string read FMonthJanuary write FMonthJanuary;
    property MonthFebruary: string read FMonthFebruary write FMonthFebruary;
    property MonthMarch: string read FMonthMarch write FMonthMarch;
    property MonthApril: string read FMonthApril write FMonthApril;
    property MonthMay: string read FMonthMay write FMonthMay;
    property MonthJune: string read FMonthJune write FMonthJune;
    property MonthJuly: string read FMonthJuly write FMonthJuly;
    property MonthAugust: string read FMonthAugust write FMonthAugust;
    property MonthSeptember: string read FMonthSeptember write FMonthSeptember;
    property MonthOctober: string read FMonthOctober write FMonthOctober;
    property MonthNovember: string read FMonthNovember write FMonthNovember;
    property MonthDecember: string read FMonthDecember write FMonthDecember;
  end;

function DaysInMonth(mo, ye: word): word;
function AlignToFlag(alignment:TAlignment): DWord;
function VAlignToFlag(VAlignment: TVAlignment): DWord;
function WordWrapToFlag(s: string; ww: boolean): DWord;
procedure RectLine(canvas:TCanvas;r:TRect;Color:TColor;width:integer);
procedure RectHorz(canvas:TCanvas;r:TRect;Color,pencolor:TColor);
procedure RectVert(canvas:TCanvas;r:TRect;Color,pencolor:TColor);
procedure RectHorzEx(Canvas:TCanvas;r:TRect;Color,BKColor,PenColor1,PenColor2:TColor;PenWidth: Integer;BrushStyle: TBrushStyle);
procedure RectVertEx(Canvas:TCanvas;r:TRect;Color,BKColor,PenColor:TColor;PenWidth: Integer;BrushStyle:TBrushStyle);
procedure RectLineEx(Canvas:TCanvas;R:TRect;Color:TColor;Width:integer);
procedure RectLineExEx(Canvas:TCanvas;R:TRect;Color:TColor;Width:integer);
procedure RectLineExExEx(Canvas:TCanvas;R:TRect;Color:TColor;Width:integer);
procedure DrawArrow(Canvas:TCanvas;Color:TColor;X,Y:Integer;ADir:TArrowDirection);

function LFToCLF(s:string):string;
function MatchStr(s1,s2:string;DoCase:Boolean): Boolean;
function HTMLStrip(s:string):string;

procedure DrawBitmapTransp(DstRect:TRect;Canvas:TCanvas;Bitmap:TBitmap;BKColor:TColor;SrcRect:TRect);
procedure DrawBumpVert(Canvas:TCanvas;r: TRect;Color: TColor);
procedure DrawBumpHorz(Canvas:TCanvas;r: TRect;Color: TColor);

procedure DrawGradient(Canvas: TCanvas; FromColor,ToColor: TColor; Steps: Integer;R:TRect; Direction: Boolean);
procedure DrawGauge(Canvas: TCanvas; R : TRect; Position : Integer; Settings : TGaugeSettings);

function BlendColor(Col1,Col2:TColor; BlendFactor:Integer): TColor;

function EndOfMonth(dt: TDateTime): TDateTime;
function NextMonth(mo: word): word;

function Limit(Value,vmin,vmax: integer): Integer;


function DynaLink_UpdateLayeredWindow(hwnd,hdcDst:thandle;
                                   pptDst,size:ppoint;hdcSrc:thandle;
                                   pptSrc:ppoint;
                                   crKey:dword;
                                   var pblend:_BLENDFUNCTION;
                                   dwFlags:DWORD):boolean;
function DynaLink_SetLayeredWindowAttributes(HWND:thandle;crKey:DWORD;bAlpha:byte;dwFlags:DWORD):boolean;
{$IFNDEF DELPHI7_LVL}
function GetFileVersion(FileName:string): Integer;
{$ENDIF}

procedure BitmapStretch(bmp:TBitmap; Canvas:TCanvas; x,y,width: integer);
procedure BitmapStretchHeight(bmp:tbitmap; canvas:tcanvas; x,y,height,width: integer);
function CanvasToHTMLFactor(ScreenCanvas,Canvas: TCanvas): Double;
function PrinterDrawString(Canvas:TCanvas; Value: string; var Rect: TRect; Format: UINT): Integer;
function ColorToHtml(const Value: TColor): string;

procedure FixControlStyles(ctrl: TControl);


implementation

type
{$IFNDEF DELPHI_UNICODE}
  TCharSet = set of char;
{$ENDIF}
{$IFDEF DELPHI_UNICODE}
  TCharSet = array of char;
{$ENDIF}

{$I DELPHIXE.INC}

procedure FixControlStyles(ctrl: TControl);
var
  I: Integer;
begin
  ctrl.ControlStyle := ctrl.ControlStyle + [csDisplayDragImage];

  if ctrl is TWinControl then
    with TWinControl(ctrl) do
      for I := 0 to ControlCount - 1 do
        FixControlStyles(Controls[I]);
end;


function ColorToHtml(const Value: TColor): string;
type
  TColorRecord = record
    RedValue: Byte;    //  clRed = TColor($0000FF);   Low byte
    GreenValue: Byte;  //  clLime = TColor($00FF00);  Middle byte
    BlueValue: Byte;   //  clBlue = TColor($FF0000);  High byte
    SystemValue: Byte; //  becomes zero when calling ColorToRgb
  end;
const
  HtmlHexColor = '"#RRGGBB"';
  HexDigit: array[0..$F] of Char = '0123456789ABCDEF';


begin
 //  HTML Color looks like this: #RRGGBB
  with TColorRecord(ColorToRGb(Value)) do
  begin
    Result := HtmlHexColor;
    Result[3] := HexDigit[RedValue shr 4];
    Result[4] := HexDigit[RedValue and $F];
    Result[5] := HexDigit[GreenValue shr 4];
    Result[6] := HexDigit[GreenValue and $F];
    Result[7] := HexDigit[BlueValue shr 4];
    Result[8] := HexDigit[BlueValue and $F];
  end;
end;


{$IFNDEF DELPHI7_LVL}
function GetFileVersion(FileName:string): Integer;
var
  FileHandle:dword;
  l: Integer;
  pvs: PVSFixedFileInfo;
  lptr: uint;
  querybuf: array[0..255] of char;
  buf: PChar;
begin
  Result := -1;

  StrPCopy(querybuf,FileName);
  l := GetFileVersionInfoSize(querybuf,FileHandle);
  if (l>0) then
  begin
    GetMem(buf,l);
    GetFileVersionInfo(querybuf,FileHandle,l,buf);
    if VerQueryValue(buf,'\',Pointer(pvs),lptr) then
    begin
      if (pvs^.dwSignature=$FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;
{$ENDIF}


function DynaLink_UpdateLayeredWindow(hwnd,hdcDst:thandle;
                                   pptDst,size:ppoint;hdcSrc:thandle;
                                   pptSrc:ppoint;
                                   crKey:dword;
                                   var pblend:_BLENDFUNCTION;
                                   dwFlags:DWORD):boolean;

var
 UserDLL: THandle;
 user_UpdateLayeredWindow:function(hwnd,hdcDst:thandle;
                                   pptDst,size:ppoint;hdcSrc:thandle;
                                   pptSrc:ppoint;
                                   crKey:dword;
                                   var pblend:_BLENDFUNCTION;
                                   dwFlags:DWORD):DWORD; stdcall;

begin
 result:=TRUE;
 UserDLL:=GetModuleHandle('USER32.DLL');
 if (UserDLL>0) then
  begin
   @user_UpdateLayeredWindow:=GetProcAddress(UserDLL,'UpdateLayeredWindow');
   if assigned(user_UpdateLayeredWindow) then
    begin
     result:=user_UpdateLayeredWindow(hwnd,hdcDst,pptDst,size,hdcSrc,pptSrc,crKey,pblend,dwFlags)<>0;
    end;
  end;
end;


function DynaLink_SetLayeredWindowAttributes(HWND:thandle;crKey:DWORD;bAlpha:byte;dwFlags:DWORD):boolean;
var
 UserDLL: THandle;
 user_SetLayeredWindowAttributes:function(HWND:thandle;crKey:DWORD;bAlpha:byte;dwFlags:DWORD):DWORD; stdcall;

begin
 result:=TRUE;
 UserDLL:=GetModuleHandle('USER32.DLL');
 if (UserDLL>0) then
  begin
   @user_SetLayeredWindowAttributes:=GetProcAddress(UserDLL,'SetLayeredWindowAttributes');
   if assigned(user_SetLayeredWindowAttributes) then
    begin
     result:=user_SetLayeredWindowAttributes(hwnd,crKey,bAlpha,dwFlags)<>0;
    end;
  end;
end;


function CanvasToHTMLFactor(ScreenCanvas,Canvas: TCanvas): Double;
begin
  Result := GetDeviceCaps(Canvas.Handle,LOGPIXELSX) / GetDeviceCaps(ScreenCanvas.Handle, LOGPIXELSX);
end;

function PrinterDrawString(Canvas:TCanvas; Value: string; var Rect: TRect; Format: UINT): Integer;
var
  sa,su,fa: string;
  xs,ys,i,j: integer;
  hr,cr: TRect;
  ci,cv,cd:string;
begin
  if Pos('</', Value) > 0 then
  begin
    HTMLDrawEx(Canvas, Value, Rect, nil, -1, -1, -1, -1, 2,false,false,true,false,false,false,true,false,1.0,
      clBlue,clNone,clNone,clGray,sa,su,fa,xs,ys,i,j,hr,cr,ci,cv,cd,nil,nil,0);
    Result := ys;
    Exit;
  end;

  if Assigned(Canvas) then
    Result := DrawText(Canvas.Handle, PChar(Value), Length(Value), Rect, Format)
  else
    Result := -1; // indicate failure
end;

function EndOfMonth(dt: TDateTime): TDateTime;
var
  da,mo,ye: word;
begin
  DecodeDate(dt,ye,mo,da);
  da := DaysInMonth(mo,ye);
  Result := EncodeDate(ye,mo,da);
end;

function NextMonth(mo: word): word;
begin
  Result := mo + 1;
  if mo = 13 then
    Result := 1;
end;

function Limit(Value,vmin,vmax: integer): Integer;
begin
  Result := Value;
  if Value < vmin then
    Result := vmin;
  if Value > vmax then
    Result := vmax;
end;


procedure DrawGradient(Canvas: TCanvas; FromColor,ToColor: TColor; Steps: Integer;R:TRect; Direction: Boolean);
var
  diffr,startr,endr: Integer;
  diffg,startg,endg: Integer;
  diffb,startb,endb: Integer;
  iend: Integer;
  rstepr,rstepg,rstepb,rstepw: Real;
  i,stepw: Word;
begin

  if R.Right <= R.Left then
    Exit;
  if R.Bottom <= R.Top then
    Exit;

  if (ToColor = clNone) or (FromColor = ToColor) then
  begin
    Canvas.Brush.Color := FromColor;
    Canvas.FillRect(r);
    Exit;
  end;

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
    for i := 0 to Steps - 1 do
    begin
      endr := startr + Round(rstepr*i);
      endg := startg + Round(rstepg*i);
      endb := startb + Round(rstepb*i);
      stepw := Round(i*rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
      begin
        iend := R.Left + stepw + Trunc(rstepw) + 1;
        if iend > R.Right then
          iend := R.Right;
        Rectangle(R.Left + stepw,R.Top,iend,R.Bottom)
      end
      else
      begin
        iend := R.Top + stepw + Trunc(rstepw)+1;
        if iend > r.Bottom then
          iend := r.Bottom;
        Rectangle(R.Left,R.Top + stepw,R.Right,iend);
      end;
    end;
  end;
end;


function DaysInMonth(mo, ye: word): word;
const
  ADaysInMonth: array[1..13] of word = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 29);
begin
  if (mo <> 2) and (mo <= 13)  then
    Result := ADaysInMonth[mo]
  else
  begin
    if (ye mod 4 = 0) then Result := 29
    else
      Result := 28;
    if (ye mod 100 = 0) then Result := 28;
    if (ye mod 400 = 0) then Result := 29;
  end;
end;



function HTMLStrip(s:string):string;
var
  Res: string;
  i: Integer;
begin
  Res := '';
  //replace line breaks by linefeeds
  while Pos('<BR>',UpperCase(s)) > 0 do s := StringReplace(s,'<BR>',chr(13)+chr(10),[rfIgnoreCase]);
  while Pos('<HR>',UpperCase(s)) > 0 do s := StringReplace(s,'<HR>',chr(13)+chr(10),[rfIgnoreCase]);

  {remove all other tags}
  while Pos('<',s) > 0 do
  begin
    i := Pos('<',s);
    Res := Res + Copy(s,1,i-1);
    if Pos('>',s) > 0 then
      Delete(s,1,Pos('>',s));
  end;

  Result := Res + s;
end;


function AlignToFlag(Alignment:TAlignment): DWord;
begin
  case Alignment of
  taLeftJustify:Result := DT_LEFT;
  taRightJustify:Result := DT_RIGHT;
  taCenter:Result := DT_CENTER;
  else Result := DT_LEFT;
  end;
end;

function VAlignToFlag(VAlignment: TVAlignment): DWord;
begin
  case VAlignment of
    vtaTop: Result := DT_TOP;
    vtaCenter: Result := DT_VCENTER;
    vtaBottom: Result := DT_BOTTOM;
  else
    Result := DT_TOP;
  end;
end;

function WordWrapToFlag(s:string; ww: boolean): DWord;
begin
  if ww then
    Result := DT_WORDBREAK
  else
    begin
      if Pos(#13,s) = 0 then
        Result := DT_SINGLELINE
      else
        Result := 0;  
    end;
end;


procedure RectLine(Canvas:TCanvas;R:TRect;Color:TColor;Width:integer);
begin
  if Color=clNone then Exit;
  Canvas.Pen.Color := Color;
  Canvas.Pen.Width := Width;
  Canvas.MoveTo(r.Left,r.Top);
  Canvas.LineTo(r.Right,r.Top);
  Canvas.LineTo(r.Right,r.Bottom);
  Canvas.LineTo(r.Left,r.Bottom);
  Canvas.LineTo(r.Left,r.Top);
end;

procedure RectLineEx(Canvas:TCanvas;R:TRect;Color:TColor;Width:integer);
begin
  if Color=clNone then Exit;
  Canvas.Pen.Color := Color;
  Canvas.Pen.Width := Width;
  Canvas.MoveTo(r.Left,r.Top);
  Canvas.LineTo(r.Right,r.Top);
  Canvas.LineTo(r.Right,r.Bottom);
  Canvas.MoveTo(r.Left,r.Bottom);
  Canvas.LineTo(r.Left,r.Top);
end;

procedure RectLineExEx(Canvas:TCanvas;R:TRect;Color:TColor;Width:integer);
begin
  if Color = clNone then Exit;
  Canvas.Pen.Color := Color;
  Canvas.Pen.Width := Width;
  Canvas.MoveTo(r.Left,r.Top);
  Canvas.LineTo(r.Right,r.Top);
  Canvas.MoveTo(r.Right,r.Bottom);
  Canvas.LineTo(r.Left,r.Bottom);
  Canvas.LineTo(r.Left,r.Top);
end;

procedure RectLineExExEx(Canvas:TCanvas;R:TRect;Color:TColor;Width:integer);
begin
  if Color = clNone then Exit;
  Canvas.Pen.Color := Color;
  Canvas.Pen.Width := Width;
  Canvas.MoveTo(r.Left,r.Top);
  Canvas.LineTo(r.Right,r.Top);
  Canvas.MoveTo(r.Left,r.Bottom);
//  Canvas.LineTo(r.Left,r.Bottom);
  Canvas.LineTo(r.Right,r.Bottom);
end;


procedure RectHorz(Canvas:TCanvas;r:TRect;Color,PenColor:TColor);
begin
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  Canvas.FillRect(r);
  if PenColor = clNone then
    Exit;
  Canvas.Pen.Color := PenColor;
  Canvas.MoveTo(r.left,r.top-1);
  Canvas.LineTo(r.right,r.top-1);
  Canvas.MoveTo(r.left,r.bottom-1);
  Canvas.LineTo(r.right,r.bottom-1);
end;

procedure RectHorzEx(Canvas:TCanvas;r:TRect;Color,BKColor,PenColor1,PenColor2:TColor;PenWidth: Integer;BrushStyle:TBrushStyle);
begin
  if Color <> clNone then
  begin
    Canvas.Brush.Color := Color;

    if BrushStyle <> bsSolid then
    begin
      Canvas.Brush.Style := BrushStyle;
      SetBkMode(Canvas.Handle,TRANSPARENT);
      SetBkColor(Canvas.Handle,ColorToRGB(BkColor));
    end;

    Canvas.Pen.Color := Color;
    Canvas.Pen.Width := 1;
    Canvas.FillRect(r);
  end;

  if PenColor1 <> clNone then
  begin
    Canvas.Pen.Color := PenColor1;
    Canvas.Pen.Width := PenWidth;
    Canvas.MoveTo(r.Right - 1,r.Top);
    Canvas.LineTo(r.Right - 1,r.Bottom);
    Canvas.Pen.Width := 1;
  end;

  if PenColor2 <> clNone then
  begin
    Canvas.Pen.Color := PenColor2;
    Canvas.Pen.Width := PenWidth;
    Canvas.MoveTo(r.Left + 0,r.Bottom - PenWidth );
    Canvas.LineTo(r.Right - PenWidth,r.Bottom - PenWidth );
    Canvas.Pen.Width := 1;
  end;
end;

procedure RectVert(Canvas:TCanvas;r:TRect;Color,PenColor:TColor);
begin
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  Canvas.FillRect(r);
  if PenColor = clNone then
    Exit;
  Canvas.Pen.Color:=PenColor;
  Canvas.MoveTo(r.Right-1,r.Top);
  Canvas.LineTo(r.Right-1,r.Bottom);
end;

procedure RectVertEx(Canvas:TCanvas;r:TRect;Color,BKColor,PenColor:TColor;PenWidth: Integer;BrushStyle: TBrushStyle);
begin
  if Color <> clNone then
  begin
    Canvas.Brush.Color := Color;
    
    if BrushStyle <> bsSolid then
    begin
      Canvas.Brush.Style := BrushStyle;
      SetBkMode(Canvas.Handle,TRANSPARENT);
      SetBkColor(Canvas.Handle,ColorToRGB(BkColor));
    end;

    Canvas.Pen.Color := Color;
    Canvas.FillRect(r);
  end;  
  if PenColor = clNone then Exit;

  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := PenWidth;
  Canvas.MoveTo(r.Right - 1 ,r.Top);
  Canvas.LineTo(r.Right - 1,r.Bottom);
  Canvas.Pen.Width := 1;
end;


function LFToCLF(s:string):string;
var
  res: string;
  i: Integer;
begin
  res := '';
  for i := 1 to length(s) do
  begin
    if s[i] = #13 then
      res := res+'\n'
    else
      if s[i] <> #10 then
        res := res + s[i];
  end;
  Result := res;
end;


procedure DrawArrow(Canvas:TCanvas;Color:TColor;X,Y:Integer;ADir:TArrowDirection);
begin
  with Canvas do
  begin
    Pen.Width := 2;
    Pen.Color := Color;

    case ADir of
    adRight:
      begin
        MoveTo(X,Y);
        LineTo(X + 3,Y + 3);
        LineTo(X,Y + 6);
        MoveTo(X + 5,Y);
        LineTo(X + 8,Y + 3);
        LineTo(X + 5,Y + 6);
      end;
    adLeft:
      begin
        MoveTo(X + 3,Y);
        LineTo(X ,Y + 3);
        LineTo(X + 3,Y + 6);
        MoveTo(X + 8,Y);
        LineTo(X + 5,Y + 3);
        LineTo(X + 8,Y + 6);
      end;
    adDown:
      begin
        MoveTo(X,Y);
        Lineto(X + 3,Y + 3);
        LineTo(X + 6,Y);
        MoveTo(X,Y + 4);
        LineTo(X + 3,Y + 7);
        LineTo(X + 6,Y + 4);
      end;
    adUp:
      begin
        MoveTo(X,Y + 3);
        LineTo(X + 3,Y);
        LineTo(X + 6,Y + 3);
        MoveTo(X,Y + 7);
        LineTo(X + 3,Y + 4);
        LineTo(X + 6,Y + 7);
      end;
    end;
  end;
end;

function VarPos(su,s:string;var Respos:Integer):Integer;
begin
  Respos := Pos(su,s);
  Result := Respos;
end;

function IsDate(s:string;var dt:TDateTime):boolean;
var
  su: string;
  da,mo,ye: word;
  err: Integer;
  dp,mp,yp,vp: Integer;
begin
  Result := False;

  su := UpperCase(ShortDateFormat);
  dp := pos('D',su);
  mp := pos('M',su);
  yp := pos('Y',su);

  da := 0;
  mo := 0;
  ye := 0;

  if VarPos(DateSeparator,s,vp) > 0 then
  begin
    su := Copy(s,1,vp - 1);

    if (dp < mp) and
       (dp < yp) then
       Val(su,da,err)
    else
    if (mp < dp) and
       (mp < yp) then
       Val(su,mo,err)
    else
    if (yp < mp) and
       (yp < dp) then
       Val(su,ye,err);

    if err <> 0 then Exit;
    Delete(s,1,vp);

    if VarPos(DateSeparator,s,vp) > 0 then
    begin
      su := Copy(s,1,vp - 1);

      if ((dp > mp) and (dp < yp)) or
         ((dp > yp) and (dp < mp)) then
         Val(su,da,err)
      else
      if ((mp > dp) and (mp < yp)) or
         ((mp > yp) and (mp < dp)) then
         val(su,mo,err)
      else
      if ((yp > mp) and (yp < dp)) or
         ((yp > dp) and (yp < mp)) then
         Val(su,ye,err);

      if err <> 0 then Exit;
      Delete(s,1,vp);

      if (dp > mp) and
         (dp > yp) then
         Val(s,da,err)
      else
      if (mp > dp) and
         (mp > yp) then
         Val(s,mo,err)
      else
      if (yp > mp) and
         (yp > dp) then
         Val(s,ye,err);

      if err <> 0 then Exit;
      if da > 31 then Exit;
      if mo > 12 then Exit;

      Result := True;

      try
        dt := EncodeDate(ye,mo,da);
      except
        Result := False;
      end;
    end;
  end;
end;

function Matches(s0a,s1a:PChar):boolean;
const
  larger = '>';
  smaller = '<';
  logand  = '&';
  logor   = '^';
  asterix = '*';
  qmark = '?';
  negation = '!';
  null = #0;

var
  matching,done: Boolean;
  len: longint;
  s0,s1,s2,s3: PChar;
  oksmaller,oklarger,negflag: Boolean;
  compstr:array[0..255] of Char;
  flag1,flag2,flag3: Boolean;
  equal: Boolean;
  n1,n2: Double;
  code1,code2: Integer;
  dt1,dt2: TDateTime;

begin
  oksmaller := True;
  oklarger := True;
  flag1 := False;
  flag2 := False;
  flag3 := False;
  negflag := False;
  equal := False;

  { [<>] string [&|] [<>] string }

  s2 := StrPos(s0a,larger);

  if s2 <> nil then
  begin
    inc(s2);
    if (s2^='=') then
    begin
      Equal := True;
      inc(s2);
    end;

    while (s2^ = ' ') do inc(s2);

    s3 := s2;
    len := 0;
    while (s2^ <> ' ') and (s2^ <> NULL) and (s2^ <> '&') and (s2^ <> '|')  do
    begin
      inc(s2);
      inc(len);
    end;

    StrLCopy(compstr,s3,len);

    Val(s1a,n1,code1);
    Val(compstr,n2,code2);

    if (code1 = 0) and (code2 = 0) then {both are numeric types}
    begin
      if equal then
        oklarger := n1 >= n2
      else
        oklarger := n1 > n2;
    end
    else
    begin
      if IsDate(StrPas(compstr),dt2) and IsDate(StrPas(s1a),dt1) then
      begin
        if equal then
         oklarger := dt1 >= dt2
        else
         oklarger := dt1 > dt2;
      end
      else
      begin
        if equal then
         oklarger := StrLComp(compstr,s1a,255) <= 0
        else
         oklarger := StrLComp(compstr,s1a,255) < 0;
      end;
    end;
    flag1 := True;
  end;

  equal := False;
  s2 := StrPos(s0a,smaller);
  if s2 <> nil then
  begin
    inc(s2);
    if (s2^ = '=') then
    begin
      equal := True;
      inc(s2);
    end;

    while s2^ = ' ' do inc(s2);
    s3 := s2;
    len := 0;
    while (s2^ <> ' ') and (s2^ <> NULL) and (s2^ <> '&') and (s2^ <> '|') do
    begin
      inc(s2);
      inc(len);
    end;

    StrLCopy(compstr,s3,len);

    Val(s1a,n1,code1);
    Val(compstr,n2,code2);

    if (code1 = 0) and (code2 = 0) then //both are numeric types
    begin
      if equal then
        oksmaller := n1 <= n2
      else
        oksmaller := n1 < n2;
    end
    else
    begin
      //check for dates here ?}
      if IsDate(StrPas(compstr),dt2) and IsDate(StrPas(s1a),dt1) then
      begin
        if equal then
          oksmaller := dt1 <= dt2
        else
          oksmaller := dt1 < dt2;
      end
      else
      begin
        if equal then
          oksmaller := StrLComp(compstr,s1a,255) >= 0
        else
          oksmaller := StrLComp(compstr,s1a,255) > 0;
      end;
    end;
    flag2 := True;
  end;

  s2 := StrPos(s0a,negation);
  if s2 <> nil then
  begin
    inc(s2);
    while s2^ = ' ' do inc(s2);
    s3 := s2;
    len := 0;
    while (s2^ <> ' ') and (s2^ <> NULL) and (s2^ <> '&') and (s2^ <> '|') do
    begin
      inc(s2);
      inc(len);
    end;
    StrLCopy(compstr,s3,len);
    flag3 := True;
  end;

  if flag3 then
  begin
    if StrPos(s0a,larger) = nil then
      flag1 := flag3;
    if StrPos(s0a,smaller) = nil then
      flag2 := flag3;
  end;

  if StrPos(s0a,logor) <> nil then
    if flag1 or flag2 then
    begin
      Matches := oksmaller or oklarger;
      Exit;
    end;

  if StrPos(s0a,logand) <> nil then
    if flag1 and flag2 then
    begin
      Matches := oksmaller and oklarger;
      Exit;
    end;

  if ((StrPos(s0a,larger) <> nil) and oklarger) or
     ((StrPos(s0a,smaller) <> nil) and oksmaller) then
  begin
    Matches := True;
    Exit;
  end;

  s0 := s0a;
  s1 := s1a;

  matching:=True;

  done := (s0^ = NULL) and (s1^ = NULL);

  while not done and matching do
  begin
    case s0^ of
    qmark:
      begin
        matching := s1^ <> NULL;
        if matching then
        begin
          inc(s0);
          inc(s1);
        end;
      end;
    negation:
      begin
         negflag := True;
         inc(s0);
      end;
    asterix:
      begin
        repeat
          inc(s0)
        until s0^ <> asterix;
        len := StrLen(s1);
        inc(s1,len);
        matching := matches(s0,s1);
        while (len >= 0) and not matching do
        begin
          dec(s1);
           dec(len);
           matching := Matches(s0,s1);
        end;
        if matching then
        begin
          s0 := StrEnd(s0);
          s1 := StrEnd(s1);
        end;
      end;
      else
      begin
        matching := s0^ = s1^;
        if matching then
        begin
          inc(s0);
          inc(s1);
        end;
      end;
    end;
    Done := (s0^ = NULL) and (s1^ = NULL);
  end;

  if negflag then
    Matches := not matching
  else
    Matches := matching;
end;

{$IFDEF DELPHI_UNICODE}
function FirstChar(Charset:TCharSet;s:string):char;
var
  i:Integer;

  function InArray(ch: char): boolean;
  var
    j: integer;
  begin
    result := false;
    for j := 0 to High(CharSet) - 1 do
    begin
      if ch = CharSet[j] then
      begin
        result := true;
        break;
      end;
    end;
  end;

begin
  i := 1;
  Result := #0;
  while i <= Length(s) do
  begin
    if InArray(s[i]) then
    begin
      Result := s[i];
      Break;
    end;
    Inc(i);
  end;
end;
{$ENDIF}

{$IFNDEF DELPHI_UNICODE}
function FirstChar(Charset:TCharSet;s:string):char;
var
  i:Integer;
begin
  i := 1;
  Result := #0;
  while i <= Length(s) do
  begin
    if s[i] in Charset then
    begin
      Result := s[i];
      Break;
    end;
    Inc(i);
  end;
end;
{$ENDIF}



function MatchStr(s1,s2:string;DoCase:Boolean): Boolean;
var
  ch,lastop: Char;
  sep: Integer;
  res,newres: Boolean;
  {$IFDEF DELPHI_UNICODE}
  CharArray: TCharSet;
  {$ENDIF}

begin
 {remove leading & trailing spaces}
  s1 := Trim(s1);
 {remove spaces between multiple filter conditions}
  while VarPos(' &',s1,sep) > 0 do Delete(s1,sep,1);
  while VarPos(' ;',s1,sep) > 0 do Delete(s1,sep,1);
  while VarPos(' ^',s1,sep) > 0 do Delete(s1,sep,1);
  while VarPos('& ',s1,sep) > 0 do Delete(s1,sep+1,1);
  while VarPos('; ',s1,sep) > 0 do Delete(s1,sep+1,1);
  while VarPos('^ ',s1,sep) > 0 do Delete(s1,sep+1,1);

  LastOp := #0;
  Res := True;

  {$IFDEF DELPHI_UNICODE}
  SetLength(CharArray,3);
  CharArray[0] := ';';
  CharArray[0] := '^';
  CharArray[0] := '|';
  {$ENDIF}



  repeat
    {$IFDEF DELPHI_UNICODE}
    ch := FirstChar(CharArray,s1);
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
    ch := FirstChar([';','^','&'],s1);
    {$ENDIF}

    {extract first part of filter}
    if ch <> #0 then
    begin
      VarPos(ch,s1,sep);
      NewRes := MatchStr(Copy(s1,1,sep-1),s2,DoCase);
      Delete(s1,1,sep);

      if LastOp = #0 then
        Res := NewRes
      else
        case LastOp of
        ';','^':Res := Res or NewRes;
        '&':Res := Res and NewRes;
        end;

      LastOp := ch;
     end;

  until ch = #0;

  if DoCase then
    NewRes := Matches(PChar(s1),PChar(s2))
  else
    NewRes := Matches(PChar(AnsiUpperCase(s1)),PChar(AnsiUpperCase(s2)));

  if LastOp = #0 then
    Res := NewRes
  else
    case LastOp of
    ';','^':Res := Res or NewRes;
    '&':Res := Res and NewRes;
    end;

  Result := Res;
end;

procedure DrawBitmapTransp(DstRect:TRect;Canvas:TCanvas;Bitmap:TBitmap;BKColor:TColor;SrcRect:TRect);
var
  tmpbmp: TBitmap;
  TgtRect: TRect;
  srcColor: TColor;
begin
  TmpBmp := TBitmap.Create;
  try
    TmpBmp.Height := Bitmap.Height;
    TmpBmp.Width := Bitmap.Width;
    TgtRect.Left :=0;
    TgtRect.Top :=0;
    TgtRect.Right := Bitmap.Width;
    TgtRect.Bottom := Bitmap.Height;
    TmpBmp.Canvas.Brush.Color := BKColor;
    SrcColor := Bitmap.Canvas.Pixels[0,0];
    TmpBmp.Canvas.BrushCopy(TgtRect,Bitmap,TgtRect,srcColor);
    Canvas.CopyRect(DstRect, TmpBmp.Canvas, SrcRect);
  finally
    TmpBmp.Free;
  end;
end;

procedure DrawBumpVert(Canvas:TCanvas;r: TRect;Color: TColor);
var
  i,j,k: integer;
  Bump: Longint;
  BumpR,BumpG,BumpB: Longint;
begin
  Bump := ColorToRGB(Color);

  BumpR := (Bump and $FF0000) shr 17;
  BumpG := (Bump and $00FF00) shr 9;
  BumpB := (Bump and $0000FF) shr 1;

  Bump := (BumpR shl 16) + (BumpG shl 16) + BumpB;

  j := r.Top;
  k := 0;

  while j < r.Bottom do
  begin
    if odd(k) then
      i := r.Left
    else
      i := r.Left + 2;

    Canvas.Pen.Color := Bump;

    Canvas.MoveTo(i + 3,j);
    Canvas.LineTo(i + 3,j + 2);
    Canvas.LineTo(i ,j + 2);

    Canvas.Pixels[i + 1,j + 1] := clWhite;

    j := j + 8;
    inc(k);

  end;

end;

procedure DrawBumpHorz(Canvas:TCanvas;r: TRect;Color: TColor);
var
  i,j,k: integer;
  Bump: Longint;
  BumpR,BumpG,BumpB: Longint;
begin
  Bump := ColorToRGB(Color);

  BumpR := (Bump and $FF0000) shr 17;
  BumpG := (Bump and $00FF00) shr 9;
  BumpB := (Bump and $0000FF) shr 1;

  Bump := (BumpR shl 16) + (BumpG shl 16) + BumpB;

  i := r.Left;
  k := 0;

  while i < r.Right do
  begin
    if odd(k) then
      j := r.Top + 2
    else
      j := r.Top + 4;

    Canvas.Pen.Color := Bump;

    Canvas.MoveTo(i + 3,j);
    Canvas.LineTo(i + 3,j + 2);
    Canvas.LineTo(i ,j + 2);

    Canvas.Pixels[i + 1,j + 1] := clWhite;

    i := i + 8;
    inc(k);
  end;

end;




function BlendColor(Col1,Col2:TColor; BlendFactor:Integer): TColor;
var
  r1,g1,b1: Integer;
  r2,g2,b2: Integer;

begin
  if BlendFactor = 100 then
  begin
    Result := Col1;
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

{ TDateTimeList }

destructor TDateTimeList.Destroy;
begin
  Clear;
  inherited;
end;


procedure TDateTimeList.Add(Value: TDateTime);
var
  tdt: TDateTimeObject;
begin
  tdt := TDateTimeObject.Create;
  tdt.DT := Value;
  inherited Add(tdt);
end;

procedure TDateTimeList.Delete(Index: Integer);
begin
  TDateTimeObject(inherited Items[Index]).Free;
  inherited Delete(Index);
end;

function TDateTimeList.GetDT(index: Integer): TDateTime;
begin
  Result := TDateTimeObject(inherited Items[Index]).DT;
end;

procedure TDateTimeList.Insert(Index: Integer; Value: TDateTime);
var
  tdt: TDateTimeObject;
begin
  tdt := TDateTimeObject.Create;
  tdt.DT := Value;
  inherited Insert(Index,tdt);
end;

procedure TDateTimeList.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

procedure TDateTimeList.SetDT(index: Integer; const Value: TDateTime);
begin
  TDateTimeObject(inherited Items[Index]).DT := Value;
end;


procedure DrawRectangle(Canvas: TCanvas; R : TRect; BrushColor : TColor);
begin
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Color := BrushColor;
  Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
  Canvas.Brush.Style := bsClear;
end;

procedure DrawGauge(Canvas: TCanvas; R : TRect; Position : Integer; Settings : TGaugeSettings);
var
  RectL : TRect;
  RectM : TRect;
  RectR : TRect;

  WidthBar : integer;
  WidthPart : Integer;
  Continue : Boolean;
  GradDir : Boolean;
  BrushColor : TColor;
  BrushColorTo : TColor;
  Percentage : Integer;
  BarFilled : Integer;
  NumberOfBlock : Integer;
  i : Integer;
  EmptyWidth : integer;

  lf : TLogFont;
  tf : TFont;

  R1 : TRect;
  R2 : TRect;

  CompletionText: string;

begin
  if (Settings.Orientation = goHorizontal) then
    WidthBar := R.Right - R.Left
  else
    WidthBar := R.Bottom - R.Top;

  Continue := true;
  Percentage := -1;
  Canvas.Brush.Color := Settings.BackgroundColor;
  GradDir := not (Settings.Orientation = goHorizontal);

  if (Settings.ShowPercentage) then
    Percentage := Position;

  //Draw Border
  if (Settings.ShowBorder) then
    Canvas.Pen.Color := Settings.BorderColor
  else
    Canvas.Pen.Color := Settings.BackgroundColor;

  if Settings.BackgroundColor = clNone then
    Canvas.Brush.Style := bsClear;

  Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
  WidthBar := WidthBar - 2;

  if (Position > 0) then
  begin
    if (Settings.Stacked) then
    begin
      if (Position >= Settings.Level1Perc) then
        WidthPart := Round((Settings.Level1Perc/100) * WidthBar)
      else
      begin
        WidthPart := Round((Position / 100) * WidthBar);
        Continue := false;
      end;

      //Draw first part
      if (Settings.Orientation = goHorizontal) then
      begin
        RectL.Left := R.Left + 1;
        RectL.Top := R.Top + 1;
        RectL.Right := RectL.Left + WidthPart;
        RectL.Bottom := r.Bottom - 1;
      end
      else
      begin
        RectL.Left := r.Left + 1;
        RectL.Right := R.Right - 1;
        RectL.Top := R.Bottom - WidthPart;
        RectL.Bottom := R.Bottom - 1;
      end;

      if (Settings.ShowGradient) then
      begin
        if not (Settings.Orientation = goHorizontal) then
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
          R1.Bottom := RectL.Bottom;
          R1.Top := RectL.Top;
          R2.Left := R1.Right;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
          R2.Top := RectL.Top;
        end
        else
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Right;
          R1.Top := RectL.Top;
          R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
          R2.Top := R1.Bottom;
          R2.Left := RectL.Left;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
        end;
        DrawGradient(Canvas, Settings.Level0ColorTo, Settings.Level0Color, Settings.Steps,R1,GradDir);
        DrawGradient(Canvas, Settings.Level0Color, Settings.Level0ColorTo, Settings.Steps,R2,GradDir);
      end
      else
        DrawRectangle(Canvas, RectL, Settings.Level0Color);

      BarFilled := WidthPart;

      if (Continue) then
      begin
        //Draw second part
        if (Settings.Orientation = goHorizontal) then
        begin
          RectM.Left := RectL.Right;
          RectM.Top := r.Top + 1;
          RectM.Bottom := r.Bottom - 1;
        end
        else
        begin
          RectM.Left := R.Left + 1;
          RectM.Right := R.Right - 1;
          RectM.Bottom := RectL.Top;
        end;

        if (Position >= Settings.Level2Perc) then
          WidthPart := Round(WidthBar * ((Settings.Level2Perc - Settings.Level1Perc) /100))
        else
        begin
          WidthPart := Round(WidthBar * ((Position - Settings.Level1Perc) /100));
          Continue := false;
        end;

        if (Settings.Orientation = goHorizontal) then
          RectM.Right := WidthPart + RectM.Left
        else
           RectM.Top := RectM.Bottom - WidthPart;

        if (Settings.ShowGradient) then
        begin
          if not (Settings.Orientation = goHorizontal) then
          begin
            R1.Left := RectM.Left;
            R1.Right := RectM.Left + (RectM.Right - RectM.Left) div 2;
            R1.Bottom := RectM.Bottom;
            R1.Top := RectM.Top;
            R2.Left := R1.Right;
            R2.Right := RectM.Right;
            R2.Bottom := RectM.Bottom;
            R2.Top := RectM.Top;
          end
          else
          begin
            R1.Left := RectM.Left;
            R1.Right := RectM.Right;
            R1.Top := RectM.Top;
            R1.Bottom := RectM.Top + (RectM.Bottom - RectM.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectM.Left;
            R2.Right := RectM.Right;
            R2.Bottom := RectM.Bottom;
          end;
          DrawGradient(Canvas, Settings.Level1ColorTo, Settings.Level1Color, Settings.Steps,R1,GradDir);
          DrawGradient(Canvas, Settings.Level1Color,Settings.Level1ColorTo, Settings.Steps,R2,GradDir);
        end
        else
          DrawRectangle(Canvas, RectM,Settings.Level1Color);

        BarFilled := BarFilled + WidthPart;
        if (Continue) then
        begin
          //Draw third part
          if (Position = 100) then
            WidthPart := Round(WidthBar - BarFilled)
          else
            WidthPart := Round(WidthBar * ((Position - Settings.Level2Perc)/ 100));

          if (Settings.Orientation = goHorizontal) then
          begin
            RectR.Left := RectM.Right;
            RectR.Top := R.Top + 1;
            RectR.Bottom := r.Bottom - 1;
            RectR.Right := RectR.Left + WidthPart;
          end
          else
          begin
            RectR.Left := R.Left + 1;
            RectR.Right := R.Right - 1;
            RectR.Bottom := RectM.Top - 1;
            RectR.Top := RectR.Bottom - WidthPart;
          end;

          if (Settings.ShowGradient) then
          begin
            if not (Settings.Orientation = goHorizontal) then
            begin
              R1.Left := RectR.Left;
              R1.Right := RectR.Left + (RectR.Right - RectR.Left) div 2;
              R1.Bottom := RectR.Bottom;
              R1.Top := RectR.Top;
              R2.Left := R1.Right;
              R2.Right := RectR.Right;
              R2.Bottom := RectR.Bottom;
              R2.Top := RectR.Top;
            end
            else
            begin
              R1.Left := RectR.Left;
              R1.Right := RectR.Right;
              R1.Top := RectR.Top;
              R1.Bottom := RectR.Top + (RectR.Bottom - RectR.Top) div 2;
              R2.Top := R1.Bottom;
              R2.Left := RectR.Left;
              R2.Right := RectR.Right;
              R2.Bottom := RectR.Bottom;
            end;
            DrawGradient(Canvas, Settings.Level3ColorTo, Settings.Level3Color, Settings.Steps,R1,GradDir);
            DrawGradient(Canvas, Settings.Level3Color, Settings.Level3ColorTo, Settings.Steps,R2,GradDir);
          end
          else
            DrawRectangle(Canvas,RectR, Settings.Level3Color);
        end;
      end;
    end
    else
    begin
      if (Position < Settings.Level1Perc) then
      begin
        BrushColor := Settings.Level0Color;
        BrushColorTo := Settings.Level0ColorTo;
      end
      else
      begin
        if (Position < Settings.Level2Perc) then
        begin
          BrushColor := Settings.Level1Color;
          BrushColorTo := Settings.Level1ColorTo;
        end
        else
        begin
          if (Position < 100) then
          begin
            BrushColor := Settings.Level2Color;
            BrushColorTo := Settings.Level2ColorTo;
          end
          else
          begin
            BrushColor := Settings.Level3Color;
            BrushColorTo := Settings.Level3ColorTo;
          end;
        end;
      end;

      if not (Settings.CompletionSmooth) then
      begin
        Canvas.Brush.Color := Settings.BackgroundColor;

        if (Round((Position * WidthBar)/100) > 9) then
        begin
          if (Settings.Orientation = goHorizontal) then
          begin
            RectL.Left := R.Left + 2;
            RectL.Right := RectL.Left + 7;
            RectL.Top := R.Top + 2;
            RectL.Bottom := R.Bottom - 2;
          end
          else
          begin
            RectL.Left := R.Left + 2;
            RectL.Right := R.Right - 2;
            RectL.Bottom := R.Bottom - 2;
            RectL.Top := RectL.Bottom - 7;
          end;

          if (Settings.ShowGradient) then
          begin
            if not (Settings.Orientation = goHorizontal) then
            begin
              R1.Left := RectL.Left;
              R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
              R1.Bottom := RectL.Bottom;
              R1.Top := RectL.Top;
              R2.Left := R1.Right;
              R2.Right := RectL.Right;
              R2.Bottom := RectL.Bottom;
              R2.Top := RectL.Top;
            end
            else
            begin
              R1.Left := RectL.Left;
              R1.Right := RectL.Right;
              R1.Top := RectL.Top;
              R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
              R2.Top := R1.Bottom;
              R2.Left := RectL.Left;
              R2.Right := RectL.Right;
              R2.Bottom := RectL.Bottom;
            end;
            DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps,R1,GradDir);
            DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps,R2,GradDir);
          end
          else
            DrawRectangle(Canvas, RectL, BrushColor);

          NumberOfBlock := (Round((Position * WidthBar)/100) div 9) - 1;
          EmptyWidth := Round((Position * WidthBar) / 100) mod 9;

          for i:=0 to NumberOfBlock-1 do
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := RectL.Right + 2;
              RectL.Right := RectL.Left + 7;
            end
            else
            begin
              RectL.Bottom := RectL.Top - 2;
              RectL.Top := RectL.Bottom - 7;
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps,R1,GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps,R2,GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;

          if (EmptyWidth > 2) then
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := RectL.Right + 2;
              RectL.Right := RectL.Left + (EmptyWidth - 1);
            end
            else
            begin
              RectL.Bottom := RectL.Top - 2;
              RectL.Top := RectL.Bottom - (EmptyWidth - 1);
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps,R1,GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps,R2,GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;
          Canvas.Brush.style := bsClear;
        end
        else
        begin
          if (Round((Position * WidthBar)/100) > 1) then
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := RectL.Left + (Round((Position * WidthBar)/100) - 1);
              RectL.Top := R.Top + 2;
              RectL.Bottom := R.Bottom - 2;
            end
            else
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := R.Right - 2;
              RectL.Bottom := R.Bottom - 2;
              RectL.Top := RectL.Bottom - (Round((Position * WidthBar)/100) - 1);
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps,R1,GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps,R2,GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;
        end;
      end
      else
      begin
        WidthPart := Round((Position / 100) * WidthBar);

        if (Settings.Orientation = goHorizontal) then
        begin
          RectL.Left := R.Left + 1;
          RectL.Top := R.Top +1;
          RectL.Right := RectL.Left + WidthPart;
          RectL.Bottom := R.Bottom - 1;
        end
        else
        begin
          RectL.Left := r.Left + 1;
          RectL.Bottom := R.Bottom - 1;
          RectL.Top := RectL.Bottom - WidthPart;
          RectL.Right := r.Right - 1;
        end;

        if (Settings.ShowGradient) then
        begin
          if not (Settings.Orientation = goHorizontal) then
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
            R1.Bottom := RectL.Bottom;
            R1.Top := RectL.Top;
            R2.Left := R1.Right;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
            R2.Top := RectL.Top;
          end
          else
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Right;
            R1.Top := RectL.Top;
            R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectL.Left;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
          end;
          DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps,R1,GradDir);
          DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps,R2,GradDir);
        end
        else
          DrawRectangle(Canvas, RectL, BrushColor);
      end;
    end;
  end;

  CompletionText := Format(Settings.CompletionFormat,[Percentage]);
  

  //Draw text with PositionPercentage
  if (Percentage <> -1) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Name := Settings.Font.Name;
    Canvas.Font.Size := Settings.Font.Size;
    Canvas.Font.Color := Settings.Font.Color;
    Canvas.Font.Style := Settings.Font.Style;
    if not (Settings.Orientation = goHorizontal) then
    begin
      tf := TFont.Create;
      try
        tf.Assign(Settings.Font);

        GetObject(tf.Handle, sizeof(lf),@lf);

        lf.lfEscapement := 900;
        lf.lfOrientation := 900;
        tf.Handle := CreateFontIndirect(lf);
        
        Canvas.Font.Assign(tf);
        Canvas.TextOut(R.Left + ((R.Right - R.Left) div 2 - (Canvas.TextHeight(CompletionText) div 2)),R.Top + ((R.Bottom - R.Top) div 2) + Canvas.TextWidth(CompletionText)div 2 ,CompletionText);
      finally
        tf.Free;
      end;
    end
    else
    begin
      Canvas.TextOut(((R.Right - R.Left) div 2) - (Canvas.TextWidth(CompletionText) div 2 ) + r.Left, r.Top + ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(CompletionText) div 2,CompletionText);
    end;
  end;
end;

procedure DrawTransparentBitmap(hdc: THandle; hBitmap: THandle;  xStart, yStart: Integer;
      width, height, offsx, offsy: Integer; cTransparentColor: TColor);
// The function draws a bitmap with a transparent background.
var
  cColor: TColor;
  bmAndBack, bmAndObject, bmAndMem, bmSave: THandle;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: THandle;
  hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave: THandle;
  ptSize: TPoint;
begin
  hdcTemp := CreateCompatibleDC(hdc);
  SelectObject(hdcTemp, hBitmap);

  ptSize.x := width;
  ptSize.y := height;

  DPtoLP(hdcTemp, ptSize, 1);


  hdcBack   := CreateCompatibleDC(hdc);
  hdcObject := CreateCompatibleDC(hdc);
  hdcMem    := CreateCompatibleDC(hdc);
  hdcSave   := CreateCompatibleDC(hdc);

  bmAndBack   := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

  bmAndMem    := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);
  bmSave      := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);

  bmBackOld   := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld    := SelectObject(hdcMem, bmAndMem);
  bmSaveOld   := SelectObject(hdcSave, bmSave);

  SetMapMode(hdcTemp, GetMapMode(hdc));

  BitBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCCOPY);

  cColor := SetBkColor(hdcTemp, cTransparentColor);

  BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCCOPY);

  SetBkColor(hdcTemp, cColor);

  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, NOTSRCCOPY);

  // take copy of existing canvas
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdc, xStart, yStart, SRCCOPY);
  // and existing canvas with copy
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);

  BitBlt(hdcTemp, offsx, offsy, ptSize.x, ptSize.y, hdcBack, 0, 0, SRCAND);
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCPAINT);
  BitBlt(hdc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, 0, 0, SRCCOPY);
  BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcSave, 0, 0, SRCCOPY);

  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  DeleteObject(SelectObject(hdcSave, bmSaveOld));

  DeleteDC(hdcMem);
  DeleteDC(hdcBack);

  DeleteDC(hdcObject);
  DeleteDC(hdcSave);
  DeleteDC(hdcTemp);
end;

procedure StretchBitmap(hdc: THandle; hBitmap: THandle;  xStart, yStart: Integer;
      width, height, offsx, offsy, bmpw, bmph: Integer; cTransparentColor: TColor);
// The function draws a bitmap with a transparent background.
var
  hdcTemp: THandle;
  oldbmp: thandle;
begin
  hdcTemp := CreateCompatibleDC(hdc);
  oldbmp := SelectObject(hdcTemp, hBitmap);
  StretchBlt(hdc, xstart, ystart, width, height, hdcTemp, offsx,offsy, bmpw, bmph, SRCCOPY);
  SelectObject(hdctemp,oldbmp);
  Deletedc(hdctemp);
end;


procedure BitmapStretch(bmp:TBitmap; Canvas:TCanvas; x,y,width: integer);
var
  mid: integer;
  fillw: integer;
  c: TColor;
  tbmp:tbitmap;
begin
  tbmp := tbitmap.Create;
  tbmp.Width := bmp.Width;
  tbmp.Height := bmp.Height;
  tbmp.Canvas.Draw(0,0,bmp);

  mid := tbmp.Width div 2;
  fillw := width - tbmp.Width;
  c := tbmp.Canvas.Pixels[0,tbmp.Height - 1];


  DrawTransparentBitmap(canvas.handle,tbmp.handle,x,y,bmp.Width div 2 ,tbmp.Height,0,0,c);
  StretchBitmap(canvas.handle,tbmp.Handle,x+mid,y,fillw,tbmp.height,mid - 1,0,2,tbmp.height,c);
  DrawTransparentBitmap(canvas.handle,tbmp.handle,x+mid+fillw,y, tbmp.width div 2, tbmp.Height,mid,0,c);

  tbmp.Free;
end;



procedure BitmapStretchHeight(bmp:TBitmap; Canvas:TCanvas; x,y,height,width: integer);
var
  mid: integer;
  c,cf: TColor;
  tbmp: tbitmap;
begin
  tbmp := TBitmap.Create;
  tbmp.Width := bmp.Width;
  tbmp.Height := bmp.Height;
  tbmp.Canvas.Draw(0,0,bmp);

  mid := tbmp.width div 2;
  cf := tbmp.canvas.Pixels[bmp.Width div 2,bmp.Height div 2];

  // c := tbmp.Canvas.Pixels[0,tbmp.Height - 1];
  // no transparency on centered item in this case
  c := $0000ff;

  StretchBitmap(canvas.handle,tbmp.Handle, x, y ,mid,height,0,0,mid,tbmp.height,c);
  StretchBitmap(canvas.handle,tbmp.Handle, x + width - mid,y,mid,height,mid,0,mid,tbmp.height,c);

  tbmp.Free;

  Canvas.Brush.Color:= ColorToRGB(cf);
  Canvas.FillRect(rect(x + mid , y, x + width - mid,  y + height));
end;




{ TRecurrencyDialogLanguage }

procedure TRecurrencyDialogLanguage.Assign(Source: TPersistent);
begin
  if (Source is TRecurrencyDialogLanguage) then
  begin
    FCaption := (Source as TRecurrencyDialogLanguage).Caption;
    FSettings := (Source as TRecurrencyDialogLanguage).Settings;
    FExceptions := (Source as TRecurrencyDialogLanguage).Exceptions;
    FPatternDetails := (Source as TRecurrencyDialogLanguage).PatternDetails;
    FRecurrencyPattern := (Source as TRecurrencyDialogLanguage).RecurrencyPattern;
    FRange := (Source as TRecurrencyDialogLanguage).Range;
    FRangeInfinite := (Source as TRecurrencyDialogLanguage).RangeInfinite;
    FRangeFor := (Source as TRecurrencyDialogLanguage).RangeFor;
    FRangeOccurences := (Source as TRecurrencyDialogLanguage).RangeOccurences;
    FRangeUntil := (Source as TRecurrencyDialogLanguage).RangeUntil;
    FFreqNone := (Source as TRecurrencyDialogLanguage).FreqNone;
    FFreqHourly := (Source as TRecurrencyDialogLanguage).FreqHourly;
    FFreqDaily := (Source as TRecurrencyDialogLanguage).FreqDaily;
    FFreqWeekly := (Source as TRecurrencyDialogLanguage).FreqWeekly;
    FFreqMonthly := (Source as TRecurrencyDialogLanguage).FreqMonthly;
    FFreqYearly := (Source as TRecurrencyDialogLanguage).FreqYearly;
    FButtonRemove := (Source as TRecurrencyDialogLanguage).ButtonRemove;
    FButtonAdd := (Source as TRecurrencyDialogLanguage).ButtonAdd;
    FButtonClear := (Source as TRecurrencyDialogLanguage).ButtonClear;
    FButtonOK := (Source as TRecurrencyDialogLanguage).ButtonOK;
    FButtonCancel := (Source as TRecurrencyDialogLanguage).ButtonCancel;
    FEveryDay := (Source as TRecurrencyDialogLanguage).EveryDay;
    FEveryWeekDay := (Source as TRecurrencyDialogLanguage).EveryWeekday;
    FEveryMonthDay := (Source as TRecurrencyDialogLanguage).EveryMonthDay;
    FEveryYearDay := (Source as TRecurrencyDialogLanguage).EveryYearDay;
    FEvery := (Source as TRecurrencyDialogLanguage).Every;

    FEveryFirst := (Source as TRecurrencyDialogLanguage).EveryFirst;
    FEverySecond := (Source as TRecurrencyDialogLanguage).EverySecond;
    FEveryThird := (Source as TRecurrencyDialogLanguage).EveryThird;
    FEveryFourth := (Source as TRecurrencyDialogLanguage).EveryFourth;

    FDayMonday := (Source as TRecurrencyDialogLanguage).DayMonday;
    FDayTuesday := (Source as TRecurrencyDialogLanguage).DayTuesday;
    FDayWednesday := (Source as TRecurrencyDialogLanguage).DayWednesday;
    FDayThursday := (Source as TRecurrencyDialogLanguage).DayThursday;
    FDayFriday := (Source as TRecurrencyDialogLanguage).DayFriday;
    FDaySaturday := (Source as TRecurrencyDialogLanguage).DaySaturday;
    FDaySunday := (Source as TRecurrencyDialogLanguage).DaySunday;
    FDayWeekday := (Source as TRecurrencyDialogLanguage).DayWeekDay;
    FDayWeekend := (Source as TRecurrencyDialogLanguage).DayWeekend;

    FMonthJanuary := (Source as TRecurrencyDialogLanguage).MonthJanuary;
    FMonthFebruary := (Source as TRecurrencyDialogLanguage).MonthFebruary;
    FMonthMarch := (Source as TRecurrencyDialogLanguage).MonthMarch;
    FMonthApril := (Source as TRecurrencyDialogLanguage).MonthApril;
    FMonthMay := (Source as TRecurrencyDialogLanguage).MonthMay;
    FMonthJune := (Source as TRecurrencyDialogLanguage).MonthJune;
    FMonthJuly := (Source as TRecurrencyDialogLanguage).MonthJuly;
    FMonthAugust := (Source as TRecurrencyDialogLanguage).MonthAugust;
    FMonthSeptember := (Source as TRecurrencyDialogLanguage).MonthSeptember;
    FMonthOctober := (Source as TRecurrencyDialogLanguage).MonthOctober;
    FMonthNovember := (Source as TRecurrencyDialogLanguage).MonthNovember;
    FMonthDecember := (Source as TRecurrencyDialogLanguage).MonthDecember;

    FInterval := (Source as TRecurrencyDialogLanguage).Interval;
  end;
end;

constructor TRecurrencyDialogLanguage.Create;
begin
  inherited;
  FCaption := 'Recurrency';
  FSettings := 'Settings';
  FExceptions := 'Exceptions';
  FPatternDetails := 'Pattern details';
  FRecurrencyPattern := 'Recurrency pattern';
  FRange := 'Range';
  FRangeInfinite := 'Infinite';
  FRangeFor := 'For';
  FRangeOccurences := 'occurrences';
  FRangeUntil := 'Until date';
  FFreqNone := 'None';
  FFreqHourly := 'Hourly';
  FFreqDaily := 'Daily';
  FFreqWeekly := 'Weekly';
  FFreqMonthly := 'Monthly';
  FFreqYearly := 'Yearly';
  FButtonRemove := 'Remove';
  FButtonAdd := 'Add';
  FButtonClear := 'Clear';
  FButtonOK := 'OK';
  FButtonCancel := 'Cancel';
  FEveryDay := 'Every day';
  FEveryWeekDay := 'Every weekday';
  FEveryMonthDay := 'Every same day of the month';
  FEveryYearDay := 'Every same day of the year';
  FEvery := 'Every';

  FEveryFirst := 'first';
  FEverySecond := 'second';
  FEveryThird := 'third';
  FEveryFourth := 'fourth';

  FDayMonday := 'Monday';
  FDayTuesday := 'Tuesday';
  FDayWednesday := 'Wednesday';
  FDayThursday := 'Thursday';
  FDayFriday := 'Friday';
  FDaySaturday := 'Saturday';
  FDaySunday := 'Sunday';
  FDayWeekday := 'week day';
  FDayWeekend := 'weekend';

  FMonthJanuary := 'J';
  FMonthFebruary := 'F';
  FMonthMarch := 'M';
  FMonthApril := 'A';
  FMonthMay := 'M';
  FMonthJune := 'J';
  FMonthJuly := 'J';
  FMonthAugust := 'A';
  FMonthSeptember := 'S';
  FMonthOctober := 'O';
  FMonthNovember := 'N';
  FMonthDecember := 'D';

  FInterval := 'Interval';

end;

end.
