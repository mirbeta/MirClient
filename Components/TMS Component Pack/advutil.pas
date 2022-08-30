{*************************************************************************}
{ TADVSTRINGGRID HELPER FUNCTIONS                                         }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by                                                              }
{  TMS Software                                                           }
{  copyright © 1996-2015                                                  }
{  Email : info@tmssoftware.com                                           }
{  Web : http://www.tmssoftware.com                                       }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}
unit AdvUtil;

{$H+}

{$I TMSDEFS.INC}

interface

uses
  Windows, SysUtils,  Graphics, Grids, Classes, Controls, AdvStyleIF, Types
  {$IFDEF DELPHI_UNICODE}
  , Character
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type
  TAutoType = (atNumeric,atFloat,atString,atDate,atTime,atScientific);

  TTextType = (ttText,ttHTML,ttRTF,ttFormula,ttURL,ttUnicode);

  TGradientPart = (gpFull, gpLeft, gpRight, gpMiddle);

  TCellShape = (csRectangle, csCircle, csTriangleLeft, csTriangleRight, csTriangleUp, csTriangleDown,
                csDiamond, csLineVert, csLineHorz, csRoundRect, csEllips, csSquare, csRoundSquare, csStar,
                csArrowUp, csArrowDown, csArrowLeft, csArrowRight, csHalfStar);

  TControlType = (ctButton, ctSpin, ctCombo);

  TControlDrawStyle = (cdsXPTheme, cdsClassic, cdsFlat, cdsMetro);

  TControlState = (csNormal, csHover, csDown, csDisabled);

  {$IFNDEF DELPHI_UNICODE}
  TCharSet = set of char;
  {$ENDIF}

  {$IFDEF DELPHI_UNICODE}
  TCharSet = array of char;
  {$ENDIF}

  TGaugeOrientation = (goHorizontal, goVertical);
  TGaugeSettings = record
    Level0Color: TColor;
    Level0ColorTo: TColor;
    Level1Color: TColor;
    Level1ColorTo: TColor;
    Level2Color: TColor;
    Level2ColorTo: TColor;
    Level3Color: TColor;
    Level3ColorTo: TColor;
    Level1Perc: Integer;
    Level2Perc: Integer;
    BorderColor: TColor;
    ShowBorder: Boolean;
    Stacked: Boolean;
    ShowPercentage: Boolean;
    Font: TFont;
    FontColor: TColor;
    CompletionSmooth: Boolean;
    ShowGradient: Boolean;
    Steps: Integer;
    Position: Integer;
    BackgroundColor: TColor;
    Orientation: TGaugeOrientation;
  end;

  function Matches(s0a,s1a:PChar):Boolean;
  function MatchStr(s1,s2:string;DoCase:Boolean):boolean;
  function MatchStrEx(s1,s2:string;DoCase:Boolean):boolean;
  procedure LineFeedsToCSV(var s:string);
  procedure LineFeedsToCSVNQ(var s:string);  
  procedure CSVToLineFeeds(var s:string);
  procedure LineFeedsToJava(var s:string);
  procedure JavaToLineFeeds(var s:string);
  function LineFeedsToXLS(s:string):string;
  function LfToFile(s:string):string;
  function CRToLF(s:string):string;
  function StripCRLF(s:string):string;
  function FileToLf(s:string;multiline:Boolean): string;
  function DoubleToSingleChar(ch:char;const s:string): string;
  function GetNextLine(var s:string;multiline:Boolean): string;
  function GetLongestLine(Canvas: TCanvas; s: string): string;
  function LinesInText(s:string;multiline:Boolean):Integer;
  procedure OemToString(var s:string);
  procedure StringToOem(var s:string);
  procedure StringToPassword(var s:string;passwordchar:char);
  function RectString(r: TRect):string;
  function FixDecimalSeparator(s:string):string;
  function GetNextDate(d:TDateTime;dye,dmo,dda:word;dtv:TDateTime):TDateTime;
  procedure DrawBitmapTransp(Canvas:TCanvas;bmp:TBitmap;bkcolor: TColor;r: TRect);
  procedure DrawBitmapResourceTransp(Canvas: TCanvas; bkColor: TColor; r: TRect; ResName:string);
  procedure DrawErrorLines(Parent:TWinControl;Canvas: TCanvas; TmpStr:string; Rect:TRect; Height,ErrPos,ErrLen: Integer);
  function SinglePos(p:char;s:string;var sp:Integer):Integer;
  function NumSingleChar(p:char;s:string):Integer;
  function NumChar(p:char;s:string):Integer;
  function NumCharDel(p,del:char;s:string):Integer;
  function IsType(s:string):TAutoType;
  function CLFToLF(s:string):string;
  function LFToCLF(s:string):string;
  function HTMLColor(l:dword):string;
  function HTMLLineBreaks(s: string):string;
  function MakeHTMLHyperlink(const Value, LinkType: string; var ValueWithLink: string): boolean;
  function TextType(s:string; AllowHTML: Boolean = True): TTextType;
  function NameToCell(s:string;var cell: TPoint): Boolean;
  function RemoveSeps(s:string):string;
  function VarPos(su,s:string;var respos:Integer):Integer;
  function FirstChar(charset:TCharSet;s:string;var spos: integer):char;
  function IsURL(const s:string):boolean;
  procedure StripURLProtocol(var s:string);
  function Max(i1,i2:Integer):Integer;
  function Min(i1,i2:Integer):Integer;
  function GetToken(var s:string;separator:string):string;
  function ShiftCase(Name: string): string;
  function StrToShortdateUS(s:string):TDateTime;
  function StrToShortDateEU(s:string):TDateTime;
  function IsInGridRect(rc:TGridRect;c,r: Integer): Boolean;
  function StringListToText(st:TStringList):string;
  function FIPos(su,s:string): Integer;
  function CharPos(ch: Char; const s: string): Integer;
  function VarCharPos(ch: Char; const s: string; var Res: Integer): Integer;
  function VarCharPosNC(ch: Char; const s: string; var Res: Integer): Integer;
  function DarkenColor(Color: TColor): TColor;
  function ChangeColorPerc(Color: TColor; Perc: Integer): TColor;
  function CheckLimits(Value,LowLimit,UpLimit: Integer): Integer;
  function NumCharInStr(p:char; s:string):Integer;
  function CSVQuotes(const S: string): string;
  procedure SplitString(sep,s: string; sl: TStringList);
  procedure DrawProgressPie(Canvas: TCanvas; r: TRect;Color: TColor; p: Integer; print:boolean);
  procedure DrawProgressLin(Canvas: TCanvas; r: TRect; Color1,TColor1,Color2,TColor2: TColor; pd: double; Mx,My,Min,Max:Integer; Fmt: string; BorderColor: TColor; print: boolean);
  procedure DrawRangeIndicator(Canvas: TCanvas; r: TRect; Value, Range: Integer; ShowValue: boolean; NegColor, PosColor: TColor);
  procedure DrawGradient(Canvas: TCanvas; FromColor,ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
  procedure DrawVistaGradient(ACanvas: TCanvas; ARect: TRect; ColorFrom, ColorTo, ColorMirrorFrom, ColorMirrorTo: TColor;
    Direction: boolean; BorderColor: TColor; LeftRightBorder: boolean = false; Fill: Boolean = True);

  function GradientAt(FromColor, ToColor: TColor; Left, Right, Pos: integer): TColor;

  procedure DrawComboButton(Canvas: TCanvas;Handle: THandle; ARect: TRect; AControlStyle: TControlDrawStyle; AState: TControlState; ATones: TColorTones);

  procedure DrawSpinButtons(Canvas: TCanvas;Handle: THandle; ARect: TRect; AControlStyle: TControlDrawStyle; AState: TControlState; ATones: TColorTones);

  procedure DrawButton(Canvas: TCanvas; AHandle: THandle; ARect: TRect; AControlStyle: TControlDrawStyle; AState: TControlState; ATones: TColorTones; Prefix: boolean; ACaption: string = '');

  function DecodeWideStr(s:string): widestring;
  function EncodeWideStr(s:widestring): string;
  function StrPosWide(SubStr, Str: widestring): integer;
  function DynaLink_UpdateLayeredWindow(hwnd,hdcDst:thandle;
                                   pptDst,size:ppoint;hdcSrc:thandle;
                                   pptSrc:ppoint;
                                   crKey:dword;
                                   var pblend:_BLENDFUNCTION;
                                   dwFlags:DWORD):boolean;
  function DynaLink_SetLayeredWindowAttributes(HWND:thandle;crKey:DWORD;bAlpha:byte;dwFlags:DWORD):boolean;
  function IsDateStr(s: string): Boolean;
  procedure MakeFragment(var HTML: string);
  procedure DrawGauge(Canvas: TCanvas; R: TRect; Position: Integer; Settings: TGaugeSettings);
  procedure DrawTriangle(Canvas: TCanvas; X,Y: integer; Color: TColor);
  function CheckNum(ch:char): boolean;
  function CheckSignedNum(ch:char): boolean;
  procedure DrawSelectionGradient(Canvas: TCanvas;
     color1,color2,mircolor1,mircolor2,linecolortop,linecolorbottom,bordercolor,edgecolor,bkgcolor: TColor; r: TRect; part: TGradientPart);
  procedure DrawShape(Canvas: TCanvas; X,Y: integer; Width, Height: integer; Shape: TCellShape; FillColor:TColor; LineColor:TColor);

  function IsNumChar(ch: char): boolean;
  function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; stdcall;
  function RemoveAccentedStr(s: string): string;
  function Limit(v,mi,ma: integer): integer;
  function CheckFloatNumNoExp(ch: char): boolean;

implementation

uses
  AdvXPVS, Messages, Stdctrls;

const
  LINEFEED = #13;

var
  FuncStrCmpLogicalW : function (psz1, psz2: PWideChar): Integer; stdcall;
  ShlDLL: THandle;

function StrCmpLogicalW(psz1, psz2: PWideChar): integer; stdcall;
{$IFNDEF DELPHI_UNICODE}
var
  s1,s2: string;
{$ENDIF}
begin
  if Assigned(FuncStrCmpLogicalW) then
    Result := FuncStrCmpLogicalW(psz1,psz2)
  else
  begin
  {$IFDEF DELPHI_UNICODE}
    Result := StrComp(psz1,psz2);
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
    s1 := WideCharToString(psz1);
    s2 := WideCharToString(psz2);
    Result := CompareStr(s1,s2);
  {$ENDIF}
  end;
end;

function Limit(v,mi,ma: integer): integer;
begin
  Result := v;

  if v < mi then
    Result := mi;
  if v > ma then
    Result := ma;
end;

function RemoveAccentedStr(s: string): string;
var
  dest_sz,i: integer;
  dest: array[0..255] of widechar;
begin
  dest_sz := foldstring(MAP_COMPOSITE,PChar(s),Length(s), @dest, sizeof(dest) );

  Result := '';
  for i := 0 to dest_sz - 1 do
  begin
    if integer(dest[i]) <= 255 then
      Result := Result + dest[i];
  end;
end;

{$I DELPHIXE.INC}

procedure DrawSelectionGradient(Canvas: TCanvas; color1,color2,mircolor1,mircolor2,linecolortop,linecolorbottom,bordercolor,edgecolor,bkgcolor: TColor; r: TRect; part: TGradientPart);
var
  dl,dr: integer;
begin
  r.Bottom := r.Bottom - 2;
  r.Right := r.Right - 1;

  // top line
  Canvas.Pen.Color := linecolortop;
  Canvas.MoveTo(r.left,r.top + 1);
  Canvas.LineTo(r.right,r.top + 1);

  if (part in [gpFull, gpLeft]) then
  begin
    Canvas.Moveto(r.left + 1,r.Top +1);
    Canvas.LineTo(r.left + 1,r.Top + (r.Bottom -r.Top) div 2);
  end;

  if (part in [gpFull, gpRight]) then
  begin
    Canvas.Moveto(r.right -2,r.Top +1);
    Canvas.LineTo(r.right -2,r.Top + (r.Bottom -r.Top) div 2);
  end;

  Canvas.Pen.Color := linecolorbottom;
  Canvas.MoveTo(r.left,r.bottom - 1);
  Canvas.LineTo(r.right,r.bottom - 1);

  if (part in [gpFull, gpLeft]) then
  begin
    Canvas.Moveto(r.left + 1,r.Top + (r.Bottom -r.Top) div 2);
    Canvas.LineTo(r.left + 1,r.Bottom - 1);
  end;

  if (part in [gpFull, gpRight]) then
  begin
    Canvas.Moveto(r.right - 2, r.Top + (r.Bottom -r.Top) div 2);
    Canvas.LineTo(r.right - 2, r.Bottom - 1);
  end;

  dl := 2;
  dr := 2;


  if (part in [gpMiddle,gpLeft]) then
    dr := 0;

  if (part in [gpMiddle,gpRight]) then
    dl := 0;

  Canvas.Pen.Color := bordercolor;

  Canvas.MoveTo(r.left + dl,r.top);
  Canvas.LineTo(r.right - dr,r.top);

  Canvas.MoveTo(r.left + dl,r.bottom);
  Canvas.LineTo(r.right - dr,r.bottom);

  if (part in [gpFull, gpRight]) then
  begin
    Canvas.MoveTo(r.right - 1,r.top + 2);
    Canvas.LineTo(r.right - 1,r.bottom - 1);
  end;

  if (part in [gpFull, gpLeft]) then
  begin
    Canvas.MoveTo(r.left,r.top + 2);
    Canvas.LineTo(r.left,r.bottom - 1);
  end;

  if (part in [gpFull, gpLeft]) then
  begin
    Canvas.Pixels[r.Left + 1,r.Top] := edgecolor;
    Canvas.Pixels[r.Left + 1,r.Top + 1] := edgecolor;
    Canvas.Pixels[r.Left,r.Top + 1] := edgecolor;
    Canvas.Pixels[r.Left + 1,r.Bottom] := edgecolor;
    Canvas.Pixels[r.Left + 1,r.Bottom - 1] := edgecolor;
    Canvas.Pixels[r.Left,r.Bottom - 1] := edgecolor;

    Canvas.Pixels[r.Left,r.Top] := bkgcolor;
    Canvas.Pixels[r.Left,r.Bottom] := bkgcolor;
  end;

  if (part in [gpFull, gpRight]) then
  begin
    Canvas.Pixels[r.right - 2,r.top] := edgecolor;
    Canvas.Pixels[r.right - 2,r.top + 1] := edgecolor;
    Canvas.Pixels[r.right - 1,r.top + 1] := edgecolor;
    Canvas.Pixels[r.right - 2,r.bottom] := edgecolor;
    Canvas.Pixels[r.right - 2,r.bottom - 1] := edgecolor;
    Canvas.Pixels[r.right - 1,r.bottom - 1] := edgecolor;

    Canvas.Pixels[r.Right - 1,r.Top] := bkgcolor;
    Canvas.Pixels[r.Right - 1,r.Bottom] := bkgcolor;
  end;

  r := Rect(r.Left + dl, r.Top + 2, r.Right - dr, r.Bottom - 1);

  if (color2 = clNone) then
  begin
    Canvas.Brush.Color := color1;
    Canvas.Pen.Color := color1;
    Canvas.FillRect(r);
  end
  else
  begin
    if mircolor1 <> clNone then
    begin
      DrawGradient(Canvas,color1,color2,16,Rect(r.Left, r.Top, r.Right, r.Top + (r.Bottom - r.Top) div 2), false);
      DrawGradient(Canvas,mircolor1,mircolor2,16,Rect(r.Left, r.Top + (r.Bottom - r.Top) div 2, r.Right, r.Bottom), false);
    end
    else
      DrawGradient(Canvas,color1,color2,16,r, false);
  end;
end;

procedure SplitString(sep,s: string; sl: TStringList);
var
  vp: integer;
begin
  while (VarPos(sep,s,vp) > 0) do
  begin
    sl.Add(copy(s,1,vp - 1));
    Delete(s,1,vp);
  end;
  sl.Add(s);
end;

function CSVQuotes(const s: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to length(s) do
  begin
    Result := Result + s[i];
    if s[i]='"' then
      Result := Result + '"';
  end;
end;

function CheckLimits(Value,LowLimit,UpLimit: Integer): Integer;
begin
  Result := Value;

  if LowLimit <> UpLimit then
  begin
    if (Value < LowLimit) and (LowLimit > 0) then
      Result := LowLimit;
    if (Value > Uplimit) and (UpLimit > 0) then
      Result := UpLimit;
  end;
end;

function DarkenColor(Color: TColor): TColor;
var
  r,g,b: longint;
  l: longint;
begin
  l := ColorToRGB(Color);
  r := ((l AND $FF0000) shr 1) and $FF0000;
  g := ((l AND $FF00) shr 1) and $FF00;
  b := ((l AND $FF) shr 1) and $FF;
  Result := r or g or b;
end;

function ChangeColorPerc(Color: TColor; Perc: Integer): TColor;
var
  r,g,b: longint;
  l: longint;
begin
  l := ColorToRGB(Color);

  r := (l AND $FF0000) shr 16;
  g := (l AND $00FF00) shr 8;
  b := (l AND $0000FF);

  r := Max(0,round(r + r * Perc /100));
  g := Max(0,round(g + g * Perc /100));
  b := Max(0,round(b + b * Perc /100));

  r := ((r and $FF) shl 16) and $FF0000;
  g := ((g and $FF) shl 8) and $FF00;
  b := (b and $FF);

  Result := r or g or b;
end;


function FIPos(su,s:string):Integer;
begin
  Result := Pos(su,UpperCase(s));
end;

function VarCharPos(ch: Char; const s: string; var Res: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i] = ch then
    begin
      Res := i;
      Result := i;
      Break;
    end;
end;

function VarCharPosNC(ch: Char; const s: string; var Res: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if upcase(s[i]) = ch then
    begin
      Res := i;
      Result := i;
      Break;
    end;
end;


function CharPos(ch: Char; const s: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i] = ch then
    begin
      Result := i;
      Break;
    end;
end;

function IsInGridRect(rc:TGridRect;c,r: Integer): Boolean;
begin
  Result := (c >= rc.Left) and (c <= rc.Right) and (r >= rc.Top) and (r <= rc.Bottom);
end;


function GetToken(var s:string;separator:string):string;
var
  sp:Integer;
begin
  Result := '';
  sp := Pos(separator,s);
  if sp > 0 then
  begin
    Result := Copy(s,1,sp - 1);
    Delete(s,1,sp);
  end;
end;

function Max(i1,i2:Integer):Integer;
begin
  if i1 > i2 then
    Result := i1
  else
    Result := i2;
end;

function Min(i1,i2:Integer):Integer;
begin
  if i1 < i2 then
    Result := i1
  else
    Result := i2;
end;

function StringListToText(st:TStringList):string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to st.Count do
    Result := Result + st.Strings[i - 1];
end;

function VarPos(su,s:string;var Respos:Integer):Integer;
begin
  Respos := Pos(su,s);
  Result := Respos;
end;

function NumPos(ch:char; num: integer; s: string): integer;
var
  occ,i: integer;
begin
  occ := -1;
  result := -1;
  i := 1;

  while (i <= Length(s)) do
  begin
    if s[i] = ch then
      inc(occ);

    if (occ = num) then
    begin
      Result := i;
      break;
    end;
    inc(i);
  end;
end;

{$IFNDEF DELPHI_UNICODE}
function FirstChar(Charset:TCharSet;s:string;var spos: integer):char;
var
  i:Integer;
  q: Integer;
begin
  i := 1;
  q := 0;
  spos := -1;
  Result := #0;

  while i <= Length(s) do
  begin
    if s[i] = '"' then
      inc(q);

    if (s[i] in Charset) and not odd(q) then
    begin
      spos := i;
      Result := s[i];
      Break;
    end;
    Inc(i);
  end;
end;
{$ENDIF}

{$IFDEF DELPHI_UNICODE}
function FirstChar(Charset:TCharSet;s:string;var spos: integer):char;
var
  i:Integer;
  q: Integer;

  function InArray(ch: char): boolean;
  var
    j: integer;
  begin
    Result := false;
    for j := 0 to High(CharSet) do
    begin
      if ch = CharSet[j] then
      begin
        Result := true;
        Break;
      end;
    end;
  end;

begin
  i := 1;
  q := 0;
  spos := -1;
  Result := #0;

  while i <= Length(s) do
  begin
    if s[i] = '"' then
      inc(q);

    if (InArray(s[i])) and not odd(q) then
    begin
      spos := i;
      Result := s[i];
      Break;
    end;
    Inc(i);
  end;
end;
{$ENDIF}


{parse RxCy naming}
function NameToCell(s:string;var Cell:TPoint):Boolean;
var
  code1,code2,cp: Integer;
  r,c: Integer;
begin
  s := Uppercase(s);
  Result := False;
  if Length(s) >= 4 then
  begin
    if s[1] = 'R' then
    begin
      Delete(s,1,1);
      if VarPos('C',s,cp) > 0 then
      begin
        Val(Copy(s,1,cp - 1),r,code1);
        Val(Copy(s,cp + 1,Length(s)),c,code2);
        if (code1 = 0) and (code2 = 0) then
        begin
          Cell.x := c;
          Cell.y := r;
          Result := True;
        end;
      end;
    end;
  end;
end;

function IsURL(const s:string):boolean;
begin
  Result := (Pos('://',s) > 0) or (Pos('mailto:',s) > 0);
end;

procedure StripURLProtocol(var s:string);
var
 vp: Integer;
begin
  if VarPos('://',s,vp)>0 then Delete(s,1,vp + 2)
  else
   if VarPos('mailto:',s,vp)>0 then Delete(s,1,vp + 6);
end;

function TextType(s:string; AllowHTML: Boolean =  true): TTextType;
var
  i: integer;
  ch,chn: char;
begin
  Result := ttText;
  s := s + ' ';

  if Length(s) > 1 then
  begin
    if s[1] = '=' then
      Result := ttFormula
    else
    begin
      if (s[1] = '|') and (s[2] = '\') then
      begin
        Result := ttUnicode;

        { unicode formula detection }
        if (Length(s) > 4) then
          if (ord(s[4]) = 61) and (ord(s[5]) = 0) then
             Result := ttFormula;

        Exit;
      end;

      if (s[1] = '{') and (s[2] = '\') then
      begin
        Result := ttRTF;
        Exit;
      end;

      if AllowHTML then
      begin
        for i := 1 to Length(s) - 2 do
        begin
          if (s[i] = '<') then
          begin
            ch := Upcase(s[i + 1]);
            chn := Upcase(s[i + 2]);

            if (ch = '/') then
            begin
              Result := ttHTML;
              Break;
            end
            else
            begin
              if ((ch = 'I') and (chn = 'M')) or
                 ((ch = 'B') and (chn = 'R')) or
                 ((ch = 'C') and (chn = 'O')) then
                 begin
                   Result := ttHTML;
                   Break;
                 end;
            end;
          end;
        end;
      end;
    end;

    if Result = ttText then
    begin
      if IsURL(s) then
        Result := ttURL;
    end;
  end;
end;

function IsNumChar(ch: char): boolean;
begin
  {$IFNDEF DELPHIXE4_LVL}

  {$IFNDEF DELPHI_UNICODE}
  Result := (ch in ['0'..'9']);
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  Result := Character.IsNumber(ch);
  {$ENDIF}
  {$ENDIF}

  {$IFDEF DELPHIXE4_LVL}
  Result := ch.IsNumber;
  {$ENDIF}
end;

function CheckNum(ch:char): boolean;
begin
  Result := IsNumChar(ch);
end;

function CheckSignedNum(ch:char): boolean;
begin
  Result := IsNumChar(ch) or (ch = '-');
end;

function CheckExpNum(ch:char): boolean;
begin
  Result := IsNumChar(ch) or (ch = 'e') or (ch = 'E');
end;

function CheckFloatNum(ch: char): boolean;
begin
  Result := IsNumChar(ch) or (ch = ThousandSeparator) or (ch = DecimalSeparator) or (ch = 'e') or (ch = 'E');
end;

function CheckFloatNumNoExp(ch: char): boolean;
begin
  Result := IsNumChar(ch) or (ch = ThousandSeparator) or (ch = DecimalSeparator) or (ch = '-') or (ch = #8) or (ch = #9) or (ch = #27) or (ch = #13);
end;

function CheckSignedDottedNum(ch: char): boolean;
begin
  Result := IsNumChar(ch) or (ch = '.') or (ch = '-') or (ch = 'e') or (ch = 'E') or (ch = '+');
end;

function CheckSignedFloatNum(ch: char): boolean;
begin
  Result := CheckFloatNum(ch) or (ch ='+') or (ch = '-');
end;

function RemoveSeps(s:string):string;
var
  i: Integer;
  Neg: Boolean;
begin
  Result := '';
  Neg := False;

  if Length(s) = 0 then Exit;

  // delete leading and / or trailing spaces
  Trim(s);

  // delete currency () negative value brackets  
  if Length(s) > 0 then
  begin
    if (s[1] = '(') and (s[Length(s)] = ')') then
      Neg := True;
  end;

  while (Length(s) > 1) and
    not CheckFloatNum(s[1]) do
    begin
      if s[1] = '-' then
        Neg := True;
      Delete(s,1,1);
    end;

  for i := 1 to Length(s) do
  begin
    if s[i] <> ThousandSeparator then
      if s[i] = DecimalSeparator then
        Result := Result + '.'
      else
      begin
        if CheckSignedDottedNum(s[i]) then
          Result := Result + s[i]
        else
          Break;
      end;
  end;

  if Neg then
    Result := '-' + Result;
end;

function HTMLLineBreaks(s:string):string;
var
  i: Integer;
  res: String;
begin
  res := '';

  if Pos(#13,s) = 0 then
    res := s
  else
    for i := 1 to Length(s) do
    begin
      if s[i] <> #13 then
        res := res + s[i]
      else
        res := res + '<br>';
    end;

  Result := res;
end;

function HTMLColor(l:dword):string;
const
  HexDigit:array[0..$F] of char = '0123456789ABCDEF';

var
  lw,hw: Word;
begin
  lw := loword(l);
  hw := hiword(l);
  HTMLColor:=HexDigit[Lo(lw) shr 4]+HexDigit[Lo(lw) and $F]+
             HexDigit[Hi(lw) shr 4]+HexDigit[Hi(lw) and $F]+
             HexDigit[Lo(hw) shr 4]+HexDigit[Lo(hw) and $F];
end;

function IsType(s:string): TAutoType;
var
  i: Integer;
  isI,isF,isS,isD,isT: Boolean;
  th,de,mi,ds,ts,ld,lt: Integer;
  isE: boolean;

begin
  Result := atString;
  if s = '' then Exit;

  isI := True;
  isF := True;
  isS := True;
  isE := False;
  isD := Pos(DateSeparator,s) > 0;
  isT := Pos(TimeSeparator,s) > 0;

  th := -1;
  ld := -1;
  lt := -1;
  de := 0;
  mi := 0;
  ds := 0;
  ts := 0;

  for i := 1 to Length(s) do
  begin
    if not CheckSignedNum(s[i]) then
      isI := False;

    if (s[i] = 'e') or (s[i] = 'E') then
    begin
      if isE then
        isS := false;
      isE := true;
    end;

    // - sign must be first char
    if (i > 1) and (s[i] = '-') then
    begin
      isI := false;
      isF := false;
    end;

    if (i = 1) and ((s[i] = 'e') or (s[i] = 'E')) then
      isS := false;

    if (i = 1) and not CheckNum(s[i]) then
    begin
      isT := false;
      isD := false;
    end;

    if (i = length(s)) and ( (s[i] = 'e') or (s[i] = 'E') or (s[i] = '+') or (s[i] = '-') ) then
      isS := false;
      
    if not (CheckNum(s[i]) or (s[i] = '-') or (s[i] = ThousandSeparator) or (s[i] = DecimalSeparator) ) then
      isF := False;

    if not (CheckNum(s[i]) or (s[i] = DateSeparator)) then
      isD := False;

    if not (CheckNum(s[i]) or (s[i] = TimeSeparator)) then
      isT := False;

    if not (CheckSignedFloatNum(s[i]) and (s[i] <> ThousandSeparator)) then
      isS := False;

    if (s[i] = ThousandSeparator) and (i - th < 4) and (th <> -1) then
      isF := False;

    if (s[i] = ThousandSeparator) then
      th := i;

    if (s[i] = DateSeparator) then
    begin
      inc(ds);
      if (ds > 2) or (ld = i - 1) then
        isD := false;
      ld := i;
    end;

    if (s[i] = TimeSeparator) then
    begin
      inc(ts);
      if (ts > 2) or (lt = i - 1) then
        isT := false;
      lt := i;
    end;

    if (s[i] = DecimalSeparator) then
      Inc(de);

    if (s[i] = '-') then
      Inc(mi);
  end;

  if isI then
    Result := atNumeric
  else
  begin
    if isF and not isE then
      Result := atFloat;
    if isS and isE then
    begin
      if mi = 2 then
        mi := 1;
      Result := atScientific;
    end;
  end;

  if isT then
    Result := atTime
  else
    if isD then
      Result := atDate
    else
     if (mi > 1) or (de > 1) then
       Result := atString;
end;

function CLFToLF(s:string):string;
var
  vp: Integer;
begin
  while VarPos('\n',s,vp)>0 do
  begin
    s := Copy(s,1,vp - 1) + #13 + Copy(s,vp + 2,Length(s));
  end;
  Result := s;
end;

function LFToCLF(s:string):string;
var
  res:string;
  i:Integer;
begin
  Res := '';
  for i := 1 to Length(s) do
  begin
    if s[i] = #13 then
      Res := Res + '\n'
    else
      if s[i] <> #10 then
        Res := Res + s[i];
   end;
  Result:=res;
end;


procedure StringToPassword(var s:string;passwordchar:char);
var
  i: Integer;
begin
  for i := 1 to Length(s) do
    s[i] := PasswordChar;
end;

procedure StringToOem(var s:string);
{$IFNDEF DELPHIXE4_LVL}
{$IFDEF WIN32}
var
  lsi,lso: integer;
  pin: PChar;
  {$IFDEF DELPHI_UNICODE}
  pout: PAnsiChar;
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  pout: PChar;
  {$ENDIF}
{$ENDIF}
{$ENDIF}
begin
  //DELPHI_UNICODE
  {$IFNDEF DELPHIXE4_LVL}
  {$IFDEF WIN32}
  lsi := Length(s) + 1;
  lso := lsi;

  {$IFDEF DELPHI_UNICODE}
  lsi := lsi * 2;
  {$ENDIF}

  GetMem(pin,lsi);
  GetMem(pout,lso);
  StrLCopy(pin,PChar(s),Length(s));
  CharToOem(pin,pout);
  s := string(StrPas(pout));
  FreeMem(pin);
  FreeMem(pout);
 {$ENDIF}
 {$ENDIF}
end;

procedure OemToString(var s:string);
{$IFNDEF DELPHIXE4_LVL}
{$IFDEF WIN32}
var
  lsi,lso: integer;
  {$IFDEF DELPHI_UNICODE}
  pin: PAnsiChar;
  sa: ansistring;
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  pin: PChar;
  sa: string;
  {$ENDIF}
  pout: PChar;
{$ENDIF}
{$ENDIF}
begin
  //DELPHI_UNICODE
  {$IFNDEF DELPHIXE4_LVL}
  {$IFDEF WIN32}
  sa := ansistring(s);
  lsi := Length(sa) + 1;
  lso := lsi;
  {$IFDEF DELPHI_UNICODE}
  lso := lso * 2;
  {$ENDIF}
  GetMem(pin,lsi);
  GetMem(pout,lso);
  StrPLCopy(pin,sa,Length(sa));
  OemToChar(pin,pout);
  s := StrPas(pout);
  FreeMem(pin);
  FreeMem(pout);
  {$ENDIF}
  {$ENDIF}
end;

function DoubleToSingleChar(ch: Char;const s:string):string;
var
  Res: string;
  i: Integer;
begin
  if (s = '') or (CharPos(ch,s) = 0) then
  begin
    DoubleToSingleChar := s;
    Exit;
  end;

  res := '';
  i   := 1;

  repeat
    Res := Res + s[i];
    // skip dbl quote
    if ((s[i] = ch) and (s[i + 1] = ch)) then
      Inc(i);
    Inc(i);
  until (i > Length(s));

  DoubleToSingleChar := Res;
end;

procedure LineFeedsToCSV(var s:string);
var
  vp: Integer;
begin
  while VarPos(#13#10,s,vp)>0 do
  begin
    Delete(s,vp,1);
    s[vp] := #11;
  end;
  s := '"' + s + '"';
end;

procedure LineFeedsToCSVNQ(var s:string);
var
  vp: Integer;
begin
  while VarPos(#13#10,s,vp)>0 do
    Delete(s,vp,1);
end;


procedure CSVToLineFeeds(var s:string);
var
  res: string;
  i: Integer;
begin
  if CharPos(#11,s) = 0 then
    Exit;
  Res := '';
  for i := 1 to Length(s) do
    if s[i] = #11 then
      Res := Res + #13#10
    else
      Res := Res + s[i];
  s := Res;
end;

procedure LineFeedsToJava(var s:string);
var
  i: Integer;
  Res: string;
begin
  Res := '';
  for i := 1 to Length(s) do
    if s[i] = #13 then
      Res := Res + '~'
    else
    begin
      if s[i] <> #10 then
        Res := Res + s[i];
    end;
  s := res;
end;

procedure JavaToLineFeeds(var s:string);
var
  Res: string;
  i: Integer;
begin
  Res := '';
  for i := 1 to Length(s) do
    if s[i] = '~' then
      Res := Res + #13#10
    else
      Res := Res + s[i];
  if Res = '^' then
    Res := '';    
  s := Res;
end;

function IsDate(s:string;var dt:TDateTime):boolean;
var
  su, ts: string;
  da,mo,ye,ho,mi,se: word;
  err: Integer;
  dp,mp,yp,vp: Integer;
begin
  Result := False;

  ts := '';

  su := UpperCase(shortdateformat);
  dp := pos('D',su);
  mp := pos('M',su);
  yp := pos('Y',su);

  da := 0;
  mo := 0;
  ye := 0;
  ho := 0;
  mi := 0;
  se := 0;

  if VarPos(DateSeparator,s,vp)>0 then
  begin
    su := Copy(s,1,vp - 1);

    if (dp<mp) and
       (dp<yp) then
       val(su,da,err)
    else
    if (mp<dp) and
       (mp<yp) then
       val(su,mo,err)
    else
    if (yp<mp) and
       (yp<dp) then
       val(su,ye,err);

    if err<>0 then Exit;
    Delete(s,1,vp);

    if VarPos(DateSeparator,s,vp)>0 then
    begin
      su := Copy(s,1,vp - 1);

      if ((dp>mp) and (dp<yp)) or
         ((dp>yp) and (dp<mp)) then
         val(su,da,err)
      else
      if ((mp>dp) and (mp<yp)) or
         ((mp>yp) and (mp<dp)) then
         val(su,mo,err)
      else
      if ((yp>mp) and (yp<dp)) or
         ((yp>dp) and (yp<mp)) then
         val(su,ye,err);

      if err<>0 then Exit;
      Delete(s,1,vp);

      s := Trim(s);

      if VarPos(' ',s, vp) > 0 then  // there is space to separate date & time
      begin
        ts := copy(s, vp, length(s));
        s := copy(s, 1, vp - 1);
      end;

      if (dp>mp) and
         (dp>yp) then
         val(s,da,err)
      else
      if (mp>dp) and
         (mp>yp) then
         val(s,mo,err)
      else
      if (yp>mp) and
         (yp>dp) then
         val(s,ye,err);

      if err<>0 then Exit;
      if (da>31) then Exit;
      if (mo>12) then Exit;

      if (ts <> '') then  // there is a time part
      begin
        if VarPos(TimeSeparator,ts,vp)>0 then
        begin
          su := Copy(ts,1,vp - 1); // hour part
          val(su,ho,err);

          if (err <> 0) then Exit;
          if (ho > 23) then Exit;

          Delete(ts,1,vp);

          if VarPos(TimeSeparator,ts,vp)>0 then // there is a second part
          begin
            su := Copy(ts,1,vp - 1); // minute part
            val(su,mi,err);

            if err <> 0 then Exit;
            Delete(ts,1,vp);

            val(ts,se,err);  // second part
            if (err <> 0) then Exit;
            if (se > 60) then Exit;
          end
          else
          begin
            val(su,mi,err); // minute part
            if (err <> 0) then Exit;
          end;

          if (mi > 59) then Exit;

          Result := true;
        end;
      end
      else
        Result := True;

      try
        dt := EncodeDate(ye,mo,da) + EncodeTime(ho,mi,se,0);
      except
        Result := False;
      end;
    end;
  end;
end;

{
function IsTime(s:string;var dt:TDateTime):boolean;
var
 su:string;
 ho,mi:word;
 err:Integer;
begin
 Result:=False;
 if (pos(timeseparator,s)>0) then
  begin
    su:=copy(s,1,pos(timeseparator,s)-1);
    val(su,ho,err);
    delete(s,1,pos(timeseparator,s)+1);
    val(su,mi,err);
    if ho>23 then Exit;
    if mi>59 then Exit;
    Result:=True;
    try
     dt:=encodetime(ho,mi,0,0);
    except
     Result:=False;
    end;
  end;
end;
}


function StripLogicSpaces(s: string): string;
var
  i: integer;
  q: integer;
begin

  q := 0;
  i := 1;
  Result := '';

  while (i <= length(s)) do
  begin
    if s[i] = '"' then
      inc(q);

    if (s[i] = ' ') then
    begin
      if odd(q) then
        result := result + s[i];
    end
    else
      Result := Result + s[i];

    inc(i);
  end;
end;

function ClosingParenthesis(s1: string): integer;
var
  i,j,k,r: integer;
begin
  r := 0;
  j := 0;
  k := 0;
  i := 1;

  while (i <= length(s1)) do
  begin
    if (s1[i] = ')') then
      inc(k);

    if (s1[i] = '(') then
      inc(j);

    if (s1[i] = ')') and (j = k) then
    begin
      r := i;
      break;
    end;


    inc(i);
  end;

  Result := r;
end;


function MatchStrEx(s1,s2:string;DoCase:Boolean): Boolean;
var
  ch,lastop: Char;
  sep,cp: Integer;
  res,newres: Boolean;
  {$IFDEF DELPHI_UNICODE}
  CharArray: TCharSet;
  {$ENDIF}

begin
  // remove leading & trailing spaces
  s1 := Trim(s1);

//  outputdebugstring(pchar('matchstrex:'+s1));

  // remove spaces between multiple filter conditions
  s1 := StripLogicSpaces(s1);

//  s1 := StringReplace(s1,'""','"',[rfReplaceAll]);

  {
  while VarPos(' &',s1,sep) > 0 do Delete(s1,sep,1);
  while VarPos(' ;',s1,sep) > 0 do Delete(s1,sep,1);
  while VarPos(' ^',s1,sep) > 0 do Delete(s1,sep,1);
  while VarPos(' |',s1,sep) > 0 do Delete(s1,sep,1);
  while VarPos(' =',s1,sep) > 0 do Delete(s1,sep,1);
  while VarPos('& ',s1,sep) > 0 do Delete(s1,sep+1,1);
  while VarPos('; ',s1,sep) > 0 do Delete(s1,sep+1,1);
  while VarPos('^ ',s1,sep) > 0 do Delete(s1,sep+1,1);
  while VarPos('| ',s1,sep) > 0 do Delete(s1,sep+1,1);
  while VarPos('= ',s1,sep) > 0 do Delete(s1,sep+1,1);
  }

  if VarPos('=',s1,sep) = 1 then
    Delete(s1,sep,1);

  LastOp := #0;
  Res := True;

  {$IFDEF DELPHI_UNICODE}
  SetLength(CharArray,5);
  CharArray[0] := '(';
  CharArray[1] := ';';
  CharArray[2] := '^';
  CharArray[3] := '&';
  CharArray[4] := '|';
  {$ENDIF}

  repeat
    {$IFDEF DELPHI_UNICODE}
    ch := FirstChar(CharArray,s1, sep);
    {$ENDIF}

    {$IFNDEF DELPHI_UNICODE}
    ch := FirstChar(['(',';','^','&','|'],s1, sep);
    {$ENDIF}

    // extract first part of filter
    if ch <> #0 then
    begin
      //VarPos(ch,s1,sep);

      if (length(s1) > 0) and (s1[1] = '(') and (pos('(',s1) > 0) then
      begin // found start of parenthesis
        cp := ClosingParenthesis(s1);
        NewRes := MatchStrEx(copy(s1,2,cp - 2),s2,DoCase);
        delete(s1,1,cp);
      end
      else
      begin
        NewRes := MatchStr(Copy(s1,1,sep - 1),s2,DoCase);
        Delete(s1,1,sep);
      end;

      if LastOp = #0 then
        Res := NewRes
      else
        case LastOp of
        ';','^','|':Res := Res or NewRes;
        '&':Res := Res and NewRes;
        end;

      LastOp := ch;
     end;

  until (ch = #0);

  NewRes := MatchStr(s1,s2,DoCase);

  if LastOp = #0 then
    Res := NewRes
  else
    case LastOp of
    ';','^','|':Res := Res or NewRes;
    '&':Res := Res and NewRes;
    end;

  Result := Res;
end;

function MatchStr(s1,s2:string;DoCase:Boolean):Boolean;
begin
  if s1 = '""' then
    MatchStr := (s2 = '')
  else
  begin
    if DoCase then
      MatchStr := Matches(PChar(s1),PChar(s2))
    else
      MatchStr := Matches(PChar(AnsiUpperCase(s1)),PChar(AnsiUpperCase(s2)));
  end;
end;

function StripThousandSep(ps: pchar):string;
var
  i: Integer;
  s: string;
begin
  Result := '';
  s := strpas(ps);
  for i := 1 to Length(s) do
  begin
    if s[i] = DecimalSeparator then
      Result := Result + '.' else
      if s[i] <> ThousandSeparator then
        Result := Result + s[i];
  end;
end;

function Matches(s0a,s1a: PChar): Boolean;
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
  matching:boolean;
  done:boolean;
  len:longint;
  lastchar:char;
  s0,s1,s2,s3:pchar;
  oksmaller,oklarger,negflag:boolean;
  compstr:array[0..255] of char;
  flag1,flag2,flag3:boolean;
  equal:boolean;
  n1,n2:double;
  code1,code2:Integer;
  dt1,dt2:TDateTime;
  q: integer;
  lq: boolean;
begin
  oksmaller := True;
  oklarger := True;
  flag1 := False;
  flag2 := False;
  flag3 := False;
  negflag := False;
  equal := False;

  { [<>] string [&|] [<>] string }

  // do larger than or larger than or equal
  s2 := StrPos(s0a,larger);
  if s2 <> nil then
  begin
    inc(s2);
    if (s2^ = '=') then
    begin
      Equal := True;
      inc(s2);
    end;

    while (s2^ = ' ') do
      inc(s2);

    s3 := s2;
    len := 0;

    lastchar := #0;

    q := 0;

    while (s2^ <> ' ') and (s2^ <> NULL) and (odd(q) or ((s2^ <> '&') and (s2^ <> '|')))  do
    begin
      if (s2^= '"') then
        inc(q);

      if (len = 0) and (s2^ = '"') then
        inc(s3)
      else
        inc(len);

      lastchar := s2^;
      inc(s2);

      if (s2^= ' ') and odd(q) then // skip space if between quotes
      begin
        lastchar := s2^;
        inc(s2);
      end;
    end;

    if (len > 0) and (lastchar = '"') then
      dec(len);

    StrLCopy(compstr,s3,len);

    Val(StripThousandSep(s1a),n1,code1);
    Val(StripThousandSep(compstr),n2,code2);

    if IsDate(compstr,dt2) then code2 := 1;         
    if IsDate(s1a,dt1) then code1 := 1;     

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
         oklarger := (strlcomp(compstr,s1a,255)<=0)
        else
         oklarger := (strlcomp(compstr,s1a,255)<0);
      end;
    end;
    flag1 := True;
  end;

  equal := False;

  // do smaller than or smaller than or equal
  s2 := strpos(s0a,smaller);
  if (s2 <> nil) then
  begin
    inc(s2);
    if (s2^ = '=') then
      begin
       equal := True;
       inc(s2);
      end;
      
    lastchar := #0;

    while (s2^=' ') do inc(s2);
    s3 := s2;
    len := 0;
    q := 0;

    while (s2^ <> ' ') and (s2^ <> NULL) and (odd(q) or ((s2^ <> '&') and (s2^ <> '|'))) do
    begin
      if s2^ = '"' then
        inc(q);

      if (len = 0) and (s2^ = '"') then
        inc(s3)
      else
        inc(len);

      lastchar := s2^;
      inc(s2);
    end;

    if (len > 0) and (lastchar = '"') then
      dec(len);

    strlcopy(compstr,s3,len);

    Val(StripThousandSep(s1a),n1,code1);
    Val(StripThousandSep(compstr),n2,code2);
    if IsDate(compstr,dt2) then code2 := 1;         
    if IsDate(s1a,dt1) then code1 := 1;             

    if (code1 = 0) and (code2 = 0) then // both are numeric types
     begin
      if equal then
       oksmaller := n1 <= n2
      else
       oksmaller := n1 < n2;
     end
    else
     begin
      // check for dates here ?
      if IsDate(strpas(compstr),dt2) and IsDate(strpas(s1a),dt1) then
       begin
        if equal then
         oksmaller := dt1 <= dt2
        else
         oksmaller := dt1 < dt2;
       end
      else
       begin
        if equal then
          oksmaller := (strlcomp(compstr,s1a,255)>=0)
        else
          oksmaller := (strlcomp(compstr,s1a,255)>0);
       end;
     end;

    flag2 := True;
  end;

  s2 := strpos(s0a,negation);
  
  if (s2 <> nil) then
  begin
    inc(s2);
    while (s2^=' ') do
      inc(s2);
    s3 := s2;
    len := 0;

    lastchar := #0;
    q := 0;

    while (s2^ <> ' ') and (s2^ <> NULL) and (odd(q) or ((s2^ <> '&') and (s2^ <> '|'))) do
    begin
      if (s2^ = '"') then
        inc(q);

      if (len = 0) and (s2^ = '"') then
        inc(s3)
      else
        inc(len);
        
      lastchar := s2^;
      inc(s2);
    end;

    if (len > 0) and (lastchar = '"') then
      dec(len);

    strlcopy(compstr,s3,len);
    flag3 := True;
  end;

  if (flag3) then
  begin
    if strpos(s0a,larger) = nil then
      flag1 := flag3;
    if strpos(s0a,smaller) = nil then
      flag2 := flag3;
  end;

  if (strpos(s0a,logor) <> nil) then
    if flag1 or flag2 then
    begin
      matches := oksmaller or oklarger;
      Exit;
    end;

  if (strpos(s0a,logand)<>nil) then
    if flag1 and flag2 then
    begin
      matches := oksmaller and oklarger;
      Exit;
    end;

  if ((strpos(s0a,larger) <> nil) and (oklarger)) or
     ((strpos(s0a,smaller) <> nil) and (oksmaller)) then
  begin
    matches := True;
    Exit;
  end;

  s0 := s0a;
  s1 := s1a;

  matching := True;

  done := (s0^ = NULL) and (s1^ = NULL);

  lq := false;

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
        lq := false;
      end;
    negation:
      begin
        negflag:=True;
        inc(s0);
        lq := false;
      end;
    '"':
      begin
        if not lq then
        begin
          inc(s0);
          lq := true;
        end
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
    (*
    '\':
      begin
        inc(s0);
        matching := s0^ = s1^;

        if matching then
        begin
          inc(s0);
          inc(s1);
        end;
      end;
    *)
    asterix:
      begin
        lq := false;
        repeat
          inc(s0)
        until (s0^ <> asterix);
        len := strlen(s1);
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
         s0 := strend(s0);
         s1 := strend(s1);
       end;
     end;
   else
     begin
       lq := false;
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

function Lftofile(s:string):string;
var
  i:Integer;
begin
  if Pos(#13,s) > 0 then
    for i := 1 to Length(s) do
    begin
      if s[i] = #13 then s[i] := #9;
      if s[i] = #10 then s[i] := #8;
      if s[i] = #0 then s[i] := #1;

    end;
  LFToFile := s;
end;

function FileToLF(s:string;multiline:boolean):string;
var
  i:Integer;
begin
  if Pos(#8,s)>0 then
    for i := 1 to Length(s) do
    begin
      if s[i] = #9 then s[i] := #13;
      if s[i] = #8 then s[i] := #10;
      if s[i] = #1 then s[i] := #0;
    end;
  if not MultiLine then
    FileToLF := GetNextLine(s,multiline)
  else
    FiletoLF := s;
end;

function GetLongestLine(Canvas: TCanvas; s: string): string;
var
  d,m: string;
  tw,nl: integer;
begin
  m := '';
  tw := 0;
  repeat
    d := GetNextLine(s, true);

    nl := Canvas.TextWidth(d);

    if nl > tw then
    begin
      m := d;
      tw := nl;
    end;

  until d = '';

  Result := m;
end;

function GetNextLine(var s:string; multiline:boolean):string;
var
  vp: Integer;
begin
  if VarPos(LINEFEED,s,vp) > 0 then
  begin
    Result := Copy(s,1,vp-1);
    Delete(s,1,vp);
    if s<>'' then
      if s[1] = #10 then
        Delete(s,1,1);
    if not Multiline then
      s := '';
  end
  else
  begin
    Result := s;
    s := '';
  end;
end;

function LinesInText(s:string;multiline:boolean):Integer;
var
  vp: Integer;
begin
  Result := 1;
  if not Multiline then Exit;

  while VarPos(LINEFEED,s,vp) > 0 do
  begin
    Delete(s,1,vp + 1);
    if s <> '' then
      Inc(Result);
  end;
end;

function RectString(r:trect):string;
begin
  Result := '['+inttostr(r.left)+':'+inttostr(r.top)+']['+inttostr(r.right)+':'+inttostr(r.left)+']';
end;

function FixDecimalSeparator(s:string):string;
var
  vp: Integer;
begin
  if Decimalseparator = ',' then
    if VarPos(',',s,vp)>0 then
      s[vp]:='.';

  Result := s;
end;

function GetNextDate(d:TDateTime;dye,dmo,dda:word;dtv:TDateTime):TDateTime;
var
  ye,mo,da:word;
begin
  decodedate(d,ye,mo,da);
  if (dmo=0) and (dye=0) and (dda<>0) then {equal month + equal year}
  begin
    Result:=d+dda;
  end
  else
  if (dmo=0) and (dye<>0) and (dda=0) then
  begin
    Result:=encodedate(ye+dye,mo,da);
  end
  else
  if (dmo<>0) and (dye=0) and (dda=0) then
  begin
    mo:=mo+dmo;
    if (mo<=0) then
    begin
      mo:=mo+12;
      dec(ye);
    end;
    if (mo>12) then
    begin
      mo:=mo-12;
      inc(ye);
    end;
    Result:=encodedate(ye,mo,da);
  end
  else
    Result := d + dtv;
end;

procedure DrawBitmapResourceTransp(Canvas: TCanvas; bkColor: TColor; r: TRect; ResName:string);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.LoadFromResourceName(hinstance,ResName);
    DrawBitmapTransp(Canvas,bmp,bkColor,r);
  finally
    bmp.Free;
  end;
end;

procedure DrawBitmapTransp(Canvas:TCanvas;bmp:TBitmap;bkcolor:TColor;r:TRect);
var
  tmpbmp: TBitmap;
  srcColor: TColor;
  tgtrect: TRect;
begin
  TmpBmp := TBitmap.Create;
  TmpBmp.Height := bmp.Height;
  TmpBmp.Width := bmp.Width;
  tgtrect.left :=0;
  tgtrect.top :=0;
  tgtrect.right := bmp.width;
  tgtrect.bottom := bmp.Height;
  r.bottom := r.top + bmp.height;
  r.Right := r.Left + bmp.width;
  TmpBmp.Canvas.Brush.Color := bkcolor;
  srcColor := bmp.canvas.pixels[0,0];
  TmpBmp.Canvas.BrushCopy(tgtrect,bmp,tgtrect,srcColor);
  Canvas.CopyRect(r, TmpBmp.Canvas, tgtrect);
  TmpBmp.Free;
end;

function SinglePos(p:char;s:string;var sp: Integer):Integer;
var
  i: Integer;
  QuoteCount: Integer;
begin
  i := 1;
  QuoteCount:= 0;
  while i <= Length(s) do
  begin
    if s[i] = p then
    begin
      if i < Length(s) then
        Inc(QuoteCount)
      else
        if i = Length(s) then
        begin
          Result := i;
          sp := i;
          Exit;
        end;
    end
    else
    begin
      if (Odd(QuoteCount)) then
      begin
        Result := i - 1;
        sp := i - 1;
        Exit;
      end
      else
        QuoteCount := 0;
    end;
    Inc(i);
  end;
  Result := 0;
  sp := 0;
end;

function NumSingleChar(p:char;s:string):Integer;
var
  Res,sp: Integer;
begin
  Res := 0;
  while SinglePos(p,s,sp) > 0 do
  begin
    Delete(s,1,sp);
    Inc(Res);
  end;
  Result := Res;
end;

function NumCharDel(p, del:char; s:string):Integer;
var
  Res, vp: Integer;
  valid: boolean;
begin
  Res := 0;

  while VarPos(p,s,vp) > 0 do
  begin
    valid := true;

    {
    if (vp = 1) and (vp < Length(s)) then
    begin
      valid := (s[vp + 1] = del);
    end;

    if (vp > 1) and (del <> #0) and (vp < Length(s)) then
    begin
      valid := (s[vp - 1] = del) or (s[vp + 1] = del);
    end;
    }
    Delete(s,1,vp);

    if valid then
      Inc(Res);
  end;
  Result := Res;
end;


function NumChar(p:char; s:string):Integer;
var
  Res,vp: Integer;
begin
  Res := 0;
  while VarPos(p,s,vp) > 0 do
  begin
    Delete(s,1,vp);
    Inc(Res);
  end;
  Result := Res;
end;

function NumCharInStr(p:char; s:string):Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i] = p then
      Result := Result + 1;
end;

function LineFeedsToXLS(s:string):string;
var
  Res: string;
  i: Integer;
begin
  Res := '';
  for i := 1 to Length(s) do
    if s[i] <> #13 then
      Res := Res + s[i];
  Result := res;
end;

function CRToLF(s:string):string;
var
  i: integer;
  res:string;
begin
  res := '';

  for i := 1 to length(s) do
    if (s[i] <> #13) then
      res := res + s[i]
    else
      res := res + #10;

  Result := res;
end;

function StripCRLF(s:string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to length(s) do
  begin
    if (s[i] <> #13) and (s[i] <> #10) then
      Result := Result + s[i];
  end;
end;

function CheckTerminator(ch: char): boolean;
const
  Terminators = [' ',',','.','-',''''];
begin
  {$IFNDEF DELPHI_UNICODE}
  Result := ch in Terminators;
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  Result := (ch = ' ') or (ch = ',') or (ch = '.') or (ch = '-') or (ch = '''');
  {$ENDIF}
end;

function ShiftCase(Name: string): string;

{$IFNDEF DELPHI_UNICODE}
 function LowCase(C: char): char;
 begin
  if C in ['A' .. 'Z'] then LowCase := Chr(Ord(C) - Ord('A') + Ord('a'))
  else Lowcase := C;
 end;
{$ENDIF}

var
  I, L: Integer;
  NewName: string;
  First: Boolean;
begin
  First := True;
  NewName := Name;
  L := Length(Name);

  for I := 1 to L do
  begin
    if CheckTerminator(NewName[I]) then
      First:= True
    else
      if First then
      begin
        {$IFNDEF DELPHI_UNICODE}
        NewName[I] := Upcase(Name[I]);
        {$ENDIF}
        {$IFDEF DELPHI_UNICODE}
        {$IFDEF DELPHIXE4_LVL}
        NewName[I] := Name[I].ToUpper;
        {$ENDIF}
        {$IFNDEF DELPHIXE4_LVL}
        NewName[I] := Character.ToUpper(Name[I]);
        {$ENDIF}
        {$ENDIF}
        First := False;
      end
      else
      {$IFNDEF DELPHI_UNICODE}
        NewName[I] := Lowcase(Name[I]);
      {$ENDIF}
      {$IFDEF DELPHI_UNICODE}
        {$IFDEF DELPHIXE4_LVL}
        NewName[I] := Name[I].ToLower;
        {$ENDIF}
        {$IFNDEF DELPHIXE4_LVL}
        NewName[I] := Character.ToLower(Name[I]);
        {$ENDIF}
      {$ENDIF}

    if (Copy(NewName, 1, I) = 'Mc') or
       ((Pos (' Mc', NewName) = I - 2) and (I > 2)) or
       ((I > L - 3) and ((Copy(NewName, I - 1, 2) = ' I') or
       (Copy(NewName, I - 2, 3) = ' II'))) then
       First:= True;
  end;

  Result := NewName;
end;


function StrToShortdateUS(s:string):TDateTime;
var
  Da,Mo,Ye,i: Word;
  Code: Integer;
  su: string;
begin
  Result := 0;

  i := Pos('/',s);
  if i = 0 then i := Pos('.',s);
  if i = 0 then i := Pos('-',s);

  if i > 0 then
  begin
    su := s[i];
    Val(Copy(s,1,i - 1),mo,Code);
    if Code <> 0 then Exit;
  end
  else
    Exit;

  Delete(s,1,i);

  i := pos(su,s);

  if i > 0 then
  begin
    Val(copy(s,1,i-1),Da,Code);
    if Code <> 0 then Exit;
  end
  else
    Exit;

  Delete(s,1,i);
  Val(s,ye,Code);
  if Code <> 0 then Exit;

  if ye <= 25 then
    ye := ye + 2000
  else
    ye := ye + 1900;

  Result := EncodeDate(ye,mo,da);
end;

function StrToShortDateEU(s:string):TDateTime;
var
  Da,Mo,Ye,i: Word;
  Code: Integer;
  su : string;

begin
  Result := 0;

  i := Pos('/',s);
  if i = 0 then i := Pos('.',s);
  if i = 0 then i := Pos('-',s);

  if i > 0 then
  begin
    su := s[i];
    Val(Copy(s,1,i-1),Da,Code);
    if Code <> 0 then Exit;
  end
  else
    Exit;

  Delete(s,1,i);
  i := Pos(su,s);

  if i > 0 then
  begin
    Val(Copy(s,1,i - 1),Mo,Code);
    if Code <> 0 then Exit;
  end
  else
    Exit;

  Delete(s,1,i);
  Val(s,ye,code);
  if Code <> 0 then Exit;

  if ye <= 25 then
    ye := ye + 2000
  else
    ye := ye + 1900;

  Result := Encodedate(ye,mo,da);
end;

procedure DrawErrorLines(Parent:TWinControl;Canvas: TCanvas; TmpStr:string; Rect:TRect; Height,ErrPos,ErrLen: Integer);
var
  Edit: TEdit;
  pt1: TPoint;
  pt2: TPoint;
  l: Integer;
  o: Integer;
  ep: Integer;
begin
  Edit := TEdit.Create(Parent);
  Edit.Visible := false;
  Edit.Parent := Parent;

  Edit.Top := Rect.Top;
  Edit.Left := Rect.Left;
  Edit.Width := Rect.Right - Rect.Left;
  Edit.Height := Rect.Bottom - Rect.Top;

  Edit.Text := TmpStr;

  if ErrPos >= Length(TmpStr) then
  begin
    ep := Length(TmpStr);
    l := SendMessage(Edit.Handle,EM_POSFROMCHAR,ep,0);
    pt1 := Point(LoWord(l),HiWord(l));
    pt1.X := pt1.X + 4;
  end
  else
  begin
    l := SendMessage(Edit.Handle,EM_POSFROMCHAR,ErrPos,0);
    pt1 := Point(LoWord(l),HiWord(l));
  end;

  if ErrPos + ErrLen >= Length(TmpStr) then
  begin
    ep := Length(TmpStr) - 1;
    l := SendMessage(Edit.Handle,EM_POSFROMCHAR,ep ,0);
    pt2 := Point(LoWord(l),HiWord(l));
    pt2.X := pt2.X + 4;
    pt2.Y := pt1.Y;
  end
  else
  begin
    l := SendMessage(Edit.Handle,EM_POSFROMCHAR,ErrPos + ErrLen - 1 ,0);
    pt2 := Point(LoWord(l),HiWord(l));
  end;

  Edit.Free;

  Canvas.Pen.Color := clRed;
  Canvas.Pen.Width := 2;

  l := pt1.X;
  o := 3;

  Canvas.MoveTo(Rect.Left + l,Rect.Top + pt1.Y + Height + o);

  while l <= pt2.X do
  begin
    if o = 3 then o := 0 else o := 3;
    Canvas.LineTo(Rect.Left + l + 3,Rect.Top + pt2.Y + Height + o);
    Inc(l,3);
  end;

  if o = 3 then o := 0 else o := 3;
  Canvas.LineTo(Rect.Left + l + 3,Rect.Top + pt2.Y + Height + o);
end;

procedure DrawRangeIndicator(Canvas: TCanvas; r: TRect; Value, Range: Integer; ShowValue: boolean; NegColor, PosColor: TColor);
var
  pw,mid: integer;
  txt: string;
begin
  InflateRect(r,-2,-2);

  mid := (R.Right - R.Left) div 2;

  if Value > Range then
    Value := Range;
  if Value < -Range then
    Value := -Range;

  pw := Round(Value * mid / Range);

  if Value < 0 then
  begin
    Canvas.Brush.Color := NegColor;
    Canvas.Pen.Color := NegColor;
    Canvas.Rectangle(r.Left + mid + pw, r.Top, r.Left + mid, r.Bottom);
  end
  else
  begin
    Canvas.Brush.Color := PosColor;
    Canvas.Pen.Color := PosColor;
    Canvas.Rectangle(r.Left + mid, r.Top, r.Left + mid + pw, r.Bottom);
  end;

  if ShowValue then
  begin
    SetBKMode(Canvas.Handle,Transparent);
    txt := inttostr(value);
    DrawText(Canvas.Handle, PChar(txt), length(txt), r, DT_CENTER);
  end;
end;


procedure DrawProgressLin(Canvas: TCanvas; r: TRect; Color1,TColor1,Color2,TColor2: TColor; pd: double;Mx,My, Min, Max: Integer; Fmt:string; BorderColor: TColor; print: boolean);
var
  SrcColor: TColor;
  SrcRect: TRect;
  TgtRect: TRect;
  txt: string;
  v: integer;
  dy: integer;
  p,fm: integer;
  isint: boolean;
  ch: char;
begin
  SrcColor := Canvas.Brush.Color;

  Canvas.Brush.Color := Color1;
  Canvas.Pen.Color := Color1;
  Canvas.Font.Color := TColor1;

  p := round(pd);

  if Fmt = '' then
  begin
    txt := IntToStr(p)+'%';
    v := round(p); // value supposed to be between 0..100
    //v := round((p - Min) *100 / (Max - Min));
  end
  else
  begin
    isint := false;
    fm := pos('d',fmt);
    if (fm > 0) then
    begin
      ch := fmt[fm - 1];
      {$IFDEF DELPHI_UNICODE}
      isint := CharInSet(ch, ['0'..'9','.','*','%']);
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      isint := (ch in ['0'..'9','.','*','%']);
      {$ENDIF}
    end;
    if not isint then
    begin
      fm := pos('u',fmt);
      if (fm > 0) then
      begin
        ch := fmt[fm - 1];
        {$IFDEF DELPHI_UNICODE}
        isint := CharInSet(ch, ['0'..'9','.','*','%']);
        {$ENDIF}
        {$IFNDEF DELPHI_UNICODE}
        isint := (ch in ['0'..'9','.','*','%']);
        {$ENDIF}
      end;
    end;

    if isint then
      txt := Format(fmt,[p])
    else
      txt := Format(fmt,[pd]);
      
    // percentage value
    v := round((pd - Min) *100 / (Max - Min));
  end;

  // limit to 0..100%
  if v > 100 then
    v := 100;
  if v < 0 then
    v := 0;

  Inflaterect(r,-Mx,-My);
  SrcRect := r;
  SrcRect.Right := SrcRect.Left + Round( (SrcRect.Right - SrcRect.Left)*(v)/100);
  TgtRect.Left := r.Left + (((r.Right-r.Left) - Canvas.TextWidth(txt)) div 2);

  dy := 1;
  if print then
    dy := -1;

  TgtRect.Top := r.Top + (((r.Bottom-r.Top) - (dy * Canvas.TextHeight(txt))) div 2);

  Canvas.TextRect(SrcRect,TgtRect.Left,TgtRect.Top,txt);

  Canvas.Brush.Color := Color2;
  Canvas.Pen.Color := Color2;

  Canvas.Font.Color := TColor2;

  SrcRect.Left := SrcRect.Right;
  SrcRect.Right := r.Right;

  Canvas.TextRect(SrcRect,TgtRect.Left,TgtRect.Top,txt);

  Canvas.Brush.Color := BorderColor;
  Canvas.Pen.Color := SrcColor;
  Inflaterect(r,1,1);

  if BorderColor <> clNone then
    Canvas.FrameRect(r);

  Canvas.Brush.Color := SrcColor;
  Inflaterect(r,1,1);
  Canvas.FrameRect(r);
end;

procedure DrawProgressPie(Canvas: TCanvas; r: TRect; Color: TColor; p: Integer; print: boolean);
var
  x,y: Integer;
  dy: Integer;
begin
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Width := 1;
  Canvas.Ellipse(r.Left,r.Top,r.Right,r.Bottom);
  Canvas.MoveTo(r.Left + (r.Right - r.Left) div 2,r.Top);
  Canvas.LineTo(r.Left + (r.Right - r.Left) div 2,r.Top + (r.Bottom - r.Top) div 2);

  x := round(0.5 * succ(r.Right - r.Left) * sin( p/100*2*PI ));
  y := round(0.5 * succ(r.Bottom - r.Top) * cos( p/100*2*PI ));

  Canvas.LineTo( r.Left + x + (r.Right - r.Left) div 2,r.Top - y + (r.Bottom - r.Top) div 2);

  Canvas.Brush.Color := Color;
  if print then
    dy := -2
  else
    dy := 2;

  if p <> 0 then
    Canvas.FloodFill(r.Left + 1 + (r.Right - r.Left) div 2,r.Top + dy,clGray,fsBorder);
end;

procedure DrawGradient(Canvas: TCanvas; FromColor,ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  startr,endr: Integer;
  startg,endg: Integer;
  startb,endb: Integer;
  rstepr,rstepg,rstepb,rstepw: Real;
  i,stepw: Word;
  dist: Integer;

begin
  if (R.Bottom = R.Top) then
    Exit;

  if (R.Bottom < R.Top) then
  begin
    dist := R.Top;
    R.Top := R.Bottom;
    R.Bottom := dist;
  end;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  // avoid all calculations when gradient end color is clNone
  if (ToColor = clNone) or (FromColor = clNone) then
  begin
    if (ToColor <> clNone) and (FromColor = clNone) then
    begin
      Canvas.Brush.Color := ToColor;
      Canvas.Pen.Color := ToColor;
    end
    else
    begin
      Canvas.Brush.Color := FromColor;
      Canvas.Pen.Color := FromColor;
    end;
    Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
    Exit;
  end;

  if Direction then
    dist := (R.Right - R.Left - 1)
  else
    dist := (R.Bottom - R.Top - 1);

  if (dist < Steps) then
    Steps := dist;

  // avoid division by zero
  if (Steps <= 0) then
    Steps := 1;

  rstepw := dist / Steps;

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;

  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  rstepr := (endr - startr) / Steps;
  rstepg := (endg - startg) / Steps;
  rstepb := (endb - startb) / Steps;

  for i := 0 to Steps do
  begin
    endr := startr + Round(rstepr * i);
    endg := startg + Round(rstepg * i);
    endb := startb + Round(rstepb * i);
    stepw := Trunc(i * rstepw);

    FromColor := endr OR (endg shl 8) OR (endb shl 16);

    Canvas.Pen.Color := FromColor;
    Canvas.Brush.Color := FromColor;

    if Direction then
      Canvas.Rectangle(R.Left + stepw,R.Top,Min(R.Right, R.Left + stepw + Trunc(1 + rstepw)),R.Bottom)
    else
      Canvas.Rectangle(R.Left,R.Top + stepw,R.Right,Min(R.Bottom, R.Top + stepw + Trunc(1 + rstepw)));
  end;
end;

procedure DrawVistaGradient(ACanvas: TCanvas; ARect: TRect; ColorFrom, ColorTo, ColorMirrorFrom, ColorMirrorTo: TColor;
  Direction: boolean; BorderColor: TColor; LeftRightBorder: boolean = false; Fill: Boolean = True);
var
  r: Trect;
  mid: integer;
begin
  if Fill and (ColorFrom <> clNone) then
  begin
    if ColorMirrorFrom <> clNone then
    begin
      r := ARect;
      if not Direction then
      begin
        mid := r.Left + ((r.Right - r.Left) div 2);
        r.Right := mid;
        DrawGradient(ACanvas,  ColorFrom, ColorTo, 128, r, true);
        r := ARect;
        r.Left := mid;
        DrawGradient(ACanvas,  ColorMirrorFrom, ColorMirrorTo, 128, r, true);
      end
      else
      begin
        mid := r.Top + ((r.Bottom - r.Top) div 2);
        r.Bottom := mid;
        DrawGradient(ACanvas,  ColorFrom, ColorTo, 128, r, false);
        r := ARect;
        r.Top := mid;
        DrawGradient(ACanvas,  ColorMirrorFrom, ColorMirrorTo, 128, r, false);
      end;
    end
    else
    begin
      if ColorTo = clNone then
      begin
        ACanvas.Brush.Color := ColorFrom;
        ACanvas.Pen.Color := ColorFrom;
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := psSolid;
        ARect.Bottom := ARect.Bottom + 1;
        ACanvas.FillRect(ARect);
      end
      else
        DrawGradient(ACanvas, ColorFrom, ColorTo, 128, ARect, not Direction);
    end;
  end;

  if (BorderColor <> clNone) then
  begin
    if LeftRightBorder then
    begin
      ACanvas.Pen.Color := BorderColor;
      ACanvas.MoveTo(ARect.Left, ARect.Top);
      ACanvas.LineTo(ARect.Left, ARect.Bottom);
      ACanvas.MoveTo(ARect.Right - 1, ARect.Top);
      ACanvas.LineTo(ARect.Right - 1, ARect.Bottom);
    end
    else
    begin
      ACanvas.Brush.Color := BorderColor;
      ACanvas.FrameRect(ARect);
    end;
  end;
end;


function GradientAt(FromColor, ToColor: TColor; Left, Right, Pos: integer): TColor;
var
  diffr,startr,endr: Integer;
  diffg,startg,endg: Integer;
  diffb,startb,endb: Integer;

begin
  if ToColor = clNone then
  begin
    Result := FromColor;
    Exit;
  end;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := startr + Round((endr - startr) / (Right - Left) * Pos);
  diffg := startg + Round((endg - startg) / (Right - Left) * Pos);
  diffb := startb + Round((endb - startb) / (Right - Left) * Pos);

  Result := diffr OR (diffg shl 8) OR (diffb shl 16);
end;

procedure DrawComboButton(Canvas: TCanvas;Handle: THandle; ARect: TRect; AControlStyle: TControlDrawStyle; AState: TControlState; ATones: TColorTones);
var
  htheme: THandle;
  DWStyle: DWORD;
  pt: Array of TPoint;
begin
  ARect.Left := ARect.Right - 17;

  InflateRect(ARect, 0, -1);
  OffsetRect(ARect,-2,0);

  DWStyle := 0;

  case AControlStyle of
  cdsXPTheme:
    begin
      if AState = csDisabled then
        DWStyle := CBXS_DISABLED
      else
        DWStyle := CBXS_NORMAL;

      htheme := OpenThemeData(Handle,'combobox');
      DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,DWStyle,@ARect,nil);
      CloseThemeData(htheme);
    end;
  cdsClassic:
    begin
      if AState = csDisabled then
        DWStyle := DFCS_Inactive;
      DrawFrameControl(Canvas.Handle, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DWStyle);
    end;
  cdsFlat:
    begin
      if AState = csDisabled then
        DWStyle := DFCS_Inactive;
      DrawFrameControl(Canvas.Handle, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT or DWStyle);
    end;
  cdsMetro:
    begin
      ARect.Left := ARect.Left + 3;
      case AState of
      csNormal:
        begin
          Canvas.Brush.Color := ATones.Background.BrushColor;
        end;
      csHover:
        begin
          Canvas.Brush.Color := ATones.Hover.BrushColor;
        end;
      csDown:
        begin
          Canvas.Brush.Color := ATones.Selected.BrushColor;
        end;
      csDisabled:
        begin
          Canvas.Brush.Color := ATones.Disabled.BrushColor;
        end;
      end;

      Canvas.Pen.Color := Canvas.Brush.Color;

      Canvas.Rectangle(ARect);

      Canvas.Pen.Color := ATones.Background.BorderColor;
      Canvas.MoveTo(ARect.Left + 1, ARect.Top);
      Canvas.LineTo(ARect.Left + 1, ARect.Bottom);

      Canvas.Brush.Color := ATones.Background.TextColor;

      Canvas.Pen.Color := Canvas.Brush.Color;

      SetLength(pt, 3);

      pt[0].X := ARect.Left + 5;
      pt[0].y := ARect.Top + 8;

      pt[1].X := ARect.Left + 11;
      pt[1].y := ARect.Top + 8;

      pt[2].X := ARect.Left + 8;
      pt[2].y := ARect.Top + 11;

      Canvas.Polygon(pt);

    end;
  end
end;

procedure DrawButton(Canvas: TCanvas; AHandle: THandle; ARect: TRect; AControlStyle: TControlDrawStyle; AState: TControlState; ATones: TColorTones; Prefix: boolean; ACaption: string = '');
var
  HTheme: THandle;
  DrawStyle : DWord;
  TextStyle: DWord;
begin
  case AControlStyle of
  cdsXPTheme:
    begin
      DrawStyle := PBS_NORMAL;
      HTheme := OpenThemeData(AHandle,'button');
      case AState of
      csHover: DrawStyle := PBS_HOT;
      csDown: DrawStyle := PBS_PRESSED;
      csDisabled: DrawStyle := PBS_DISABLED;
      end;
      DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON, DrawStyle ,@ARect,nil);
      CloseThemeData(HTheme);
    end;
  cdsClassic:
    begin
      DrawStyle := DFCS_BUTTONPUSH;
      case AState of
      csHover: DrawStyle := DrawStyle or DFCS_HOT;
      csDown: DrawStyle := DrawStyle or DFCS_PUSHED;
      csDisabled: DrawStyle := DrawStyle or DFCS_INACTIVE;
      end;
      DrawFrameControl(Canvas.Handle,ARect,DFC_BUTTON, DrawStyle);
    end;
  cdsFlat:
    begin
      DrawStyle := DFCS_BUTTONPUSH or DFCS_FLAT;
      case AState of
      csHover: DrawStyle := DrawStyle or DFCS_HOT;
      csDown: DrawStyle := DrawStyle or DFCS_PUSHED;
      csDisabled: DrawStyle := DrawStyle or DFCS_INACTIVE;
      end;
      DrawFrameControl(Canvas.Handle,ARect,DFC_BUTTON, DrawStyle);
    end;
  cdsMetro:
    begin
      case AState of
      csNormal:
        begin
          Canvas.Brush.Color := ATones.Background.BrushColor;
          Canvas.Pen.Color := ATones.Background.BorderColor;
        end;
      csHover:
        begin
          Canvas.Brush.Color := ATones.Hover.BrushColor;
          Canvas.Pen.Color := ATones.Hover.BorderColor;
        end;
      csDown:
        begin
          Canvas.Brush.Color := ATones.Selected.BrushColor;
          Canvas.Pen.Color := ATones.Selected.BorderColor;
        end;
      csDisabled:
        begin
          Canvas.Brush.Color := ATones.Disabled.BrushColor;
          Canvas.Pen.Color := ATones.Disabled.BorderColor;
        end;
      end;

      Canvas.Rectangle(ARect);
    end;
  end;

  if (ACaption <> '') then
  begin
    Canvas.Brush.Style := bsClear;
    ARect.Top := ARect.Top + 4;
    TextStyle := 0;
    if not Prefix then
      TextStyle := DT_NOPREFIX;

    DrawText(Canvas.Handle, PChar(ACaption), Length(ACaption), ARect, DT_CENTER or TextStyle or DT_VCENTER or DT_SINGLELINE);
  end;

end;

procedure DrawSpinButtons(Canvas: TCanvas;Handle: THandle; ARect: TRect;  AControlStyle: TControlDrawStyle; AState: TControlState; ATones: TColorTones);
var
  htheme: THandle;
  DRect: TRect;
  mid: integer;
  DRWFLAG: DWORD;
  pt: array of TPoint;
begin
  ARect.Left := ARect.Right - 15;
  InflateRect(ARect, 0, 0);
  OffsetRect(ARect,-1,-1);
  DRect := ARect;

  mid := ARect.Top + ((ARect.Bottom - ARect.Top) div 2);

  case AControlStyle of
  cdsXPTheme:
    begin
      DRWFLAG := UPS_NORMAL;
      if AState = csDisabled then
        DRWFLAG := UPS_DISABLED;

      htheme := OpenThemeData(Handle,'spin');
      DRect.Bottom := mid;

      DrawThemeBackground(htheme,Canvas.Handle,SPNP_UP,DRWFLAG,@DRect,nil);

      DRect.Top := DRect.Bottom;
      DRect.Bottom := ARect.Bottom;

      DrawThemeBackground(htheme,Canvas.Handle,SPNP_DOWN,DRWFLAG,@DRect,nil);

      CloseThemeData(htheme);
    end;
  cdsClassic:
    begin
      DRect.Bottom := mid;

      DRWFLAG := DFCS_SCROLLUP;
      if AState = csDisabled then
        DRWFLAG := DRWFLAG OR DFCS_INACTIVE;

      DrawFrameControl(Canvas.Handle, DRect, DFC_SCROLL, DRWFLAG);

      DRect := ARect;
      DRect.Top := mid + 1;

      DRWFLAG := DFCS_SCROLLDOWN;

      if AState = csDisabled then
        DRWFLAG := DRWFLAG OR DFCS_INACTIVE;

      DrawFrameControl(Canvas.Handle, DRect, DFC_SCROLL, DRWFLAG);
    end;
  cdsFlat:
    begin
      DRect.Bottom := mid;

      DRWFLAG := DFCS_SCROLLUP OR DFCS_FLAT;
      if AState = csDisabled then
        DRWFLAG := DRWFLAG OR DFCS_INACTIVE;

      DrawFrameControl(Canvas.Handle, DRect, DFC_SCROLL, DRWFLAG);

      DRect := ARect;
      DRect.Top := mid + 1;

      DRWFLAG := DFCS_SCROLLDOWN OR DFCS_FLAT;

      if AState = csDisabled then
        DRWFLAG := DRWFLAG OR DFCS_INACTIVE;

      DrawFrameControl(Canvas.Handle, DRect, DFC_SCROLL, DRWFLAG);
    end;
  cdsMetro:
    begin
      case AState of
      csNormal:
        begin
          Canvas.Brush.Color := ATones.Background.BrushColor;
          Canvas.Pen.Color := ATones.Background.BorderColor;
        end;
      csHover:
        begin
          Canvas.Brush.Color := ATones.Hover.BrushColor;
          Canvas.Pen.Color := ATones.Hover.BorderColor;
        end;
      csDown:
        begin
          Canvas.Brush.Color := ATones.Selected.BrushColor;
          Canvas.Pen.Color := ATones.Selected.BorderColor;
        end;
      csDisabled:
        begin
          Canvas.Brush.Color := ATones.Disabled.BrushColor;
          Canvas.Pen.Color := ATones.Disabled.BorderColor;
        end;
      end;

      Canvas.Pen.Color := Canvas.Brush.Color;

      SetLength(pt,3);

      DRect.Bottom := mid;

      Canvas.Rectangle(DRect);

      DRect := ARect;
      DRect.Top := mid + 1;

      Canvas.Rectangle(DRect);

      DRect := ARect;
      DRect.Bottom := mid;

      pt[0].X := DRect.Left + 5;
      pt[0].Y := DRect.Top + 5;
      pt[1].X := DRect.Left + 9;
      pt[1].Y := DRect.Top + 5;
      pt[2].X := DRect.Left + 7;
      pt[2].Y := DRect.Top + 3;

      Canvas.Brush.Color := clBlack;
      Canvas.Pen.Color := Canvas.Brush.Color;
      Canvas.Polygon(pt);

      DRect := ARect;
      DRect.Top := mid + 1;

      pt[0].X := DRect.Left + 5;
      pt[0].Y := DRect.Top + 1;
      pt[1].X := DRect.Left + 9;
      pt[1].Y := DRect.Top + 1;
      pt[2].X := DRect.Left + 7;
      pt[2].Y := DRect.Top + 3;

      Canvas.Brush.Color := clBlack;
      Canvas.Pen.Color := Canvas.Brush.Color;
      Canvas.Polygon(pt);
    end;
  end;
end;


function DecodeWideStr(s:string): widestring;
var
  wc: widechar;
  wsi: Integer;

begin
  Result := '';
  if pos('|\',s) = 1 then
  begin
    delete(s,1,2);
    for wsi := 1 to length(s) div 2 do
    begin
      wc := widechar(smallint(ord(s[wsi * 2]) + 256 * ord(s[wsi * 2 - 1])));
      Result := Result + wc;
    end;
  end
  else
    Result := s;
end;

function EncodeWideStr(s:widestring): string;
var
  k: Integer;
  wc: widechar;
  d: string;
begin
  d := '|\'; // unicode start marker

  for k := 1 to length(s) do
  begin
    wc := s[k];
    d := d + chr(((smallint(wc) and $FF00) shr 8));
    d := d + chr(smallint(wc) and $FF);
  end;
  Result := d;
end;

{$IFDEF DELPHI_UNICODE}
function StrPosWide(SubStr, Str: widestring): integer;
begin
  Result := Pos(SubStr, Str);
end;
{$ENDIF}

{$IFNDEF DELPHI_UNICODE}
function StrPosW(Str, SubStr: PWideChar): PWideChar;
// returns a pointer to the first occurance of SubStr in Str
asm
         PUSH EDI
         PUSH ESI
         PUSH EBX
         OR EAX, EAX
         JZ @@2
         OR EDX, EDX
         JZ @@2
         MOV EBX, EAX
         MOV EDI, EDX
         XOR AX, AX
         MOV ECX, 0FFFFFFFFH
         REPNE SCASW
         NOT ECX
         DEC ECX
         JZ @@2
         MOV ESI, ECX
         MOV EDI, EBX
         MOV ECX, 0FFFFFFFFH
         REPNE SCASW
         NOT ECX
         SUB ECX, ESI
         JBE @@2
         MOV EDI, EBX
         LEA EBX, [ESI - 1] // Note: 2 would be wrong here, we are dealing with numbers not an address
@@1:     MOV ESI, EDX
         LODSW
         REPNE SCASW
         JNE @@2
         MOV EAX, ECX
         PUSH EDI
         MOV ECX, EBX
         REPE CMPSW
         POP EDI
         MOV ECX, EAX
         JNE @@1
         LEA EAX, [EDI - 2]
         JMP @@3

@@2:     XOR EAX, EAX
@@3:     POP EBX
         POP ESI
         POP EDI
end;

function StrPosWide(SubStr, Str: widestring): integer;
var
  wc: PWideChar;
begin
  wc := StrPosW(PWideChar(Str), PWideChar(SubStr));
  if (wc <> nil) then
    Result := 1 + (integer(wc) - integer(PWideChar(Str))) div 2
  else
    Result := 0;
end;
{$ENDIF}

function IsDateStr(s:string): Boolean;
var
  vp: integer;
begin
  Result := false;
  if VarPos(DateSeparator, s,vp) > 1 then
  begin
    delete(s,1,vp);
    Result := Pos(DateSeparator,s) > 1;
  end;
end;

procedure MakeFragment(var HTML: string);
// Helper routine to build a properly-formatted HTML fragment.
const
  Version = 'Version:1.0'#13#10;
  StartHTML = 'StartHTML:';
  EndHTML = 'EndHTML:';
  StartFragment = 'StartFragment:';
  EndFragment = 'EndFragment:';
  DocType = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">';
  HTMLIntro = '<html><head><META http-equiv=Content-Type content="text/html; charset=iso-8859-1">' +
    '</head><body><!--StartFragment-->';
  HTMLExtro = '<!--EndFragment--></body></html>';
  NumberLengthAndCR = 10;

  // Let the compiler determine the description length.
  DescriptionLength = Length(Version) + Length(StartHTML) + Length(EndHTML) + Length(StartFragment) +
    Length(EndFragment) + 4 * NumberLengthAndCR;

var
  Description: string;
  StartHTMLIndex,
    EndHTMLIndex,
    StartFragmentIndex,
    EndFragmentIndex: Integer;

begin
  // The HTML clipboard format is defined by using byte positions in the entire block where HTML text and
  // fragments start and end. These positions are written in a description. Unfortunately the positions depend on the
  // length of the description but the description may change with varying positions.
  // To solve this dilemma the offsets are converted into fixed length strings which makes it possible to know
  // the description length in advance.
  StartHTMLIndex := DescriptionLength; // position 0 after the description
  StartFragmentIndex := StartHTMLIndex + Length(DocType) + Length(HTMLIntro);
  EndFragmentIndex := StartFragmentIndex + Length(HTML);
  EndHTMLIndex := EndFragmentIndex + Length(HTMLExtro);

  Description := Version +
    SysUtils.Format('%s%.8d', [StartHTML, StartHTMLIndex]) + #13#10 +
    SysUtils.Format('%s%.8d', [EndHTML, EndHTMLIndex]) + #13#10 +
    SysUtils.Format('%s%.8d', [StartFragment, StartFragmentIndex]) + #13#10 +
    SysUtils.Format('%s%.8d', [EndFragment, EndFragmentIndex]) + #13#10;
  HTML := Description + DocType + HTMLIntro + HTML + HTMLExtro;
end;

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
  Result := TRUE;
  UserDLL := GetModuleHandle('USER32.DLL');
  if (UserDLL > 0) then
  begin
    @user_UpdateLayeredWindow:=GetProcAddress(UserDLL,'UpdateLayeredWindow');
    if Assigned(user_UpdateLayeredWindow) then
    begin
      Result := user_UpdateLayeredWindow(hwnd,hdcDst,pptDst,size,hdcSrc,pptSrc,crKey,pblend,dwFlags)<>0;
    end;
  end;
end;


function DynaLink_SetLayeredWindowAttributes(HWND:thandle;crKey:DWORD;bAlpha:byte;dwFlags:DWORD):boolean;
var
  UserDLL: THandle;
  user_SetLayeredWindowAttributes:function(HWND:thandle;crKey:DWORD;bAlpha:byte;dwFlags:DWORD):DWORD; stdcall;

begin
  Result := TRUE;
  UserDLL := GetModuleHandle('USER32.DLL');
  if (UserDLL > 0) then
  begin
    @user_SetLayeredWindowAttributes := GetProcAddress(UserDLL,'SetLayeredWindowAttributes');
    if Assigned(user_SetLayeredWindowAttributes) then
    begin
      Result := user_SetLayeredWindowAttributes(hwnd,crKey,bAlpha,dwFlags)<>0;
    end;
  end;
end;


//---------------------------------------------------------------- DrawRectangle

procedure DrawRectangle(Canvas: TCanvas; R: TRect; aColor: TColor);
begin
  Canvas.Brush.Color := aColor;
  Canvas.FillRect(R);
end;

//----------------------------------------------------------------- DrawTriangle

procedure DrawTriangle(Canvas: TCanvas; X,Y: integer; Color: TColor);
var
  ar: Array[1..3] of TPoint;
begin
  ar[1] := point(x-3,y);
  ar[2] := point(x+3,y);
  ar[3] := point(x,y+3);

  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;

  Canvas.Polygon(ar);
end;

//-------------------------------------------------------------------- DrawGauge

procedure DrawGauge(Canvas: TCanvas; R: TRect; Position: Integer;
  Settings: TGaugeSettings);
var
  RectL: TRect;
  RectM: TRect;
  RectR: TRect;

  WidthBar: integer;
  WidthPart: Integer;
  Continue: Boolean;
  GradDir: Boolean;
  BrushColor: TColor;
  BrushColorTo: TColor;
  Percentage: Integer;
  BarFilled: Integer;
  NumberOfBlock: Integer;
  i: Integer;
  EmptyWidth: integer;

{$IFNDEF TMSCLX}
  lf: TLogFont;
{$ENDIF}
  tf: TFont;

  R1: TRect;
  R2: TRect;
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

  Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  
  WidthBar := WidthBar - 2;

  if (Position > 0) then
  begin
    if (Settings.Stacked) then
    begin
      if (Position >= Settings.Level1Perc) then
        WidthPart := Round((Settings.Level1Perc / 100) * WidthBar)
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
        DrawGradient(Canvas, Settings.Level0ColorTo,
          Settings.Level0Color, Settings.Steps, R1, GradDir);
        DrawGradient(Canvas, Settings.Level0Color,
          Settings.Level0ColorTo, Settings.Steps, R2, GradDir);
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
          WidthPart := Round(WidthBar * ((Settings.Level2Perc -
            Settings.Level1Perc) / 100))
        else
        begin
          WidthPart := Round(WidthBar * ((Position -
            Settings.Level1Perc) / 100));
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
          DrawGradient(Canvas, Settings.Level1ColorTo,
            Settings.Level1Color, Settings.Steps, R1, GradDir);
          DrawGradient(Canvas,
            Settings.Level1Color, Settings.Level1ColorTo,
            Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectM, Settings.Level1Color);

        BarFilled := BarFilled + WidthPart;
        if (Continue) then
        begin
          //Draw third part
          if (Position = 100) then
            WidthPart := Round(WidthBar - BarFilled)
          else
            WidthPart := Round(WidthBar * ((Position -
              Settings.Level2Perc) / 100));

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
              R1.Right := RectR.Left + (RectR.Right - RectR.Left) div
                2;
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
              R1.Bottom := RectR.Top + (RectR.Bottom - RectR.Top) div
                2;
              R2.Top := R1.Bottom;
              R2.Left := RectR.Left;
              R2.Right := RectR.Right;
              R2.Bottom := RectR.Bottom;
            end;
            DrawGradient(Canvas, Settings.Level3ColorTo,
              Settings.Level3Color, Settings.Steps, R1, GradDir);
            DrawGradient(Canvas, Settings.Level3Color,
              Settings.Level3ColorTo, Settings.Steps, R2, GradDir);
          end
          else
            DrawRectangle(Canvas, RectR, Settings.Level3Color);
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

        if (Round((Position * WidthBar) / 100) > 9) then
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
              R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                2;
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
              R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                2;
              R2.Top := R1.Bottom;
              R2.Left := RectL.Left;
              R2.Right := RectL.Right;
              R2.Bottom := RectL.Bottom;
            end;
            DrawGradient(Canvas, BrushColorTo, BrushColor,
              Settings.Steps, R1, GradDir);
            DrawGradient(Canvas, BrushColor, BrushColorTo,
              Settings.Steps, R2, GradDir);
          end
          else
            DrawRectangle(Canvas, RectL, BrushColor);

          NumberOfBlock := (Round((Position * WidthBar) / 100) div 9) -
            1;
          EmptyWidth := Round((Position * WidthBar) / 100) mod 9;

          for i := 0 to NumberOfBlock - 1 do
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
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
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
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
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
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
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
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;
          Canvas.Brush.style := bsClear;
        end
        else
        begin
          if (Round((Position * WidthBar) / 100) > 1) then
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := RectL.Left + (Round((Position *
                WidthBar) / 100) - 1);
              RectL.Top := R.Top + 2;
              RectL.Bottom := R.Bottom - 2;
            end
            else
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := R.Right - 2;
              RectL.Bottom := R.Bottom - 2;
              RectL.Top := RectL.Bottom - (Round((Position *
                WidthBar) / 100) - 1);
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
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
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
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
          RectL.Top := R.Top + 1;
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
          DrawGradient(Canvas, BrushColorTo, BrushColor,
            Settings.Steps, R1, GradDir);
          DrawGradient(Canvas, BrushColor, BrushColorTo,
            Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectL, BrushColor);
      end;
    end;
  end;

  //Draw text with PositionPercentage
  if (Percentage <> -1) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Name := Settings.Font.Name;
    Canvas.Font.Size := Settings.Font.Size;
    Canvas.Font.Color := Settings.FontColor;
    Canvas.Font.Style := Settings.Font.Style;
    if not (Settings.Orientation = goHorizontal) then
    begin
      tf := TFont.Create;
      try
        tf.Assign(Settings.Font);

{$IFNDEF TMSCLX}

        GetObject(tf.Handle, sizeof(lf), @lf);

        lf.lfEscapement := 900;
        lf.lfOrientation := 900;
        tf.Handle := CreateFontIndirect(lf);
{$ENDIF}

        Canvas.Font.Assign(tf);
        Canvas.TextOut(R.Left + ((R.Right - R.Left) div 2 -
          (Canvas.TextHeight(IntToStr(Percentage) + '%') div 2)), R.Top +
          ((R.Bottom
          - R.Top) div 2) + Canvas.TextWidth(IntToStr(Percentage) + '%') div 2
          , IntToStr(Percentage) + '%');
      finally
        tf.Free;
      end;
    end
    else
    begin
      Canvas.TextOut(((R.Right - R.Left) div 2) -
        (Canvas.TextWidth(IntToStr(Percentage) + '%') div 2) + r.Left, r.Top +
        ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(IntToStr(Percentage) +
        '%') div 2, IntToStr(Percentage) + '%');
    end;
  end;

  if (Settings.ShowBorder) then
    Canvas.Pen.Color := Settings.BorderColor
  else
    Canvas.Pen.Color := Settings.BackgroundColor;

  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
end;

procedure DrawShape(Canvas: TCanvas; X,Y: integer; Width, Height: integer; Shape: TCellShape; FillColor:TColor; LineColor:TColor);
var
  szx,szy: integer;
  cntrx,cntry: integer;
  wx,wy: integer;
  wxs,wys: double;
begin
  Canvas.Brush.Color := FillColor;
  Canvas.Pen.Color := LineColor;

  szx := Width;
  szy := Height;

  if (Shape in [csCircle, csSquare, csRoundSquare]) then
  begin
    if szx <> szy then
    begin
      if szx > szy then
        szx := szy
      else
        szy := szx;
    end;
  end;

  case Shape of
  csRectangle,csSquare: Canvas.Rectangle(X + 1,Y + 1,X+szx - 1,Y+szy - 1);
  csCircle,csEllips: Canvas.Ellipse(X,Y,X+szx,Y+szy);
  csTriangleLeft: Canvas.Polygon([Point(X+szx,Y),Point(X+szx,Y+szy),Point(X,Y+szy div 2)]);
  csTriangleRight: Canvas.Polygon([Point(X,Y),Point(X,Y+szy),Point(X+szx,Y+szy div 2)]);
  csTriangleUp: Canvas.Polygon([Point(X,Y+szy),Point(X+szx,Y+szy),Point(X+szx div 2,Y)]);
  csTriangleDown: Canvas.Polygon([Point(X,Y),Point(X+szx,Y),Point(X+szx div 2,Y+szy)]);
  csDiamond:
    begin
      szx := szx div 2;
      szy := szy div 2;
      Canvas.Polygon([Point(X,Y+szy),Point(X + szx,Y),Point(X+ 2 * szx,Y + szy),Point(X + szx,Y+ 2 * szy)]);
    end;
  csLineVert: Canvas.Rectangle(X+szx div 2, Y, X+ szx div 2 + 1, Y + szy);
  csLineHorz: Canvas.Rectangle(X,Y+szy div 2,X+szx, Y+szy div 2 + 1);
  csRoundRect,csRoundSquare: Canvas.RoundRect(X,Y,X+szx,Y+szy,10,10);
  csHalfStar:
    begin
      wx := szx div 2;
      wy := szy div 2;
      wxs := wx / 3;
      wys := wy / 3;
      cntrx := X + wx;
      cntry := Y + wy;

      Canvas.Polygon([
                      Point(cntrx + round(wx * sin(0)),cntry - round(wy * cos(0))),

                      Point(cntrx + round(wxs  * sin(5 * 2*PI/10)),cntry - round(wys  * cos(5*2*PI/10))),

                      Point(cntrx + round(wx * sin(2*3*PI/5)),cntry - round(wy * cos(2*3*PI/5))),

                      Point(cntrx + round(wxs  * sin(7 * 2*PI/10)),cntry - round(wys  * cos(7*2*PI/10))),

                      Point(cntrx + round(wx * sin(2*4*PI/5)),cntry - round(wy * cos(2*4*PI/5))),

                      Point(cntrx + round(wxs  * sin(9 * 2*PI/10)),cntry - round(wys  * cos(9*2*PI/10)))
                     ]);
    end;
  csStar:
    begin
      wx := szx div 2;
      wy := szy div 2;
      wxs := wx / 3;
      wys := wy / 3;
      cntrx := X + wx;
      cntry := Y + wy;

      Canvas.Polygon([
                      Point(cntrx + round(wx * sin(0)),cntry - round(wy * cos(0))),

                      Point(cntrx + round(wxs  * sin(2*PI/10)),cntry - round(wys  * cos(2*PI/10))),

                      Point(cntrx + round(wx * sin(2*PI/5)),cntry - round(wy * cos(2*PI/5))),

                      Point(cntrx + round(wxs  * sin(3 * 2*PI/10)),cntry - round(wys  * cos(3*2*PI/10))),

                      Point(cntrx + round(wx * sin(2*2*PI/5)),cntry - round(wy * cos(2*2*PI/5))),

                      Point(cntrx + round(wxs  * sin(5 * 2*PI/10)),cntry - round(wys  * cos(5*2*PI/10))),

                      Point(cntrx + round(wx * sin(2*3*PI/5)),cntry - round(wy * cos(2*3*PI/5))),

                      Point(cntrx + round(wxs  * sin(7 * 2*PI/10)),cntry - round(wys  * cos(7*2*PI/10))),

                      Point(cntrx + round(wx * sin(2*4*PI/5)),cntry - round(wy * cos(2*4*PI/5))),

                      Point(cntrx + round(wxs  * sin(9 * 2*PI/10)),cntry - round(wys  * cos(9*2*PI/10)))
                     ]);

    end;
  csArrowUp:
    begin
      if odd(szx) then dec(szx);

      Canvas.Polygon([
                       Point(X,Y + szy div 2), Point(X + szx div 2, Y), Point(X + szx, Y + szy div 2),
                       Point(X + 3 * (szx div 4), Y + szy div 2),
                       Point(X + 3 * (szx div 4), Y + szy),
                       Point(X + (szx div 4), Y + szy),
                       Point(X + (szx div 4), Y + szy div 2)]);
    end;
  csArrowDown:
    begin
      if odd(szx) then dec(szx);
      Canvas.Polygon([
                       Point(X,Y + szy div 2), Point(X + szx div 2, Y + szy), Point(X + szx, Y + szy div 2),
                       Point(X + 3 * (szx div 4), Y + szy div 2),
                       Point(X + 3 * (szx div 4), Y),
                       Point(X + (szx div 4), Y),
                       Point(X + (szx div 4), Y + szy div 2)]);
    end;
  csArrowLeft:
    begin
      if odd(szy) then dec(szy);

      Canvas.Polygon([
                       Point(X,Y + szy div 2), Point(X + szx div 2, Y),
                       Point(X + szx div 2, Y + szy div 4),
                       Point(X + szx, Y + szy div 4),
                       Point(X + szx, Y + 3 * (szy div 4)),
                       Point(X + szx div 2, Y + 3 * (szy div 4)),
                       Point(X + szx div 2, Y + szy) ]);
    end;
  csArrowRight:
    begin
      if odd(szy) then dec(szy);

      Canvas.Polygon([
                       Point(X + szx,Y + szy div 2), Point(X + szx div 2, Y),
                       Point(X + szx div 2, Y + szy div 4),
                       Point(X, Y + szy div 4),
                       Point(X, Y + 3 * (szy div 4)),
                       Point(X + szx div 2, Y + 3 * (szy div 4)),
                       Point(X + szx div 2, Y + szy) ]);

    end;
  end;
end;

function MakeHTMLHyperlink(const Value, LinkType: string; var ValueWithLink: string): boolean;
var
  lstart,lend: integer;
  i: integer;
  link:string;
  HTMLValue: string;
begin
  Result := false;

  lstart := Pos(LinkType,Uppercase(Value));

  if lstart > 0 then
  begin
    HTMLValue := StringReplace(Value,'<','&lt;',[rfReplaceAll]);
    HTMLValue := StringReplace(HTMLValue,'>','&gt;',[rfReplaceAll]);
    ValueWithLink := HTMLValue;

    lstart := Pos(LinkType,Uppercase(HTMLValue));
    lend := length(HTMLValue);

    for i := lstart to lend do
    begin
      {$IFDEF DELPHI_UNICODE}
      if not CharInSet(HTMLValue[i],['a'..'z','A'..'Z','0'..'9','/','\','_','-',':','.','%','(',')','{','}','@'])  then
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      if not (HTMLValue[i] in ['a'..'z','A'..'Z','0'..'9','/','\','_','-',':','.','%','(',')','{','}','@'])  then
      {$ENDIF}
      begin
        lend := i - 1;
        break;
      end;
    end;

    link := Copy(HTMLValue, lstart,1 + lend - lstart); // this is the URL

    Insert('<a href="'+ link + '">',ValueWithLink, lstart);
    Insert('</a>', ValueWithLink, 11 + 2 * length(link) + lstart);
    Result := true;
  end;
end;


initialization

  ShlDLL := GetModuleHandle('shlwapi.dll');
  @FuncStrCmpLogicalW := nil;
  if (ShlDLL > 0) then
    @FuncStrCmpLogicalW := GetProcAddress(ShlDLL, 'StrCmpLogicalW');

end.
