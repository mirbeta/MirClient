{*************************************************************************}
{ TADVLISTVIEW HELPER FUNCTIONS                                           }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 1998-2013                                        }
{            Email : info@tmssoftware.com                                 }
{            Web : http://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AlvUtil;

{$I TMSDEFS.INC}

interface

uses
  Windows, SysUtils, Graphics, Classes;

type
  TFileStringList = class(TStringList)
  private
    fp: integer;
    cache: string;
    function GetEOF: boolean;
  public
    procedure Reset;
    procedure ReadLn(var s: string);
    procedure Write(s: string);
    procedure WriteLn(s: string);
    property Eof: boolean read GetEOF;
  end;


function Matches(s0a, s1a: pchar): boolean;
function Matchstr(s1, s2: string): boolean;
procedure LineFeedsToCSV(var s: string);
procedure CSVToLineFeeds(var s: string);
function LfToFile(s: string): string;
function FileToLf(s: string; multiline: boolean): string;
function DoubleToSingleChar(ch: char; const s: string): string;
function UpStr(s: string): string;
function GetNextLine(var s: string; multiline: boolean): string;
function LinesInText(s: string; multiline: boolean): integer;
procedure OemToString(var s: string);
procedure StringToOem(var s: string);
function RectString(r: trect): string;
function FixDecimalSeparator(s: string): string;
function GetNextDate(d: tdatetime; dye, dmo, dda: word; dtv: tdatetime): tdatetime;
function NumSingleChar(p: char; s: string): integer;
function SinglePos(p: char; s: string): integer;
procedure DrawProgress(Canvas: TCanvas; r: TRect; Color1, FontColor1, Color2, FontColor2: TColor; Pos: Integer; ValueFormat: string; ShowValue: boolean = true);

implementation

const
  LINEFEED = #13;

{$I DELPHIXE.INC}

procedure StringToOem(var s:string);
{$IFNDEF DELPHIXE4_LVL}
{$IFDEF WIN32}
var
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
  GetMem(pin,Length(s) + 1);
  GetMem(pout,Length(s) + 1);
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
  GetMem(pin,Length(sa) + 1);
  GetMem(pout,Length(sa) + 1);
  StrPLCopy(pin,sa,Length(sa));
  OemToChar(pin,pout);
  s := StrPas(pout);
  FreeMem(pin);
  FreeMem(pout);
  {$ENDIF}
  {$ENDIF}
end;

function DoubleToSingleChar(ch: char; const s: string): string;
var
  res: string;
  i: integer;
begin
  if (s = '') then
  begin
    DoubleToSingleChar := s;
    Exit;
  end;
  res := s[1];
  for i := 2 to length(s) do
  begin
    if (s[i] <> ch) then res := res + s[i] else
      if ((s[i] = ch) and (s[i - 1] <> ch)) then res := res + s[i];
  end;
  DoubleToSingleChar := res;
end;

procedure LineFeedsToCSV(var s: string);
begin
  while (pos(#13#10, s) > 0) do delete(s, pos(#13#10, s), 1);
  s := '"' + s + '"';
end;

procedure CSVToLineFeeds(var s: string);
var
  res: string;
  i: integer;
begin
  res := '';
  for i := 1 to length(s) do
    if s[i] = #10 then res := res + #13#10 else res := res + s[i];
  s := res;
end;

function UpStr(s: string): string;
var
  j: integer;
begin
  for j := 1 to length(s) do s[j] := upcase(s[j]);
  Upstr := s;
end;

function MatchStr(s1, s2: string): boolean;
var
  s0a, s1a: array[0..255] of char;
begin
  strpcopy(s0a, s1);
  strpcopy(s1a, s2);
  matchstr := matches(s0a, s1a);
end;

function Matches(s0a, s1a: pchar): boolean;
const
  larger = '>';
  smaller = '<';
  logand = '&';
  logor = '^';
  asterix = '*';
  qmark = '?';
  negation = '!';
  null = #0;

var
  matching: boolean;
  done: boolean;
  len: longint;
  s0, s1, s2, s3: pchar;
  oksmaller, oklarger, negflag: boolean;
  compstr: array[0..20] of char;
  flag1, flag2, flag3: boolean;
  equal: boolean;

begin
  oksmaller := true;
  oklarger := true;
  flag1 := false;
  flag2 := false;
  flag3 := false;
  negflag := false;
  equal := false;

 { [<>] string [&|] [<>] string }

  s2 := strpos(s0a, larger);
  if (s2 <> nil) then
  begin
    inc(s2);
    if (s2^ = '=') then
    begin
      equal := true;
      inc(s2);
    end;

    while (s2^ = ' ') do inc(s2);
    s3 := s2;
    len := 0;
    while (s2^ <> ' ') and (s2^ <> NULL) and (s2^ <> '&') and (s2^ <> '|') do
    begin
      inc(s2);
      inc(len);
    end;
    strlcopy(compstr, s3, len);
    if equal then
      oklarger := (strlcomp(compstr, s1a, 255) <= 0)
    else
      oklarger := (strlcomp(compstr, s1a, 255) < 0);
    flag1 := true;
  end;

  equal := false;
  s2 := strpos(s0a, smaller);
  if (s2 <> nil) then
  begin
    inc(s2);
    if (s2^ = '=') then
    begin
      equal := true;
      inc(s2);
    end;

    while (s2^ = ' ') do inc(s2);
    s3 := s2;
    len := 0;
    while (s2^ <> ' ') and (s2^ <> NULL) and (s2^ <> '&') and (s2^ <> '|') do
    begin
      inc(s2);
      inc(len);
    end;
    strlcopy(compstr, s3, len);
    if equal then
      oksmaller := (strlcomp(compstr, s1a, 255) >= 0)
    else
      oksmaller := (strlcomp(compstr, s1a, 255) > 0);

    flag2 := true;
  end;

  s2 := strpos(s0a, negation);
  if (s2 <> nil) then
  begin
    inc(s2);
    while (s2^ = ' ') do inc(s2);
    s3 := s2;
    len := 0;
    while (s2^ <> ' ') and (s2^ <> NULL) and (s2^ <> '&') and (s2^ <> '|') do
    begin
      inc(s2);
      inc(len);
    end;
    strlcopy(compstr, s3, len);
    flag3 := true;
  end;

  if (flag3) then
  begin
    if strpos(s0a, larger) = nil then flag1 := flag3;
    if strpos(s0a, smaller) = nil then flag2 := flag3;
  end;

  if (strpos(s0a, logor) <> nil) then
    if flag1 or flag2 then
    begin
      matches := oksmaller or oklarger;
      Exit;
    end;

  if (strpos(s0a, logand) <> nil) then
    if flag1 and flag2 then
    begin
      matches := oksmaller and oklarger;
      Exit;
    end;

  if ((strpos(s0a, larger) <> nil) and (oklarger)) or
    ((strpos(s0a, smaller) <> nil) and (oksmaller)) then
  begin
    matches := true;
    Exit;
  end;

  s0 := s0a;
  s1 := s1a;

  matching := true;

  done := (s0^ = NULL) and (s1^ = NULL);

  while not done and matching do
  begin
    case s0^ of
      qmark: begin
          matching := s1^ <> NULL;
          if matching then
          begin
            inc(s0);
            inc(s1);
          end;
        end;
      negation: begin
          negflag := true;
          inc(s0);
        end;
      asterix: begin
          repeat inc(s0)
          until (s0^ <> asterix);
          len := strlen(s1);
          inc(s1, len);
          matching := matches(s0, s1);
          while (len >= 0) and not matching do
          begin
            dec(s1);
            dec(len);
            matching := matches(s0, s1);
          end;
          if matching then
          begin
            s0 := strend(s0);
            s1 := strend(s1);
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

  if negflag then Matches := not matching else Matches := matching;
end;

function Lftofile(s: string): string;
var
  i: integer;
begin
  for i := 1 to length(s) do
  begin
    if s[i] = #13 then s[i] := #9;
    if s[i] = #10 then s[i] := #8;
  end;
  lftofile := s;
end;

function FileToLF(s: string; multiline: boolean): string;
var
  i: integer;
begin
  for i := 1 to length(s) do
  begin
    if s[i] = #9 then s[i] := #13;
    if s[i] = #8 then s[i] := #10;
  end;
  if not MultiLine then FileToLF := GetNextLine(s, multiline) else Filetolf := s;
end;

function GetNextLine(var s: string; multiline: boolean): string;
begin
  if pos(LINEFEED, s) > 0 then
  begin
    Result := copy(s, 1, pos(LINEFEED, s) - 1);
    delete(s, 1, pos(LINEFEED, s));
    if (s <> '') then if (s[1] = #10) then delete(s, 1, 1);
    if not Multiline then s := '';
  end
  else
  begin
    Result := s;
    s := '';
  end;
end;

function LinesInText(s: string; multiline: boolean): integer;
begin
  Result := 1;
  if not Multiline then Exit;
  while (pos(LINEFEED, s) > 0) do
  begin
    inc(Result);
    delete(s, 1, pos(LINEFEED, s));
  end;
end;

function RectString(r: trect): string;
begin
  Result := '[' + inttostr(r.left) + ':' + inttostr(r.top) + '][' + inttostr(r.right) + ':' + inttostr(r.left) + ']';
end;

function FixDecimalSeparator(s: string): string;
begin
  if (decimalseparator = ',') then
    if pos(',', s) > 0 then s[pos(',', s)] := '.';
  Result := s;
end;

function GetNextDate(d: tdatetime; dye, dmo, dda: word; dtv: tdatetime): tdatetime;
var
  ye, mo, da: word;
begin
  decodedate(d, ye, mo, da);
  if (dmo = 0) and (dye = 0) and (dda <> 0) then {equal month + equal year}
  begin
    Result := d + dda;
  end
  else
    if (dmo = 0) and (dye <> 0) and (dda = 0) then
    begin
      Result := encodedate(ye + dye, mo, da);
    end
    else
      if (dmo <> 0) and (dye = 0) and (dda = 0) then
      begin
        mo := mo + dmo;
        if (mo <= 0) then
        begin
          mo := mo + 12;
          dec(ye);
        end;
        if (mo > 12) then
        begin
          mo := mo - 12;
          inc(ye);
        end;
        Result := encodedate(ye, mo, da);
      end
      else
        Result := d + dtv;
end;

function SinglePos(p: char; s: string): Integer;
var
  i: Integer;
begin
  i := 1;
  while (i <= Length(s)) do
  begin
    if (s[i] = p) then
    begin
      if (i < Length(s)) then
      begin
        if (s[i + 1] <> p) then
        begin
          if ((i > 1) and (s[i - 1] <> p)) or (i = 1) then
          begin
            SinglePos := i;
            Exit;
          end;
        end
      end
      else
        if (i = Length(s)) then
        begin
          SinglePos := i;
          Exit;
        end;
    end;
    inc(i);
  end;
  SinglePos := 0;
end;

function NumSingleChar(p: char; s: string): integer;
var
  res: Integer;
begin
  res := 0;
  while (SinglePos(p, s) > 0) do
  begin
    delete(s, 1, singlepos(p, s));
    inc(res);
  end;
  Result := res;
end;


procedure DrawProgress(Canvas: TCanvas; r: TRect; Color1, FontColor1, Color2, FontColor2: TColor; Pos: Integer; ValueFormat:string; ShowValue: boolean = true);
var
  SrcRect, TgtRect: TRect;
  txt: string;
  tw,th: integer;
begin
  Canvas.Brush.Color := Color1;
  Canvas.Pen.Color := Color1;
  Canvas.Font.Color := FontColor1;

  if ShowValue then
    txt := Format(ValueFormat, [Pos]) //IntToStr(Pos) + '%'
  else
    txt := '';

  tw := Canvas.TextWidth(txt);
  th := Canvas.TextHeight(txt);

  Inflaterect(r, -2, -2);
  SrcRect := r;
  SrcRect.Right := SrcRect.Left + Round((SrcRect.Right - SrcRect.Left) * (Pos) / 100);
  TgtRect.Left := r.Left + (((r.Right - r.Left) - tw) div 2);
  TgtRect.Top := r.Top + (((r.Bottom - r.Top) - th) div 2);
  Canvas.TextRect(SrcRect, TgtRect.Left, TgtRect.Top, txt);

  Canvas.Brush.Color := Color2;
  Canvas.Pen.Color := Color2;
  Canvas.Font.Color := FontColor2;

  SrcRect.Left := SrcRect.Right;
  SrcRect.Right := r.Right;
  Canvas.TextRect(SrcRect, TgtRect.Left, TgtRect.Top, txt);

  Canvas.Pen.Color := clGray;
  Canvas.Brush.Color := clGray;
  Inflaterect(r, 1, 1);
  Canvas.FrameRect(r);
end;


procedure TFileStringList.Reset;
begin
  fp := 0;
  cache := '';
end;

function TFileStringList.GetEOF;
begin
  Result := fp >= Count;
end;

procedure TFileStringList.ReadLn(var s: string);
begin
  s := Strings[fp];
  inc(fp);
end;

procedure TFileStringList.Write(s: string);
begin
  cache := cache + s;
end;

procedure TFileStringList.WriteLn(s: string);
begin
  Add(cache + s);
  cache := '';
end;



begin
end.
