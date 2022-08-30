{*******************************************************}
{               MiTeC Common Routines                   }
{              String format routines                   }
{                                                       }
{         Copyright (c) 2010-2013 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_StrFormat;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

{$IFDEF WIN64}'Not compatible with 64bit compilers!!'{$ENDIF}

const
  fmtDEFAULT     =    -1;     // use DefaultStringFormat
  fmtNONE        =     0;     // allways an Empty String, nothing Action
  fmtCOPY        =     1;     // one to one binary (input = output)

  fmtHEX         = $1016;     // Hexadecimal
  fmtHEXL        = $1017;     // Hexadecimal lowercase
  fmtMIME64      = $1064;     // MIME Base 64
  fmtUU          = $5555;     // UU Coding  $5555 = 'UU'
  fmtXX          = $5858;     // XX Coding  $5858 = 'XX'

type
  EStringFormat = class(Exception);

  TStringFormatClass = class of TStringFormat;

  TStringFormat = class(TPersistent) // for binary one to one convert = fmtCOPY
  protected
    class function IsFormat(Value: Integer): Boolean; virtual;
  public
    class function ToStr(Value: PAnsiChar; Len, Format: Integer): AnsiString; virtual;
    class function StrTo(Value: PAnsiChar; Len, Format: Integer): AnsiString; virtual;
    class function Format: Integer; virtual;
    class function IsValid(Value: PAnsiChar; Len, Format: Integer; ToStr: Boolean): Boolean; virtual;
  end;

  TStringFormat_HEX = class(TStringFormat) // Hexadecimal = fmtHEX
  public
    class function ToStr(Value: PAnsiChar; Len, Format: Integer): AnsiString; override;
    class function StrTo(Value: PAnsiChar; Len, Format: Integer): AnsiString; override;
    class function Format: Integer; override;
    class function IsValid(Value: PAnsiChar; Len, Format: Integer; ToStr: Boolean): Boolean; override;
// most TStringFormat_XXX use here a LookUpTable, we can so, when needed, faster and
// easier change to other formatingchars
    class function CharTable: PAnsiChar; virtual;
  end;

  TStringFormat_HEXL = class(TStringFormat_HEX) // Hexadecimal lowercase = fmtHEXL
  public
    class function Format: Integer; override;
    class function CharTable: PAnsiChar; override;
  end;

  TStringFormat_MIME64 = class(TStringFormat_HEX)  // MIME Base 64 = fmtMIME64
  public
    class function ToStr(Value: PAnsiChar; Len, Format: Integer): AnsiString; override;
    class function StrTo(Value: PAnsiChar; Len, Format: Integer): AnsiString; override;
    class function Format: Integer; override;
    class function CharTable: PAnsiChar; override;
  end;

  TStringFormat_UU = class(TStringFormat) // UU Encode = fmtUU
  public
    class function ToStr(Value: PAnsiChar; Len, Format: Integer): AnsiString; override;
    class function StrTo(Value: PAnsiChar; Len, Format: Integer): AnsiString; override;
    class function Format: Integer; override;
    class function IsValid(Value: PAnsiChar; Len, Format: Integer; ToStr: Boolean): Boolean; override;
    class function CharTable: PAnsiChar; virtual;
  end;

  TStringFormat_XX = class(TStringFormat_UU) // XX Encode = fmtXX
  public
    class function Format: Integer; override;
    class function CharTable: PAnsiChar; override;
  end;

function _StrToFormat(Value: PAnsiChar; Len, Format: Integer): AnsiString;
function _FormatToStr(Value: PAnsiChar; Len, Format: Integer): AnsiString;
function _ConvertFormat(Value: PAnsiChar; Len, FromFormat, ToFormat: Integer): AnsiString;

function StrToFormat(Value: string; Format: Integer; AOld: Boolean = False): string;
function FormatToStr(Value: string; Format: Integer; AOld: Boolean = False): string;
function ConvertFormat(Value: string; FromFormat, ToFormat: Integer; AOld: Boolean = False): string;

function StringFormat(Format: Integer): TStringFormatClass;
procedure RegisterStringFormats(const StringFormats: array of TStringFormatClass);

implementation

uses MiTeC_StrUtils;

var
  FStrFMTs: TList = nil;

class function TStringFormat.IsFormat(Value: Integer): Boolean;
begin
  Result := Format = Value;
end;

class function TStringFormat.ToStr(Value: PAnsiChar; Len, Format: Integer): AnsiString;
begin
  SetLength(Result, Len);
  Move(Value^, PAnsiChar(Result)^, Len*SizeOf(Char));
end;

class function TStringFormat.StrTo(Value: PAnsiChar; Len, Format: Integer): AnsiString;
begin
  SetLength(Result, Len);
  Move(Value^, PAnsiChar(Result)^, Len*SizeOf(Char));
end;

class function TStringFormat.Format: Integer;
begin
  Result := fmtCOPY;
end;

class function TStringFormat.IsValid(Value: PAnsiChar; Len, Format: Integer; ToStr: Boolean): Boolean;
begin
  Result := True;
end;

function TableFind(Value: AnsiChar; Table: PAnsiChar; Len: Integer): Integer; {$IFNDEF WIN64}assembler;{$ENDIF}
// Utility for TStringFormat_XXXXX
{$IFDEF WIN64}
begin
  // not implemented yet
{$ELSE}
asm
      PUSH  EDI
      MOV   EDI,EDX
      REPNE SCASB
      MOV   EAX,0
      JNE   @@1
      MOV   EAX,EDI
      SUB   EAX,EDX
@@1:  DEC   EAX
      POP   EDI
{$ENDIF}
end;

class function TStringFormat_HEX.ToStr(Value: PAnsiChar; Len, Format: Integer): AnsiString;
var
  D: PByte;
  T: PAnsiChar;
  I,P: Integer;
  HasIdent: Boolean;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := StrLen(Value);
  if Len = 0 then Exit;
  SetLength(Result, Len div 2 +1);
  T := CharTable;
  D := PByte(Result);
  I := 0;
  HasIdent := False;
  while Len > 0 do
  begin
    P := TableFind(Value^, T, 18);
    if P < 0 then P := TableFind(UpCase(Value^), T, 16);
    Inc(Value);
    if P >= 0 then
      if P > 16 then
      begin
        if not HasIdent then
        begin
          HasIdent := True;
          I := 0;
          D := PByte(Result);
        end;
      end else
      begin
        if Odd(I) then
        begin
          D^ := D^ or P;
          Inc(D);
        end else D^ := P shl 4;
        Inc(I);
      end;
    Dec(Len);
  end;
  SetLength(Result, PAnsiChar(D) - PAnsiChar(Result));
end;

class function TStringFormat_HEX.StrTo(Value: PAnsiChar; Len, Format: Integer): AnsiString;
var
  D,T: PAnsiChar;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := StrLen(Value);
  if Len = 0 then Exit;
  SetLength(Result, Len * 2);
  T := CharTable;
  D := PAnsiChar(Result);
  while Len > 0 do
  begin
    D[0] := T[Byte(Value^) shr  4];
    D[1] := T[Byte(Value^) and $F];
    Inc(D, 2);
    Inc(Value);
    Dec(Len);
  end;
end;

class function TStringFormat_HEX.IsValid(Value: PAnsiChar; Len, Format: Integer; ToStr: Boolean): Boolean;
var
  T: PAnsiChar;
  L: Integer;
begin
  Result := not ToStr;
  if not Result then
  begin
    T := CharTable;
    L := StrLen(T);
    while Len > 0 do
      if TableFind(Value^, T, L) >= 0 then
      begin
        Dec(Len);
        Inc(Value);
      end else Exit;
  end;
  Result := True;
end;

class function TStringFormat_HEX.Format: Integer;
begin
  Result := fmtHEX;
end;

class function TStringFormat_HEX.CharTable: PAnsiChar; {$IFNDEF WIN64}assembler;{$ENDIF}
{$IFDEF WIN64}
begin
  Result:='0123456789ABCDEF'+
          'X$ abcdefhHx()[]{},;:-_/\*+"'''#9#10#13#0;
{$ELSE}
asm
      MOV  EAX,OFFSET @@1
      RET
@@1:  DB   '0123456789ABCDEF'     // Table must be >= 18 Chars
      DB   'X$ abcdefhHx()[]{},;:-_/\*+"''',9,10,13,0
{$ENDIF}
end;

class function TStringFormat_HEXL.Format: Integer;
begin
  Result := fmtHEXL;
end;

class function TStringFormat_HEXL.CharTable: PAnsiChar;{$IFNDEF WIN64}assembler;{$ENDIF}
{$IFDEF WIN64}
begin
  Result:='0123456789abcdef'+
          'X$ ABCDEFhHx()[]{},;:-_/\*+"'''#9#10#13#0;
{$ELSE}
asm
      MOV  EAX,OFFSET @@1
      RET
@@1:  DB   '0123456789abcdef'     // Table must be >= 18 Chars
      DB   'X$ ABCDEFhHx()[]{},;:-_/\*+"''',9,10,13,0
{$ENDIF}
end;

class function TStringFormat_MIME64.ToStr(Value: PAnsiChar; Len, Format: Integer): AnsiString;
var
  B: Cardinal;
  J,I: Integer;
  S,D,L,T: PAnsiChar;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := Length(Value);
  if Len = 0 then Exit;
  SetLength(Result, Len);
  Move(PAnsiChar(Value)^, PAnsiChar(Result)^, Len*SizeOf(Char));
  T := CharTable;
  D := PAnsiChar(Result);
  S := D;
  L := S + Len;
  J := 0;
  while S < L do
  begin
    B := 0;
    J := 4;
    while (J > 0) and (S < L) do
    begin
      I := TableFind(S^, T, 65);
      Inc(S);
      if I >= 0 then
        if I < 64 then
        begin
          B := B shl 6 or Byte(I);
          Dec(J);
        end else L := S;
    end;
    if J > 0 then
      if J >= 4 then
      begin
        J := 0;
        Break;
      end else B := B shl (6 * J);
    I := 2;
    while I >= 0 do
    begin
      D[I] := AnsiChar(B);
      B := B shr 8;
      Dec(I);
    end;
    Inc(D, 3);
  end;
  SetLength(Result, D - PAnsiChar(Result) - J);
end;

class function TStringFormat_MIME64.StrTo(Value: PAnsiChar; Len, Format: Integer): AnsiString;
var
  B: Cardinal;
  I: Integer;
  D,T: PAnsiChar;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := StrLen(Value);
  if Len = 0 then Exit;
  SetLength(Result, Len * 4 div 3 + 4);
  D := PAnsiChar(Result);
  T := CharTable;
  while Len > 0 do
  begin
    B := 0;
    for I := 0 to 2 do
    begin
      B := B shl 8;
      if Len > 0 then
      begin
        B := B or Byte(Value^);
        Inc(Value);
      end;
      Dec(Len);
    end;
    for I := 3 downto 0 do
    begin
      if Len < 0 then
      begin
        D[I] := T[64];
        Inc(Len);
      end else D[I] := T[B and $3F];
      B := B shr 6;
    end;
    Inc(D, 4);
  end;
  SetLength(Result, D - PAnsiChar(Result));
end;

class function TStringFormat_MIME64.Format: Integer;
begin
  Result := fmtMIME64;
end;

class function TStringFormat_MIME64.CharTable: PAnsiChar; {$IFNDEF WIN64}assembler;{$ENDIF}
{$IFDEF WIN64}
begin
  Result:='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='+
          ' $()[]{},;:-_\*"'''#9#10#13#0;
{$ELSE}
asm
      MOV  EAX,OFFSET @@1
      RET  // must be >= 65 Chars
@@1:  DB  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='
      DB  ' $()[]{},;:-_\*"''',9,10,13,0  // special and skipped chars
{$ENDIF}
end;

class function TStringFormat_UU.ToStr(Value: PAnsiChar; Len, Format: Integer): AnsiString;
var
  T,D,L: PAnsiChar;
  I,E: Integer;
  B: Cardinal;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := StrLen(Value);
  if Len = 0 then Exit;
  SetLength(Result, Len);
  L := Value + Len;
  D := PAnsiChar(Result);
  T := CharTable;
  repeat
    Len := TableFind(Value^, T, 64);
    if (Len < 0) or (Len > 45) then
      raise EStringFormat.Create('Invalid AnsiString Format: UU');
    Inc(Value);
    while Len > 0 do
    begin
      B := 0;
      I := 4;
      while (I > 0) and (Value <= L) do
      begin
        E := TableFind(Value^, T, 64);
        if E >= 0 then
        begin
          B := B shl 6 or Byte(E);
          Dec(I);
        end;
        Inc(Value);
      end;
      I := 2;
      repeat
        D[I] := AnsiChar(B);
        B    := B shr 8;
        Dec(I);
      until I < 0;
      if Len > 3 then Inc(D, 3) else Inc(D, Len);
      Dec(Len, 3);
    end;
  until Value >= L;
  SetLength(Result, D - PAnsiChar(Result));
end;

class function TStringFormat_UU.StrTo(Value: PAnsiChar; Len, Format: Integer): AnsiString;
var
  T,D: PAnsiChar;
  L,I: Integer;
  B: Cardinal;
begin
  Result := '';
  if Value = nil then Exit;
  if Len < 0 then Len := StrLen(Value);
  if Len = 0 then Exit;
  SetLength(Result, Len * 4 div 3 + Len div 45 + 10);
  D := PAnsiChar(Result);
  T := CharTable;
  while Len > 0 do
  begin
    L := Len;
    if L > 45 then L := 45;
    Dec(Len, L);
    D^ := T[L];
    while L > 0 do
    begin
      B := 0;
      for I := 0 to 2 do
      begin
        B := B shl 8;
        if L > 0 then
        begin
          B := B or Byte(Value^);
          Inc(Value);
        end;
        Dec(L);
      end;
      for I := 4 downto 1 do
      begin
        D[I] := T[B and $3F];
        B := B shr 6;
      end;
      Inc(D, 4);
    end;
    Inc(D);
  end;
  SetLength(Result, D - PAnsiChar(Result));
end;

class function TStringFormat_UU.Format: Integer;
begin
  Result := fmtUU;
end;

class function TStringFormat_UU.IsValid(Value: PAnsiChar; Len, Format: Integer; ToStr: Boolean): Boolean;
var
  T: PAnsiChar;
  L,I,P: Integer;
begin
  Result := not ToStr;
  if not Result then
  begin
    T := CharTable;
    L := StrLen(T);
    P := 0;
    while Len > 0 do
    begin
      I := TableFind(Value^, T, L);
      if I >= 0 then
      begin
        Dec(Len);
        Inc(Value);
        if P = 0 then
        begin
          if I > 45 then Exit;
          P := (I * 4 + 2) div 3;
        end else
          if I < 64 then Dec(P);
      end else Exit;
    end;
    if P <> 0 then Exit;
  end;
  Result := True;
end;

class function TStringFormat_UU.CharTable: PAnsiChar;{$IFNDEF WIN64}assembler;{$ENDIF}
{$IFDEF WIN64}
begin
  Result:='`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_'+
          ' '#9#10#13#0;
{$else}
asm
      MOV  EAX,OFFSET @@1
      RET  // must be >= 64 Chars
@@1:  DB   '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_'
      DB   ' ',9,10,13,0
{$ENDIF}
end;

class function TStringFormat_XX.Format: Integer;
begin
  Result := fmtXX;
end;

class function TStringFormat_XX.CharTable: PAnsiChar;{$IFNDEF WIN64}assembler;{$ENDIF}
{$IFDEF WIN64}
begin
  Result:='+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'+
          ' "()[]'''#9#10#13#0;
{$ELSE}
asm
      MOV  EAX,OFFSET @@1
      RET
@@1:  DB   '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
      DB   ' "()[]''',9,10,13,0
{$ENDIF}
end;

function _StrToFormat(Value: PAnsiChar; Len, Format: Integer): AnsiString;
var
  Fmt: TStringFormatClass;
begin
  Result := '';
  if (Value = nil) or (Format = fmtNONE) then
    Exit;
  if Len <  0 then
    Len := StrLen(Value);
  if Len <= 0 then
    Exit;
  if Format = fmtCOPY then begin
    SetLength(Result, Len);
    Move(Value^, PAnsiChar(Result)^, Len*SizeOf(Char));
  end else begin
    Fmt := StringFormat(Format);
    if Fmt <> nil then
      if Fmt.IsValid(Value, Len, Format, False) then
        Result := Fmt.StrTo(Value, Len, Format)
      else
        raise EStringFormat.Create('Invalid Format AnsiString: '+FMT.ClassName)
    else
      raise EStringFormat.CreateFmt('AnsiString Format Exists: %d',[Format]);
  end;
end;

function _FormatToStr(Value: PAnsiChar; Len, Format: Integer): AnsiString;
var
  Fmt: TStringFormatClass;
begin
  Result := '';
  if (Value = nil) or (Format = fmtNONE) then Exit;
  if Len < 0 then Len := StrLen(Value);
  if Len = 0 then Exit;
  if Format = fmtCOPY then
  begin
    SetLength(Result, Len);
    Move(Value^, PAnsiChar(Result)^, Len*SizeOf(Char));
  end else
  begin
    Fmt := StringFormat(Format);
    if Fmt <> nil then
      if Fmt.IsValid(Value, Len, Format, True) then Result := Fmt.ToStr(Value, Len, Format)
        else raise EStringFormat.Create('Invalid AnsiString Format: '+FMT.ClassName)
      else raise EStringFormat.CreateFmt('AnsiString Format Exists: %d',[Format]);
  end;
end;

function _ConvertFormat(Value: PAnsiChar; Len, FromFormat, ToFormat: Integer): AnsiString;
begin
  Result := '';
  if (FromFormat = fmtNONE) or (ToFormat = fmtNONE) then Exit;
  if FromFormat <> ToFormat then
  begin
    Result := _FormatToStr(Value, Len, FromFormat);
    Result := _StrToFormat(PAnsiChar(Result), Length(Result), ToFormat);
  end else
  begin
    if Value = nil then Exit;
    if Len < 0 then Len := StrLen(Value);
    if Len = 0 then Exit;
    SetLength(Result, Len);
    Move(Value^, PAnsiChar(Result)^, Len*SizeOf(Char));
  end;
end;

function StrToFormat;
var
  s: AnsiString;
begin
  if Value='' then begin
    Result:='';
    Exit;
  end;
  {$IFDEF UNICODE}
  if AOld then
    s:=WideToAnsi(Value)
  else
    s:=Value;
  {$ELSE}
  s:=Value;
  {$ENDIF}
  Result:=_StrToFormat(PAnsiChar(s),Length(s),Format);
end;

function FormatToStr;
var
  s: AnsiString;
begin
  if Value='' then begin
    Result:='';
    Exit;
  end;
  {$IFDEF UNICODE}
  if AOld then
    s:=WideToAnsi(Value)
  else
    s:=Value;
  {$ELSE}
  s:=Value;
  {$ENDIF}
  Result:=_FormatToStr(PAnsiChar(s),Length(s),Format);
end;

function ConvertFormat;
var
  s: AnsiString;
begin
  if Value='' then begin
    Result:='';
    Exit;
  end;
  {$IFDEF UNICODE}
  if AOld then
    s:=WideToAnsi(Value)
  else
    s:=Value;
  {$ELSE}
  s:=Value;
  {$ENDIF}
  Result:=_ConvertFormat(PAnsiChar(s),Length(s),FromFormat,ToFormat);
end;

procedure RegisterStringFormats(const StringFormats: array of TStringFormatClass);
var
  I,J: Integer;
  FMT: TStringFormatClass;
begin
  if FStrFMTs = nil then
    FStrFMTs:=TList.Create;
  for I := Low(StringFormats) to High(StringFormats) do
    if (StringFormats[I] <> nil) and
       (StringFormats[I].Format <> fmtDEFAULT) then
    begin
      FMT := StringFormat(StringFormats[I].Format);
      if (FMT <> nil) and
         FMT.IsFormat(StringFormats[I].Format) and
         StringFormats[I].IsFormat(FMT.Format) then
      begin
        J := FStrFMTs.IndexOf(FMT);
        FStrFMTs[J] := StringFormats[I];
      end else FStrFMTs.Add(StringFormats[I]);
    end;
end;

function StringFormat(Format: Integer): TStringFormatClass;
var
  I: Integer;
begin
  if Format = fmtDefault then
    Format := fmtMIME64;
  Result := nil;
  if FStrFmts <> nil then
    for I := 0 to FStrFMTs.Count-1 do
      if TStringFormatClass(FStrFmts[I]).IsFormat(Format) then
      begin
        Result := FStrFMTS[I];
        Exit;
      end;
end;

initialization
  RegisterStringFormats([TStringFormat, TStringFormat_HEX, TStringFormat_HEXL,
      TStringFormat_MIME64, TStringFormat_UU, TStringFormat_XX]);
finalization
  FStrFMTs.Free;
  FStrFMTs:=nil;
end.



