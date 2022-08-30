{*******************************************************}
{               MiTeC Common Routines                   }
{                  String routines                      }
{                                                       }
{                                                       }
{          Copyright (c) 1997-2019 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_StrUtils;

interface

uses {$IFDEF RAD9PLUS}
     System.Variants, System.SysUtils, System.Classes, WinAPI.Windows;
     {$ELSE}
     Variants, SysUtils, Classes, Windows;
     {$ENDIF}

type
  TTextType = (ANSI, UTF16LE, UTF16BE, UTF16, UTF8);

  CharSet = set of AnsiChar;

function GetStrFromBuf(var Buffer: array of Byte; Len: Cardinal = 0): string;
function GetStrFromCharBuf(Buffer: PAnsiChar; Len: Cardinal = 0): ansistring;
function GetStrFromWideCharBuf(Buffer: PWideChar; Len: Integer = 0): WideString;
function GetStrListFromWideCharBuf(Buffer: PWideChar; Len: Integer): WideString;
function TrimAll(ASource: string): string;
function StripSpaces(ASource: string): string;
function StripNPAnsiChars(ASource: string): string;
function BoolToStr(AValue, AVerbose: Boolean): string;
function StrToBool(ASource: string): Boolean;
procedure AddWord(var ADest :string; const AWord,ADelimiter: string);
function GetDelimitedText(AList: TStrings; ADelimiter: string): string;
procedure SetDelimitedText(ASource: string; ADelimiter: string; var AList: TStringList);
function FitStr(const ASource, AEllipsis :string; ALength :integer) :string;
function CenterStr(const ASource: string; ALength :integer; AFillChar: Char = #32) :string;
function LeftPad(const ASource: string; ALength :integer; AFillChar: Char = #32) :string;
function RightPad(const ASource: string; ALength :Integer; AFillChar: Char = #32) :string;
function AnsiGetCSVData(const ARecord: ansistring; AField: Cardinal; const ADelimiter: ansistring = ';'): ansistring;
function WideGetCSVData(const ARecord: widestring; AField: Cardinal; const ADelimiter: widestring = ';'): widestring;
function GetCSVIndex(const AHeader, AFieldName: string; const ADelimiter: string = ';'): Integer; overload;
function GetCSVIndex(const AHeader: string; AFieldNames: array of string; const ADelimiter: string = ';'): Integer; overload;
function GetCSVData(const ARecord: string; AField: Cardinal; const ADelimiter: string = ';'): string;
function GetToken(s :string; index :Integer; adelimiter: string = ';') :string;
procedure SetToken(adelimiter, newvalue :string; index :integer; var s :string);
function GetTokenCount(s, adelimiter :string) :Integer;
function GetWordCount(s :string; WordDelims: CharSet) :Integer;
function ExtractWord(N: integer; S: String; WordDelims: CharSet): string;
function GetCharCount(ASource: string; AChar: char): Integer;
function PosIdx(Substr,S: string; Index: Byte): Integer;
function DeleteFromLast(S: string; StoPAnsiChar: char; DeleteStoPAnsiChar: Boolean = True): string;
function Capitalize(const Source: string): string;
function DeCapitalize(const Source: string): string;

function IsValidNumber(S: string; var V: double): boolean;
function IsValidDateTime(const S: string; var D: TDateTime): Boolean;
function DequoteStr(Source: string; Quote: Char = '"'): string;
function EncodeBase (I: Int64; Base: Byte): string;

function ListValue(AList: TStrings; AName: string; ASep: string = '='): string;
function ListName(AList: TStrings; AIndex: integer; ASep: string = '='): string;
function ListNameExists(AList: TStrings; AName: string; ASep: string = '='): integer;
function ListValueFromIndex(AList: TStrings; AIndex: integer; ASep: string = '='): string;
function ListIndexOfValue(AList: TStrings; const AValue: string; ASep: string = '='): integer;
function GetValueFromStr(ASource: string; ASep: string = '='): string;
function GetNameFromStr(ASource: string; ASep: string = '='): string;
function ScanList(AText: string; AList: TStrings): Integer;
procedure AddNameValue(AList: TStrings; AName,AValue: string; ASep: string = '=');

function FormatTimer(ATime: Int64): string;

function Empty(Source: string): Boolean;

function NormalizeDate(Source: string): string;
function NormalizeNumber(Source: string): string; overload;
function NormalizeNumberEx(Source: string): string; overload;
function NormalizeNumberEx(Source: integer): string; overload;
function NormalizeNumber(Source: Double; APrec: Byte = 2): string; overload;
function NormalizeDataValue(AValue: Double): string; overload;
function NormalizeDataValue(AValue: Int64): string; overload;

function QuoteStr(Source: string; Quote: Char = ''''): string;
function QuoteTrimStr(Source: string; Quote: Char = ''''): string;
function QuoteTrimStrEx(Source: string; Quote: Char = ''''): string;

function CustomSort(Value1,Value2: Variant): Integer;
function AdvancedCustomSort(Value1,Value2: string; ALimit: Byte = 25): Integer;

function PosRev(Substring: string; Source: string): integer;

procedure SplitVersion(ASource: string; var Major: Cardinal; var Minor: Cardinal);

function TrimWideString(Source: WideString): WideString;

procedure BufferToHexView(var Buffer; DataSize: Int64; AList: TStringList);

function CountText(ASource,AText: string): Word;
function PosText(const ASubText, AText: string; ACaseSensitive: Boolean = False; AWholeWord: Boolean = False): Integer;
function PosTextList(const ASubCommaText, AText: string): Integer; overload;
function PosTextList(AList: TStrings; const AText: string): Integer; overload;

function StreamToHex(ASource: TStream): AnsiString;

{$if defined(RAD5PLUS) or defined(FPC)}
function BytesToHex(ASource: TBytes): string;
function HexToBytes(ASource: string): TBytes;
function BytesToString(ASource: TBytes): string;
//function StringToBytes(const ASource: string): TBytes;
function StringToBytes(const ASource: WideString): TBytes; overload;
function StringToBytes(const ASource: AnsiString): TBytes; overload;
{$ifend}

function StripUnprintable(const ASource: string; SubstChar: Char = '_'): string;
function StripUnprintableW(const ASource: widestring; SubstChar: WideChar = '_'): widestring;
function StripUnprintableA(const ASource: ansistring; SubstChar: AnsiChar = '_'): ansistring;

function DeleteLast(const S: string; Count: integer = 1): string;

procedure RemoveDuplicates(const stringList: TStringList);

{$IFNDEF RAD6PLUS}
function LoadUnicodeFile(const AFileName: string; AStrings: TStrings): TTextType;
procedure SaveAsUnicode(const AFilename: string; AStrings: TStrings; ATextType: TTextType = UTF16LE);
{$ENDIF}

function Int2Bin(A: Int64; Len: Byte = 8): string;
function Oct2Int(Value: string): Longint;
function Int2Oct(Value: Longint; digits: Integer): string;
function ByteToBin(AValue : Byte) : string;
function WordToBin(AValue : Word) : string;

function FindNumber(AText: string): Integer;

function ExtractDomainFromURL(AURL: string; AOnly2nd: Boolean = False): string;
function ExtractUrlFileName(const AUrl: string): string;
function ExtractTagValue(const Tag: string; PropName: string): string;

function StrToFloatEx(const S: string): Extended;

function WideToAnsi(const ws: WideString; codePage: Word = CP_ACP): AnsiString;
function AnsiToWide(const s: AnsiString; codePage: Word = CP_ACP): WideString;

function StripHTML(S: string): string;

function IsCap(AChar: Char): Boolean;
function SplitByCaps(const ASource: string): string;
function Alter(AValue: string; ADefault: string = ''): string;

function CheckXMLValue(AValue: string): string;
function FastStringReplace(const ASource, APattern, AReplace: string): string;

function FormatAmount(AAmount: Double; ACurrency: string = 'CZK'; ARate: Double = 1): string;
function FormatCurrency(ACurrency: string): string;

function PosLast(const ASubStr,AStr: string): Integer;
function PosNth(const ASubStr,AStr: string; APos: Cardinal): Integer;

function IfText(ACondition: boolean; const ATrueValue,AFalseValue: string): string;

const
  BooleanEn: array[Boolean] of string = ('No','Yes');
  BooleanCz: array[Boolean] of string = ('Ne','Ano');

resourcestring
  rsTrue = 'True';
  rsFalse = 'False';


implementation

uses {$IFDEF RAD9PLUS}
     System.StrUtils, System.DateUtils, System.Math, System.AnsiStrings
     {$ELSE}
     StrUtils, DateUtils, Math {$IFDEF UNICODE},AnsiStrings{$ENDIF}
     {$ENDIF}
     ;


function TrimAll(ASource: string): string;
begin
  Result:=FastStringReplace(ASource,#32,'');
  Result:=FastStringReplace(Result,#160,'');
  Result:=FastStringReplace(Result,#13,'');
  Result:=FastStringReplace(Result,#10,'');
end;

function BoolToStr(AValue, AVerbose: Boolean): string;
begin
  if AValue then begin
    if AVerbose then
      result:=rsTrue
    else
      result:='1';
  end else begin
    if AVerbose then
      result:=rsFalse
    else
      result:='0';
  end;
end;

function StrToBool(ASource: string): Boolean;
begin
  Result:=false;
  ASource:=UpperCase(ASource);
  if (ASource='TRUE') or (ASource='1') then
    Result:=true;
end;

procedure AddWord(var ADest :string; const AWord,ADelimiter: string);
begin
  if Length(ADest)>0 then
    ADest:=ADest+ADelimiter+AWord
  else
    ADest:=ADest+AWord;
end;

function GetDelimitedText(AList: TStrings; ADelimiter: string): string;
var
  i :integer;
begin
  result:='';
  for i:=0 to AList.Count-1 do
    Result:=Result+AList[i]+ADelimiter;
    if Result<>'' then
      Delete(Result,Length(Result)-Length(ADelimiter)+1,Length(ADelimiter));
end;

procedure SetDelimitedText(ASource: string; ADelimiter: string; var AList: TStringList);
var
  p: integer;
begin
  AList.Clear;
  p:=Pos(ADelimiter,ASource);
  while p>0 do begin
    AList.Add(Copy(ASource,1,p-1));
    Delete(ASource,1,p+Length(Adelimiter)-1);
    p:=Pos(ADelimiter,ASource);
  end;
  AList.Add(ASource);
end;

function CenterStr(const ASource: string; ALength :integer; AFillChar: Char = #32) :string;
var
  l1,l2: Integer;
begin
  l1:=(ALength-Length(ASource)) div 2;
  l2:=Round((ALength-Length(ASource))/2);
  Result:=StringOfChar(AFillChar,l1)+ASource+StringOfChar(AFillChar,l2);
end;

function LeftPad(const ASource: string; ALength :integer; AFillChar: Char = #32) :string;
begin
  Result:=ASource+StringOfChar(AFillChar,ALength-Length(ASource));
end;

function RightPad(const ASource: string; ALength :Integer; AFillChar: Char = #32) :string;
begin
  Result:=StringOfChar(AFillChar,ALength-Length(ASource))+ASource;
end;

function FitStr(const ASource, AEllipsis :string; ALength :integer) :string;
var
  lf :integer;
  s :string;
begin
  lf:=Length(ASource);
  if lf>ALength then begin
    result:=Copy(ASource,1,3);
    s:=Copy(ASource,lf-ALength+7,lf);
    result:=Result+AEllipsis+s;
  end else
    result:=ASource;
end;

function AnsiGetCSVData(const ARecord: ansistring; AField: Cardinal; const ADelimiter: ansistring = ';'): AnsiString;
var
  s: ansistring;
  p: Integer;
  i: Cardinal;
begin
  Result:='';
  s:=ARecord+ADelimiter;
  p:=Pos(ADelimiter,s);
  i:=0;
  while p>0 do begin
    if AField=i then begin
      Result:=Copy(s,1,p-1);
      Break;
    end;
    Delete(s,1,p);
    p:=Pos(ADelimiter,s);
    Inc(i);
  end;
end;

function WideGetCSVData(const ARecord: widestring; AField: Cardinal; const ADelimiter: widestring = ';'): WideString;
var
  s: widestring;
  p: Integer;
  i: Cardinal;
begin
  Result:='';
  s:=ARecord+ADelimiter;
  p:=Pos(ADelimiter,s);
  i:=0;
  while p>0 do begin
    if AField=i then begin
      Result:=Copy(s,1,p-1);
      Break;
    end;
    Delete(s,1,p);
    p:=Pos(ADelimiter,s);
    Inc(i);
  end;
end;

function GetCSVIndex(const AHeader, AFieldName: string; const ADelimiter: string = ';'): Integer;
var
  s: string;
  p: Integer;
  i: Cardinal;
begin
  Result:=-1;
  s:=AHeader+ADelimiter;
  p:=Pos(ADelimiter,s);
  i:=0;
  while p>0 do begin
    if SameText(AFieldname,Copy(s,1,p-1)) then begin
      Result:=i;
      Break;
    end;
    Delete(s,1,p);
    p:=Pos(ADelimiter,s);
    Inc(i);
  end;
end;

function GetCSVIndex(const AHeader: string; AFieldNames: array of string; const ADelimiter: string = ';'): Integer;
var
  s: string;
  p,j: Integer;
  i: Cardinal;
begin
  Result:=-1;
  for j:=0 to High(AFieldnames) do begin
    s:=AHeader+ADelimiter;
    p:=Pos(ADelimiter,s);
    i:=0;
    while p>0 do begin
      if SameText(AFieldnames[j],Copy(s,1,p-1)) then begin
        Result:=i;
        Break;
      end;
      Delete(s,1,p);
      p:=Pos(ADelimiter,s);
      Inc(i);
    end;
    if Result>-1 then
      Break;
  end;
end;


function GetCSVData(const ARecord: string; AField: Cardinal; const ADelimiter: string = ';'): string;
var
  s: string;
  p: Integer;
  i: Cardinal;
begin
  Result:='';
  s:=ARecord+ADelimiter;
  p:=Pos(ADelimiter,s);
  i:=0;
  while p>0 do begin
    if AField=i then begin
      Result:=Copy(s,1,p-1);
      Break;
    end;
    Delete(s,1,p);
    p:=Pos(ADelimiter,s);
    Inc(i);
  end;
end;

function GetToken(s :string; index :Integer; adelimiter: string = ';') :string;
var
  i,p :integer;
begin
  p:=pos(adelimiter,s);
  i:=1;
  while (p>0) and (i<index) do begin
    inc(i);
    delete(s,1,p);
    p:=pos(adelimiter,s);
  end;
  result:=copy(s,1,p-1);
end;

procedure SetToken(adelimiter, newvalue :string; index :integer; var s :string);
var
  i,p,sx,ex :integer;
  s1 :string;
begin
  s1:=s;
  p:=pos(adelimiter,s1);
  sx:=0;
  i:=0;
  while (p>0) and (i<index) do begin
    inc(sx,p);
    inc(i);
    delete(s1,1,p);
    p:=pos(adelimiter,s1);
  end;
  ex:=sx+p;
  s:=copy(s,1,sx)+newvalue+copy(s,ex,255);
end;

function GetTokenCount(s, adelimiter :string) :Integer;
begin
  Result:=0;
  while GetToken(s,Result,adelimiter)<>'' do
    Inc(Result);
end;

function GetWordCount(s :string; WordDelims: CharSet) :Integer;
var
  SLen, I: Cardinal;
begin
  Result:=0;
  I:=1;
  SLen:=Length(S);
  while I <= SLen do begin
    while (I <= SLen) and {$IFDEF RAD7PLUS}CharInSet(S[i],WordDelims){$ELSE}(S[I] in WordDelims){$ENDIF} do Inc(I);
      if I <= SLen then
        Inc(Result);
    while (I <= SLen) and not({$IFDEF RAD7PLUS}CharInSet(S[i],WordDelims){$ELSE}(S[I] in WordDelims){$ENDIF}) do
      Inc(I);
  end;
end;

function ExtractWord(N: integer; S: String; WordDelims: CharSet): string;
var
  I,J,Count,SLen:Integer;
begin
  Count:=0;
  I:=1;
  Result:='';
  SLen:=Length(S);
  while I <= SLen do begin
    while (I <= SLen) and {$IFDEF RAD7PLUS}CharInSet(S[i],WordDelims){$ELSE}(S[I] in WordDelims){$ENDIF} do
      Inc(I);
    if I<=SLen then Inc(Count);
    J:=I;
    while (J <= SLen) and not({$IFDEF RAD7PLUS}CharInSet(S[i],WordDelims){$ELSE}(S[I] in WordDelims){$ENDIF}) do Inc(J);
    if Count = N then begin
      Result:=Copy(S,I,J-I);
      Exit
    end;
    I:=J;
  end;
end;

function PosIdx(Substr,S: string; Index: Byte): Integer;
var
  i,p: Integer;
begin
  Result:=0;
  i:=0;
  p:=1;
  while (p>0) and (i<Index) do begin
    p:=PosEx(Substr,S,p);
    Inc(i);
  end;
  if (p>0) and (i=Index) then
    Result:=p;
end;

function DeleteFromLast(S: string; StoPAnsiChar: char; DeleteStoPAnsiChar: Boolean = True): string;
begin
  Result:=S;
  while Copy(Result,Length(Result),1)<>StoPAnsiChar do begin
    Delete(Result,Length(Result),1);
  end;
  if DeleteStoPAnsiChar then
    Delete(Result,Length(Result),1);
end;

function Capitalize(const Source: string): string;
begin
  Result:=Source;
  if Length(Result)>0 then
    Result:=Copy(AnsiUpperCase(Result),1,1)+Copy(Result,2,Length(Result)-1);
end;

function DeCapitalize(const Source: string): string;
begin
  Result:=Source;
  if Length(Result)>0 then
    Result:=Copy(AnsiLowerCase(Result),1,1)+Copy(Result,2,Length(Result)-1);
end;

function GetCharCount(ASource: string; AChar: char): Integer;
var
  i: Integer;
begin
  Result:=0;
  for i:=1 to Length(ASource) do
    if AChar=ASource[i] then
      Inc(Result);
end;

function IsValidNumber(S: string; var V: double): boolean;
var
  NumCode: integer;
  FirstSpace: integer;
begin
  FirstSpace:=Pos(' ', S);
  if FirstSpace > 0 then
    S:=Copy(S, 1, FirstSpace - 1);
  Val(S, V, NumCode);
  Result:=(NumCode = 0);
  if not Result then
  begin
    // Remove all thousands seperators
    S:=StringReplace(S, {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}ThousandSeparator, '', [rfReplaceAll]);
    // change DecimalSeperator to '.' because Val only recognizes that, not
    // the locale specific decimal char.  Stupid Val.
    S:=StringReplace(S, {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator, '.', [rfReplaceAll]);
    // and try again
    Val(S, V, NumCode);
    Result:=(NumCode = 0);
  End;
end;

// date conversion will fail if using long format, e.g. '1 January 1994'
function IsValidDateTime(const S: string; var D: TDateTime): boolean;
var
  i: integer;
  HasDate: boolean;
  HasTime: boolean;
begin
  // Check for two date seperators.  This is because some regions use a "-"
  //  to seperate dates, so if we just checked for one we would flag negative
  //  numbers as being dates.
  i:=Pos({$IFDEF FS}FormatSettings.{$ENDIF}DateSeparator, S);
  HasDate:=i > 0;
  if HasDate and (i <> Length(S)) then
    HasDate:=Pos({$IFDEF FS}FormatSettings.{$ENDIF}DateSeparator, Copy(S, i+1, Length(S)-i)) > 0;
  HasTime:=Pos({$IFDEF FS}FormatSettings.{$ENDIF}TimeSeparator, S) > 0;
  Result:=HasDate or HasTime;
  if Result then
  begin
    try
      if HasDate and HasTime then
        D:=StrToDateTime(S)
      else if HasDate then
        D:=StrToDate(S)
      else if HasTime then
        D:=StrToTime(S);
    except
      // Something failed to convert...
      D:=0;
      Result:=FALSE;
    end;
  end;
end; { IsValidDateTime }

function EncodeBase (I: Int64; Base: Byte): String;
var
  D,J: Int64;
  N: Byte;
const ConversionAlphabeth : String [36] = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
begin
  if I = 0 then begin
     Result:='0';
     exit;
  end;
  D:=Round (Power (Base, Trunc (Log10 (I) / Log10 (Base)) + 1));            // +1 to fix occasional real "fuzz"
  J:=I;
  Result:='';
  While D > 0 do begin
    N:=J div D;
    if (N <> 0) or (Result <> '') then                                      // "fuzz" bug
      Result:=Result + string(ConversionAlphabeth [N + 1]);
    J:=J mod D;
    D:=D div Base;
  end;
end;

function DequoteStr(Source: string; Quote: Char = '"'): string;
begin
  Result:=Source;
  if Length(Source)>1 then
    if (Source[1]=Quote) and (Source[Length(Source)]=Quote) then
      Result:=Copy(Source,2,Length(Source)-2);
end;

function StrToFloatEx(const S: string): Extended;
var
  ds: char;
begin
  try
    Result:=StrToFloat(S);
  except
    try
      ds:={$IFDEF FS}FormatSettings.{$ENDIF}DecimalSeparator;
      try
        Result:=StrToFloat(S);
      finally
        {$IFDEF FS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
      end;
    except
      Result:=0;
    end;
  end;
end;

function GetNameFromStr(ASource: string; ASep: string = '='): string;
var
  p: integer;
begin
  p:=Pos(ASep,ASource);
  if p>0 then
    Result:=Trim(Copy(ASource,1,p-1))
  else
    Result:=ASource;
end;

function ScanList(AText: string; AList: TStrings): Integer;
var
  i: Integer;
begin
  Result:=0;
  AText:=UpperCase(AText);
  for i:=0 to AList.Count-1 do
    if AText=UpperCase(AList[i]) then
      Inc(Result);
end;

procedure AddNameValue(AList: TStrings; AName,AValue: string; ASep: string = '=');
var
  i: Integer;
begin
  i:=ListNameExists(AList,AName);
  if i=-1 then
    AList.Add(Format('%s%s%s',[AName,ASep,AValue]))
  else
    AList[i]:=Format('%s%s%s',[AName,ASep,AValue]);
end;

function GetStrFromBuf(var Buffer: array of Byte; Len: Cardinal = 0): string;
var
  i,j :cardinal;
  a: ansistring;
  p: array[0..255] of char;
begin
  a:='';
  j:=0;
  i:=0;
  repeat
    if (buffer[i]<>0) then begin
      a:=a+AnsiChar(Chr(buffer[i]));
      j:=0;
    end else begin
      if Len=0 then
        Break;
      inc(j);
    end;
    inc(i);
  until (j>1) or (i=Len);
  {$IFDEF UNICODE}
  a:={$IFDEF RAD9PLUS}System.AnsiStrings.Trim(a);{$ELSE}AnsiStrings.Trim(a);{$ENDIF}
  {$ELSE}
  a:=Trim(a);
  {$ENDIF}
  OemToCharBuff(@a[1],@p[0],len);
  if len>0 then
    Result:=p
  else
    Result:=string(a);
end;

function GetStrFromCharBuf(Buffer: PAnsiChar; Len: Cardinal = 0): ansistring;
var
  i,j :cardinal;
begin
  result:='';
  j:=0;
  i:=0;
  repeat
    if (buffer[i]<>#0) then begin
      result:=Result+buffer[i];
      j:=0;
    end else begin
      if Len=0 then
        Break;
      inc(j);
    end;
    inc(i);
  until (j>1) or (i=Len);
end;

function GetStrFromWideCharBuf(Buffer: PWideChar; Len: Integer = 0): WideString;
var
  i,j :integer;
begin
  result:='';
  j:=0;
  i:=0;
  repeat
    if (buffer[i]<>#0) then begin
      result:=Result+buffer[i];
      j:=0;
    end else begin
      if Len=0 then
        Break;
      inc(j);
    end;
    inc(i);
  until (j>1) or (i=Len);
end;

function GetStrListFromWideCharBuf(Buffer: PWideChar; Len: Integer): WideString;
var
  i,j :integer;
  s: WideString;
begin
  result:='';
  j:=0;
  i:=0;
  s:='';
  repeat
    if (buffer[i]<>#0) then begin
      s:=s+buffer[i];
      j:=0;
    end else begin
      if Trim(s)<>'' then begin
        if Pos('=',s)=0 then begin
          s:='';
          break;
        end;
        Result:=Result+sLineBreak+s;
      end;
      s:='';
      inc(j);
    end;
    inc(i);
  until (j>2) or (i=Len);
  if Trim(s)<>'' then
    Result:=Result+sLineBreak+s;
  Result:=Trim(Result);
end;

function ListValue(AList: TStrings; AName: string; ASep: string = '='): string;
var
  i: integer;
begin
  Result:='';
  AName:=UpperCase(AName);
  for i:=0 to Alist.Count-1 do
    if UpperCase(GetNameFromStr(AList[i],ASep))=AName then begin
      Result:=GetValueFromStr(AList[i],ASep);
      Break;
    end;
end;

function ListName(AList: TStrings; AIndex: integer; ASep: string = '='): string;
begin
  Result:=GetNameFromStr(AList[AIndex],ASep);
end;

function ListValueFromIndex(AList: TStrings; AIndex: integer; ASep: string = '='): string;
begin
  Result:=GetValueFromStr(AList[AIndex],ASep);
end;

function ListIndexOfValue(AList: TStrings; const AValue: string; ASep: string = '='): integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to AList.Count-1 do
    if SameText(AValue,ListValueFromIndex(AList,i)) then begin
      Result:=i;
      Break;
    end;
end;

function ListNameExists(AList: TStrings; AName: string; ASep: string = '='): integer;
var
  i: integer;
begin
  Result:=-1;
  for i:=0 to Alist.Count-1 do
    if SameText(GetNameFromStr(AList[i],ASep),AName) then begin
      Result:=i;
      Break;
    end;
end;

function GetValueFromStr(ASource: string; ASep: string = '='): string;
var
  p: integer;
begin
  p:=Pos(ASep,ASource);
  if p>0 then
    Result:=Copy(ASource,p+Length(ASep),1024)
  else
    Result:='';
end;

function FormatTimer(ATime: Int64): string;
begin
  Result:=Format('%2.2d:%2.2d:%2.2d',[ATime div 3600,
                                      (ATime div 60) mod 60,
                                      ATime mod 60]);
end;

function Empty(Source: string): Boolean;
begin
  Result:=Length(Trim(Source))=0;
end;

function NormalizeNumber(Source: string): string;
begin
  if Empty(Source) then
    Result:='0'
  else
    Result:=StringReplace(Trim(Source),',','.',[rfIgnoreCase]);
end;

function NormalizeNumberEx(Source: string): string;
begin
  if Empty(Source) then
    Result:='NULL'
  else
    Result:=StringReplace(Trim(Source),',','.',[rfIgnoreCase]);
end;

function NormalizeNumberEx(Source: integer): string;
begin
  if Source=0 then
    Result:='NULL'
  else
    Result:=StringReplace(Trim(IntToStr(Source)),',','.',[rfIgnoreCase]);
end;

function NormalizeNumber(Source: Double; APrec: Byte = 2): string;
begin
  Result:=StringReplace(Trim(Format('%1.'+IntToStr(APrec)+'f',[Source])),',','.',[rfIgnoreCase]);
end;

function NormalizeDate(Source: string): string;
begin
  if Empty(Source) then
    Result:='NULL'
  else
    Result:=QuoteStr(Source);
end;

function NormalizeDataValue(AValue: Int64): string; overload;
begin
  if AValue shr 30>0 then
    Result:=Format('%d GB',[AValue shr 30])
  else if AValue shr 20>0 then
    Result:=Format('%d MB',[AValue shr 20])
  else if AValue shr 10>0 then
    Result:=Format('%d KB',[AValue shr 10])
  else
    Result:=Format('%d B',[AValue]);
end;

function NormalizeDataValue(AValue: Double): string; overload;
begin
  if Round(AValue) shr 30>0 then
    Result:=Format('%1.2f GB',[AValue/(1024*1024*1024)])
  else if Round(AValue) shr 20>0 then
    Result:=Format('%1.2f MB',[AValue/(1024*1024)])
  else if Round(AValue) shr 10>0 then
    Result:=Format('%1.2f KB',[AValue/1024])
  else
    Result:=Format('%1.2f B',[AValue]);
end;

function QuoteStr(Source: string; Quote: Char = ''''): string;
begin
  Result:=Source;
  if Copy(Source,1,1)=Quote then
    Exit;
  if Quote='''' then
    Result:=Quote+StringReplace(Source,'''','''''',[rfReplaceAll,rfIgnoreCase])+Quote
  else
    Result:=Quote+Source+Quote;
end;

function QuoteTrimStr(Source: string; Quote: Char = ''''): string;
begin
  Result:=Source;
  if Copy(Trim(Source),1,1)=Quote then
    Exit;
  if Quote='''' then
    Result:=''''+StringReplace(Trim(Source),'''','''''',[rfReplaceAll,rfIgnoreCase])+''''
  else
    Result:=Quote+Trim(Source)+Quote;
end;

function QuoteTrimStrEx(Source: string; Quote: Char = ''''): string;
begin
  Result:=Source;
  if Copy(Trim(Source),1,1)=Quote then
    Exit;
  if Empty(Source) then
    Result:='NULL'
  else begin
    if Quote='''' then
      Result:=''''+StringReplace(Trim(Source),'''','''''',[rfReplaceAll,rfIgnoreCase])+''''
    else
      Result:=Quote+Trim(Source)+Quote;
  end;
end;

function CustomSort(Value1,Value2: Variant): Integer;
var
  Str1, Str2: string;
  Dbl1,Dbl2: Double;
  Int1,Int2: Int64;
begin
  case TVarData(Value1).VType of
    varSmallInt,
    varInteger,
    varShortInt,
    varWord,
    varLongWord,
    varInt64,
    varByte


    : begin
      Int1:=Value1;
      Int2:=Value2;
      Result:=Sign(Int1-Int2);
    end;
    varSingle,
    varDouble,
    varCurrency,
    varDate: begin
      Dbl1:=Value1;
      Dbl2:=Value2;
      Result:=Sign(Dbl1-Dbl2);
    end;
  else begin
      Str1:=VarToStr(Value1);
      Str2:=VarToStr(Value2);
      Result:=AnsiCompareStr(Str1,Str2);
    end;
  end;
end;

function AdvancedCustomSort(Value1,Value2: string; ALimit: Byte = 25): Integer;
var
  Val1, Val2: double;
  Date1, Date2: TDateTime;
begin
  if (Length(Value1)<ALimit) then begin
    if (IsValidDateTime(Value1,Date1) and IsValidDateTime(Value2,Date2)) then
      Result:=CompareDateTime(Date1,Date2)
    else if (IsValidNumber(Value1,Val1) and IsValidNumber(Value2,Val2)) then
      Result:=CompareValue(Val1,Val2)
    else
      Result:=CompareText(Value1,Value2);
  end else
    Result:=CompareText(Value1,Value2);
end;

function PosRev(Substring: string; Source: string): integer;
var
  i,l: integer;
  s: string;
begin
  l:=Length(Substring);
  i:=Length(Source)-l;
  repeat
    s:=Copy(Source,i,l);
    Dec(i,l);
  until (CompareText(s,Substring)=0) or (i<1);
  if i>1 then
    Result:=i+l
  else
    Result:=0;
end;

procedure SplitVersion(ASource: string; var Major: Cardinal; var Minor: Cardinal);
var
  p: Integer;
begin
  Major:=0;
  Minor:=0;
  p:=Pos('.',ASource);
  if p>0 then begin
    try
      Major:=StrToInt(Copy(ASource,1,p-1));
      Minor:=StrToInt(Copy(ASource,p+1,255));
    except
    end;
  end;
end;

function TrimWideString(Source: WideString): WideString;
var
  p: integer;
begin
  p:=Pos(#0,Source);
  if p>0 then
    Result:=Copy(Source,1,p-1)
  else
    Result:=Source;

end;

procedure BufferToHexView(var Buffer; DataSize: Int64; AList: TStringList);
var
  i: Integer;
  st,sh: string;
  c: ansichar;
begin
  for i:=0 to DataSize-1 do begin
    c:=PAnsiChar(@Buffer)[i];
    if IsCharAlphaNumeric(Char(c)) then
      st:=st+string(c)
    else
      st:=st+'.';
    sh:=sh+IntToHex(Ord(c),2);
    if Length(st) mod 2 = 0 then
      sh:=sh+' ';
    if Length(st)=16 then begin
      AList.Add(Format('%s  %s',[sh,st]));
      st:='';
      sh:='';
    end;
  end;
  if Length(st)>0 then
    AList.Add(Format('%-40s  %s',[sh,st]));
end;

function CountText(ASource,AText: string): Word;
var
  p: Integer;
begin
  Result:=0;
  p:=PosText(AText,ASource);
  while p>0 do begin
    Inc(Result);
    Delete(ASource,1,p+Length(AText));
    p:=PosText(AText,ASource);
  end;
end;

function PosText(const ASubText, AText: string; ACaseSensitive: Boolean = False; AWholeWord: Boolean = False): Integer;
var
  s,r: PChar;
  o: TStringSearchOptions;
begin
  o:=[soDown];
  if ACaseSensitive then
    o:=o+[soMatchCase];
  if AWholeWord then
    o:=o+[soWholeWord];
  s:=PChar(AText);
  r:=SearchBuf(s,Length(AText),0,0,ASubText,o);
  if r=nil then
    Result:=0
  else
    Result:=r-s+1;
end;

function PosTextList(AList: TStrings; const AText: string): integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to AList.Count-1 do
    if PosText(AList[i],AText)>0 then begin
      Result:=i;
      Break;
    end;
end;

function PosTextList(const ASubCommaText, AText: string): Integer; overload;
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    sl.CommaText:=ASubCommaText;
    Result:=PosTextList(sl,AText);
  finally
    sl.Free;
  end;
end;

function StreamToHex(ASource: TStream): AnsiString;
var
  b: Byte;
  i: Integer;
  s: ansistring;
begin
  ASource.Position:=0;
  SetLength(Result,ASource.Size*2);
  i:=1;
  while ASource.Position<ASource.Size do begin
    ASource.read(b,SizeOf(b));
    s:={$IFDEF UNICODE}WideToAnsi{$ENDIF}(IntToHex(b,2));
    Result[i]:=s[1];
    Result[i+1]:=s[2];
    Inc(i,2);
  end;
end;

{$if defined(RAD5PLUS) or defined(FPC)}
function BytesToHex(ASource: TBytes): string;
var
  i: Integer;
begin
  Result:='';
  for i:=0 to High(ASource) do
    Result:=Result+IntToHex(ASource[i],2);
end;

function HexToBytes(ASource: string): TBytes;
var
  i,j: integer;
begin
  SetLength(Result,Length(ASource) div 2);
  i:=1;
  j:=0;
  while i<Length(ASource) do begin
    Result[j]:=StrToInt('$'+Copy(ASource,i,2));
    inc(j);
    inc(i,2);
  end;
end;

function BytesToString(ASource: TBytes): string;
begin
  SetString(Result,PAnsiChar(@ASource[0]),Length(ASource));
end;

(*
function StringToBytes(const ASource: string): TBytes;
begin
  Result:={$IFDEF UNICODE}StringToBytesW(ASource){$ELSE}StringToBytesA(ASource){$ENDIF};
end;
*)

function StringToBytes(const ASource: WideString): TBytes;
begin
  SetLength(Result,Length(ASource)*sizeof(char));
  Move(ASource[1],Result[0],Length(Result));
end;

function StringToBytes(const ASource: AnsiString): TBytes;
begin
  SetLength(Result,Length(ASource));
  Move(ASource[1],Result[0],Length(Result));
end;
{$ifend}

function StripUnprintableW(const ASource: widestring; SubstChar: WideChar = '_'): widestring;
var
  i: Integer;
begin
  Result:='';
  for i:=1 to Length(ASource) do
    if {$IFDEF UNICODE}CharInSet(ASource[i],[WideChar(#0)..WideChar(#31)]){$ELSE}ASource[i] in [WideChar(#0)..WideChar(#31)]{$ENDIF} then begin
      if SubstChar<>#0 then
        Result:=Result+SubstChar
    end else
      Result:=Result+ASource[i];
end;

function StripUnprintable(const ASource: string; SubstChar: Char = '_'): string;
var
  i: Integer;
begin
  Result:='';
  for i:=1 to Length(ASource) do
    if {$IFDEF UNICODE}CharInSet(ASource[i],[#0..#31]){$ELSE}ASource[i] in [#0..#31]{$ENDIF} then begin
      if SubstChar<>#0 then
        Result:=Result+SubstChar
    end else
      Result:=Result+ASource[i];
end;

function StripUnprintableA(const ASource: ansistring; SubstChar: AnsiChar = '_'): ansistring;
var
  i: Integer;
begin
  Result:='';
  for i:=1 to Length(ASource) do
    if ASource[i] in [#0..#31] then begin
      if SubstChar<>#0 then
        Result:=Result+SubstChar
    end else
      Result:=Result+ASource[i];
end;

function DeleteLast(const S: string; Count: integer = 1): string;
begin
  Result:=S;
  Delete(Result,Length(Result)-Count,Count);
end;

procedure RemoveDuplicates(const stringList: TStringList);
var
  buffer: TStringList;
  cnt: Integer;
begin
  stringList.Sort;
  buffer:=TStringList.Create;
  try
    buffer.Sorted:=True;
    buffer.Duplicates:=dupIgnore;
    buffer.BeginUpdate;
    for cnt:=0 to stringList.Count - 1 do
      buffer.Add(stringList[cnt]) ;
    buffer.EndUpdate;
    stringList.Assign(buffer) ;
  finally
    FreeandNil(buffer) ;
  end;
end;

procedure SwapWideChars(p: PWideChar);
begin
  while p^<>#0000 do begin
    p^:=WideChar(Swap(Word(p^)));
    Inc(p);
  end;
end;

{$IFNDEF RAD6PLUS}
function LoadUnicodeFile(const AFileName: string; AStrings: TStrings): TTextType;
var
  ms: TMemoryStream;
  wc: WideChar;
  pwc: PWideChar;
begin
  Result:=ANSI;
  ms:=TMemoryStream.Create;
  try
    ms.LoadFromFile(AFilename);
    ms.Seek(0,soFromEnd);
    wc:=#0000;
    ms.Write(wc,sizeof(wc));
    pwc:=ms.Memory;
    if pwc^=#$FEFF then begin// normal byte order mark
      inc(pwc);
      Result:=UTF16LE;
    end else
      if pwc^= #$FFFE then begin // byte order is big-endian
        SwapWideChars(pwc);
        Inc(pwc);
        Result:=UTF16BE;
      end else begin
        if (pwc^=#$BBEF) and (Lo(word(pwc[1]))=$BF) then begin// utf-8
          pwc:=AllocMem(ms.Size);
          if UTF8ToUnicode(pwc,Cardinal(-1),PAnsiChar(ms.Memory)+3,ms.Size)>-1 then
            Result:=UTF8;
        end;
      end;  // else no byte order mark

    if (Result=ANSI) then begin
      AStrings.LoadFromFile(AFilename);
      if (Abs(Length(AStrings.Text)-ms.Size)>2) then
        Result:=UTF16;
    end;

    if Result<>ANSI then
      AStrings.Text:=WideCharToString(pwc);
  finally
    ms.Free;
    try Freemem(pwc) except end;
  end;
end;

procedure SaveAsUnicode(const AFilename: string; AStrings: TStrings; ATextType: TTextType = UTF16LE);
var
  s: string;
  pwc: PWideChar;
  fs: TFileStream;
  byteorder_marker: Word;
  byteorder_marker1: Byte;
  sz: Cardinal;
begin
  if ATextType=ANSI then begin
    AStrings.SaveToFile(AFilename);
    Exit;
  end;
  sz:=Length(AStrings.Text)*2+1;
  pwc:=AllocMem(sz);
  fs:=TFileStream.Create(AFilename,fmCreate);
  try
    StringToWideChar(AStrings.Text,pwc,sz);
    if AStrings.Count=0 then
      Exit;
    case ATextType of
      UTF8: begin
        byteorder_marker:=$BBEF;
        byteorder_marker1:=$BF;
        fs.WriteBuffer(byteorder_marker,sizeof(byteorder_marker));
        fs.WriteBuffer(byteorder_marker1,sizeof(byteorder_marker1));
        s:=UTF8Encode(AStrings.Text);
        fs.WriteBuffer(s[1],Length(s)*SizeOf(s[1]));
      end;
      UTF16LE: begin
        byteorder_marker:=$FEFF;
        fs.WriteBuffer(byteorder_marker,sizeof(byteorder_marker));
        fs.WriteBuffer(pwc^,sz);
      end;
      UTF16BE: begin
        byteorder_marker:=$FFFE;
        fs.WriteBuffer(byteorder_marker,sizeof(byteorder_marker));
        SwapWideChars(pwc);
        fs.WriteBuffer(pwc^,sz);
      end
      else fs.WriteBuffer(pwc^,sz);
    end;

  finally
    Freemem(pwc);
    fs.Free;
  end;
end;
{$ENDIF}

function Int2Bin(A: Int64; Len: Byte = 8): string;
var
  i: Int64;
begin
  i:=1;
  Result:='';
  while ((i<=Abs(A)) and (i<>0)) or (Length(Result)<Len) do begin
    if A and i = i then
      Result:='1'+Result
    else
      Result:='0'+Result;
    i:=i*2;
  end;
end;

function ByteToBin(AValue : Byte) : string;
const
  Bits: array [1 .. 8] of Byte = (128, 64, 32, 16, 8, 4, 2, 1);
var
  i: Integer;
begin
  Result:='00000000';
  if (AValue<>0) then
    for i:=1 to 8 do
      if (AValue and Bits[i]) <> 0 then
        Result[i]:='1';
end;

function WordToBin(AValue : Word) : string;
const
  Bits : array [1 .. 16] of Word = (32768, 16384, 8192, 4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1);
var
  i : Integer;
begin
  Result:='0000000000000000';
  if (AValue<>0) then
    for i:=1 to 16 do
      if (AValue and Bits[i]) <> 0 then
        Result[i]:='1';
end;



function Oct2Int(Value: string): Longint;
 var
    i: Integer;
   int: Integer;
 begin
   int:=0;
   for i:=1 to Length(Value) do
   begin
     int:=int * 8 + StrToInt(Copy(Value, i, 1));
   end;
   Result:=int;
 end;


function Int2Oct(Value: Longint; digits: Integer): string;
 var
   rest: Longint;
   oct: string;
   i: Integer;
 begin
   oct:='';
   while Value <> 0 do
   begin
     rest :=Value mod 8;
     Value:=Value div 8;
     oct:=IntToStr(rest) + oct;
   end;
   for i:=Length(oct) + 1 to digits do
     oct:='0' + oct;
   Result:=oct;
 end;

function FindNumber(AText: string): Integer;
var
  s: string;
  i: Integer;
begin
  s:='';
  for i:=1 to Length(AText) do
    if {$IFDEF UNICODE}CharInSet(AText[i],[#48..#57]){$ELSE}AText[i] in [#48..#57]{$ENDIF} then
      s:=s+AText[i]
    else
      if s<>'' then
        Break;
  Result:=StrToIntDef(s,0);        
end;

function ExtractDomainFromURL(AURL: string; AOnly2nd: Boolean = False): string;
var
  s: string;
  i,p,c: Integer;
begin
  s:=AURL;
  s:=StringReplace(s,'http://','',[rfIgnoreCase]);
  s:=StringReplace(s,'https://','',[rfIgnoreCase]);
  s:=StringReplace(s,'ftp://','',[rfIgnoreCase]);
  s:=StringReplace(s,'ftps://','',[rfIgnoreCase]);
  p:=Pos('/',s);
  if p>0 then
    Delete(s,p,Length(s));
  c:=CountText(s,'.');
  if (PosText('.co.',s)>0) or (PosText('.com.',s)>0) then
    Dec(c);
  if AOnly2nd then
    c:=c-2
  else
    c:=c-3;
  for i:=0 to c do begin
    p:=Pos('.',s);
    Delete(s,1,p);
  end;
  Result:=s;
end;

function ExtractUrlFileName(const AUrl: string): string;
var
  i: Integer;
begin
  i:=LastDelimiter('/',AUrl);
  Result:=Copy(AUrl,i+1,Length(AUrl)-(i));
end;

function ExtractTagValue(const Tag: string; PropName: string): string;
var
  I: Integer;
begin
  Result:='';
  PropName:=UpperCase(PropName);
  if Pos(PropName, UpperCase(Tag)) > 0 then
  begin
    Result:=Copy(Tag, Pos(PropName, UpperCase(Tag)) + Length(PropName), Length(Tag));
   if Pos('"', Result) <> 0 then
   begin
     Result:=Copy(Result, Pos('"', Result) + 1, Length(Result));
     Result:=Copy(Result, 1, Pos('"', Result) - 1);
   end
   else
   if Pos('''', Result) <> 0 then
   begin
     Result:=Copy(Result, Pos('''', Result) + 1, Length(Result));
     Result:=Copy(Result, 1, Pos('''', Result) - 1);
   end
   else
   begin
     Result:=Trim(Result);
     Delete(Result, 1, 1);
     Result:=Trim(Result);
     I:=1;
     while (I < Length(Result)) and (Result[I+1] <> ' ') do
       Inc(I);
     Result:=Copy(Result, 1, I);
   end;
  end;
end;

function StripSpaces(ASource: string): string;
var
  l,c,i: Integer;
begin
  c:=0;
  l:=Length(ASource);
  Result:='';
  for i:=1 to l do
    if ASource[i]=' ' then begin
      Inc(c);
      if c<2 then
        Result:=Result+ASource[i];
    end else begin
      Result:=Result+ASource[i];
      c:=0;
    end;
end;

function StripNPAnsiChars(ASource: string): string;
var
  l,i: Integer;
begin
  l:=Length(ASource);
  Result:='';
  for i:=1 to l do
    if not (Ord(ASource[i]) in [0..9,11,12,14..31]) then
      Result:=Result+ASource[i];
end;

function WideToAnsi(const ws: WideString; codePage: Word = CP_ACP): AnsiString;
var
  l: integer;
  f: Cardinal;
begin
  f:=WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR;
  if codepage=CP_UTF8 then
    f:=0;
  if ws = '' then
    Result:=''
  else begin
    l:=WideCharToMultiByte(codePage,f,@ws[1],-1,nil,0,nil,nil);
    SetLength(Result,l-1);
    if l>1 then
      WideCharToMultiByte(codePage,f,@ws[1],-1,@Result[1],l-1,nil,nil);
  end;
end;


function AnsiToWide(const s: AnsiString; codePage: Word = CP_ACP): WideString;
var
  l: integer;
  f: Cardinal;
begin
  f:=MB_PRECOMPOSED;
  if codepage=CP_UTF8 then
    f:=0;
  if s = '' then
    Result:=''
  else begin
    l:=MultiByteToWideChar(codePage,f,PAnsiChar(@s[1]),-1,nil,0);
    SetLength(Result,l-1);
    if l>1 then
      MultiByteToWideChar(CodePage,f,PAnsiChar(@s[1]),-1,PWideChar(@Result[1]),l-1);
  end;
end;

function StripHTML(S: string): string;
var
  TagBegin, TagEnd, TagLength: integer;
begin
  S:=StringReplace(S,'<br>',#13#10,[rfReplaceAll,rfIgnorecase]);
  S:=StringReplace(S,'<br/>',#13#10,[rfReplaceAll,rfIgnorecase]);
  S:=StringReplace(S,'<br />',#13#10,[rfReplaceAll,rfIgnorecase]);
  TagBegin:=Pos('<',S);
  while (TagBegin>0) do begin
    TagEnd:=PosEx('>',S,TagBegin);
    TagLength:=TagEnd-TagBegin+1;
    Delete(S,TagBegin,TagLength);
    TagBegin:=Pos('<',S);
  end;
  S:=StringReplace(S,'&lt;','<',[rfReplaceAll,rfIgnorecase]);
  S:=StringReplace(S,'&gt;','>',[rfReplaceAll,rfIgnorecase]);
  S:=StringReplace(S,'&nbsp;',' ',[rfReplaceAll,rfIgnorecase]);
  S:=StringReplace(S,'&amp;','&',[rfReplaceAll,rfIgnorecase]);
  Result:=Trim(S);
end;

function IsCap(AChar: Char): Boolean;
begin
  Result:=AChar=UpCase(AChar);
end;

function SplitByCaps(const ASource: string): string;
var
  l,i: Integer;
begin
  Result:='';
  l:=Length(ASource);
  if l=0 then
    Exit;
  for i:=1 to l do begin
    if (i<l) and (IsCap(ASource[i]) and not IsCap(ASource[i+1])) then
      Result:=Result+' ';
    Result:=Result+ASource[i];
  end;
  Result:=Trim(Result);
end;

function Alter(AValue: string; ADefault: string = ''): string;
begin
  Result:=AValue;
  if Result='' then
    Result:=ADefault;
end;

function CheckXMLValue(AValue: string): string;
var
  i: Integer;
  c: Char;
begin
  Result:=Trim(AValue);
  try
    StrToFloat(Result);
    Result:=StringReplace(Result,',','.',[rfIgnoreCase])
  except
    i:=1;
    while i<=Length(Result) do begin
      c:=Result[i];
      if {$IFDEF UNICODE}CharInSet(c,[#0..#31]){$ELSE}(c in [#0..#31]){$ENDIF} then begin
        Delete(Result,i,1);
        Insert('_',Result,i);
      end else if (c='&') then begin
        Delete(Result,i,1);
        Insert('&amp;',Result,i);
      end else if (c='"') then begin
        Delete(Result,i,1);
        Insert('&quot;',Result,i);
      end else if (c='''') then begin
        Delete(Result,i,1);
        Insert('&apos;',Result,i);
      end else if (c='>') then begin
        Delete(Result,i,1);
        Insert('&gt;',Result,i);
      end else if (c='<') then begin
        Delete(Result,i,1);
        Insert('&lt;',Result,i);
      end else if (c='™') then begin
        Delete(Result,i,1);
        Insert('&#x2122;',Result,i);
      end else if (c='®') then begin
        Delete(Result,i,1);
        Insert('&#x00AE;',Result,i);
      end else if (c='©') then begin
        Delete(Result,i,1);
        Insert('&#xA9;',Result,i);
      end;
      Inc(i);
    end;
  end;
end;

function FastStringReplace(const ASource, APattern, AReplace: string): string;
var
  i: Integer;
  s: string;
begin
  s:=ASource;
  Result:='';
  repeat
    i:=Pos(APattern,s);
    if i>0 then begin
      Result:=Result+Copy(s,1,i- 1)+AReplace;
      s:=Copy(s,i+Length(APattern),MaxInt);
    end else
      Result:=Result+s;
  until i<=0;
end;

function FormatAmount(AAmount: Double; ACurrency: string = 'CZK'; ARate: Double = 1): string;
var
  s,f: string;
begin
  s:=ACurrency;
  s:=FormatCurrency(s);
  if SameText(ACurrency,'CZK') then
    f:='%1.2n %s'
  else
    f:='%1:s %0:1.2n';
  if (Trim(ACurrency)<>'') and (ARate>0) then
    Result:=Format(f,[AAmount/ARate,s])
  else
    Result:=Format(f,[AAmount,s]);
end;

function FormatCurrency(ACurrency: string): string;
begin
  Result:=ACurrency;
  if SameText(ACurrency,'EUR') then
    Result:='€'
  else if SameText(ACurrency,'CZK') then
    Result:='Kè'
  else if SameText(ACurrency,'USD') then
    Result:='$'
  else if SameText(ACurrency,'PLZ') then
    Result:='Z³'
  else if SameText(ACurrency,'CHF') then
    Result:='Chf'
  else if SameText(ACurrency,'GBP') then
    Result:='L';
end;

function PosLast(const ASubStr,AStr: string): Integer;
begin
  Result:=PosNth(ASubstr,AStr,Length(AStr));
end;

function PosNth(const ASubStr,AStr: string; APos: Cardinal): Integer;
var
  s: string;
  p,idx: integer;
  i: Cardinal;
begin
  Result:=0;
  if (ASubStr='') or (AStr='') then
    Exit;
  i:=0;
  s:=AStr;
  idx:=0;
  while (i<APos) do begin
    p:=Pos(ASubStr,s);
    if (p=0) then
      Break;
    idx:=idx+p;
    s:=Copy(AStr,idx+1,Length(AStr)-idx);
    Inc(i);
  end;
  Result:=idx;
end;

function IfText(ACondition: boolean; const ATrueValue,AFalseValue: string): string;
begin
  if ACondition then
    Result:=ATrueValue
  else
    Result:=AFalseValue;
end;


end.
