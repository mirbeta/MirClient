{*******************************************************}
{                MiTeC Simple Crypt                     }
{         Simple string XOR and ROT encryption          }
{                                                       }
{         Copyright (c) 2014-2015 Michal Mutl           }
{                                                       }
{*******************************************************}

{$I Compilers.inc}

unit MiTeC_SimpleCrypt;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils
     {$ELSE}
     Windows, SysUtils
     {$ENDIF}
     ;

const
  binC1 = 51265;
  binC2 = 21424;

  dbC1 = 51855;
  dbC2 = 21729;

  cKey1 = 210;
  cKey2 = 211;
  cKey3 = 100;

function EncryptString(const s: string; Key: Integer = cKey1; AC1: Integer = dbC1; AC2: Integer = dbC2): string;
function DecryptString(const s: string; Key: Integer = cKey1; AC1: Integer = dbC1; AC2: Integer = dbC2): string;

function EncryptStringA(const S: AnsiString; Key,AC1,AC2: LongInt): AnsiString;
function DecryptStringA(const S: AnsiString; Key,AC1,AC2: LongInt): AnsiString;

function EncryptStringW(const S: WideString; Key,AC1,AC2: LongInt): WideString;
function DecryptStringW(const S: WideString; Key,AC1,AC2: LongInt): WideString;

function RotateBy(AStr: String; const ARotate: Integer = 47): string;

implementation

function EncryptStringA(const S: AnsiString; Key,AC1,AC2: LongInt): AnsiString;
var
  i,l: byte;
begin
  Result:='';
  for i:=1 to Length(S) do begin
    l:=byte(S[i]) xor (Key and $07);
    if l = 35 then
      l:=127;
    Result:=Result+ansichar(l);
    Key:=(byte(Result[i])+Key)*AC1+AC2;
  end;
end;

function DecryptStringA(const S: AnsiString; Key,AC1,AC2: LongInt): AnsiString;
var
  i,l: byte;
begin
  Result:='';
  for i:=1 to Length(S) do begin
    l:=byte(S[i]);
    if l=127 then
      l:=35;
    Result:=Result+ansichar(l xor (Key and $07));
    Key:=(byte(S[i])+Key)*AC1+AC2;
  end;
end;

function EncryptStringW(const S: WideString; Key,AC1,AC2: LongInt): WideString;
var
  i,l: word;
begin
  Result:='';
  for i:= 1 to Length(s) do begin
    l:=word(S[i]) xor (Key and $07);
    if l = 35 then
      l:=127;
    Result:=Result+widechar(l);
    Key:=(word(Result[i])+Key)*AC1+AC2;
  end;
end;

function DecryptStringW(const S: WideString; Key,AC1,AC2: LongInt): WideString;
var
  i,l: word;
begin
  Result:='';
  for i:=1 to Length(s) do begin
    l:=word(s[i]);
    if l=127 then
      l:=35;
    Result:=Result+widechar(l xor (Key and $07));
    Key:=(word(S[i])+Key)*AC1+AC2;
  end;
end;

function EncryptString;
begin
  if Length(s)>256 then
    raise Exception.Create('Cannot encrypt string longer than 256 chars.');
  {$IFDEF UNICODE}
  Result:=EncryptStringW(S,Key,AC1,AC2);
  {$ELSE}
  Result:=EncryptStringA(S,Key,AC1,AC2);
  {$ENDIF}
end;

function DecryptString;
begin
  {$IFDEF UNICODE}
  Result:=DecryptStringW(S,Key,AC1,AC2);
  {$ELSE}
  Result:=DecryptStringA(S,Key,AC1,AC2);
  {$ENDIF}
end;

function RotateBy(AStr: String; const ARotate: Integer = 47): String;

  function RotateChar(AChr: Char; ARotate: Integer): Char;
  var
    iStart, iRotate: Integer;
  begin
    iStart := 0;
    case ARotate of
       5: if AChr in ['0'..'9'] then
          begin
            iStart := 48;                                   // '0'
            iRotate := 5;
          end;
      13: if AChr in ['A'..'Z'] then
          begin
            iStart := 65;                                   // 'A'
            iRotate := 13;
          end
          else if Achr in ['a'..'z'] then
          begin
            iStart := 97;                                   // 'a'
            iRotate := 13;
          end;
      18: if AChr in ['0'..'9'] then
          begin
            iStart := 48;                                   // '0'
            iRotate := 5;
          end
          else if AChr in ['A'..'Z'] then
          begin
            iStart := 65;                                   // 'A'
            iRotate := 13;
          end
          else if AChr in ['a'..'z'] then
          begin
            iStart := 97;                                   // 'a'
            iRotate := 13;
          end;
      47: if AChr in ['!'..'~'] then
          begin
            iStart := 33;                                   // '!'
            iRotate := 47;
          end
    end;
   
    if iStart>0 then
      Result := Chr(iStart + ((Ord(AChr) - iStart + iRotate) mod (iRotate * 2)))
    else
      Result := AChr;
  end;
  
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AStr) do
    Result := Result + RotateChar(AStr[I], ARotate);
end;


end.