{*******************************************************}
{               MiTeC Common Routines                   }
{                    RC4 routines                       }
{                                                       }
{                                                       }
{          Copyright (c) 1997-2013 Michal Mutl          }
{                                                       }
{*******************************************************}

{$I Compilers.inc}

unit MiTeC_RC4;

interface

uses {$IFDEF RAD9PLUS}
     WinAPi.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

type
  TRC4Context = record
    D: array[Byte] of Byte;
    I,J: Byte;
  end;

procedure RC4Init(var RC4: TRC4Context; const AKey: string);
procedure RC4Done(var RC4: TRC4Context);
procedure RC4Code(var RC4: TRC4Context; const Source; var Dest; Count: Integer); overload;
function RC4Code(var RC4: TRC4Context; const AValue: string): string; overload;
function RC4Code(const AValue, APassword: string): string; overload;
procedure RC4FileCode(AInFile,AOutFile,APassword: string);

implementation

function WideToAnsi(const ws: WideString; codePage: Word = CP_ACP): AnsiString;
var
  l: integer;
  f: Cardinal;
begin
  f:=WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR;
  if codepage=CP_UTF8 then
    f:=0;
  if ws = '' then
    Result := ''
  else begin
    l := WideCharToMultiByte(codePage,f,@ws[1],-1,nil,0,nil,nil);
    SetLength(Result,l-1);
    if l>1 then
      WideCharToMultiByte(codePage,f,@ws[1],-1,@Result[1],l-1,nil,nil);
  end;
end;

procedure RC4Init(var RC4: TRC4Context; const AKey: string);
var
  R,S,T,K: Byte;
  U,L: Integer;
  Key: AnsiString;
begin
{$R-}
{$Q-}
  Key:={$IFDEF UNICODE}WideToAnsi{$ENDIF}(AKey);
  L:=Length(Key);
  with RC4 do begin
    I:=0;
    J:=0;
    for S:=0 to 255 do
      D[S]:=S;
    R:=0;
    U:=0;
    for S:=0 to 255 do begin
      if U<L then
        K:=PByteArray(Key)[U]
      else
        K:=0;
      Inc(U);
      if U>=L then
        U:=0;

      Inc(R, D[S] + K);
      T:=D[S];
      D[S]:=D[R];
      D[R]:=T;
    end;
  end;
end;

procedure RC4Done(var RC4: TRC4Context);
begin
  FillChar(RC4,SizeOf(RC4),0);
end;

procedure RC4Code(var RC4: TRC4Context; const Source; var Dest; Count: Integer); overload;
var
  S: Integer;
  T: Byte;
begin
  with RC4 do
    for S:=0 to Count-1 do begin
      Inc(I);
      T:=D[I];
      Inc(J,T);
      D[I]:=D[J];
      D[J]:=T;
      Inc(T,D[I]);
      TByteArray(Dest)[S]:=TByteArray(Source)[S] xor D[T];
    end;
end;

function RC4Code(var RC4: TRC4Context; const AValue: string): string;
var
  Count: Integer;
  Value,r: AnsiString;
begin
  Value:={$IFDEF UNICODE}WideToAnsi{$ENDIF}(AValue);
  Count:=Length(Value);
  SetLength(r,Count);
  RC4Code(RC4,Value[1],r[1],Count);
  Result:=string(r);
end;

function RC4Code(const AValue, APassword: string): string; overload;
var
  RC4: TRC4Context;
begin
  RC4Init(RC4,APassword);
  try
    Result:=RC4Code(RC4,AValue);
  finally
    RC4Done(RC4);
  end;
end;

procedure RC4FileCode(AInFile,AOutFile,APassword: string);
var
  RC4: TRC4Context;
  fs: TFileStream;
  ms: TMemoryStream;
  s,d: TByteArray;
  sz: Integer;
begin
  ms:=TMemoryStream.Create;
  RC4Init(RC4,APassword);
  try
    fs:=TFileStream.Create(AInFile,fmOpenReadWrite or fmShareExclusive);
    try
      fs.Position:=0;
      ms.Position:=0;
      while fs.Position<fs.Size do begin
        sz:=SizeOf(s);
        if fs.Size-fs.Position<sz then
          sz:=fs.Size-fs.Position;
        fs.read(s,sz);
        RC4Code(RC4,s,d,sz);
        ms.Write(d,sz);
      end;
      ms.Position:=0;
      fs.Position:=0;
    finally
      RC4Done(RC4);
      fs.Free;
    end;
    ms.SaveToFile(AOutFile);
  finally
    ms.Free;
  end;
end;

end.
