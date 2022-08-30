{*******************************************************}
{               MiTeC Common Routines                   }
{               Wide String routines                    }
{                                                       }
{        Copyright (c) 2009-2016 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_WideStrings;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

type
  TTextType = (ANSI, UTF16LE, UTF16BE, UTF16, UTF8);

  CharSet = set of AnsiChar;

  TWString = record
    WString: WideString;
  end;

  TWideStrings = class
  private
    FWideStringList: TList;
    function Get(Index: Integer): WideString;
    procedure Put(Index: Integer; const S: WideString);
  public
    constructor Create;
    destructor Destroy; override;
    function  Count: Integer;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    function Add(const S: WideString): Integer;
    function IndexOf(const S: WideString): Integer;
    function IndexOfIgnoreCase(const S: WideString): Integer;
    procedure Insert(AIndex: Integer; const S: WideString);
    property Strings[Index: Integer]: WideString read Get write Put; default;
  end;

function LoadUnicodeFile(const AFileName: string; AStrings: TStrings): TTextType;
procedure SaveAsUnicode(const AFilename: string; AStrings: TStrings; ATextType: TTextType = UTF16LE);

implementation

procedure SwapWideChars(p: PWideChar);
begin
  while p^<>#0000 do begin
    p^:=WideChar(Swap(Word(p^)));
    Inc(p);
  end;
end;

constructor TWideStrings.Create;
begin
  FWideStringList:=TList.Create;
end;

procedure TWideStrings.Delete(AIndex: Integer);
begin
  FWideStringList.Delete(AIndex);
end;

destructor TWideStrings.Destroy;
var
  i: Integer;
  PWStr: ^TWString;
begin
  for i:=0 to FWideStringList.Count-1 do begin
    PWStr:=FWideStringList.Items[i];
    if PWStr<>nil then
      Dispose(PWStr);
  end;
  FWideStringList.Free;
  inherited Destroy;
end;

function TWideStrings.Get(Index: Integer): WideString;
var
  PWStr: ^TWString;
begin
  Result:='';
  if ((Index>=0) and (Index<FWideStringList.Count)) then begin
    PWStr:=FWideStringList.Items[Index];
    if PWStr<>nil then
      Result:=PWStr^.WString;
  end;
end;

procedure TWideStrings.Put(Index: Integer; const S: WideString);
begin
  Insert(Index,S);
end;

function TWideStrings.Add(const S: WideString): Integer;
var
  PWStr: ^TWString;
begin
  New(PWStr);
  PWStr^.WString:=S;
  Result:=FWideStringList.Add(PWStr);
end;

function TWideStrings.IndexOfIgnoreCase(const S: WideString): Integer;
var
  i: Integer;
  PWStr: ^TWString;
begin
  Result:=-1;
  for i:=0 to FWideStringList.Count-1 do begin
    PWStr:=FWideStringList.Items[i];
    if PWStr<>nil then begin
      if SameText(S,PWStr^.WString) then begin
        Result:=i;
        break;
      end;
    end;
  end;
end;

function TWideStrings.IndexOf(const S: WideString): Integer;
var
  i: Integer;
  PWStr: ^TWString;
begin
  Result:=-1;
  for i:=0 to FWideStringList.Count-1 do begin
    PWStr:=FWideStringList.Items[i];
    if PWStr<>nil then begin
      if S=PWStr^.WString then begin
        Result:=i;
        break;
      end;
    end;
  end;
end;

function TWideStrings.Count: Integer;
begin
  Result:=FWideStringList.Count;
end;

procedure TWideStrings.Clear;
var
  i: Integer;
  PWStr: ^TWString;
begin
  for i:=0 to FWideStringList.Count-1 do begin
    PWStr:=FWideStringList.Items[i];
    if PWStr<>nil then
      Dispose(PWStr);
  end;
  FWideStringList.Clear;
end;

procedure TWideStrings.Insert(AIndex: Integer; const S: WideString);
var
  PWStr: ^TWString;
begin
  if((AIndex<0) or (AIndex>FWideStringList.Count)) then
    raise Exception.Create('WideString index out of bounds.');
  if AIndex<FWideStringList.Count then begin
    PWStr:=FWideStringList.Items[AIndex];
    if PWStr<>nil then
      PWStr.WString:=S;
  end
  else
    Add(S);
end;

function LoadUnicodeFile;
var
  ms: TMemoryStream;
  wc: WideChar;
  pwc: PWideChar;
begin
  pwc:=nil;
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
          if UTF8ToUnicode(pwc,Cardinal(-1),PAnsiChar(ms.Memory)+3,ms.Size)<>Cardinal(-1) then
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

procedure SaveAsUnicode;
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
        s:=string(UTF8Encode(AStrings.Text));
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

end.
