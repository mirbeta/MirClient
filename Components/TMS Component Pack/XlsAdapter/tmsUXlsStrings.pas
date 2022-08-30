unit tmsUXlsStrings;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses tmsXlsMessages, SysUtils, Classes, tmsUXlsBaseRecords, tmsUFlxMessages;
type
  TStrLenLength= 1..2;
  TCharSize=1..2;

  TExcelString= class
  private
    // Common data
    StrLenLength: TStrLenLength; //this stores if it's a one or two bytes length
  public
    StrLen: word;
    OptionFlags: byte;

    WideData: UTF16String;
    ShortData: AnsiString;

    //Rich text
    NumberRichTextFormats: word;
    RichTextFormats: PArrayOfByte;

    //FarEast
    FarEastDataSize: LongWord;
    FarEastData: PArrayOfByte;
    function GetValue: UTF16String;

  public
    constructor Create(const aStrLenLength: TStrLenLength; var aRecord: TBaseRecord; var Ofs: integer);overload;
    constructor Create(const aStrLenLength: TStrLenLength;const s: UTF16String; const ForceWide: boolean= false); overload;

    destructor Destroy;override;

    procedure SaveToStream(const DataStream: TStream);overload;
    procedure SaveToStream(const DataStream: TStream; const IncludeLen: boolean); overload;
    function GetCharSize: TCharSize;
    function HasRichText: boolean;
    function HasFarInfo: boolean;

    function Compare(const Str2: TExcelString): integer; //-1 if less, 0 if equal, 1 if more
    function TotalSize: int64;

    procedure CopyToPtr(const Ptr: PArrayOfByte; const aPos: integer);overload;
    procedure CopyToPtr(const Ptr: PArrayOfByte; const aPos: integer; const IncludeLen: boolean);overload;
    property Value: UTF16String read GetValue;
  end;

implementation

{ TExcelString }

{$IFDEF DELPHI2008UP}
function MyShortCompareStr(const S1, S2: AnsiString): Integer;
var
  i:integer;
begin
  Result:=0;
  if Length(S1)<Length(S2) then Result:=-1 else if Length(S1)>Length(S2) then Result:=1
  else
  for i:=1 to Length(S1) do
  begin
    if S1[i]=S2[i] then continue
    else if S1[i]<S2[i] then Result:=-1 else Result:=1;
    exit;
  end;
end;
{$ELSE}
function MyWideCompareStr(const S1, S2: UTF16String): Integer;
var
  i:integer;
begin
  Result:=0;
  if Length(S1)<Length(S2) then Result:=-1 else if Length(S1)>Length(S2) then Result:=1
  else
  for i:=1 to Length(S1) do
  begin
    if S1[i]=S2[i] then continue
    else if S1[i]<S2[i] then Result:=-1 else Result:=1;
    exit;
  end;
end;
{$ENDIF}

constructor TExcelString.Create(const aStrLenLength: TStrLenLength; var aRecord: TBaseRecord; var Ofs: integer);
var
  StrLenByte: byte;
  DestPos: integer;
  ActualOptionFlags: byte;
begin
  inherited Create;
  StrLenLength:=aStrLenLength;
  if StrLenLength=1 then
  begin
    ReadMem(aRecord, Ofs, StrLenLength, @StrLenByte);
    StrLen:=StrLenByte;
  end
  else ReadMem(aRecord, Ofs, StrLenLength, @StrLen);

  ReadMem(aRecord, Ofs, SizeOf(OptionFlags), @OptionFlags);
  ActualOptionFlags:=OptionFlags;

  if HasRichText then ReadMem(aRecord, Ofs, SizeOf(NumberRichTextFormats), @NumberRichTextFormats)
  else NumberRichTextFormats:=0;

  if HasFarInfo then ReadMem(aRecord, Ofs, SizeOf(FarEastDataSize), @FarEastDataSize)
  else FarEastDataSize:=0;

  DestPos:=0;
  SetLength( ShortData, StrLen);
  SetLength( WideData, StrLen);
  ReadStr(aRecord, Ofs, ShortData, WideData, OptionFlags, ActualOptionFlags, DestPos, StrLen);
  if GetCharSize=1 then WideData:='' else ShortData:='';

  if NumberRichTextFormats>0 then
  begin
    GetMem(RichTextFormats, 4* NumberRichTextFormats);
    ReadMem(aRecord, Ofs, 4* NumberRichTextFormats, RichTextFormats)
  end;

  if FarEastDataSize>0 then
  begin
    GetMem(FarEastData, FarEastDataSize);
    ReadMem(aRecord, Ofs, FarEastDataSize, FarEastData)
  end;

end;

function TExcelString.Compare(const Str2: TExcelString): integer;
begin
  if StrLenLength< Str2.StrLenLength then begin;Result:=-1;exit; end
  else if StrLenLength> Str2.StrLenLength then begin;Result:=1;exit; end;

  if OptionFlags< Str2.OptionFlags then begin; Result:=-1; exit; end
  else if OptionFlags> Str2.OptionFlags then begin; Result:=1; exit; end;

  {$IFDEF DELPHI2008UP}
  if GetCharSize=1 then Result:=MyShortCompareStr(ShortData, Str2.ShortData) else
    Result:= CompareStr(WideData, Str2.WideData);
  {$ELSE}
  if GetCharSize=1 then Result:=CompareStr(ShortData, Str2.ShortData) else
    Result:= MyWideCompareStr(WideData, Str2.WideData);
  {$ENDIF}
end;

constructor TExcelString.Create(const aStrLenLength: TStrLenLength;
  const s: UTF16String; const ForceWide: boolean);
begin
  inherited Create;
  StrLenLength:=aStrLenLength;
  case StrLenLength of
    1: if Length(s)> $FF then raise Exception.Create(ErrInvalidStringRecord);
  end; //case
  StrLen:=Length(s);

  OptionFlags:=0;
  if ForceWide or IsWide(s) then OptionFlags:=1;
  NumberRichTextFormats:=0;
  FarEastDataSize:=0;

  if GetCharSize= 1 then ShortData:=WideStringToStringNoCodePage(s) else WideData:=s;
end;

destructor TExcelString.Destroy;
begin
  FreeMem(FarEastData);
  FreeMem(RichTextFormats);
  inherited;
end;

function TExcelString.GetCharSize: TCharSize;
begin
  if OptionFlags and $1 = 0 then Result:=1 else Result:=2;
end;

function TExcelString.HasFarInfo: boolean;
begin
  Result:= OptionFlags and $4 = $4;
end;

function TExcelString.HasRichText: boolean;
begin
  Result:= OptionFlags and $8 = $8;
end;

function TExcelString.TotalSize: int64;
begin
  Result:=
    StrLenLength+
    SizeOf(OptionFlags)+
    StrLen* GetCharSize;

    //Rich text
    if HasRichText then
      Result:=Result + SizeOf(NumberRichTextFormats)+ 4* NumberRichTextFormats;

    //FarEast
    if HasFarInfo then
      Result:=Result+ SizeOf(FarEastDataSize) + FarEastDataSize;
end;

procedure TExcelString.SaveToStream(const DataStream: TStream);
begin
  SaveToStream(DataStream, true);
end;

procedure TExcelString.SaveToStream(const DataStream: TStream; const IncludeLen: boolean);
begin
  if IncludeLen then
  begin
    case StrLenLength of
      1: DataStream.Write(StrLen, SizeOf(Byte));
      2: DataStream.Write(StrLen, SizeOf(Word));
      else raise Exception.Create(ErrInvalidStrLenLength);
    end; //case
  end;

  DataStream.Write(OptionFlags, SizeOf(OptionFlags));

  if HasRichText then
    DataStream.Write(NumberRichTextFormats, SizeOf(NumberRichTextFormats));

  if HasFarInfo then
    DataStream.Write(FarEastDataSize, SizeOf(FarEastDataSize));

  if GetCharSize= 1 then
    if StrLen>0 then DataStream.Write(ShortData[1], Length(ShortData)) else //nothing
  else
    if StrLen>0 then DataStream.Write(WideData[1], Length(WideData)* SizeOf(UTF16Char));

  if NumberRichTextFormats>0 then
    DataStream.Write(RichTextFormats^, 4*NumberRichTextFormats);

  if FarEastDataSize>0 then
    DataStream.Write(FarEastData^, FarEastDataSize);

end;

procedure TExcelString.CopyToPtr(const Ptr: PArrayOfByte;
  const aPos: integer);
begin
  CopyToPtr(Ptr, aPos, true);
end;

procedure TExcelString.CopyToPtr(const Ptr: PArrayOfByte;
  const aPos: integer; const IncludeLen: boolean);
var
  Ms: TMemoryStream;
begin
  //Not the most efficient... but this way we avoid duplicating code
  Ms:= TMemoryStream.Create;
  try
    SaveToStream(Ms, IncludeLen);
    Ms.Position:=0;
    Ms.ReadBuffer(Ptr[aPos], Ms.Size);
  finally
    FreeAndNil(Ms);
  end;
end;

function TExcelString.GetValue: UTF16String;
begin
  if GetCharSize=1 then Result:= StringToWideStringNoCodePage(ShortData) else Result:= WideData;
end;

end.
