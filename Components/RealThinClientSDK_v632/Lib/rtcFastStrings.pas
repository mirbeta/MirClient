{
  "Fast Huge String manipulation and search classes"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}

unit rtcFastStrings;

{$include rtcDefs.inc}

interface

uses
  SysUtils, Classes,

  rtcTypes,
  memStrIntList,
  memStringIntList;


type
  // "Fix" Types for RTC_STRING_FIXMODE
  TRtcStrFixType = (
    //Do NOT modify RtcString data when converting to/from RtcByteArray
    rtcStr_NoFIX,
    // Replace Unicode characters above #255 with ANSI (Win-1252) when converting RtcString to RtcByteArray
    rtcStr_FixDown,
    // rtcStr_FixDown + Replace ANSI (Win-1252) characters with Unicode when converting RtcByteArray to RtcString
    rtcStr_FixUpDown
    );

var
  // RtcString "fix" mode (rtcStr_NoFix, rtcStr_FixDown, rtcStr_FixUpDown)
  RTC_STRING_FIXMODE:TRtcStrFixType=rtcStr_FixDown;

  // Raise an exception if conversion from RtcString to RtcByteArray would result in data loss
  RTC_STRING_CHECK:boolean=False;

  // Character to be used as a replacement for all Unicode characters not in the Win-1252 ANSI table
  RTC_INVALID_CHAR:byte=63;

const
  RTC_STROBJ_SHIFT = 4; // = 16
  RTC_STROBJ_PACK = 1 shl RTC_STROBJ_SHIFT;
  RTC_STROBJ_AND = RTC_STROBJ_PACK-1;

type
  tRtcStrRec=record
    str:RtcString;
    siz:integer;
    end;
  tRtcStrArr=array[0..RTC_STROBJ_PACK-1] of tRtcStrRec;
  PRtcStrArr=^tRtcStrArr;
  tRtcStrArray=array of PRtcStrArr;

  TRtcHugeString=class
  private
    FSize:int64;

    FData:tRtcStrArray;
    FPack:PRtcStrArr;

    FDataCnt,
    FPackCnt,
    FPackFree,
    FPackLoc:integer;

    FCount:integer;

    procedure GrowHugeStringList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AddEx(const s:RtcByteArray; len:Integer=-1);
    procedure Add(const s:RtcString; len:Integer=-1);

    function GetEx:RtcByteArray;
    function Get:RtcString;

    property Size:int64 read FSize;
    end;

  tRtcBytesRec=record
    str:RtcByteArray;
    siz:integer;
    end;
  tRtcBytesArr=array[0..RTC_STROBJ_PACK-1] of tRtcBytesRec;
  PRtcBytesArr=^tRtcBytesArr;
  tRtcBytesArray=array of PRtcBytesArr;

  TRtcHugeByteArray=class
  private
    FSize:int64;
    
    FData:tRtcBytesArray;
    FPack:PRtcBytesArr;

    FDataCnt,
    FPackCnt,
    FPackFree,
    FPackLoc:integer;

    FCount:integer;

    procedure GrowHugeStringList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AddEx(const s:RtcByteArray; len:Integer=-1; loc:integer=0);
    procedure Add(const s:RtcString; len:Integer=-1);

    procedure AddPackEx(const s:RtcByteArray; packSize:integer; len:Integer=-1; loc:integer=0);

    function GetStartEx(len:integer):RtcByteArray;
    procedure DelStart(len:integer);

    function GetEx:RtcByteArray;
    function Get:RtcString;

    property Size:int64 read FSize;
    end;

  tRtcStrObjRec=record
    str:RtcString;
    obj:TObject;
    end;
  tRtcStrObjArr=array[0..RTC_STROBJ_PACK-1] of tRtcStrObjRec;
  PRtcStrObjArr=^tRtcStrObjArr;

  tRtcStrObjArray=array of PRtcStrObjArr;

  tRtcFastStrObjList=class
  private
    FData:tRtcStrObjArray; // array of PRtcStrObjArr;
    FPack:PRtcStrObjArr;
    Tree:TStrIntList;

    FDataCnt, 
    FPackCnt:integer;
    FCnt:integer;
    FOnChange: TNotifyEvent;

    function GetName(const index: integer): RtcString;
    function GetValue(const index: integer): TObject;
    procedure SetName(const index: integer; const _Value: RtcString);
    procedure SetValue(const index: integer; const _Value: TObject);
    function GetCount: integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure DestroyObjects;

    function Add(const Name:RtcString; _Value:TObject=nil):integer;
    function Find(const Name:RtcString):integer;
    function IndexOf(const Name:RtcString):integer;

    // Case-sensitive Add, Find and IndexOf
    function AddCS(const Name:RtcString; _Value:TObject=nil):integer;
    function FindCS(const Name:RtcString):integer;
    function IndexOfCS(const Name:RtcString):integer;

    property Objects[const index:integer]:TObject read GetValue write SetValue;
    property Strings[const index:integer]:RtcString read GetName write SetName;

    property Count:integer read GetCount;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    end;

  tRtcStringObjRec=record
    str:RtcWideString;
    obj:TObject;
    end;
  tRtcStringObjArr=array[0..RTC_STROBJ_PACK-1] of tRtcStringObjRec;
  PRtcStringObjArr=^tRtcStringObjArr;

  tRtcStringObjArray=array of PRtcStringObjArr;

  tRtcFastStringObjList=class
  private
    FData:tRtcStringObjArray; // array of PRtcStringObjArr;
    FPack:PRtcStringObjArr;
    Tree:TStringIntList;

    FDataCnt,
    FPackCnt:integer;
    FCnt:integer;
    FOnChange: TNotifyEvent;

    function GetName(const index: integer): RtcWideString;
    function GetValue(const index: integer): TObject;
    procedure SetName(const index: integer; const _Value: RtcWideString);
    procedure SetValue(const index: integer; const _Value: TObject);
    function GetCount: integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure DestroyObjects;

    function Add(const Name:RtcWideString; _Value:TObject=nil):integer;
    function Find(const Name:RtcWideString):integer;
    function IndexOf(const Name:RtcWideString):integer;

    // Case-sensitive Add, Find and IndexOf
    function AddCS(const Name:RtcWideString; _Value:TObject=nil):integer;
    function FindCS(const Name:RtcWideString):integer;
    function IndexOfCS(const Name:RtcWideString):integer;

    property Objects[const index:integer]:TObject read GetValue write SetValue;
    property Strings[const index:integer]:RtcWideString read GetName write SetName;

    property Count:integer read GetCount;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    end;

{$IFDEF UNICODE}
  {$IFDEF RTC_BYTESTRING}
function UpperCase(const s:RtcString):RtcString; overload;
function Trim(const S: RtcString): RtcString; overload;
  {$ENDIF}
{$ENDIF}

function Upper_Case(const s:RtcString):RtcString;
function Same_Text(const s1,s2:RtcString):boolean;

function Up_Case(const c:RtcChar):RtcChar;

function UpperCaseStr(const s:RtcWideString):RtcWideString;

// Convert Unicode character code to ANSI (Win-1252) character code
function RtcUnicodeToAnsiChar(Chr:Word):Byte;
// Convert ANSI (Win-1252) character code to Unicode character code
function RtcAnsiToUnicodeChar(Chr:Byte):Word;

// Convert Unicode String to ANSI (Win-1252) String
function RtcUnicodeToAnsiString(const Source:RtcString):RtcString;
// Convert ANSI (Win-1252) String to Unicode String
function RtcAnsiToUnicodeString(const Source:RtcString):RtcString;

implementation

function RtcUnicodeToAnsiChar(Chr:Word):Byte;
  begin
{$IFDEF RTC_BYTESTRING}
  Result:=Byte(Chr);
{$ELSE}
  if Chr<=255 then
    Result:=Chr
  else case Chr of
    8364: Result:=128;
    8218: Result:=130;
    402:  Result:=131;
    8222: Result:=132;
    8230: Result:=133;
    8224: Result:=134;
    8225: Result:=135;
    710:  Result:=136;
    8240: Result:=137;
    352:  Result:=138;
    8249: Result:=139;
    338:  Result:=140;
    381:  Result:=142;
    8216: Result:=145;
    8217: Result:=146;
    8220: Result:=147;
    8221: Result:=148;
    8226: Result:=149;
    8211: Result:=150;
    8212: Result:=151;
    732:  Result:=152;
    8482: Result:=153;
    353:  Result:=154;
    8250: Result:=155;
    339:  Result:=156;
    382:  Result:=158;
    376:  Result:=159;
    else  Result:=RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeChar(Chr:Byte):Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result:=Chr;
{$ELSE}
  if (Chr<128) or (Chr>159) then
    Result:=Chr
  else case Chr of
    128: Result:=8364;
    130: Result:=8218;
    131: Result:=402;
    132: Result:=8222;
    133: Result:=8230;
    134: Result:=8224;
    135: Result:=8225;
    136: Result:=710;
    137: Result:=8240;
    138: Result:=352;
    139: Result:=8249;
    140: Result:=338;
    142: Result:=381;
    145: Result:=8216;
    146: Result:=8217;
    147: Result:=8220;
    148: Result:=8221;
    149: Result:=8226;
    150: Result:=8211;
    151: Result:=8212;
    152: Result:=732;
    153: Result:=8482;
    154: Result:=353;
    155: Result:=8250;
    156: Result:=339;
    158: Result:=382;
    159: Result:=376;
    else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiString(const Source:RtcString):RtcString;
{$IFDEF RTC_BYTESTRING}
  begin
  Result:=Source;
  end;
{$ELSE}
  var
    i: Integer;
  begin
  SetLength(Result,length(Source));
  for i := 1 to Length(Source) do
    Result[i]:=RtcChar(RtcUnicodeToAnsiChar(Word(Source[i])));
  end;
{$ENDIF}

function RtcAnsiToUnicodeString(const Source:RtcString):RtcString;
{$IFDEF RTC_BYTESTRING}
  begin
  Result:=Source;
  end;
{$ELSE}
  var
    i: Integer;
  begin
  SetLength(Result,length(Source));
  for i := 1 to Length(Source) do
    Result[i]:=RtcChar(RtcAnsiToUnicodeChar(Byte(Source[i])));
  end;
{$ENDIF}

const
  CHR_A = Ord('a');
  CHR_Z = Ord('z');

{$IFDEF UNICODE}
  {$IFDEF RTC_BYTESTRING}
function UpperCase(const s:RtcString):RtcString; overload;
  var
    i:integer;
    c,d:^RtcBinChar;
  begin
  i:=length(s);
  SetLength(Result,i);
  if i>0 then
    begin
    c:=@(s[1]);
    d:=@(Result[1]);
    for i:=1 to i do
      begin
      if (c^>=CHR_A) and (c^<=CHR_Z) then
        d^:=c^ - 32
      else
        d^:=c^;
      Inc(d);
      Inc(c);
      end;
    end;
  end;

function Trim(const S: RtcString): RtcString; overload;
  var
    I, L: Integer;
  begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    SetLength(Result,0)
  else
    begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
    end;
  end;
  {$ENDIF}
{$ENDIF}

function Upper_Case(const s:RtcString):RtcString;
  var
    i:integer;
    c,d:^RtcBinChar;
  begin
  i:=length(s);
  SetLength(Result,i);
  if i>0 then
    begin
    c:=@(s[1]);
    d:=@(Result[1]);
    for i:=1 to i do
      begin
      if (c^>=CHR_A) and (c^<=CHR_Z) then
        d^:=c^ - 32
      else
        d^:=c^;
      Inc(d);
      Inc(c);
      end;
    end;
  end;

function Up_Case(const c:RtcChar):RtcChar;
  begin
  if (c>='a') and (c<='z') then
    Result:=RtcChar(Ord(c) - 32)
  else
    Result:=c;
  end;

function Same_Text(const s1,s2:RtcString):boolean;
  var
    i:integer;
    c,d:^RtcBinChar;
    e,f:RtcBinChar;
  begin
  i:=length(s1);
  if i<>length(s2) then
    Result:=False
  else if i>0 then
    begin
    Result:=True;
    c:=@(s1[1]);
    d:=@(s2[1]);
    for i:=1 to i do
      begin
      if (c^>=CHR_A) and (c^<=CHR_Z) then
        e:=c^ - 32
      else
        e:=c^;
      if (d^>=CHR_A) and (d^<=CHR_Z) then
        f:=d^ - 32
      else
        f:=d^;
      if e<>f then
        begin
        Result:=False;
        Break;
        end;
      Inc(d);
      Inc(c);
      end;
    end
  else
    Result:=True;
  end;

function UpperCaseStr(const s:RtcWideString):RtcWideString;
  var
    i:integer;
    c,d:^RtcBinWideChar;
  begin
  i:=length(s);
  SetLength(Result,i);
  if i>0 then
    begin
    c:=@(s[1]);
    d:=@(Result[1]);
    for i:=1 to i do
      begin
      if (c^>=CHR_A) and (c^<=CHR_Z) then
        d^:=c^ - 32
      else
        d^:=c^;
      Inc(d);
      Inc(c);
      end;
    end;
  end;

procedure RtcStringToByteArray(const Source:RtcString; var Dest:RtcByteArray; SourceLoc:Integer=1; len:Integer=-1; DestLoc:Integer=0);
  var
    i, k: Integer;
  begin
  if len<0 then len:=length(Source)-SourceLoc+1;
  if len = 0 then Exit;
  k := SourceLoc;
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=DestLoc to DestLoc+len-1 do
      begin
      if Ord(Source[k])>255 then
        begin
        Dest[i] := RtcUnicodeToAnsiChar(Ord(Source[k]));
        if RTC_STRING_CHECK and (Dest[i]=RTC_INVALID_CHAR) then
          raise Exception.Create('RtcStringToByteArray: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Source[k]);
        end
      else
        Dest[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=DestLoc to DestLoc+len-1 do
      begin
      if Ord(Source[k])>255 then
        raise Exception.Create('RtcStringToByteArray: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Source[k])
      else
        Dest[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=DestLoc to DestLoc+len-1 do
      begin
      Dest[i] := Byte(Source[k]);
      Inc(k);
      end;
    end;
  end;

procedure RtcByteArrayToString(const Source:RtcByteArray; var Dest:RtcString; SourceLoc:Integer=0; len:Integer=-1; DestLoc:Integer=1);
  var
    i, k: Integer;
  begin
  if len<0 then len:=length(Source)-SourceLoc;
  if len = 0 then Exit;
  k := SourceLoc;
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=DestLoc to DestLoc+len-1 do
      begin
      Dest[i] := RtcChar(RtcAnsiToUnicodeChar(Source[k]));
      Inc(k);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=DestLoc to DestLoc+len-1 do
      begin
      Dest[i] := RtcChar(Source[k]);
      Inc(k);
      end;
    end;
  end;

{ TRtcHugeString }

constructor TRtcHugeString.Create;
  begin
  inherited;

  FPack:=nil;
  SetLength(FData,0);
  FDataCnt:=0;

  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);

  FPackCnt:=0;
  FPackFree:=0;
  FPackLoc:=0;

  FCount:=0;
  FSize:=0;
  end;

destructor TRtcHugeString.Destroy;
  begin
  Clear;
  if FPack<>nil then Dispose(FPack);
  inherited;
  end;

procedure TRtcHugeString.Clear;
  var
    a,b:integer;
    FPack2:PRtcStrArr;
  begin
  if FDataCnt>0 then
    begin
    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        SetLength(FPack2^[b].str,0);
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    FDataCnt:=0;
    end;

  if FPackCnt>0 then
    begin
    for b:=0 to FPackCnt-1 do
      SetLength(FPack^[b].str,0);
    FPackCnt:=0;
    FPackFree:=0;
    FPackLoc:=0;
    end;

  FSize:=0;
  FCount:=0;
  end;

procedure TRtcHugeString.GrowHugeStringList;
  begin
  if length(FData)<=FDataCnt then
    SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
  FData[FDataCnt]:=FPack;
  Inc(FDataCnt);

  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);
  FPackCnt:=0;
  end;

procedure TRtcHugeString.Add(const s: RtcString; len:Integer=-1);
  begin
  if len<0 then len:=length(s);
  if len>0 then
    begin
    FSize:=FSize + len;

    if FPackFree>=len then
      begin
      with FPack^[FPackCnt-1] do
        begin
        Move(s[1], str[FPackLoc], len * SizeOf(RtcChar));
        Inc(siz, len);
        end;
      Inc(FPackLoc,len);
      Dec(FPackFree,len);
      end
    else
      begin
      if FPackCnt>=RTC_STROBJ_PACK then
        GrowHugeStringList;

      if len>=255 then
        begin
        with FPack^[FPackCnt] do
          begin
          {$IFDEF RTC_WIDESTRING}
            SetLength(str, len);
            Move(s[1],str[1],len * SizeOf(RtcChar));
          {$ELSE}
            str:=s;
          {$ENDIF}
          siz:=len;
          end;
        FPackFree:=0;
        FPackLoc:=0;
        end
      else
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, 254);
          Move(s[1],str[1],len * SizeOf(RtcChar));
          siz:=len;
          end;
        FPackFree:=254-len;
        FPackLoc:=len+1;
        end;
      Inc(FPackCnt);
      Inc(FCount);
      end;
    end;
  end;

procedure TRtcHugeString.AddEx(const s:RtcByteArray; len: Integer);
  begin
  if len<0 then len:=length(s);
  if len>0 then
    begin
    FSize:=FSize + len;

    if FPackFree>=len then
      begin
      with FPack^[FPackCnt-1] do
        begin
      {$IFDEF RTC_BYTESTRING}
        Move(s[0], str[FPackLoc], len);
      {$ELSE}
        RtcByteArrayToString(s,str,0,len,FPackLoc);
      {$ENDIF}
        Inc(siz, len);
        end;
      Inc(FPackLoc,len);
      Dec(FPackFree,len);
      end
    else
      begin
      if FPackCnt>=RTC_STROBJ_PACK then
        GrowHugeStringList;

      if len>=255 then
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, len);
        {$IFDEF RTC_BYTESTRING}
          Move(s[0],str[1],len);
        {$ELSE}
          RtcByteArrayToString(s,str,0,len,1);
        {$ENDIF}
          siz:=len;
          end;
        FPackFree:=0;
        FPackLoc:=0;
        end
      else
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, 254);
        {$IFDEF RTC_BYTESTRING}
          Move(s[0],str[1],len);
        {$ELSE}
          RtcByteArrayToString(s,str,0,len,1);
        {$ENDIF}
          siz:=len;
          end;
        FPackFree:=254-len;
        FPackLoc:=len+1;
        end;
      Inc(FPackCnt);
      Inc(FCount);
      end;
    end;
  end;

function TRtcHugeString.Get: RtcString;
  var
    a,b,loc:integer;
    FPack2:PRtcStrArr;
  begin
  if FCount>1 then
    begin
    SetLength(Result, FSize);
    loc:=1;

    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          Move(str[1], Result[loc], siz * SizeOf(RtcChar));
          Inc(loc, siz);
          end;
      end;

    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
        Move(str[1], Result[loc], siz * SizeOf(RtcChar));
        Inc(loc, siz);
        end;

    if loc<>FSize+1 then
      raise Exception.Create('TRtcHugeString.Get: Internal error.');
    end
  else if FCount>0 then
    begin
    with FPack^[0] do
      if siz>=254 then
        Result:=str
      else
        begin
        SetLength(Result, siz);
        Move(str[1], Result[1], siz * SizeOf(RtcChar));
        end;
    end
  else
    SetLength(Result,0);
  end;

function TRtcHugeString.GetEx: RtcByteArray;
  var
    a,b,loc:integer;
    FPack2:PRtcStrArr;
  begin
  if FCount>1 then
    begin
    SetLength(Result, FSize);
    loc:=0;

    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
        {$IFDEF RTC_BYTESTRING}
          Move(str[1], Result[loc], siz);
        {$ELSE}
          RtcStringToByteArray(str,Result,1,siz,loc);
        {$ENDIF}
          Inc(loc, siz);
          end;
      end;

    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
      {$IFDEF RTC_BYTESTRING}
        Move(str[1], Result[loc], siz);
      {$ELSE}
        RtcStringToByteArray(str,Result,1,siz,loc);
      {$ENDIF}
        Inc(loc, siz);
        end;

    if loc<>FSize then
      raise Exception.Create('TRtcHugeString.GetEx: Internal error.');
    end
  else if FCount>0 then
    begin
    with FPack^[0] do
      begin
      SetLength(Result, siz);
    {$IFDEF RTC_BYTESTRING}
      Move(str[1], Result[0], siz);
    {$ELSE}
      RtcStringToByteArray(str,Result,1,siz,0);
    {$ENDIF}
      end;
    end
  else
    SetLength(Result,0);
  end;

{ TRtcHugeByteArray }

constructor TRtcHugeByteArray.Create;
  begin
  inherited;

  FPack:=nil;
  SetLength(FData,0);
  FDataCnt:=0;

  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);

  FPackCnt:=0;
  FPackFree:=0;
  FPackLoc:=0;

  FCount:=0;
  FSize:=0;
  end;

destructor TRtcHugeByteArray.Destroy;
  begin
  Clear;
  if FPack<>nil then Dispose(FPack);
  inherited;
  end;

procedure TRtcHugeByteArray.Clear;
  var
    a,b:integer;
    FPack2:PRtcBytesArr;
  begin
  if FDataCnt>0 then
    begin
    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        SetLength(FPack2^[b].str,0);
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    FDataCnt:=0;
    end;

  if FPackCnt>0 then
    begin
    for b:=0 to FPackCnt-1 do
      SetLength(FPack^[b].str,0);
    FPackCnt:=0;
    FPackFree:=0;
    FPackLoc:=0;
    end;

  FSize:=0;
  FCount:=0;
  end;

procedure TRtcHugeByteArray.GrowHugeStringList;
  begin
  if length(FData)<=FDataCnt then
    SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
  FData[FDataCnt]:=FPack;
  Inc(FDataCnt);

  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);
  FPackCnt:=0;
  end;

procedure TRtcHugeByteArray.AddEx(const s:RtcByteArray; len:integer=-1; loc:integer=0);
  begin
  if len=-1 then len:=length(s)-loc;
  if len>0 then
    begin
    FSize:=FSize + len;

    if FPackFree>=len then
      begin
      with FPack^[FPackCnt-1] do
        begin
        Move(s[loc], str[FPackLoc], len);
        Inc(siz, len);
        end;
      Inc(FPackLoc,len);
      Dec(FPackFree,len);
      end
    else
      begin
      if FPackCnt>=RTC_STROBJ_PACK then
        GrowHugeStringList;

      if len>=255 then
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, len);
          Move(s[loc],str[0],len);
          siz:=len;
          end;
        FPackFree:=0;
        FPackLoc:=0;
        end
      else
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, 254);
          Move(s[loc],str[0],len);
          siz:=len;
          end;
        FPackFree:=254-len;
        FPackLoc:=len;
        end;
      Inc(FPackCnt);
      Inc(FCount);
      end;
    end;
  end;

procedure TRtcHugeByteArray.Add(const s: RtcString; len: Integer);
  begin
  if len=-1 then len:=length(s);
  if len>0 then
    begin
    FSize:=FSize + len;

    if FPackFree>=len then
      begin
      with FPack^[FPackCnt-1] do
        begin
        {$IFDEF RTC_BYTESTRING}
        Move(s[1], str[FPackLoc], len);
        {$ELSE}
        RtcStringToByteArray(s,str,1,len,FPackLoc);
        {$ENDIF}
        Inc(siz, len);
        end;
      Inc(FPackLoc,len);
      Dec(FPackFree,len);
      end
    else
      begin
      if FPackCnt>=RTC_STROBJ_PACK then
        GrowHugeStringList;

      if len>=255 then
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, len);
          {$IFDEF RTC_BYTESTRING}
          Move(s[1],str[0],len);
          {$ELSE}
          RtcStringToByteArray(s,str,1,len,0);
          {$ENDIF}
          siz:=len;
          end;
        FPackFree:=0;
        FPackLoc:=0;
        end
      else
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, 254);
          {$IFDEF RTC_BYTESTRING}
          Move(s[1],str[0],len);
          {$ELSE}
          RtcStringToByteArray(s,str,1,len,0);
          {$ENDIF}
          siz:=len;
          end;
        FPackFree:=254-len;
        FPackLoc:=len;
        end;
      Inc(FPackCnt);
      Inc(FCount);
      end;
    end;
  end;

function TRtcHugeByteArray.GetEx: RtcByteArray;
  var
    a,b,loc:integer;
    FPack2:PRtcBytesArr;
  begin
  if FCount>1 then
    begin
    SetLength(Result, FSize);
    loc:=0;

    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          if siz>0 then
            begin
            Move(str[0], Result[loc], siz);
            Inc(loc, siz);
            end;
      end;

    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        if siz>0 then
          begin
          Move(str[0], Result[loc], siz);
          Inc(loc, siz);
          end;

    if loc<>FSize then
      raise Exception.Create('TRtcHugeByteArray.GetEx: Internal Error.');
    end
  else if FCount>0 then
    begin
    with FPack^[0] do
      if siz>=254 then
        Result:=str
      else if siz>0 then
        begin
        SetLength(Result, siz);
        Move(str[0], Result[0], siz);
        end
      else
        SetLength(Result,0);
    end
  else
    SetLength(Result,0);
  end;

function TRtcHugeByteArray.Get: RtcString;
  var
    a,b,loc:integer;
    FPack2:PRtcBytesArr;
  begin
  if FCount>1 then
    begin
    SetLength(Result, FSize);
    loc:=1;

    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          if siz>0 then
            begin
            {$IFDEF RTC_BYTESTRING}
            Move(str[0], Result[loc], siz);
            {$ELSE}
            RtcByteArrayToString(str,Result,0,siz,loc);
            {$ENDIF}
            Inc(loc, siz);
            end;
      end;

    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        if siz>0 then
          begin
          {$IFDEF RTC_BYTESTRING}
          Move(str[0], Result[loc], siz);
          {$ELSE}
          RtcByteArrayToString(str,Result,0,siz,loc);
          {$ENDIF}
          Inc(loc, siz);
          end;

    if loc<>FSize+1 then
      raise Exception.Create('TRtcHugeByteArray.Get: Internal Error.');
    end
  else if FCount>0 then
    begin
    with FPack^[0] do
      if siz>0 then
        begin
        SetLength(Result, siz);
        {$IFDEF RTC_BYTESTRING}
        Move(str[0], Result[1], siz);
        {$ELSE}
        RtcByteArrayToString(str,Result,0,siz,1);
        {$ENDIF}
        end
      else
        SetLength(Result,0);
    end
  else
    SetLength(Result,0);
  end;

function TRtcHugeByteArray.GetStartEx(len: integer): RtcByteArray;
  var
    a,b,loc:integer;
    FPack2:PRtcBytesArr;
  begin
  if FCount>1 then
    begin
    if len>FSize then len:=FSize;

    SetLength(Result, len);

    if len=0 then Exit;

    loc:=0;

    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          if siz>=len then
            begin
            Move(str[0], Result[loc], len);
            Exit;
            end
          else
            begin
            Move(str[0], Result[loc], siz);
            Inc(loc, siz);
            Dec(len, siz);
            end;
          end;
      end;

    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
        if siz>=len then
          begin
          Move(str[0], Result[loc], len);
          Exit;
          end
        else
          begin
          Move(str[0], Result[loc], siz);
          Inc(loc, siz);
          Dec(len, siz);
          end;
        end;

    if loc<>FSize then
      raise Exception.Create('TRtcHugeByteArray.GetStartEx: Internal Error.');
    end
  else if FCount>0 then
    begin
    with FPack^[0] do
      begin
      if siz>=len then
        begin
        SetLength(Result, len);
        Move(str[0], Result[0], len);
        end
      else
        begin
        SetLength(Result, siz);
        Move(str[0], Result[0], siz);
        end;
      end;
    end
  else
    SetLength(Result,0);
  end;


procedure TRtcHugeByteArray.DelStart(len: integer);
  var
    a,b,loc:integer;
    FPack2:PRtcBytesArr;
  begin
  if len=0 then
    Exit
  else if len>=Size then
    begin
    Clear;
    Exit;
    end;

  if FCount>1 then
    begin
    FSize:=FSize-len;

    loc:=0;

    FPackFree:=0;
    FPackLoc:=0;

    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          if siz>len then
            begin
            Move(str[len], str[0], siz-len);
            siz:=siz-len;
            Exit;
            end
          else
            begin
            SetLength(str,0);
            Inc(loc, siz);
            Dec(len, siz);
            siz:=0;
            end;
          end;
      end;

    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
        if siz>len then
          begin
          Move(str[len], str[0], siz-len);
          siz:=siz-len;
          Exit;
          end
        else
          begin
          SetLength(str,0);
          Inc(loc, siz);
          Dec(len, siz);
          siz:=0;
          end;
        end;

    if loc<>FSize then
      raise Exception.Create('TRtcHugeByteArray.DelStart: Internal Error.');
    end
  else if FCount>0 then
    begin
    FPackFree:=0;
    FPackLoc:=0;
    FSize:=FSize-len;

    with FPack^[0] do
      begin
      if siz>len then
        begin
        Move(str[len], str[0], siz-len);
        siz:=siz-len;
        end
      else
        begin
        SetLength(str,0);
        siz:=0;
        end;
      end;
    end;
  end;

procedure TRtcHugeByteArray.AddPackEx(const s: RtcByteArray;
                                      packSize:integer; len:Integer=-1; loc:integer=0);
  var
    pack:integer;
  begin
  if len=-1 then len:=length(s)-loc;
  while len>0 do
    begin
    pack:=len;
    if pack>packSize then pack:=packSize;
    AddEx(s,pack,loc);
    len:=len-pack;
    loc:=loc+pack;
    end;
  end;

{ tRtcFastStrObjList }

constructor tRtcFastStrObjList.Create;
  begin
  inherited;
  FPack:=nil;
  Tree:=tStrIntList.Create(RTC_STROBJ_PACK);

  SetLength(FData,0);
  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);

  FCnt:=0;
  FDataCnt:=0;
  FPackCnt:=0;
  end;

destructor tRtcFastStrObjList.Destroy;
  begin
  Clear;
  if FPack<>nil then Dispose(FPack);
  RtcFreeAndNil(Tree);
  inherited;
  end;

procedure tRtcFastStrObjList.Clear;
  var
    a,b:integer;
    FPack2:PRtcStrObjArr;
  begin
  if self=nil then Exit;
  
  if FPackCnt>0 then
    begin
    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
        SetLength(str,0);
        obj:=nil;
        end;
    FPackCnt:=0;
    end;

  if FDataCnt>0 then
    begin
    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          SetLength(str,0);
          obj:=nil;
          end;
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    FDataCnt:=0;
    end;

  if assigned(Tree) then
    Tree.removeall;
  FCnt:=0;

  if assigned(FOnChange) then FOnChange(self);
  end;

procedure tRtcFastStrObjList.DestroyObjects;
  var
    a,b,c:integer;
    FPack2:PRtcStrObjArr;
  begin
  if self=nil then Exit;

  if FPackCnt>0 then
    begin
    c:=FPackCnt;
    FPackCnt:=0;
    for b:=0 to c-1 do
      with FPack^[b] do
        begin
        SetLength(str,0);
        obj.Free;
        end;
    end;

  if FDataCnt>0 then
    begin
    c:=FDataCnt;
    FDataCnt:=0;
    for a:=0 to c-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          SetLength(str,0);
          obj.Free;
          end;
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    end;

  if assigned(Tree) then
    Tree.removeall;
  FCnt:=0;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStrObjList.Add(const Name: RtcString; _Value:TObject=nil): integer;
  procedure GrowStrObjList;
    begin
    if length(FData)<=FDataCnt then
      SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
    FData[FDataCnt]:=FPack;
    Inc(FDataCnt);

    New(FPack);
    FillChar(FPack^,SizeOf(FPack^),0);
    FPackCnt:=0;
    end;
  begin
  if FPackCnt>=RTC_STROBJ_PACK then
    GrowStrObjList;

  Tree.insert(Upper_Case(Name), FCnt);
  with FPack[FPackCnt] do
    begin
    str:=Name;
    obj:=_Value;
    end;
  Inc(FPackCnt);
  Inc(FCnt);

  Result:=FCnt-1;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStrObjList.Find(const Name: RtcString): integer;
  begin
  Result:=Tree.search(Upper_Case(Name));
  end;

function tRtcFastStrObjList.IndexOf(const Name: RtcString): integer;
  begin
  Result:=Tree.search(Upper_Case(Name));
  end;

function tRtcFastStrObjList.AddCS(const Name: RtcString; _Value:TObject=nil): integer;
  begin
  if FPackCnt>=RTC_STROBJ_PACK then
    begin
    if length(FData)<=FDataCnt then
      SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
    FData[FDataCnt]:=FPack;
    Inc(FDataCnt);

    New(FPack);
    FillChar(FPack^,SizeOf(FPack^),0);
    FPackCnt:=0;
    end;

  Tree.insert(Name, FCnt);
  with FPack[FPackCnt] do
    begin
    str:=Name;
    obj:=_Value;
    end;
  Inc(FPackCnt);
  Inc(FCnt);

  Result:=FCnt-1;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStrObjList.FindCS(const Name: RtcString): integer;
  begin
  Result:=Tree.search(Name);
  end;

function tRtcFastStrObjList.IndexOfCS(const Name: RtcString): integer;
  begin
  Result:=Tree.search(Name);
  end;

function tRtcFastStrObjList.GetName(const index: integer): RtcString;
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    Result:=FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].str
  else
    Result:=FPack^[index and RTC_STROBJ_AND].str;
  end;

function tRtcFastStrObjList.GetValue(const index: integer): TObject;
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    Result:=FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].obj
  else
    Result:=FPack^[index and RTC_STROBJ_AND].obj;
  end;

procedure tRtcFastStrObjList.SetName(const index: integer; const _Value: RtcString);
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    begin
    with FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND] do
      begin
      Tree.remove(Upper_Case(str));
      str:=_Value;
      Tree.insert(Upper_Case(_Value), index);
      end;
    end
  else
    begin
    with FPack^[index and RTC_STROBJ_AND] do
      begin
      Tree.remove(Upper_Case(str));
      str:=_Value;
      Tree.insert(Upper_Case(_Value), index);
      end;
    end;
  if assigned(FOnChange) then FOnChange(self);
  end;

procedure tRtcFastStrObjList.SetValue(const index: integer; const _Value: TObject);
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].obj:=_Value
  else
    FPack^[index and RTC_STROBJ_AND].obj:=_Value;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStrObjList.GetCount: integer;
  begin
  Result:=FCnt;
  end;

{ tRtcFastStringObjList }

constructor tRtcFastStringObjList.Create;
  begin
  inherited;
  FPack:=nil;
  Tree:=tStringIntList.Create(RTC_STROBJ_PACK);

  SetLength(FData,0);
  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);

  FCnt:=0;
  FDataCnt:=0;
  FPackCnt:=0;
  end;

destructor tRtcFastStringObjList.Destroy;
  begin
  Clear;
  if FPack<>nil then Dispose(FPack);
  RtcFreeAndNil(Tree);
  inherited;
  end;

procedure tRtcFastStringObjList.Clear;
  var
    a,b:integer;
    FPack2:PRtcStringObjArr;
  begin
  if FPackCnt>0 then
    begin
    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
        SetLength(str,0);
        obj:=nil;
        end;
    FPackCnt:=0;
    end;

  if FDataCnt>0 then
    begin
    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          SetLength(str,0);
          obj:=nil;
          end;
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    FDataCnt:=0;
    end;

  if assigned(Tree) then
    Tree.removeall;
  FCnt:=0;

  if assigned(FOnChange) then FOnChange(self);
  end;

procedure tRtcFastStringObjList.DestroyObjects;
  var
    a,b,c:integer;
    FPack2:PRtcStringObjArr;
  begin
  if FPackCnt>0 then
    begin
    c:=FPackCnt;
    FPackCnt:=0;
    for b:=0 to c-1 do
      with FPack^[b] do
        begin
        SetLength(str,0);
        obj.Free;
        end;
    end;

  if FDataCnt>0 then
    begin
    c:=FDataCnt;
    FDataCnt:=0;
    for a:=0 to c-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          SetLength(str,0);
          obj.Free;
          end;
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    end;

  if assigned(Tree) then
    Tree.removeall;
  FCnt:=0;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStringObjList.Add(const Name: RtcWideString; _Value:TObject=nil): integer;
  procedure FastStringListGrow;
    begin
    if length(FData)<=FDataCnt then
      SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
    FData[FDataCnt]:=FPack;
    Inc(FDataCnt);

    New(FPack);
    FillChar(FPack^,SizeOf(FPack^),0);
    FPackCnt:=0;
    end;
  begin
  if FPackCnt>=RTC_STROBJ_PACK then
    FastStringListGrow;

  Tree.insert(UpperCaseStr(Name), FCnt);
  with FPack[FPackCnt] do
    begin
    str:=Name;
    obj:=_Value;
    end;
  Inc(FPackCnt);
  Inc(FCnt);

  Result:=FCnt-1;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStringObjList.Find(const Name: RtcWideString): integer;
  begin
  Result:=Tree.search(UpperCaseStr(Name));
  end;

function tRtcFastStringObjList.IndexOf(const Name: RtcWideString): integer;
  begin
  Result:=Tree.search(UpperCaseStr(Name));
  end;

function tRtcFastStringObjList.AddCS(const Name: RtcWideString; _Value:TObject=nil): integer;
  procedure FastStringListGrow;
    begin
    if length(FData)<=FDataCnt then
      SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
    FData[FDataCnt]:=FPack;
    Inc(FDataCnt);

    New(FPack);
    FillChar(FPack^,SizeOf(FPack^),0);
    FPackCnt:=0;
    end;
  begin
  if FPackCnt>=RTC_STROBJ_PACK then
    FastStringListGrow;

  Tree.insert(Name, FCnt);
  with FPack[FPackCnt] do
    begin
    str:=Name;
    obj:=_Value;
    end;
  Inc(FPackCnt);
  Inc(FCnt);

  Result:=FCnt-1;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStringObjList.FindCS(const Name: RtcWideString): integer;
  begin
  Result:=Tree.search(Name);
  end;

function tRtcFastStringObjList.IndexOfCS(const Name: RtcWideString): integer;
  begin
  Result:=Tree.search(Name);
  end;

function tRtcFastStringObjList.GetName(const index: integer): RtcWideString;
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    Result:=FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].str
  else
    Result:=FPack^[index and RTC_STROBJ_AND].str;
  end;

function tRtcFastStringObjList.GetValue(const index: integer): TObject;
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    Result:=FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].obj
  else
    Result:=FPack^[index and RTC_STROBJ_AND].obj;
  end;

procedure tRtcFastStringObjList.SetName(const index: integer; const _Value: RtcWideString);
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    begin
    with FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND] do
      begin
      Tree.remove(UpperCaseStr(str));
      str:=_Value;
      Tree.insert(UpperCaseStr(_Value), index);
      end;
    end
  else
    begin
    with FPack^[index and RTC_STROBJ_AND] do
      begin
      Tree.remove(UpperCaseStr(str));
      str:=_Value;
      Tree.insert(UpperCaseStr(_Value), index);
      end;
    end;
  if assigned(FOnChange) then FOnChange(self);
  end;

procedure tRtcFastStringObjList.SetValue(const index: integer; const _Value: TObject);
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].obj:=_Value
  else
    FPack^[index and RTC_STROBJ_AND].obj:=_Value;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStringObjList.GetCount: integer;
  begin
  Result:=FCnt;
  end;

end.

