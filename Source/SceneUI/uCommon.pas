unit uCommon;

interface

uses
  Windows, SysUtils, Classes, Math, Graphics, StrUtils, GraphUtil, ZLib, PXL.SwapChains,
  DIB, uTPLb_StreamUtils, PXL.Devices, PXL.Textures, PXL.Canvas, PXL.Providers, PXL.Types,PXL.Timing;

const
  DefaultFontName = '宋体';
  DefaultFontSize = 9;
  DefaultFontStyle = [];

  //微端模块之间(与客户端之间的消息)
  CL_CLIENTOPEN  = 10000;
  CL_CLIENTCLOSE = 10001;
  CL_RES_REQUST = 10002;
  CL_RES_NEWFILE = 10003;
  CL_RES_IMGDOWNLOADED = 10004;
  CL_RES_FILEDOWNLOADED = 10005;
  CL_RES_UPDATEFILE = 10006;//通知所有客户端有图片文件需要更新。

type
  TGraphicType = (gtDIB, gtPng, gtRealPng,gtNone);
  TMaskCharAddProc = reference to procedure(const Mask, Value: String);

function CheckIsIpAddr(Name: string): Boolean; // 检查IP地址格式
function TickInterval(const EndTick, StartTick: LongWord): LongWord; inline;
function GetRGB(c256: Byte): TColor; inline;
function ISVlidateMaketName(const Name: String): Boolean; //摊位名称是否有效
function ConvertMaskChar(C: Char): String; inline;
function ConvertMaskString(const Source: String): String; inline;
function MakeMaskString(const Source: String): String; inline;
procedure AddMaskChars(AddProc: TMaskCharAddProc);
function M2StrToColor(const Value: String; Def: TColor=Graphics.clNone): TColor;
function SaveStreamToString(Source: TStream): AnsiString;
procedure LoadStreamFromString(const Source: AnsiString; Dest: TStream);
function MakeNewGUID38: String;
function MakeNewGUID36: String;
function MakeNewGUID32: String;
function MakeNewGUID16: String;
function WidthBytes(Width, BitCount: integer): integer; inline;
procedure CompressBufZ(const InBuf: PAnsiChar; InBytes: Integer;  out OutBuf: PAnsiChar; out OutBytes: Integer);
procedure DecompressBufZ(const inBuffer: PAnsiChar; inSize: Integer; outEstimate: Integer; out outBuffer: PAnsiChar; out outSize: Integer);
procedure FreeAndNilEx(var Obj);
procedure FreeAndNilSafe(var Obj);
function XStrToDateTimeDef(const S: string; const Default: TDateTime): TDateTime;
function XDateTimeToStr(const Default: TDateTime): String;
function ShortRect(const Rect1, Rect2: TIntRect): TIntRect;


var
  Bit8MainPalette: TRGBQuads;
  ColorTable_565: array[0..255] of Word;
  ColorTable_R5G6B5_32: array[0..65535] of Cardinal;
  ColorTable_A1R5G5B5: array[0..65535] of Word;
  DateTimeFormatSettings: TFormatSettings;
  ColorPalette: array[0..255] of Cardinal;
  g_DeviceProvider: TGraphicsDeviceProvider;
  g_GameDevice: TCustomSwapChainDevice;
  g_GameCanvas: PXL.Canvas.TCustomCanvas;
  g_DisplaySize: TPoint2i;
  EngineTimer: TMultimediaTimer;
  G_DPI:Cardinal;

implementation
  uses EDCode, WIL;
const
  ColorArray: array[0..1023] of Byte = (
    $00, $00, $00, $00, $00, $00, $80, $00, $00, $80, $00, $00, $00, $80, $80, $00,
    $80, $00, $00, $00, $80, $00, $80, $00, $80, $80, $00, $00, $C0, $C0, $C0, $00,
    $97, $80, $55, $00, $C8, $B9, $9D, $00, $73, $73, $7B, $00, $29, $29, $2D, $00,
    $52, $52, $5A, $00, $5A, $5A, $63, $00, $39, $39, $42, $00, $18, $18, $1D, $00,
    $10, $10, $18, $00, $18, $18, $29, $00, $08, $08, $10, $00, $71, $79, $F2, $00,
    $5F, $67, $E1, $00, $5A, $5A, $FF, $00, $31, $31, $FF, $00, $52, $5A, $D6, $00,
    $00, $10, $94, $00, $18, $29, $94, $00, $00, $08, $39, $00, $00, $10, $73, $00,
    $00, $18, $B5, $00, $52, $63, $BD, $00, $10, $18, $42, $00, $99, $AA, $FF, $00,
    $00, $10, $5A, $00, $29, $39, $73, $00, $31, $4A, $A5, $00, $73, $7B, $94, $00,
    $31, $52, $BD, $00, $10, $21, $52, $00, $18, $31, $7B, $00, $10, $18, $2D, $00,
    $31, $4A, $8C, $00, $00, $29, $94, $00, $00, $31, $BD, $00, $52, $73, $C6, $00,
    $18, $31, $6B, $00, $42, $6B, $C6, $00, $00, $4A, $CE, $00, $39, $63, $A5, $00,
    $18, $31, $5A, $00, $00, $10, $2A, $00, $00, $08, $15, $00, $00, $18, $3A, $00,
    $00, $00, $08, $00, $00, $00, $29, $00, $00, $00, $4A, $00, $00, $00, $9D, $00,
    $00, $00, $DC, $00, $00, $00, $DE, $00, $00, $00, $FB, $00, $52, $73, $9C, $00,
    $4A, $6B, $94, $00, $29, $4A, $73, $00, $18, $31, $52, $00, $18, $4A, $8C, $00,
    $11, $44, $88, $00, $00, $21, $4A, $00, $10, $18, $21, $00, $5A, $94, $D6, $00,
    $21, $6B, $C6, $00, $00, $6B, $EF, $00, $00, $77, $FF, $00, $84, $94, $A5, $00,
    $21, $31, $42, $00, $08, $10, $18, $00, $08, $18, $29, $00, $00, $10, $21, $00,
    $18, $29, $39, $00, $39, $63, $8C, $00, $10, $29, $42, $00, $18, $42, $6B, $00,
    $18, $4A, $7B, $00, $00, $4A, $94, $00, $7B, $84, $8C, $00, $5A, $63, $6B, $00,
    $39, $42, $4A, $00, $18, $21, $29, $00, $29, $39, $46, $00, $94, $A5, $B5, $00,
    $5A, $6B, $7B, $00, $94, $B1, $CE, $00, $73, $8C, $A5, $00, $5A, $73, $8C, $00,
    $73, $94, $B5, $00, $73, $A5, $D6, $00, $4A, $A5, $EF, $00, $8C, $C6, $EF, $00,
    $42, $63, $7B, $00, $39, $56, $6B, $00, $5A, $94, $BD, $00, $00, $39, $63, $00,
    $AD, $C6, $D6, $00, $29, $42, $52, $00, $18, $63, $94, $00, $AD, $D6, $EF, $00,
    $63, $8C, $A5, $00, $4A, $5A, $63, $00, $7B, $A5, $BD, $00, $18, $42, $5A, $00,
    $31, $8C, $BD, $00, $29, $31, $35, $00, $63, $84, $94, $00, $4A, $6B, $7B, $00,
    $5A, $8C, $A5, $00, $29, $4A, $5A, $00, $39, $7B, $9C, $00, $10, $31, $42, $00,
    $21, $AD, $EF, $00, $00, $10, $18, $00, $00, $21, $29, $00, $00, $6B, $9C, $00,
    $5A, $84, $94, $00, $18, $42, $52, $00, $29, $5A, $6B, $00, $21, $63, $7B, $00,
    $21, $7B, $9C, $00, $00, $A5, $DE, $00, $39, $52, $5A, $00, $10, $29, $31, $00,
    $7B, $BD, $CE, $00, $39, $5A, $63, $00, $4A, $84, $94, $00, $29, $A5, $C6, $00,
    $18, $9C, $10, $00, $4A, $8C, $42, $00, $42, $8C, $31, $00, $29, $94, $10, $00,
    $10, $18, $08, $00, $18, $18, $08, $00, $10, $29, $08, $00, $29, $42, $18, $00,
    $AD, $B5, $A5, $00, $73, $73, $6B, $00, $29, $29, $18, $00, $4A, $42, $18, $00,
    $4A, $42, $31, $00, $DE, $C6, $63, $00, $FF, $DD, $44, $00, $EF, $D6, $8C, $00,
    $39, $6B, $73, $00, $39, $DE, $F7, $00, $8C, $EF, $F7, $00, $00, $E7, $F7, $00,
    $5A, $6B, $6B, $00, $A5, $8C, $5A, $00, $EF, $B5, $39, $00, $CE, $9C, $4A, $00,
    $B5, $84, $31, $00, $6B, $52, $31, $00, $D6, $DE, $DE, $00, $B5, $BD, $BD, $00,
    $84, $8C, $8C, $00, $DE, $F7, $F7, $00, $18, $08, $00, $00, $39, $18, $08, $00,
    $29, $10, $08, $00, $00, $18, $08, $00, $00, $29, $08, $00, $A5, $52, $00, $00,
    $DE, $7B, $00, $00, $4A, $29, $10, $00, $6B, $39, $10, $00, $8C, $52, $10, $00,
    $A5, $5A, $21, $00, $5A, $31, $10, $00, $84, $42, $10, $00, $84, $52, $31, $00,
    $31, $21, $18, $00, $7B, $5A, $4A, $00, $A5, $6B, $52, $00, $63, $39, $29, $00,
    $DE, $4A, $10, $00, $21, $29, $29, $00, $39, $4A, $4A, $00, $18, $29, $29, $00,
    $29, $4A, $4A, $00, $42, $7B, $7B, $00, $4A, $9C, $9C, $00, $29, $5A, $5A, $00,
    $14, $42, $42, $00, $00, $39, $39, $00, $00, $59, $59, $00, $2C, $35, $CA, $00,
    $21, $73, $6B, $00, $00, $31, $29, $00, $10, $39, $31, $00, $18, $39, $31, $00,
    $00, $4A, $42, $00, $18, $63, $52, $00, $29, $73, $5A, $00, $18, $4A, $31, $00,
    $00, $21, $18, $00, $00, $31, $18, $00, $10, $39, $18, $00, $4A, $84, $63, $00,
    $4A, $BD, $6B, $00, $4A, $B5, $63, $00, $4A, $BD, $63, $00, $4A, $9C, $5A, $00,
    $39, $8C, $4A, $00, $4A, $C6, $63, $00, $4A, $D6, $63, $00, $4A, $84, $52, $00,
    $29, $73, $31, $00, $5A, $C6, $63, $00, $4A, $BD, $52, $00, $00, $FF, $10, $00,
    $18, $29, $18, $00, $4A, $88, $4A, $00, $4A, $E7, $4A, $00, $00, $5A, $00, $00,
    $00, $88, $00, $00, $00, $94, $00, $00, $00, $DE, $00, $00, $00, $EE, $00, $00,
    $00, $FB, $00, $00, $94, $5A, $4A, $00, $B5, $73, $63, $00, $D6, $8C, $7B, $00,
    $D6, $7B, $6B, $00, $FF, $88, $77, $00, $CE, $C6, $C6, $00, $9C, $94, $94, $00,
    $C6, $94, $9C, $00, $39, $31, $31, $00, $84, $18, $29, $00, $84, $00, $18, $00,
    $52, $42, $4A, $00, $7B, $42, $52, $00, $73, $5A, $63, $00, $F7, $B5, $CE, $00,
    $9C, $7B, $8C, $00, $CC, $22, $77, $00, $FF, $AA, $DD, $00, $2A, $B4, $F0, $00,
    $9F, $00, $DF, $00, $B3, $17, $E3, $00, $F0, $FB, $FF, $00, $A4, $A0, $A0, $00,
    $80, $80, $80, $00, $00, $00, $FF, $00, $00, $FF, $00, $00, $00, $FF, $FF, $00,
    $FF, $00, $00, $00, $FF, $00, $FF, $00, $FF, $FF, $00, $00, $FF, $FF, $FF, $00
  );

function M2StrToColor(const Value: String; Def: TColor): TColor;
begin
  Result  :=  Def;
  if Value = '' then Exit;
  try
    case Value[1] of
      '$', 'c':  Result  :=  StringToColor(Value);
      '#':
      begin
        if Length(Value) in [6, 7] then
          Result  :=  GraphUtil.WebColorStrToColor(Value);
      end;
      '0'..'9': Result  :=  GetRGB(StrToIntDef(Value, 0));
    end;
  except
  end;
end;

// 检查IP地址格式
function CheckIsIpAddr(Name: string): Boolean;
var
  PStr: char;
  Temp: PChar;
  i:    Integer;
begin
  result := True;
  if length(Name) <= 15 then
  begin
    for i := 0 to length(Name) do
    begin
      Temp := PChar(copy(Name, i, 1));
      PStr := Temp^;

      if not CharInSet(PStr, ['0' .. '9', '.']) then
      begin
        result := False;
        break
      end;
    end;
  end
  else
    result := False;
end;

function TickInterval(const EndTick, StartTick: LongWord): LongWord;
begin
  if EndTick >= StartTick then
    Result  :=  EndTick - StartTick
  else
    Result  :=  4294967295{High(LongWord)} - StartTick + EndTick;
end;

function GetRGB(c256: Byte): TColor;
begin
  Result := RGB(Bit8MainPalette[c256].rgbRed, Bit8MainPalette[c256].rgbGreen, Bit8MainPalette[c256].rgbBlue);
end;

function ISVlidateMaketName(const Name: String): Boolean;
begin
  Result  :=  True;
end;

function ConvertMaskChar(C: Char): String;
begin
  case C of
    '#':  Result  :=  '#35';
    '/':  Result  :=  '#47';
    '\':  Result  :=  '#92';
    '<':  Result  :=  '#60';
    '>':  Result  :=  '#62';
    '@':  Result  :=  '#64';
    '{':  Result  :=  '#123';
    '}':  Result  :=  '#125';
    '(':  Result  :=  '#40';
    ')':  Result  :=  '#41';
    ';':  Result  :=  '#59';
    '[':  Result  :=  '#91';
    ']':  Result  :=  '#93';
    ',':  Result  :=  '#44';
    '&':  Result  :=  '#38';
    '=':  Result  :=  '#61';
  else
    Result  :=  C;
  end;
end;

function ConvertMaskString(const Source: String): String;
begin
  Result  :=  StrUtils.ReplaceStr(Source, '#47', '/');
  Result  :=  StrUtils.ReplaceStr(Result, '#92', '\');
  Result  :=  StrUtils.ReplaceStr(Result, '#60', '<');
  Result  :=  StrUtils.ReplaceStr(Result, '#62', '>');
  Result  :=  StrUtils.ReplaceStr(Result, '#64', '@');
  Result  :=  StrUtils.ReplaceStr(Result, '#123', '{');
  Result  :=  StrUtils.ReplaceStr(Result, '#125', '}');
  Result  :=  StrUtils.ReplaceStr(Result, '#40', '(');
  Result  :=  StrUtils.ReplaceStr(Result, '#41', ')');
  Result  :=  StrUtils.ReplaceStr(Result, '#59', ';');
  Result  :=  StrUtils.ReplaceStr(Result, '#91', '[');
  Result  :=  StrUtils.ReplaceStr(Result, '#93', ']');
  Result  :=  StrUtils.ReplaceStr(Result, '#44', ',');
  Result  :=  StrUtils.ReplaceStr(Result, '#38', '&');
  Result  :=  StrUtils.ReplaceStr(Result, '#61', '=');
  Result  :=  StrUtils.ReplaceStr(Result, '#35', '#');
end;

function MakeMaskString(const Source: String): String;
begin
  Result  :=  StrUtils.ReplaceStr(Source, '#', '#35');
  Result  :=  StrUtils.ReplaceStr(Result, '/', '#47');
  Result  :=  StrUtils.ReplaceStr(Result, '\', '#92');
  Result  :=  StrUtils.ReplaceStr(Result, '<', '#60');
  Result  :=  StrUtils.ReplaceStr(Result, '>', '#62');
  Result  :=  StrUtils.ReplaceStr(Result, '@', '#64');
  Result  :=  StrUtils.ReplaceStr(Result, '{', '#123');
  Result  :=  StrUtils.ReplaceStr(Result, '}', '#125');
  Result  :=  StrUtils.ReplaceStr(Result, '(', '#40');
  Result  :=  StrUtils.ReplaceStr(Result, ')', '#41');
  Result  :=  StrUtils.ReplaceStr(Result, ';', '#59');
  Result  :=  StrUtils.ReplaceStr(Result, '[', '#91');
  Result  :=  StrUtils.ReplaceStr(Result, ']', '#93');
  Result  :=  StrUtils.ReplaceStr(Result, ',', '#44');
  Result  :=  StrUtils.ReplaceStr(Result, '&', '#38');
  Result  :=  StrUtils.ReplaceStr(Result, '=', '#61');
end;

procedure AddMaskChars(AddProc: TMaskCharAddProc);
begin
  AddProc('#35', '#');
  AddProc('#38', '&');
  AddProc('#47', '/');
  AddProc('#92', '\');
  AddProc('#60', '<');
  AddProc('#62', '>');
  AddProc('#64', '@');
  AddProc('#123', '{');
  AddProc('#125', '}');
  AddProc('#40', '(');
  AddProc('#41', ')');
  AddProc('#59', ';');
  AddProc('#91', '[');
  AddProc('#93', ']');
  AddProc('#44', ',');
  AddProc('#61', '=');
end;

function SaveStreamToString(Source: TStream): AnsiString;
var
  ACompStream: TStream;
begin
  ACompStream :=  TMemoryStream.Create;
  try
    Source.Seek(0, soBeginning);
    ZLib.ZCompressStream(Source, ACompStream);
    ACompStream.Seek(0, soBeginning);
    Result  :=  uTPLb_StreamUtils.Stream_to_Base64(ACompStream);
  finally
    ACompStream.Free;
  end;
end;

procedure LoadStreamFromString(const Source: AnsiString; Dest: TStream);
var
  AStream: TStream;
begin
  if Source <> '' then
  begin
    AStream :=  TMemoryStream.Create;
    try
      base64_to_stream(Source, AStream);
      AStream.Seek(0, soBeginning);
      ZLib.ZDecompressStream(AStream, Dest);
      Dest.Seek(0, soBeginning);
    finally
      AStream.Free;
    end;
  end;
end;

function MakeNewGUID38: String;
var
  AGuid: TGUID;
begin
  CreateGUID(AGuid);
  Result := GuidToString(AGuid);
end;

function MakeNewGUID36: String;
begin
  Result := Copy(MakeNewGUID38, 2, 36);
end;

function MakeNewGUID32: String;
begin
  Result := StrUtils.ReplaceStr(MakeNewGUID36, '-', '');
end;

function MakeNewGUID16: String;
var
  Tmp: String;
  I: Integer;
begin
  Tmp := MakeNewGUID32;
  for I := 1 to Length(Tmp) do
  begin
    if I mod 2 = 0 then
      Result  :=  Result + Tmp[I];
  end;
end;

function WidthBytes(Width, BitCount: integer): integer;
begin
  Result := (((Width * BitCount) + 31) div 32) * 4;
end;
{$IF RTLVersion>= 29.0}//xe8+
{$LEGACYIFEND ON}
{$IFEND}

{$IF RTLVersion>= 29.0}//xe8+
function ZLibPtr(Ptr:PAnsiChar):PByte;
begin
  Result := PByte(Ptr);
end;
{$ELSE}
function ZLibPtr(Ptr:PAnsiChar):PAnsiChar;
begin
  Result := Ptr;
end;
{$IFEND}

procedure CompressBufZ(const InBuf: PAnsiChar; InBytes: Integer; out OutBuf: PAnsiChar; out OutBytes: Integer);
const
  delta = 256;
var
  zstream: TZStreamRec;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  OutBytes := ((InBytes + (InBytes div 10) + 12) + 255) and not 255;
  GetMem(OutBuf, OutBytes);

  try
    zstream.next_in := ZLibPtr(InBuf);
    zstream.avail_in := InBytes;
    zstream.next_out := ZLibPtr(OutBuf);
    zstream.avail_out := OutBytes;

    DeflateInit_(zstream, Z_BEST_COMPRESSION, ZLIB_VERSION, SizeOf(TZStreamRec));
    try
      while deflate(zstream, Z_FINISH) <> Z_STREAM_END do
      begin
        Inc(OutBytes, delta);
        ReallocMem(OutBuf, OutBytes);
        zstream.next_out := ZLibPtr(PAnsiChar(Integer(OutBuf) + zstream.total_out));
        zstream.avail_out := delta;
      end;
    finally
      deflateEnd(zstream);
    end;

    ReallocMem(OutBuf, zstream.total_out);
    OutBytes := zstream.total_out;
  except
    FreeMem(OutBuf);
    raise;
  end;
end;


procedure DecompressBufZ(const inBuffer: PAnsiChar; inSize: Integer; outEstimate: Integer;
  out outBuffer: PAnsiChar; out outSize: Integer);
var
  zstream: TZStreamRec;
  delta: Integer;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);
  delta := (inSize + 255) and not 255;
  if outEstimate = 0 then
    outSize := delta
  else
    outSize := outEstimate;

  GetMem(outBuffer, outSize);
  zstream.next_in := ZLibPtr(inBuffer);
  zstream.avail_in := inSize;
  zstream.next_out := ZLibPtr(outBuffer);
  zstream.avail_out := outSize;
  inflateInit_(zstream, zlib_version, sizeof(zstream));
  try
    while inflate(zstream, Z_NO_FLUSH) <> Z_STREAM_END do
    begin
      Inc(outSize, delta);
      ReallocMem(outBuffer, outSize);

      zstream.next_out := ZLibPtr(PAnsiChar(Cardinal(outBuffer) + zstream.total_out));
      zstream.avail_out := delta;
    end;
  finally
    inflateEnd(zstream);
  end;

  ReallocMem(outBuffer, zstream.total_out);
  outSize := zstream.total_out;
end;

procedure FreeAndNilEx(var Obj);
var
  Temp: TObject;
begin
  try
    Temp := TObject(Obj);
    Pointer(Obj) := nil;
    Temp.Free;
  except
  end;
end;

procedure FreeAndNilSafe( var Obj);
var
  Temp: TObject;
begin
  try
    Temp := TObject(Obj);
    if Temp  <> nil then
    begin
      Pointer(Obj) := nil;
      Temp.Free;
    end;
  except
  end;
end;

function XStrToDateTimeDef(const S: string; const Default: TDateTime): TDateTime;
begin
  Result := StrToDateTimeDef(S, Default, DateTimeFormatSettings);
end;

function XDateTimeToStr(const Default: TDateTime): String;
begin
  Result := DateTimeToStr(Default, DateTimeFormatSettings);
end;

function ShortRect(const Rect1, Rect2: TIntRect): TIntRect;
begin
  Result.Left := Max(Rect1.Left, Rect2.Left);
  Result.Top := Max(Rect1.Top, Rect2.Top);
  Result.Right := Min(Rect1.Right, Rect2.Right);
  Result.Bottom := Min(Rect1.Bottom, Rect2.Bottom);
end;

//procedure Pixel565To32(Value: Word; var AResult: Cardinal);
//begin
//  AResult := $000000;
//  if Value > 0 then
//  begin
//    AResult := ((Value and 31) * 255) div 31;
//    AResult := AResult or (((((Value shr 5) and 63) * 255) div 63) shl 8);
//    AResult := AResult or (((((Value shr 11) and 31) * 255) div 31) shl 16);
//    AResult := AResult or $FF000000;
//  end;
//end;

procedure Pixel565To32(Value: Word; var AResult: Cardinal);
 var
  R, G, B, A: Byte;
begin
  if Value <> 0 then
  begin
    R := Value and $F800 shr 8;
    G := Value and $07E0 shr 3;
    B := Value and $001F shl 3;
    A := $FF;
    AResult := B or (G shl 8) or (R shl 16) or (A shl 24);
  end else
  begin
    AResult := 0;
  end;
end;

procedure Pixel32To1555(Source: Cardinal; var Value: Word);
var
  AValue: Cardinal;
begin
  Value := 0;
  if Source > 0 then
  begin
    AValue := ((Source and $FF) * 31) div 255;
    AValue := AValue or ((((Source shr 8) and $FF) * 31) div 255) shl 5;
    AValue := AValue or ((((Source shr 16) and $FF) * 31) div 255) shl 10;
    AValue := AValue or (((Source shr 24) and $FF) div 255) shl 15;
  end;
  Value := AValue;
end;

function RGBToASPColor(RGB:TRGBQuad):Cardinal;
begin
  Result := RGB.rgbBlue or (RGB.rgbGreen shl 8) or (RGB.rgbRed shl 16) or ($FF000000);
end;

procedure BuildColorTable;
var
  I: Integer;
  ACardinal: Cardinal;
begin
  Move(ColorArray[0], Bit8MainPalette[0], SizeOf(ColorArray));
  for I := 0 to Length(ColorTable_565) - 1 do
  begin
    if Integer(Bit8MainPalette[I]) = 0 then
      ColorTable_565[I] := 0
    else
      ColorTable_565[I] := Word((Max(Bit8MainPalette[I].rgbRed and $F8, 8) shl 8)
      or (Max(Bit8MainPalette[I].rgbGreen and $FC, 8) shl 3)
      or (Max(Bit8MainPalette[I].rgbBlue and $F8, 8) shr 3));
  end;

  for I := Low(Word) to High(Word) do
    Pixel565To32(I, ColorTable_R5G6B5_32[I]);
  for I := Low(Word) to High(Word) do
    Pixel32To1555(ColorTable_R5G6B5_32[I], ColorTable_A1R5G5B5[I]);

  for i := 0 to High(ColorPalette) do
  begin
    ColorPalette[i] := RGBToASPColor(Bit8MainPalette[i]);
  end;

end;

procedure UnBuildColorTable;
begin
  Finalize(Bit8MainPalette);
  Finalize(ColorTable_565);
  Finalize(ColorTable_R5G6B5_32);
  Finalize(ColorTable_A1R5G5B5);
end;

initialization
  BuildColorTable;
  DateTimeFormatSettings := TFormatSettings.Create;
  DateTimeFormatSettings.DateSeparator := '-';
  DateTimeFormatSettings.TimeSeparator := ':';
  DateTimeFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
  DateTimeFormatSettings.LongDateFormat := 'yyyy-MM-dd hh:mm:ss';

finalization
  UnBuildColorTable;

end.
