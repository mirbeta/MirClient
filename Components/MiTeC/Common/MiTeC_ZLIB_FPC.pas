unit MiTeC_ZLib_FPC;

interface

uses
  Windows,
  Sysutils,
  Classes,
  ZLIB;

type
  TZAlloc = function(opaque: Pointer; items, size: Integer): Pointer;
  TZFree = procedure(opaque, block: Pointer);
  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);

  TCompressionLevel = (clNone = Integer(zcNone), clFastest, clDefault, clMax);

  TCustomZStream = class(TStream)
  private
    FStream: TStream;
    FStreamPos: Int64;
    FOnProgress: TNotifyEvent;
    FZStream: TZStreamRec;
    FBuffer: array[Word] of AnsiChar;
  protected
    constructor Create(stream: TStream);
    procedure DoProgress; dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  TCustomZLibStream = TCustomZStream;

  TZCompressionStream = class(TCustomZStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(dest: TStream; compressionLevel: TZCompressionLevel = zcDefault); overload;
    constructor Create(compressionLevel: TCompressionLevel; dest: TStream); overload;
    destructor Destroy; override;
    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Seek(offset: Longint; origin: Word): Longint; override;
    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

  TCompressionStream = TZCompressionStream;

  TZDecompressionStream = class(TCustomZStream)
  public
    constructor Create(source: TStream);
    destructor Destroy; override;
    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Seek(offset: Longint; origin: Word): Longint; override;
    property OnProgress;
  end;

  TDecompressionStream = TZDecompressionStream;

procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TZCompressionLevel = zcDefault); overload;

procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TCompressionLevel); overload; inline;

procedure ZDecompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer = 0);

function ZCompressStr(const s: string; level: TZCompressionLevel = zcDefault): TBytes; overload;
function ZCompressStr(const s: string; level: TCompressionLevel): TBytes; overload; inline;

function ZDecompressStr(const s: TBytes): string;

procedure ZCompressStream(inStream, outStream: TStream;
  level: TZCompressionLevel = zcDefault); overload;
procedure ZCompressStream(inStream, outStream: TStream;
  level: TCompressionLevel); overload; inline;

procedure ZDecompressStream(inStream, outStream: TStream);

function zlibAllocMem(AppData: Pointer; Items, Size: Integer): Pointer;
procedure zlibFreeMem(AppData, Block: Pointer);

type
  EZLibError = class(Exception);
  EZCompressionError = class(EZLibError);
  EZDecompressionError = class(EZLibError);

const
  _z_errmsg: array[0..9] of PChar = (
    'need dictionary', // Z_NEED_DICT      (2)   //do not localize
    'stream end', // Z_STREAM_END     (1)        //do not localize
    '', // Z_OK             (0)                  //do not localize
    'file error', // Z_ERRNO          (-1)       //do not localize
    'stream error', // Z_STREAM_ERROR   (-2)     //do not localize
    'data error', // Z_DATA_ERROR     (-3)       //do not localize
    'insufficient memory', // Z_MEM_ERROR      (-4)//do not localize
    'buffer error', // Z_BUF_ERROR      (-5)       //do not localize
    'incompatible version', // Z_VERSION_ERROR  (-6)//do not localize
    ''//do not localize
    );

  ZLevels: array[TZCompressionLevel] of Shortint = (
    Z_NO_COMPRESSION,
    Z_BEST_SPEED,
    Z_DEFAULT_COMPRESSION,
    Z_BEST_COMPRESSION
    );

  SZInvalid = 'Invalid ZStream operation!';

implementation

function ZCompressCheck(code: Integer): Integer;
begin
  result := code;

  if code < 0 then
  begin
    raise EZCompressionError.Create(_z_errmsg[2 - code]);
  end;
end;

function ZDecompressCheck(code: Integer): Integer;
begin
  Result := code;

  if code < 0 then
  begin
    raise EZDecompressionError.Create(_z_errmsg[2 - code]);
  end;
end;

procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TZCompressionLevel);
const
  delta = 256;
var
  zstream: TZStreamRec;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;
  GetMem(outBuffer, outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    ZCompressCheck(DeflateInit(zstream, ZLevels[level]));

    try
      while ZCompressCheck(deflate(zstream, Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(outSize, delta);
        ReallocMem(outBuffer, outSize);

        zstream.next_out := pBytef(PAnsiChar(Integer(outBuffer) + zstream.total_out));
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(deflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TCompressionLevel);
begin
  ZCompress(inBuffer, inSize, outBuffer, outSize, TZCompressionLevel(Integer(level)));
end;

procedure ZCompress2(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer);
const
  delta = 256;
var
  zstream: TZStreamRec;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;
  GetMem(outBuffer, outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    ZCompressCheck(DeflateInit2(zstream, 1, 8, -15, 9, 0));

    try
      while ZCompressCheck(deflate(zstream, Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(outSize, delta);
        ReallocMem(outBuffer, outSize);

        zstream.next_out := pBytef(PAnsiChar(Integer(outBuffer) + zstream.total_out));
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(deflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

procedure ZDecompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer);
var
  zstream: TZStreamRec;
  delta: Integer;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  delta := (inSize + 255) and not 255;

  if outEstimate = 0 then outSize := delta
  else outSize := outEstimate;

  GetMem(outBuffer, outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    ZDecompressCheck(InflateInit(zstream));

    try
      while ZDecompressCheck(inflate(zstream, Z_NO_FLUSH)) <> Z_STREAM_END do
      begin
        Inc(outSize, delta);
        ReallocMem(outBuffer, outSize);

        zstream.next_out := pBytef(PAnsiChar(Integer(outBuffer) + zstream.total_out));
        zstream.avail_out := delta;
      end;
    finally
      ZDecompressCheck(inflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

function ZCompressStr(const s: string; level: TZCompressionLevel): TBytes;
var
  buffer: Pointer;
  size: Integer;
begin
  ZCompress(PChar(s), Length(s) * SizeOf(Char), buffer, size, level);
  SetLength(result, size);
  Move(buffer^, pointer(result)^, size);
  FreeMem(buffer);
end;

function ZCompressStr(const s: string; level: TCompressionLevel): TBytes;
begin
  Result := ZCompressStr(s, TZCompressionLevel(Integer(level)));
end;

procedure ZSendToBrowser(var s: string);
var
  outBuf: Pointer;
  outBytes: Integer;
begin
  ZCompress2(pointer(s), ByteLength(s), outBuf, outBytes);
  SetLength(s, outBytes);
  Move(pointer(outBuf)^, pointer(s)^, outBytes);
  FreeMem(outBuf);
end;

function ZDecompressStr(const s: TBytes): string;
var
  buffer: Pointer;
  size: Integer;
begin
  ZDecompress(Pointer(s), Length(s), buffer, size);
  SetLength(result, size div SizeOf(Char));
  Move(buffer^, pointer(result)^, size);
  FreeMem(buffer);
end;

procedure ZCompressStream(inStream, outStream: TStream;
  level: TZCompressionLevel);
const
  bufferSize = 32768;
var
  zstream: TZStreamRec;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of AnsiChar;
  outBuffer: array[0..bufferSize - 1] of AnsiChar;
  inSize: Integer;
  outSize: Integer;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  ZCompressCheck(DeflateInit(zstream, ZLevels[level]));

  inSize := inStream.Read(inBuffer, bufferSize);

  while inSize > 0 do
  begin
    zstream.next_in := pBytef(@inBuffer);
    zstream.avail_in := inSize;

    repeat
      zstream.next_out := pBytef(@outBuffer);
      zstream.avail_out := bufferSize;

      ZCompressCheck(deflate(zstream, Z_NO_FLUSH));

      outSize := bufferSize - zstream.avail_out;

      outStream.Write(outBuffer, outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);

    inSize := inStream.Read(inBuffer, bufferSize);
  end;

  repeat
    zstream.next_out := pBytef(@outBuffer);
    zstream.avail_out := bufferSize;

    zresult := ZCompressCheck(deflate(zstream, Z_FINISH));

    outSize := bufferSize - zstream.avail_out;

    outStream.Write(outBuffer, outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);

  ZCompressCheck(deflateEnd(zstream));
end;

procedure ZCompressStream(inStream, outStream: TStream;
  level: TCompressionLevel);
begin
  ZCompressStream(inStream, outStream, TZCompressionLevel(Integer(level)))
end;

procedure ZDecompressStream(inStream, outStream: TStream);
const
  bufferSize = 32768;
var
  zstream: TZStreamRec;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of AnsiChar;
  outBuffer: array[0..bufferSize - 1] of AnsiChar;
  inSize: Integer;
  outSize: Integer;
begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  ZCompressCheck(InflateInit(zstream));

  inSize := inStream.Read(inBuffer, bufferSize);

  while inSize > 0 do
  begin
    zstream.next_in := pBytef(@inBuffer);
    zstream.avail_in := inSize;

    repeat
      zstream.next_out := pBytef(@outBuffer);
      zstream.avail_out := bufferSize;

      ZCompressCheck(inflate(zstream, Z_NO_FLUSH));

      outSize := bufferSize - zstream.avail_out;

      outStream.Write(outBuffer, outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);

    inSize := inStream.Read(inBuffer, bufferSize);
  end;

  repeat
    zstream.next_out := pBytef(@outBuffer);
    zstream.avail_out := bufferSize;

    zresult := ZCompressCheck(inflate(zstream, Z_FINISH));

    outSize := bufferSize - zstream.avail_out;

    outStream.Write(outBuffer, outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);

  ZCompressCheck(inflateEnd(zstream));
end;

{ TCustomZStream }

constructor TCustomZStream.Create(stream: TStream);
begin
  inherited Create;
  FStream := stream;
  FStreamPos := stream.Position;
end;

procedure TCustomZStream.DoProgress;
begin
  if Assigned(FOnProgress) then FOnProgress(Self);
end;

{ TZCompressionStream }

constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TZCompressionLevel);
begin
  inherited Create(dest);

  FZStream.next_out := pBytef(@FBuffer);
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(DeflateInit(FZStream, ZLevels[compressionLevel]));
end;

constructor TZCompressionStream.Create(compressionLevel: TCompressionLevel; dest: TStream);
begin
  Create(dest, TZCompressionLevel(Byte(compressionLevel)));
end;

destructor TZCompressionStream.Destroy;
begin
  FZStream.next_in := nil;
  FZStream.avail_in := 0;

  try
    if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

    while ZCompressCheck(deflate(FZStream, Z_FINISH)) <> Z_STREAM_END do
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);

      FZStream.next_out := pBytef(@FBuffer);
      FZStream.avail_out := SizeOf(FBuffer);
    end;

    if FZStream.avail_out < SizeOf(FBuffer) then
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);
    end;
  finally
    deflateEnd(FZStream);
  end;

  inherited Destroy;
end;

function TZCompressionStream.Read(var buffer; count: Longint): Longint;
begin
  raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.Write(const buffer; count: Longint): Longint;
begin
  FZStream.next_in := @buffer;
  FZStream.avail_in := count;

  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  while FZStream.avail_in > 0 do
  begin
    ZCompressCheck(deflate(FZStream, Z_NO_FLUSH));

    if FZStream.avail_out = 0 then
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer));

      FZStream.next_out := pBytef(@FBuffer);
      FZStream.avail_out := SizeOf(FBuffer);

      FStreamPos := FStream.Position;

      DoProgress;
    end;
  end;

  result := Count;
end;

function TZCompressionStream.Seek(offset: Longint; origin: Word): Longint;
begin
  if (offset = 0) and (origin = soFromCurrent) then
  begin
    result := FZStream.total_in;
  end
  else raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.GetCompressionRate: Single;
begin
  if FZStream.total_in = 0 then result := 0
  else result := (1.0 - (FZStream.total_out / FZStream.total_in)) * 100.0;
end;

{ TZDecompressionStream }

constructor TZDecompressionStream.Create(source: TStream);
begin
  inherited Create(source);
  FZStream.next_in := pBytef(@FBuffer);
  FZStream.avail_in := 0;
  ZDecompressCheck(InflateInit(FZStream));
end;

destructor TZDecompressionStream.Destroy;
begin
  inflateEnd(FZStream);
  inherited Destroy;
end;

function TZDecompressionStream.Read(var buffer; count: Longint): Longint;
var
  zresult: Integer;
begin
  FZStream.next_out := @buffer;
  FZStream.avail_out := count;

  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  zresult := Z_OK;

  while (FZStream.avail_out > 0) and (zresult <> Z_STREAM_END) do
  begin
    if FZStream.avail_in = 0 then
    begin
      FZStream.avail_in := FStream.Read(FBuffer, SizeOf(FBuffer));

      if FZStream.avail_in = 0 then
      begin
        result := count - FZStream.avail_out;

        Exit;
      end;

      FZStream.next_in := pBytef(@FBuffer);
      FStreamPos := FStream.Position;

      DoProgress;
    end;

    zresult := ZDecompressCheck(inflate(FZStream, Z_NO_FLUSH));
  end;

  if (zresult = Z_STREAM_END) and (FZStream.avail_in > 0) then
  begin
    FStream.Position := FStream.Position - FZStream.avail_in;
    FStreamPos := FStream.Position;

    FZStream.avail_in := 0;
  end;

  result := count - FZStream.avail_out;
end;

function TZDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EZDecompressionError.Create(SZInvalid);
end;

function TZDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  buf: array[0..8191] of Char;
  i: Integer;
begin
  if (offset = 0) and (origin = soFromBeginning) then
  begin
    ZDecompressCheck(inflateReset(FZStream));

    FZStream.next_in := pBytef(@FBuffer);
    FZStream.avail_in := 0;

    FStream.Position := 0;
    FStreamPos := 0;
  end
  else if ((offset >= 0) and (origin = soFromCurrent)) or
    (((offset - FZStream.total_out) > 0) and (origin = soFromBeginning)) then
  begin
    if origin = soFromBeginning then Dec(offset, FZStream.total_out);

    if offset > 0 then
    begin
      for i := 1 to offset div SizeOf(buf) do ReadBuffer(buf, SizeOf(buf));
      ReadBuffer(buf, offset mod SizeOf(buf));
    end;
  end
  else if (offset = 0) and (origin = soFromEnd) then
  begin
    while Read(buf, SizeOf(buf)) > 0 do ;
  end
  else raise EZDecompressionError.Create(SZInvalid);

  result := FZStream.total_out;
end;

function zlibAllocMem(AppData: Pointer; Items, Size: Integer): Pointer;
{$IFDEF MSWINDOWS}
  register;
{$ENDIF}
{$IFDEF LINUX}
  cdecl;
{$ENDIF}
begin
  Result := AllocMem(Items * Size);
end;

procedure zlibFreeMem(AppData, Block: Pointer);
{$IFDEF MSWINDOWS}
  register;
{$ENDIF}
{$IFDEF LINUX}
  cdecl;
{$ENDIF}
begin
  FreeMem(Block);
end;

end.
