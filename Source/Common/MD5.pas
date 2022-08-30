unit MD5;

interface

uses
  Windows, SysUtils;

type
  MD5Count = array[0..1] of DWORD;
  MD5State = array[0..3] of DWORD;
  MD5Block = array[0..15] of DWORD;
  MD5CBits = array[0..7] of byte;

  MD5Digest = array[0..15] of byte;
  PMD5Digest = ^MD5Digest;

  MD5Buffer = array[0..63] of byte;
  MD5Context = record
    State: MD5State;
    Count: MD5Count;
    Buffer: MD5Buffer;
  end;

procedure MD5Init(var Context: MD5Context);
procedure MD5Update(var Context: MD5Context; Input: PChar; Length: LongWord);
procedure MD5Final(var Context: MD5Context; var Digest: MD5Digest);

function MD5String(m: string): MD5Digest;
function MD5File(n: string): MD5Digest;
function MD5Print(d: MD5Digest): string;
function MD5UnPrint(s: string): MD5Digest;
function RivestStr(Str: string): string;       //MD5 string
function RivestFile(FileName: string): string; //MD5 file
function MD5Match(D1, D2: MD5Digest): Boolean;

var
  g_MD5EmptyDigest: MD5Digest = (00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00);

  // -----------------------------------------------------------------------------------------------
implementation
// -----------------------------------------------------------------------------------------------

var
  PADDING: MD5Buffer = (
    $80, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00
    );

function F(x, y, z: DWORD): DWORD;
begin
  Result := (x and y) or ((not x) and z);
end;

function g(x, y, z: DWORD): DWORD;
begin
  Result := (x and z) or (y and (not z));
end;

function H(x, y, z: DWORD): DWORD;
begin
  Result := x xor y xor z;
end;

function i(x, y, z: DWORD): DWORD;
begin
  Result := y xor (x or (not z));
end;

procedure rot(var x: DWORD; n: byte);
begin
  x := (x shl n) or (x shr (32 - n));
end;

procedure FF(var a: DWORD; b, c, d, x: DWORD; s: byte; AC: DWORD);
begin
  Inc(a, F(b, c, d) + x + AC);
  rot(a, s);
  Inc(a, b);
end;

procedure GG(var a: DWORD; b, c, d, x: DWORD; s: byte; AC: DWORD);
begin
  Inc(a, g(b, c, d) + x + AC);
  rot(a, s);
  Inc(a, b);
end;

procedure HH(var a: DWORD; b, c, d, x: DWORD; s: byte; AC: DWORD);
begin
  Inc(a, H(b, c, d) + x + AC);
  rot(a, s);
  Inc(a, b);
end;

procedure ii(var a: DWORD; b, c, d, x: DWORD; s: byte; AC: DWORD);
begin
  Inc(a, i(b, c, d) + x + AC);
  rot(a, s);
  Inc(a, b);
end;

// -----------------------------------------------------------------------------------------------

// Encode Count bytes at Source into (Count / 4) DWORDs at Target

procedure Encode(Source, Target: Pointer; Count: LongWord);
var
  s: pByte;
  t: PDWORD;
  i: LongWord;
begin
  s := Source;
  t := Target;
  for i := 1 to Count div 4 do begin
    t^ := s^;
    Inc(s);
    t^ := t^ or (s^ shl 8);
    Inc(s);
    t^ := t^ or (s^ shl 16);
    Inc(s);
    t^ := t^ or (s^ shl 24);
    Inc(s);
    Inc(t);
  end;
end;

// Decode Count DWORDs at Source into (Count * 4) Bytes at Target

procedure Decode(Source, Target: Pointer; Count: LongWord);
var
  s: PDWORD;
  t: pByte;
  i: LongWord;
begin
  s := Source;
  t := Target;
  for i := 1 to Count do begin
    t^ := s^ and $FF;
    Inc(t);
    t^ := (s^ shr 8) and $FF;
    Inc(t);
    t^ := (s^ shr 16) and $FF;
    Inc(t);
    t^ := (s^ shr 24) and $FF;
    Inc(t);
    Inc(s);
  end;
end;

// Transform State according to first 64 bytes at Buffer

procedure Transform(Buffer: Pointer; var State: MD5State);
var
  a, b, c, d: DWORD;
  Block: MD5Block;
begin
  Encode(Buffer, @Block, 64);
  a := State[0];
  b := State[1];
  c := State[2];
  d := State[3];
  FF(a, b, c, d, Block[0], 7, $D76AA478);
  FF(d, a, b, c, Block[1], 12, $E8C7B756);
  FF(c, d, a, b, Block[2], 17, $242070DB);
  FF(b, c, d, a, Block[3], 22, $C1BDCEEE);
  FF(a, b, c, d, Block[4], 7, $F57C0FAF);
  FF(d, a, b, c, Block[5], 12, $4787C62A);
  FF(c, d, a, b, Block[6], 17, $A8304613);
  FF(b, c, d, a, Block[7], 22, $FD469501);
  FF(a, b, c, d, Block[8], 7, $698098D8);
  FF(d, a, b, c, Block[9], 12, $8B44F7AF);
  FF(c, d, a, b, Block[10], 17, $FFFF5BB1);
  FF(b, c, d, a, Block[11], 22, $895CD7BE);
  FF(a, b, c, d, Block[12], 7, $6B901122);
  FF(d, a, b, c, Block[13], 12, $FD987193);
  FF(c, d, a, b, Block[14], 17, $A679438E);
  FF(b, c, d, a, Block[15], 22, $49B40821);
  GG(a, b, c, d, Block[1], 5, $F61E2562);
  GG(d, a, b, c, Block[6], 9, $C040B340);
  GG(c, d, a, b, Block[11], 14, $265E5A51);
  GG(b, c, d, a, Block[0], 20, $E9B6C7AA);
  GG(a, b, c, d, Block[5], 5, $D62F105D);
  GG(d, a, b, c, Block[10], 9, $2441453);
  GG(c, d, a, b, Block[15], 14, $D8A1E681);
  GG(b, c, d, a, Block[4], 20, $E7D3FBC8);
  GG(a, b, c, d, Block[9], 5, $21E1CDE6);
  GG(d, a, b, c, Block[14], 9, $C33707D6);
  GG(c, d, a, b, Block[3], 14, $F4D50D87);
  GG(b, c, d, a, Block[8], 20, $455A14ED);
  GG(a, b, c, d, Block[13], 5, $A9E3E905);
  GG(d, a, b, c, Block[2], 9, $FCEFA3F8);
  GG(c, d, a, b, Block[7], 14, $676F02D9);
  GG(b, c, d, a, Block[12], 20, $8D2A4C8A);
  HH(a, b, c, d, Block[5], 4, $FFFA3942);
  HH(d, a, b, c, Block[8], 11, $8771F681);
  HH(c, d, a, b, Block[11], 16, $6D9D6122);
  HH(b, c, d, a, Block[14], 23, $FDE5380C);
  HH(a, b, c, d, Block[1], 4, $A4BEEA44);
  HH(d, a, b, c, Block[4], 11, $4BDECFA9);
  HH(c, d, a, b, Block[7], 16, $F6BB4B60);
  HH(b, c, d, a, Block[10], 23, $BEBFBC70);
  HH(a, b, c, d, Block[13], 4, $289B7EC6);
  HH(d, a, b, c, Block[0], 11, $EAA127FA);
  HH(c, d, a, b, Block[3], 16, $D4EF3085);
  HH(b, c, d, a, Block[6], 23, $4881D05);
  HH(a, b, c, d, Block[9], 4, $D9D4D039);
  HH(d, a, b, c, Block[12], 11, $E6DB99E5);
  HH(c, d, a, b, Block[15], 16, $1FA27CF8);
  HH(b, c, d, a, Block[2], 23, $C4AC5665);
  ii(a, b, c, d, Block[0], 6, $F4292244);
  ii(d, a, b, c, Block[7], 10, $432AFF97);
  ii(c, d, a, b, Block[14], 15, $AB9423A7);
  ii(b, c, d, a, Block[5], 21, $FC93A039);
  ii(a, b, c, d, Block[12], 6, $655B59C3);
  ii(d, a, b, c, Block[3], 10, $8F0CCC92);
  ii(c, d, a, b, Block[10], 15, $FFEFF47D);
  ii(b, c, d, a, Block[1], 21, $85845DD1);
  ii(a, b, c, d, Block[8], 6, $6FA87E4F);
  ii(d, a, b, c, Block[15], 10, $FE2CE6E0);
  ii(c, d, a, b, Block[6], 15, $A3014314);
  ii(b, c, d, a, Block[13], 21, $4E0811A1);
  ii(a, b, c, d, Block[4], 6, $F7537E82);
  ii(d, a, b, c, Block[11], 10, $BD3AF235);
  ii(c, d, a, b, Block[2], 15, $2AD7D2BB);
  ii(b, c, d, a, Block[9], 21, $EB86D391);
  Inc(State[0], a);
  Inc(State[1], b);
  Inc(State[2], c);
  Inc(State[3], d);
end;

// -----------------------------------------------------------------------------------------------

// Initialize given Context

procedure MD5Init(var Context: MD5Context);
begin
  with Context do begin
    State[0] := $67452301;
    State[1] := $EFCDAB89;
    State[2] := $98BADCFE;
    State[3] := $10325476;
    Count[0] := 0;
    Count[1] := 0;
    ZeroMemory(@Buffer, SizeOf(MD5Buffer));
  end;
end;

// Update given Context to include Length bytes of Input

procedure MD5Update(var Context: MD5Context; Input: PChar; Length: LongWord);
var
  Index: LongWord;
  PartLen: LongWord;
  i: LongWord;
begin
  with Context do begin
    Index := (Count[0] shr 3) and $3F;
    Inc(Count[0], Length shl 3);
    if Count[0] < (Length shl 3) then Inc(Count[1]);
    Inc(Count[1], Length shr 29);
  end;
  PartLen := 64 - Index;
  if Length >= PartLen then begin
    CopyMemory(@Context.Buffer[Index], Input, PartLen);
    Transform(@Context.Buffer, Context.State);
    i := PartLen;
    while i + 63 < Length do begin
      Transform(@Input[i], Context.State);
      Inc(i, 64);
    end;
    Index := 0;
  end else i := 0;
  CopyMemory(@Context.Buffer[Index], @Input[i], Length - i);
end;

// Finalize given Context, create Digest and zeroize Context

procedure MD5Final(var Context: MD5Context; var Digest: MD5Digest);
var
  Bits: MD5CBits;
  Index: LongWord;
  PadLen: LongWord;
begin
  Decode(@Context.Count, @Bits, 2);
  Index := (Context.Count[0] shr 3) and $3F;
  if Index < 56 then PadLen := 56 - Index else PadLen := 120 - Index;
  MD5Update(Context, @PADDING, PadLen);
  MD5Update(Context, @Bits, 8);
  Decode(@Context.State, @Digest, 4);
  ZeroMemory(@Context, SizeOf(MD5Context));
end;

// -----------------------------------------------------------------------------------------------

// Create digest of given Message

function MD5String(m: string): MD5Digest;
var
  Context: MD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, PChar(m), Length(m));
  MD5Final(Context, Result);
end;

// Create digest of file with given Name

function MD5File(n: string): MD5Digest;
var
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
  Context: MD5Context;
begin
  MD5Init(Context);
  FileHandle := CreateFile(PChar(n), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if FileHandle <> INVALID_HANDLE_VALUE then try
    MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if MapHandle <> 0 then try
      ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
      if ViewPointer <> nil then try
        MD5Update(Context, ViewPointer, GetFileSize(FileHandle, nil));
      finally
        UnmapViewOfFile(ViewPointer);
      end;
    finally
      CloseHandle(MapHandle);
    end;
  finally
    CloseHandle(FileHandle);
  end;
  MD5Final(Context, Result);
end;

// Create hex representation of given Digest

function MD5Print(d: MD5Digest): string;
var
  i: byte;
const
  Digits: array[0..15] of Char =
  ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  Result := '';
  for i := 0 to 15 do Result := Result + Digits[(d[i] shr 4) and $0F] + Digits[d[i] and $0F];
end;

function MD5UnPrint(s: string): MD5Digest;
var
  i: byte;
  Digest: MD5Digest;
begin
  FillChar(Result, SizeOf(MD5Digest), 0);
  if Length(s) = 32 then begin
    try
      for i := 0 to 15 do begin
        Digest[i] := StrToInt('$' + Copy(s, 1 + i * 2, 2));
      end;
    except
      FillChar(Digest, SizeOf(MD5Digest), 0);
    end;
  end;
  Result := Digest;
end;

// -----------------------------------------------------------------------------------------------

// Compare two Digests

function MD5Match(D1, D2: MD5Digest): Boolean;
var
  i: byte;
begin
  i := 0;
  Result := True;
  while Result and (i < 16) do begin
    Result := D1[i] = D2[i];
    Inc(i);
  end;
end;

function RivestStr(Str: string): string;
begin
  Result := MD5Print(MD5String(Str));
end;

function RivestFile(FileName: string): string;
begin
  Result := MD5Print(MD5File(FileName));
end;

end.
