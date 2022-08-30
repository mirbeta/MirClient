unit vCardBase64;

interface

function Decode64(const S: ANSIString): ANSIString;
function Encode64(const Input: string): string;

implementation

const
  Codes64:ANSIString = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';


function Decode64(const S: ANSIString): ANSIString;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Pos(s[i], codes64) - 1;
    if x >= 0 then
    begin
      b := b * 64 + x;
      a := a + 6;
      if a >= 8 then
      begin
        a := a - 8;
        x := b shr a;
        b := b AND ((1 SHL a)-1);
        Result := Result + ANSIChar(chr(x));
      end;
    end
    else
      Exit;
  end;
end;

var
  EncodeTable: array[0..63] of Char =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
    'abcdefghijklmnopqrstuvwxyz' +
    '0123456789+/';

  DecodeTable: array[#0..#127] of Integer = (
    Byte('='), 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 62, 64, 64, 64, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 64, 64, 64, 64, 64, 64,
    64,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64,
    64, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 64, 64, 64, 64, 64);

type
  PPacket = ^TPacket;
  TPacket = packed record
    case Integer of
      0: (b0, b1, b2, b3: Byte);
      1: (i: Integer);
      2: (a: array[0..3] of Byte);
      3: (c: array[0..3] of Char);
  end;

procedure EncodePacket(const Packet: TPacket; NumChars: Integer; OutBuf: PChar);
begin
  OutBuf[0] := EnCodeTable[Packet.a[0] shr 2];
  OutBuf[1] := EnCodeTable[((Packet.a[0] shl 4) or (Packet.a[1] shr 4)) and $0000003f];

  if NumChars < 2 then
    OutBuf[2] := '='
  else
    OutBuf[2] := EnCodeTable[((Packet.a[1] shl 2) or (Packet.a[2] shr 6)) and $0000003f];

  if NumChars < 3 then
    OutBuf[3] := '='
  else
    OutBuf[3] := EnCodeTable[Packet.a[2] and $0000003f];
end;

function Encode64(const Input: string): string;
var
  I, K, J: Integer;
  Packet: TPacket;
begin
  Result := '';
  I := (Length(Input) div 3) * 4;
  if Length(Input) mod 3 > 0 then Inc(I, 4);
  SetLength(Result, I);
  J := 1;
  for I := 1 to Length(Input) div 3 do
  begin
    Packet.i := 0;
    Packet.a[0] := Byte(Input[(I - 1) * 3 + 1]);
    Packet.a[1] := Byte(Input[(I - 1) * 3 + 2]);
    Packet.a[2] := Byte(Input[(I - 1) * 3 + 3]);
    EncodePacket(Packet, 3, PChar(@Result[J]));
    Inc(J, 4);
  end;
  K := 0;
  Packet.i := 0;
  for I := Length(Input) - (Length(Input) mod 3) + 1 to Length(Input) do
  begin
    Packet.a[K] := Byte(Input[I]);
    Inc(K);
    if I = Length(Input) then
      EncodePacket(Packet, Length(Input) mod 3, PChar(@Result[J]));
  end;
end;


end.
