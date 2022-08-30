unit RSA;
interface
const
  ItemMaxLen                = 64;
type
  TInt64Item = record
    Length: Integer;
    Items: array[0..ItemMaxLen] of Int64;
  end;
  TChItem = array[0..ItemMaxLen] of Char;

procedure RSAEncode(Src: TChItem; Len: Integer; var Des: TInt64Item; E: Int64; N: Int64);
procedure RSADecode(Src: TInt64Item; var Des: TChItem; D: Int64; N: Int64);
procedure XORCode(XORMode: Char; Src: PChar; var Des: TChItem; Len: Integer);

implementation

function PowMod(base: Int64; pow: Int64; N: Int64): Int64;
var
  A, B, C                   : Int64;
begin
  A := base;
  B := pow;
  C := 1;
  while (B > 0) do begin
    while (not ((B and 1) > 0)) do begin
      B := B shr 1;
      A := A * A mod N;
    end;
    Dec(B);
    C := A * C mod N;
  end;
  Result := C;
end;

procedure RSAEncode(Src: TChItem; Len: Integer; var Des: TInt64Item; E: Int64; N: Int64);
var
  i                         : Integer;
begin
  Des.Length := Len;
  for i := 0 to Len - 1 do
    Des.Items[i] := PowMod(Int64(Src[i]), E, N);
end;

procedure RSADecode(Src: TInt64Item; var Des: TChItem; D: Int64; N: Int64);
var
  i, A                      : Integer;
begin
  FillChar(Des, ItemMaxLen, 0);
  for i := 0 to Src.Length - 1 do begin
    A := PowMod(Src.Items[i], D, N);
    Des[i] := Char(A);
  end;
end;

procedure XORCode(XORMode: Char; Src: PChar; var Des: TChItem; Len: Integer);
var
  i                         : Integer;
begin
  FillChar(Des, ItemMaxLen, 0);
  for i := 0 to Len - 1 do
    Des[i] := Char(Integer(Src[i]) xor Integer(XORMode));
end;

end.
