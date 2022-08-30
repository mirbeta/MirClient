unit uFilePICConverter;

interface
  uses Classes, SysUtils, Graphics, Math, DBXCommon;

function FileToBitmap(const AFileName: String): TBitmap;
function StreamToBitmap(AStream: TStream): TBitmap; overload;
procedure WriteBitmapFileToStream(const AFileName: String; AStream: TStream);
procedure WriteBitmapStreamToStream(ABitmap, AStream: TStream);

implementation

function FileToBitmap(const AFileName: String): TBitmap;
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    F.Position := 0;
    Result := StreamToBitmap(F);
  finally
    F.Free;
  end;
end;

function StreamToBitmap(AStream: TStream): TBitmap;
var
  ASqrt: Integer;
  ASize, Row, ALineSize: Integer;
  ALine: PByteArray;
  Col: Integer;
begin
  ASize := AStream.Size;
  ASqrt := Ceil(Sqrt((ASize + 6) / 3));

  Result := TBitmap.Create;
  Result.PixelFormat := pf24Bit;
  Result.Width := ASqrt;
  Result.Height := ASqrt;
  for Row := 0 to ASqrt - 1 do
  begin
    ALine := Result.ScanLine[Row];
    if Row = 0 then
    begin
      ALine[0] := Byte(ASize);
      ALine[1] := Byte(ASize shr 8);
      ALine[2] := Byte(ASize shr 16);
      ALine[3] := Byte(ASize shr 24);
      ALine[4] := 0;
      ALine[5] := 0;
      ALineSize := ASqrt * 3 - 6;
      ALineSize := Min(ALineSize, AStream.Size - AStream.Position);
      AStream.ReadBuffer(ALine[6], ALineSize);
    end
    else
    begin
      ALineSize := ASqrt * 3;
      ALineSize := Min(ALineSize, AStream.Size - AStream.Position);
      AStream.ReadBuffer(ALine[0], ALineSize);
    end;
  end;
end;

procedure WriteBitmapFileToStream(const AFileName: String; AStream: TStream);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    WriteBitmapStreamToStream(F, AStream);
  finally
    F.Free;
  end;
end;

procedure WriteBitmapStreamToStream(ABitmap, AStream: TStream);

  procedure Write(const Value: Byte);
  begin
    AStream.WriteBuffer(Value, 1);
  end;

var
  Row, Col, ASize, ALineSize, AReadSize: Integer;
  ALine: PByteArray;
  ABmp: TBitmap;
begin
  ABmp := TBitmap.Create;
  try
    ABmp.PixelFormat := pf24Bit;
    ABmp.LoadFromStream(ABitmap);
    AReadSize := 0;
    for Row := 0 to ABmp.Height - 1 do
    begin
      ALine := ABmp.ScanLine[Row];
      if Row = 0 then
      begin
        Move(ALine[0], ASize, 4);
        ALineSize := ABmp.Width * 3-6;
        ALineSize := Min(ALineSize, ASize - AReadSize);
        AStream.WriteBuffer(ALine[6], ALineSize);
      end
      else
      begin
        ALineSize := ABmp.Width * 3;
        ALineSize := Min(ALineSize, ASize - AReadSize);
        AStream.WriteBuffer(ALine[0], ALineSize);
      end;
      AReadSize := AReadSize + ALineSize;
    end;
  finally
    ABmp.Free;
  end;
end;

end.
