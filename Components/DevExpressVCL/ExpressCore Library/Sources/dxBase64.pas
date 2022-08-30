{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressOfficeCore Library classes                        }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL     }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxBase64;

{$I cxVer.inc}

interface

uses
  SysUtils, dxCore;

type
   EdxBase64FormatException = class(EdxException);

  { TdxBase64 }

  TdxBase64 = class
  public const
    Base64LineBreakPosition = 76;
  strict protected const
    Base64Table: array [0..64] of Char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
    BadBase64CharArrayLength = 'Bad Base64 char array length';
    BadBase64Char = 'Bad Base64 char';

    class function FromPChar(AInputPtr: PChar; AInputLength: Integer): TBytes; static;
  public
    class function CalculateDecodedOutputLength(AInputPtr: PChar; AInputLength: Integer): Integer; static;
    class function CalculateEncodedOutputLength(AInputLength: Integer; AInsertLineBreaks: Boolean): Integer; static;
    class function EncodeArray(AOutChars: PChar; AInData: PByte; AOffset: Integer; ALength: Integer; AInsertLineBreaks: Boolean): Integer; static;
    class function DecodeArray(AStartInputPtr: PChar; AInputLength: Integer; AStartDestPtr: PByte; ADestLength: Integer): Integer; static;

    class function FromBase64CharArray(const AInArray: TCharArray; AOffset, ALength: Integer): TBytes; static;
    class function FromBase64String(const S: string): TBytes; static;

    class function ToBase64CharArray(const AInArray: TBytes; AOffsetIn: Integer; ALength: Integer;
      const AOutArray: TCharArray; AOffsetOut: Integer): Integer; overload; static;
    class function ToBase64CharArray(const AInArray: TBytes; AOffsetIn: Integer; ALength: Integer;
      const AOutArray: TCharArray; AOffsetOut: Integer; AInsertLineBreaks: Boolean): Integer; overload; static;
    class function ToBase64String(const AInArray: TBytes): string; overload; static;
    class function ToBase64String(const AInArray: TBytes; AInsertLineBreaks: Boolean): string; overload; static;
    class function ToBase64String(const AInArray: TBytes; AOffset: Integer; ALength: Integer): string; overload; static;
    class function ToBase64String(const AInArray: TBytes; AOffset: Integer; ALength: Integer; AInsertLineBreaks: Boolean): string; overload; static;
  end;

  { TdxBase64Encoder }

  TdxBase64Encoder = class abstract
  strict protected const
    Base64LineSize = TdxBase64.Base64LineBreakPosition;
    LineSizeInBytes = Base64LineSize div 4 * 3;
  strict private
    FCharsLine: TCharArray;
    FLeftOverBytes: TBytes;
    FLeftOverBytesCount: Integer;
  public
    constructor Create;
    procedure WriteChars(const AChars: TCharArray; AIndex: Integer; ACount: Integer); virtual; abstract;
    procedure Encode(const ABuffer: TBytes; AIndex: Integer; ACount: Integer);
    procedure Flush;
  end;

implementation

{ TdxBase64 }

class function TdxBase64.CalculateEncodedOutputLength(AInputLength: Integer; AInsertLineBreaks: Boolean): Integer;
var
  AOutLength, ANewLines: Int64;
begin
  AOutLength := Int64(AInputLength) div 3 * 4;
  if (AInputLength mod 3) <> 0 then
    Inc(AOutLength, 4);
  if AOutLength = 0 then
    Exit(0);
  if AInsertLineBreaks then
  begin
    ANewLines := AOutLength div Base64LineBreakPosition;
    if (AOutLength mod Base64LineBreakPosition) = 0 then
      Dec(ANewLines);
    Inc(AOutLength, ANewLines * 2);
  end;
  if AOutLength > MaxInt then
    raise EOutOfMemory.Create('');
  Result := Integer(AOutLength);
end;

class function TdxBase64.EncodeArray(AOutChars: PChar; AInData: PByte; AOffset, ALength: Integer;
  AInsertLineBreaks: Boolean): Integer;
var
  ALengthMod3, ACalcLength, ACharCount, I: Integer;
  AStart: PChar;
begin
  ALengthMod3 := ALength mod 3;
  ACalcLength := AOffset + (ALength - ALengthMod3);
  ACharCount  := 0;
  AStart      := AOutChars;
  I := AOffset;
  while I < ACalcLength do
  begin
    if AInsertLineBreaks then
    begin
      if ACharCount = Base64LineBreakPosition then
      begin
        AOutChars^ := #13;
        Inc(AOutChars);
        AOutChars^ := #10;
        Inc(AOutChars);
        ACharCount := 0;
      end;
      Inc(ACharCount, 4);
    end;
    AOutChars^ := Base64Table[(AInData[I] and $FC) shr 2];
    Inc(AOutChars);
    AOutChars^ := Base64Table[((AInData[I] and $03) shl 4) or ((AInData[I + 1] and $F0) shr 4)];
    Inc(AOutChars);
    AOutChars^ := Base64Table[((AInData[I + 1] and $0F) shl 2) or ((AInData[I + 2] and $C0) shr 6)];
    Inc(AOutChars);
    AOutChars^ := Base64Table[AInData[I + 2] and $3F];
    Inc(AOutChars);
    Inc(I, 3);
  end;
  I := ACalcLength;
  if AInsertLineBreaks and (ALengthMod3 <> 0) and (ACharCount = Base64LineBreakPosition) then
  begin
    AOutChars^ := #13;
    Inc(AOutChars);
    AOutChars^ := #10;
    Inc(AOutChars);
  end;
  case ALengthMod3 of
    2:
      begin
        AOutChars^ := Base64Table[(AInData[I] and $FC) shr 2];
        Inc(AOutChars);
        AOutChars^ := Base64Table[((AInData[I] and $03) shl 4) or ((AInData[I + 1] and $F0) shr 4)];
        Inc(AOutChars);
        AOutChars^ := Base64Table[(AInData[I + 1] and $0F) shl 2];
        Inc(AOutChars);
        AOutChars^ := Base64Table[64];
        Inc(AOutChars);
      end;
    1:
      begin
        AOutChars^ := Base64Table[(AInData[I] and $FC) shr 2];
        Inc(AOutChars);
        AOutChars^ := Base64Table[(AInData[I] and $03) shl 4];
        Inc(AOutChars);
        AOutChars^ := Base64Table[64];
        Inc(AOutChars);
        AOutChars^ := Base64Table[64];
        Inc(AOutChars);
      end;
  end;
  Result := AOutChars - AStart;
end;

class function TdxBase64.FromBase64CharArray(const AInArray: TCharArray; AOffset, ALength: Integer): TBytes;
var
  AInArrayPtr: PChar;
begin
  if AInArray = nil then
    raise EdxException.Create('inArray');
  if ALength < 0 then
    raise EArgumentOutOfRangeException.Create('length');
  if AOffset < 0 then
    raise EArgumentOutOfRangeException.Create('offset');
  if AOffset > Length(AInArray) - ALength then
    raise EArgumentOutOfRangeException.Create('offset');

  AInArrayPtr := @AInArray[0];
  Inc(AInArrayPtr, AOffset);
  Result := FromPChar(AInArrayPtr, ALength);
end;

class function TdxBase64.FromBase64String(const S: string): TBytes;
begin
  if S = '' then
    raise EArgumentException.Create('s');
  Result := FromPChar(PChar(S), Length(S));
end;

class function TdxBase64.ToBase64CharArray(const AInArray: TBytes; AOffsetIn: Integer;
  ALength: Integer; const AOutArray: TCharArray; AOffsetOut: Integer): Integer;
begin
  Result := ToBase64CharArray(AInArray, AOffsetIn, ALength, AOutArray, AOffsetOut, False);
end;

class function TdxBase64.ToBase64CharArray(const AInArray: TBytes; AOffsetIn: Integer; ALength: Integer;
  const AOutArray: TCharArray; AOffsetOut: Integer; AInsertLineBreaks: Boolean): Integer;
var
  AInArrayLength, AOutArrayLength, ANumElementsToCopy: Integer;
begin
  if AInArray = nil then
    raise EdxException.Create('inArray');
  if AOutArray = nil then
    raise EdxException.Create('outArray');
  if ALength < 0 then
    raise EArgumentOutOfRangeException.Create('length');
  if AOffsetIn < 0 then
    raise EArgumentOutOfRangeException.Create('offsetIn');
  if AOffsetOut < 0 then
    raise EArgumentOutOfRangeException.Create('offsetOut');

  AInArrayLength := Length(AInArray);

  if AOffsetIn > Integer(AInArrayLength - ALength) then
    raise EArgumentOutOfRangeException.Create('offsetIn');

  if AInArrayLength = 0 then
    Exit(0);
  AOutArrayLength := Length(AOutArray);
  ANumElementsToCopy := CalculateEncodedOutputLength(ALength, AInsertLineBreaks);

  if AOffsetOut > Integer((AOutArrayLength - ANumElementsToCopy)) then
    raise EArgumentOutOfRangeException.Create('offsetOut');
  Result := EncodeArray(@AOutArray[AOffsetOut], @AInArray[0], AOffsetIn, ALength, AInsertLineBreaks);
end;

class function TdxBase64.ToBase64String(const AInArray: TBytes): string;
begin
  if AInArray = nil then
    raise EdxException.Create('inArray');
  Result := ToBase64String(AInArray, 0, Length(AInArray), False);
end;

class function TdxBase64.ToBase64String(const AInArray: TBytes; AInsertLineBreaks: Boolean): string;
begin
  if AInArray = nil then
    raise EdxException.Create('inArray');
  Result := ToBase64String(AInArray, 0, Length(AInArray), AInsertLineBreaks);
end;

class function TdxBase64.ToBase64String(const AInArray: TBytes; AOffset: Integer; ALength: Integer): string;
begin
  Result := ToBase64String(AInArray, AOffset, ALength, False);
end;

class function TdxBase64.ToBase64String(const AInArray: TBytes; AOffset: Integer; ALength: Integer; AInsertLineBreaks: Boolean): string;
var
  AInArrayLength, AStringLength: Integer;
begin
  if AInArray = nil then
    raise EdxException.Create('inArray');
  if ALength < 0 then
    raise EArgumentOutOfRangeException.Create('length');
  if AOffset < 0 then
    raise EArgumentOutOfRangeException.Create('offset');

  AInArrayLength := Length(AInArray);
  if AOffset > (AInArrayLength - ALength) then
    raise EArgumentOutOfRangeException.Create('offset');

  if AInArrayLength = 0 then
    Exit('');

  AStringLength := CalculateEncodedOutputLength(ALength, AInsertLineBreaks);

  SetLength(Result, AStringLength);
  EncodeArray(PChar(Result), @AInArray[0], AOffset, ALength, AInsertLineBreaks);
end;

class function TdxBase64.FromPChar(AInputPtr: PChar; AInputLength: Integer): TBytes;
var
  AResultLength, AActualResultLength: Integer;
begin
  Assert(0 <= AInputLength);
  while AInputLength > 0 do
  begin
    if not CharInSet(AInputPtr[AInputLength - 1], [' ', #10, #13, #9]) then
      Break;
    Dec(AInputLength);
  end;
  AResultLength := CalculateDecodedOutputLength(AInputPtr, AInputLength);
  Assert(0 <= AResultLength);
  SetLength(Result, AResultLength);
  AActualResultLength := DecodeArray(AInputPtr, AInputLength, Pointer(@Result[0]), AResultLength);
  if AActualResultLength <> AResultLength then
    SetLength(Result, AActualResultLength);
end;

{$WARNINGS OFF}
class function TdxBase64.DecodeArray(AStartInputPtr: PChar; AInputLength: Integer; AStartDestPtr: PByte; ADestLength: Integer): Integer;
const
  Ord_Eq    = Ord('=');
  Ord_Plus  = Ord('+');
  Ord_Slash = Ord('/');
  Ord_Space = Ord(' ');
  Ord_Tab   = Ord(#9);
  Ord_NL    = Ord(#10);
  Ord_CR    = Ord(#13);
  Ord_AtoZ  = Ord('Z') - Ord('A');
  Ord_0to9  = Ord('9') - Ord('0');
label
  Done;
var
  ACurrCode, ACurrBlockCodes: Cardinal;
  AInputPtr, AEndInputPtr: PChar;
  ADestPtr, AEndDestPtr: PByte;
begin
  AInputPtr := AStartInputPtr;
  ADestPtr := AStartDestPtr;
  AEndInputPtr := AInputPtr + AInputLength;
  AEndDestPtr := ADestPtr + ADestLength;
  ACurrBlockCodes := $000000FF;
  while True do
  begin
    if AInputPtr >= AEndInputPtr then
      goto Done;
    ACurrCode := Ord(AInputPtr^);
    Inc(AInputPtr);
    if ACurrCode - Ord('A') <= Ord_AtoZ then
      Dec(ACurrCode, Ord('A'))
    else
      if ACurrCode - Ord('a') <= Ord_AtoZ then
        Dec(ACurrCode, Ord('a') - 26)
      else
        if ACurrCode - Ord('0') <= Ord_0to9 then
          Dec(ACurrCode, Ord('0') - 52)
        else
        begin
          case ACurrCode of
            Ord_Plus:
              ACurrCode := 62;
            Ord_Slash:
              ACurrCode := 63;
            Ord_CR, Ord_NL, Ord_Space, Ord_Tab:
              Continue;
            Ord_Eq:
              Break;
            else
              raise EArgumentException.Create('');
          end;
        end;
    ACurrBlockCodes := (ACurrBlockCodes shl 6) or ACurrCode;
    if (ACurrBlockCodes and $80000000) <> 0 then
    begin
      if AEndDestPtr - ADestPtr < 3 then
        Exit(-1);
      ADestPtr^ := Byte(ACurrBlockCodes shr 16);
      Inc(ADestPtr);
      ADestPtr^ := Byte(ACurrBlockCodes shr 8);
      Inc(ADestPtr);
      ADestPtr^ := Byte(ACurrBlockCodes);
      Inc(ADestPtr);
      ACurrBlockCodes := $000000FF;
    end;
  end;
  if AInputPtr = AEndInputPtr then
  begin
    ACurrBlockCodes := ACurrBlockCodes shl 6;
    if (ACurrBlockCodes and $80000000) = 0 then
      raise EdxBase64FormatException.Create(BadBase64CharArrayLength);

    if (AEndDestPtr - ADestPtr) < 2 then
      Exit(-1);
    ADestPtr^ := Byte(ACurrBlockCodes shr 16);
    Inc(ADestPtr);
    ADestPtr^ := Byte(ACurrBlockCodes shr 8);
    Inc(ADestPtr);
    ACurrBlockCodes := $000000FF;
  end
  else
  begin
    while AInputPtr < AEndInputPtr - 1 do
    begin
      if not CharInSet(AInputPtr^, [' ', #10, #13, #9]) then
        Break;
      Inc(AInputPtr);
    end;
    if (AInputPtr = AEndInputPtr - 1) and (AInputPtr^ = '=') then
    begin
      ACurrBlockCodes := ACurrBlockCodes shl 12;
      if (ACurrBlockCodes and $80000000) = 0 then
        raise EdxBase64FormatException.Create(BadBase64CharArrayLength);

      if AEndDestPtr - ADestPtr < 1 then
        Exit(-1);
      ADestPtr^ := Byte(ACurrBlockCodes shr 16);
      Inc(ADestPtr);
      ACurrBlockCodes := $000000FF;
    end
    else
      raise EdxBase64FormatException.Create(BadBase64Char);
  end;
Done:
  if ACurrBlockCodes <> $000000FF then
    raise EdxBase64FormatException.Create(BadBase64CharArrayLength);

  Result := ADestPtr - AStartDestPtr;
end;
{$WARNINGS ON}

class function TdxBase64.CalculateDecodedOutputLength(AInputPtr: PChar; AInputLength: Integer): Integer;
const
  Ord_Eq    = Ord('=');
  Ord_Space = Ord(' ');
var
  C: Char;
  AInputEndPtr: PChar;
  AUsefulInputLength, APadding: Integer;
begin
  Assert(0 <= AInputLength);

  AInputEndPtr := AInputPtr + AInputLength;
  AUsefulInputLength := AInputLength;
  APadding := 0;

  while AInputPtr < AInputEndPtr do
  begin
    C := AInputPtr^;
    Inc(AInputPtr);
    if C <= ' ' then
      Dec(AUsefulInputLength)
    else
      if C = '=' then
      begin
        Dec(AUsefulInputLength);
        Inc(APadding);
      end;
  end;
  Assert(0 <= AUsefulInputLength);
  Assert(0 <= APadding);
  if APadding <> 0 then
  begin
    if APadding = 1 then
      APadding := 2
    else
      if APadding = 2 then
        APadding := 1
      else
        raise EdxBase64FormatException.Create(BadBase64Char);
  end;
  Result := (AUsefulInputLength div 4) * 3 + APadding;
end;

{ TdxBase64Encoder }

constructor TdxBase64Encoder.Create;
begin
  inherited Create;
  SetLength(FCharsLine, Base64LineSize);
end;

procedure TdxBase64Encoder.Encode(const ABuffer: TBytes; AIndex: Integer; ACount: Integer);
var
  I, ALeftOverChars, AEndIndex, AChunkSize, ACharCount: Integer;
begin
  if ABuffer = nil then
    raise EdxException.Create('buffer');
  if AIndex < 0 then
    raise EArgumentOutOfRangeException.Create('index');
  if ACount < 0 then
    raise EArgumentOutOfRangeException.Create('count');
  if ACount > Length(ABuffer) - AIndex then
    raise EArgumentOutOfRangeException.Create('count');
  if FLeftOverBytesCount > 0 then
  begin
    I := FLeftOverBytesCount;
    while (I < 3) and (ACount > 0) do
    begin
      FLeftOverBytes[I] := ABuffer[AIndex];
      Inc(I);
      Inc(AIndex);
      Dec(ACount);
    end;
    if (ACount = 0) and (I < 3) then
    begin
      FLeftOverBytesCount := I;
      Exit;
    end;
    ALeftOverChars := TdxBase64.ToBase64CharArray(FLeftOverBytes, 0, 3, FCharsLine, 0);
    WriteChars(FCharsLine, 0, ALeftOverChars);
  end;
  FLeftOverBytesCount := ACount mod 3;
  if FLeftOverBytesCount > 0 then
  begin
    Dec(ACount, FLeftOverBytesCount);
    if FLeftOverBytes = nil then
      SetLength(FLeftOverBytes, 3);
    for I := 0 to FLeftOverBytesCount - 1 do
      FLeftOverBytes[I] := ABuffer[AIndex + ACount + I];
  end;
  AEndIndex := AIndex + ACount;
  AChunkSize := LineSizeInBytes;
  while AIndex < AEndIndex do
  begin
    if AIndex + AChunkSize > AEndIndex then
      AChunkSize := AEndIndex - AIndex;
    ACharCount := TdxBase64.ToBase64CharArray(ABuffer, AIndex, AChunkSize, FCharsLine, 0);
    WriteChars(FCharsLine, 0, ACharCount);
    Inc(AIndex, AChunkSize);
  end;
end;

procedure TdxBase64Encoder.Flush;
var
  ALeftOverChars: Integer;
begin
  if FLeftOverBytesCount > 0 then
  begin
    ALeftOverChars := TdxBase64.ToBase64CharArray(FLeftOverBytes, 0, FLeftOverBytesCount, FCharsLine, 0);
    WriteChars(FCharsLine, 0, ALeftOverChars);
    FLeftOverBytesCount := 0;
  end;
end;

end.
