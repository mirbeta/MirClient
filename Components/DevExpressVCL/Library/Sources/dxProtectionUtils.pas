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

unit dxProtectionUtils;

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

uses
  dxCore, dxHash, SysUtils;

type
  TdxGetPasswordEvent = function (Sender: TObject; {$IFDEF BCBCOMPATIBLE}var{$ELSE}out{$ENDIF} Password: string): Boolean of object;

  TdxHashAlgorithmType = (
    None,
    MD2,
    MD4,
    MD5,
    SHA1,
    MAC,
    Ripemd,
    Ripemd160,
    Reserved1,
    HMAC,
    Reserved2,
    Reserved3,
    SHA256,
    SHA384,
    SHA512
  );

  { TdxPasswordHashCodeCalculator }

  TdxPasswordHashCodeCalculatorClass = class of TdxPasswordHashCodeCalculator;
  TdxPasswordHashCodeCalculator = class
  strict private const
  {$REGION 'strict private const'}
    InitialValues: array [0..14] of Integer = (
       $E1F0, $1D0F, $CC9C, $84C0, $110C, $0E10, $F1CE,
       $313E, $1872, $E139, $D40F, $84F9, $280C, $A96A, $4EC3 );
    EncryptionMatrix: array[0..14, 0..6] of Integer = (
      ($AEFC, $4DD9, $9BB2, $2745, $4E8A, $9D14, $2A09),
      ($7B61, $F6C2, $FDA5, $EB6B, $C6F7, $9DCF, $2BBF),
      ($4563, $8AC6, $05AD, $0B5A, $16B4, $2D68, $5AD0),
      ($0375, $06EA, $0DD4, $1BA8, $3750, $6EA0, $DD40),
      ($D849, $A0B3, $5147, $A28E, $553D, $AA7A, $44D5),
      ($6F45, $DE8A, $AD35, $4A4B, $9496, $390D, $721A),
      ($EB23, $C667, $9CEF, $29FF, $53FE, $A7FC, $5FD9),
      ($47D3, $8FA6, $0F6D, $1EDA, $3DB4, $7B68, $F6D0),
      ($B861, $60E3, $C1C6, $93AD, $377B, $6EF6, $DDEC),
      ($45A0, $8B40, $06A1, $0D42, $1A84, $3508, $6A10),
      ($AA51, $4483, $8906, $022D, $045A, $08B4, $1168),
      ($76B4, $ED68, $CAF1, $85C3, $1BA7, $374E, $6E9C),
      ($3730, $6E60, $DCC0, $A9A1, $4363, $86C6, $1DAD),
      ($3331, $6662, $CCC4, $89A9, $0373, $06E6, $0DCC),
      ($1021, $2042, $4084, $8108, $1231, $2462, $48C4));
  {$ENDREGION}
  protected
    class function CalculateLegacyPasswordHashInt(const APassword: string): Integer;
    class function ProcessLowWordByte(AKey: Integer; B: Byte): Integer;
    class function ProcessHighWordByte(AKey: Integer; B: Byte; ARowIndex: Integer): Integer;
  public
    class function CalculateKeyHighWord(const ABytes: TBytes): Integer;
    class function CalculateKeyLowWord(const ABytes: TBytes): Integer;
    class function CalculateLegacyPasswordHash(const APassword: string): TBytes;
    class function CalculateOpenOfficePasswordHash(const APassword: string): TBytes;
    class function CalculatePasswordBytes(const APassword: string): TBytes;

    class function CalculatePasswordHash(const APassword: string; const ASalt: TBytes;
      ASpinCount: Integer; AHashType: TdxHashAlgorithmType; AIteratorFirst: Boolean = False): TBytes; overload;
    class procedure CalculatePasswordHash(const APassword: string; const ASalt: TBytes;
      ASpinCount: Integer; AHash: TdxHashAlgorithm; AIteratorFirst: Boolean = False); overload;
    class procedure CalculatePasswordHash(const APassword: TBytes; const ASalt: TBytes;
      ASpinCount: Integer; AHash: TdxHashAlgorithm; AIteratorFirst: Boolean = False); overload;

    class function CalculateRichEditPasswordHash(const APassword: string;
      const ASalt: TBytes; ASpinCount: Integer; AHashAlgorithmType: TdxHashAlgorithmType): TBytes; overload;
    class function CalculateRichEditPasswordHash(const APassword: string;
      const ASalt: TBytes; ASpinCount: Integer; AHashAlgorithm: TdxHashAlgorithmClass): TBytes; overload;

    class function GetHashAlgorithm(AHashAlgorithmType: TdxHashAlgorithmType): TdxHashAlgorithmClass; virtual;
  end;

function dxCallGetPasswordEvent(AEvent: TdxGetPasswordEvent; ASender: TObject; out APassword: string): Boolean;
implementation

uses
  Math;

const
  sdxUnsupportedHashAlgorithm = 'Unsupported hash algorithm (Code: %d).';

function dxCallGetPasswordEvent(AEvent: TdxGetPasswordEvent; ASender: TObject; out APassword: string): Boolean;
begin
  Result := Assigned(AEvent) and AEvent(ASender, APassword);
end;

{ TdxPasswordHashCodeCalculator }

class function TdxPasswordHashCodeCalculator.CalculateKeyHighWord(const ABytes: TBytes): Integer;
var
  ACount, I: Integer;
begin
  ACount := Length(ABytes);
  Result := InitialValues[ACount - 1];
  for I := 0 to ACount - 1 do
    Result := ProcessHighWordByte(Result, ABytes[I], 15 - (ACount - I));
end;

class function TdxPasswordHashCodeCalculator.CalculateKeyLowWord(const ABytes: TBytes): Integer;
var
  ACount, I: Integer;
begin
  Result := 0;
  ACount := Length(ABytes);
  for I := ACount - 1 downto 0 do
    Result := ProcessLowWordByte(Result, ABytes[I]);
  Result := ProcessLowWordByte(Result, Byte(ACount)) xor $CE4B;
end;

class function TdxPasswordHashCodeCalculator.CalculateLegacyPasswordHash(const APassword: string): TBytes;
var
  AHash: Integer;
begin
  if APassword = '' then
    Exit(nil);
  AHash := CalculateLegacyPasswordHashInt(APassword);
  SetLength(Result, SizeOf(AHash));
  Move(AHash, Result[0], SizeOf(AHash));
end;

class function TdxPasswordHashCodeCalculator.CalculateOpenOfficePasswordHash(const APassword: string): TBytes;
begin
  Result := TdxSHA1HashAlgorithm.Calculate(TEncoding.Unicode.GetBytes(APassword));
end;

class function TdxPasswordHashCodeCalculator.CalculatePasswordBytes(const APassword: string): TBytes;
var
  I, ACount, ACh: Integer;
begin
  ACount := Min(Length(APassword), 15);
  SetLength(Result, ACount);
  for I := 0 to ACount - 1 do
  begin
    ACh := Ord(APassword[I + 1]);
    if (ACh and $00FF) = 0 then
      Result[I] := Byte(ACh shr 8)
    else
      Result[I] := Byte(ACh and $00FF);
  end;
end;

class function TdxPasswordHashCodeCalculator.CalculatePasswordHash(const APassword: string;
  const ASalt: TBytes; ASpinCount: Integer; AHashType: TdxHashAlgorithmType; AIteratorFirst: Boolean = False): TBytes;
var
  AHashAlgorithm: TdxHashAlgorithm;
begin
  AHashAlgorithm := GetHashAlgorithm(AHashType).Create;
  try
    CalculatePasswordHash(APassword, ASalt, ASpinCount, AHashAlgorithm);
    Result := AHashAlgorithm.GetHash;
  finally
    AHashAlgorithm.Free;
  end;
end;

class procedure TdxPasswordHashCodeCalculator.CalculatePasswordHash(const APassword: string;
  const ASalt: TBytes; ASpinCount: Integer; AHash: TdxHashAlgorithm; AIteratorFirst: Boolean = False);
begin
  CalculatePasswordHash(TEncoding.Unicode.GetBytes(APassword), ASalt, ASpinCount, AHash, AIteratorFirst);
end;

class procedure TdxPasswordHashCodeCalculator.CalculatePasswordHash(const APassword, ASalt: TBytes;
  ASpinCount: Integer; AHash: TdxHashAlgorithm; AIteratorFirst: Boolean = False);

  function CalculateHash(AData1, AData2: Pointer; AData1Size, AData2Size: Integer): TBytes; overload;
  begin
    AHash.Reset;
    AHash.Add(AData1, AData1Size);
    AHash.Add(AData2, AData2Size);
    Result := AHash.GetHash;
  end;

  function CalculateHash(const AData1, AData2: TBytes): TBytes; overload;
  begin
    Result := CalculateHash(@AData1[0], @AData2[0], Length(AData1), Length(AData2));
  end;

var
  AIndex: Integer;
  AKey: TBytes;
begin
  AKey := CalculateHash(ASalt, APassword);
  for AIndex := 0 to ASpinCount - 1 do
  begin
    if AIteratorFirst then
      AKey := CalculateHash(@AIndex, @AKey[0], SizeOf(AIndex), Length(AKey))
    else
      AKey := CalculateHash(@AKey[0], @AIndex, Length(AKey), SizeOf(AIndex));
  end;
end;

class function TdxPasswordHashCodeCalculator.CalculateRichEditPasswordHash(
  const APassword: string; const ASalt: TBytes; ASpinCount: Integer; AHashAlgorithmType: TdxHashAlgorithmType): TBytes;
var
  AHashAlgorithm: TdxHashAlgorithmClass;
begin
  AHashAlgorithm := GetHashAlgorithm(AHashAlgorithmType);
  if AHashAlgorithm <> nil then
    Result := CalculateRichEditPasswordHash(APassword, ASalt, ASpinCount, AHashAlgorithm)
  else
    Result := TdxByteArray.Concatenate(nil, CalculateLegacyPasswordHashInt(APassword));
end;

class function TdxPasswordHashCodeCalculator.CalculateRichEditPasswordHash(
  const APassword: string; const ASalt: TBytes; ASpinCount: Integer; AHashAlgorithm: TdxHashAlgorithmClass): TBytes;
var
  AHash: TdxHashAlgorithm;
begin
  AHash := AHashAlgorithm.Create;
  try
    CalculatePasswordHash(
      TEncoding.Unicode.GetBytes(IntToHex(CalculateLegacyPasswordHashInt(APassword), 8)),
      ASalt, ASpinCount, AHash);
    Result := AHash.GetHash;
  finally
    AHash.Free;
  end;
end;

class function TdxPasswordHashCodeCalculator.GetHashAlgorithm(AHashAlgorithmType: TdxHashAlgorithmType): TdxHashAlgorithmClass;
begin
  case AHashAlgorithmType of
    TdxHashAlgorithmType.SHA1:
      Result := TdxSHA1HashAlgorithm;
    TdxHashAlgorithmType.SHA256:
      Result := TdxSHA256HashAlgorithm;
    TdxHashAlgorithmType.SHA384:
      Result := TdxSHA384HashAlgorithm;
    TdxHashAlgorithmType.SHA512:
      Result := TdxSHA512HashAlgorithm;
    TdxHashAlgorithmType.MD2:
      Result := TdxMD2HashAlgorithm;
    TdxHashAlgorithmType.MD4:
      Result := TdxMD4HashAlgorithm;
    TdxHashAlgorithmType.MD5:
      Result := TdxMD5HashAlgorithm;
    TdxHashAlgorithmType.None:
      Result := nil;
  else
    raise Exception.CreateFmt(sdxUnsupportedHashAlgorithm, [Ord(AHashAlgorithmType)]);
  end;
end;

class function TdxPasswordHashCodeCalculator.CalculateLegacyPasswordHashInt(const APassword: string): Integer;
var
  ABytes: TBytes;
  AHigh, ALow: Cardinal;
begin
  if APassword = '' then
    Exit(0);
  ABytes := CalculatePasswordBytes(APassword);
  AHigh := Cardinal(CalculateKeyHighWord(ABytes));
  ALow := Cardinal(CalculateKeyLowWord(ABytes));
  Result := Integer(((ALow shl 24) and $FF000000) or ((ALow shl 8) and $00FF0000) or
    ((AHigh shl 8) and $0000FF00) or ((AHigh shr 8) and $000000FF));
end;

class function TdxPasswordHashCodeCalculator.ProcessLowWordByte(AKey: Integer; B: Byte): Integer;
begin
  Result := (((AKey shr 14) and $0001) or ((AKey shl 1) and $7FFF)) xor B;
end;

class function TdxPasswordHashCodeCalculator.ProcessHighWordByte(AKey: Integer; B: Byte; ARowIndex: Integer): Integer;
var
  AMask, I: Integer;
begin
  AMask := 1;
  I := 0;
  while I <= 6 do
  begin
    if (B and AMask) <> 0 then
      AKey := AKey xor EncryptionMatrix[ARowIndex, I];
    Inc(I);
    AMask := AMask shl 1;
  end;
  Result := AKey;
end;

end.
