{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetFormatXLSProtection;

{$I cxVer.inc}

interface

uses
  cxVariants, SysUtils, Classes, dxCrypto, dxCryptoAPI, dxCoreClasses, cxClasses, dxCore, dxHash,
  dxSpreadSheetCore, dxSpreadSheetFormatXLS, dxSpreadSheetStrs, dxOLECryptoContainer;

type

  { TdxXLSAbstractEncryptor }

  TdxXLSAbstractEncryptorClass = class of TdxXLSAbstractEncryptor;
  TdxXLSAbstractEncryptor = class abstract
  protected
    procedure Check(AValue: Boolean);
    procedure Initialize(const APassword: string); virtual; abstract;
  public
    constructor Create(const APassword: string); overload;
    function CheckPassword(const APassword: string): Boolean; virtual; abstract;
    procedure Decrypt(AData: PByte; APosition, ACount: Integer); virtual; abstract;
    procedure Encrypt(AData: PByte; APosition, ACount: Integer); virtual; abstract;
    procedure Load(AReader: TcxReader); virtual; abstract;
    procedure Save(AWriter: TcxWriter); virtual; abstract;
  end;

  { TdxXLSCustomEncryptor }

  TdxXLSCustomEncryptor = class abstract(TdxXLSAbstractEncryptor)
  strict private const
    CRYPTED_BLOCK_SIZE = $0400;
  protected
    FBlockNumber: Integer;
    FEncryptedVerifier: TBytes;
    FEncryptedVerifierHash: TBytes;
    FPasswordHash: TBytes;
    FPosition: Int64;
    FSalt: TBytes;

    function CreateHash: TdxHashAlgorithm; virtual; abstract;
    function CreatePasswordHash(const APassword: UnicodeString; const ASalt: TBytes): TBytes; virtual; abstract;
    procedure DecryptSkip(APosition, ACount: Integer); virtual;
    procedure InitializeKey(const APasswordHash: TBytes; ABlockIndex: Integer); virtual; abstract;
  public
    procedure AfterConstruction; override;
    function CheckPassword(const APassword: string): Boolean; override;
    procedure Decrypt(AData: PByte; APosition, ACount: Integer); override;
    procedure DecryptBlock(ABlock: PByte; ABlockSize: Integer); overload; virtual; abstract;
    procedure DecryptBlock(ABlock: TBytes); overload;
  end;

  { TdxXLSStandardEncryptor }

  TdxXLSStandardEncryptor = class(TdxXLSCustomEncryptor)
  strict private const
    KEY_DIGEST_LENGTH = 5;
    PASSWORD_HASH_USAGE = 5;
  strict private
    FRC4Key: TdxRC4Key;
  protected
    function CreateHash: TdxHashAlgorithm; override;
    function CreatePasswordHash(const APassword: UnicodeString; const ASalt: TBytes): TBytes; override;
    procedure Initialize(const APassword: string); override;
    procedure InitializeKey(const APasswordHash: TBytes; ABlockIndex: Integer); override;
  public
    procedure DecryptBlock(ABlock: PByte; ABlockSize: Integer); override;
    procedure Encrypt(AData: PByte; APosition, ACount: Integer); override;
    procedure Load(AReader: TcxReader); override;
    procedure Save(AWriter: TcxWriter); override;
  end;

  { TdxXLSStrongEncryptionHelper }

  TdxXLSStrongEncryptionHelper = class(TdxOLECryptoContainerEncryptorStandard);

  { TdxXLSStrongEncryptor }

  TdxXLSStrongEncryptor = class(TdxXLSCustomEncryptor)
  protected
    FCipherAlgorithm: Integer;
    FCipherKey: IdxCryptoKey;
    FHashAlgorithm: Integer;
    FKeySize: Integer;
    FProvider: IdxCryptoProvider;

    function CreateHash: TdxHashAlgorithm; override;
    function CreatePasswordHash(const APassword: string; const ASalt: TBytes): TBytes; override;
    procedure Initialize(const APassword: string); override;
    procedure InitializeKey(const APasswordHash: TBytes; ABlockIndex: Integer); override;
  public
    procedure DecryptBlock(ABlock: PByte; ABlockSize: Integer); override;
    procedure Encrypt(AData: PByte; APosition: Integer; ACount: Integer); override;
    procedure Load(AReader: TcxReader); override;
    procedure Save(AWriter: TcxWriter); override;
  end;

  { TdxXLSEncryptors }

  TdxXLSEncryptors = class
  public
    class function CreateEncryptor(AVersion: Word): TdxXLSAbstractEncryptor;
  end;

implementation

uses
  Math, dxSpreadSheetFormatXLSReader, dxSpreadSheetCoreStrs;

{ TdxXLSAbstractEncryptor }

constructor TdxXLSAbstractEncryptor.Create(const APassword: string);
begin
  inherited Create;
  Initialize(APassword);
end;

procedure TdxXLSAbstractEncryptor.Check(AValue: Boolean);
begin
  if not AValue then
    raise EdxSpreadSheetReaderError.CreateFmt(cxGetResourceString(@sdxErrorInternal), [ClassName]);
end;

{ TdxXLSCustomEncryptor }

procedure TdxXLSCustomEncryptor.AfterConstruction;
begin
  inherited AfterConstruction;
  FBlockNumber := -1;
end;

function TdxXLSCustomEncryptor.CheckPassword(const APassword: string): Boolean;
var
  ADecryptedVerifier: TBytes;
  ADecryptedVerifierHash: TBytes;
  AHash: TdxHashAlgorithm;
begin
  FPasswordHash := CreatePasswordHash(APassword, FSalt);

  ADecryptedVerifier := TdxByteArray.Clone(FEncryptedVerifier);
  ADecryptedVerifierHash := TdxByteArray.Clone(FEncryptedVerifierHash);

  InitializeKey(FPasswordHash, 0);
  DecryptBlock(ADecryptedVerifier);
  DecryptBlock(ADecryptedVerifierHash);

  AHash := CreateHash;
  try
    AHash.Add(ADecryptedVerifier);
    Result := TdxByteArray.Compare(ADecryptedVerifierHash, AHash.GetHash);
  finally
    AHash.Free;
  end;
end;

procedure TdxXLSCustomEncryptor.Decrypt(AData: PByte; APosition, ACount: Integer);
var
  ABlockNumber: integer;
  ASize: integer;
begin
  DecryptSkip(FPosition, APosition - FPosition);
  repeat
    ABlockNumber := (APosition + ACount) div CRYPTED_BLOCK_SIZE;
    if ABlockNumber <> FBlockNumber then
    begin
      ASize := CRYPTED_BLOCK_SIZE - APosition mod CRYPTED_BLOCK_SIZE;
      DecryptBlock(AData, ASize);
      Inc(APosition, ASize);
      Dec(ACount, ASize);
      Inc(AData, ASize);
      Inc(FBlockNumber);
      InitializeKey(FPasswordHash, FBlockNumber);
    end;
  until ABlockNumber = FBlockNumber;
  DecryptBlock(AData, ACount);
  FPosition := APosition + ACount;
end;

procedure TdxXLSCustomEncryptor.DecryptBlock(ABlock: TBytes);
begin
  DecryptBlock(@ABlock[0], Length(ABlock));
end;

procedure TdxXLSCustomEncryptor.DecryptSkip(APosition, ACount: Integer);
var
  ABlockNumber: integer;
  ABuffer: TBytes;
begin
  ABlockNumber := (APosition + ACount) div CRYPTED_BLOCK_SIZE;
  if ABlockNumber <> FBlockNumber then
  begin
    FBlockNumber := ABlockNumber;
    InitializeKey(FPasswordHash, FBlockNumber);
    ACount := (APosition + ACount) mod CRYPTED_BLOCK_SIZE;
  end;
  if ACount > 0 then
  begin
    SetLength(ABuffer, ACount);
    DecryptBlock(ABuffer);
  end;
end;

{ TdxXLSStandardEncryptor }

procedure TdxXLSStandardEncryptor.DecryptBlock(ABlock: PByte; ABlockSize: Integer);
begin
  dxRC4Crypt(FRC4Key, PByteArray(ABlock), PByteArray(ABlock), ABlockSize);
end;

procedure TdxXLSStandardEncryptor.Encrypt(AData: PByte; APosition, ACount: Integer);
begin
  Decrypt(AData, APosition, ACount);
end;

procedure TdxXLSStandardEncryptor.Load(AReader: TcxReader);
begin
  if AReader.ReadWord <> 1 then // Version
    raise EdxSpreadSheetReaderError.Create(sdxUnsupportedEncryption);
  FSalt := ReadBytesFunc(AReader.Stream, 16);
  FEncryptedVerifier := ReadBytesFunc(AReader.Stream, 16);
  FEncryptedVerifierHash := ReadBytesFunc(AReader.Stream, 16);
end;

procedure TdxXLSStandardEncryptor.Save(AWriter: TcxWriter);
begin
  AWriter.WriteWord(1);
  WriteBytesProc(AWriter.Stream, FSalt);
  WriteBytesProc(AWriter.Stream, FEncryptedVerifier);
  WriteBytesProc(AWriter.Stream, FEncryptedVerifierHash);
end;

procedure TdxXLSStandardEncryptor.Initialize(const APassword: string);
begin
  FSalt := dxGenerateSalt(16);
  FPasswordHash := CreatePasswordHash(APassword, FSalt);
  FEncryptedVerifier := dxGenerateSalt(16);
  FEncryptedVerifierHash := TdxMD5HashAlgorithm.Calculate(FEncryptedVerifier);
  InitializeKey(FPasswordHash, 0);
  DecryptBlock(FEncryptedVerifier);
  DecryptBlock(FEncryptedVerifierHash);
end;

function TdxXLSStandardEncryptor.CreateHash: TdxHashAlgorithm;
begin
  Result := TdxMD5HashAlgorithm.Create;
end;

function TdxXLSStandardEncryptor.CreatePasswordHash(const APassword: UnicodeString; const ASalt: TBytes): TBytes;
const
  MaxPasswordLength = 255;
var
  AHash: TdxHashAlgorithm;
  APasswordHash: TBytes;
  I: Integer;
begin
  if Length(ASalt) <> 16 then
    raise EdxSpreadSheetReaderError.CreateFmt(cxGetResourceString(@sdxErrorInternal), [ClassName]);

  AHash := CreateHash;
  try
    AHash.Add(@APassword[1], Min(Length(APassword), MaxPasswordLength) * SizeOf(WideChar));
    APasswordHash := AHash.GetHash;

    AHash.Reset;
    for I := 0 to 15 do
    begin
      AHash.Add(APasswordHash, 0, PASSWORD_HASH_USAGE);
      AHash.Add(ASalt);
    end;
    Result := AHash.GetHash;
  finally
    AHash.Free;
  end;
end;

procedure TdxXLSStandardEncryptor.InitializeKey(const APasswordHash: TBytes; ABlockIndex: Integer);
var
  AHash: TdxHashAlgorithm;
begin
  AHash := CreateHash;
  try
    AHash.Add(APasswordHash, 0, KEY_DIGEST_LENGTH);
    AHash.Add(@ABlockIndex, SizeOf(ABlockIndex));
    dxRC4Initialize(FRC4Key, AHash.GetHash);
  finally
    AHash.Free;
  end;
end;

{ TdxXLSStrongEncryptor }

function TdxXLSStrongEncryptor.CreateHash: TdxHashAlgorithm;
begin
  Result := TdxCryptoHashAlgorithm.Create(FHashAlgorithm, FProvider);
end;

function TdxXLSStrongEncryptor.CreatePasswordHash(const APassword: string; const ASalt: TBytes): TBytes;
var
  AHash: TdxHashAlgorithm;
begin
  AHash := CreateHash;
  try
    AHash.Add(ASalt);
    AHash.Add(TEncoding.Unicode.GetBytes(APassword));
    Result := AHash.GetHash;
  finally
    AHash.Free;
  end;
end;

procedure TdxXLSStrongEncryptor.DecryptBlock(ABlock: PByte; ABlockSize: Integer);
var
  ABuffer: TBytes;
begin
  SetLength(ABuffer, ABlockSize);
  Move(ABlock^, ABuffer[0], ABlockSize);
  ABuffer := TdxCipher.Decrypt(FCipherKey, ABuffer, False);
  Check(ABlockSize = Length(ABuffer));
  Move(ABuffer[0], ABlock^, ABlockSize);
end;

procedure TdxXLSStrongEncryptor.Encrypt(AData: PByte; APosition, ACount: Integer);
begin
  Check(False);
end;

procedure TdxXLSStrongEncryptor.Initialize(const APassword: string);
begin
  Check(False);
end;

procedure TdxXLSStrongEncryptor.InitializeKey(const APasswordHash: TBytes; ABlockIndex: Integer);
var
  AHash: TdxHashAlgorithm;
begin
  AHash := CreateHash;
  try
    AHash.Add(APasswordHash);
    AHash.Add(@ABlockIndex, SizeOf(ABlockIndex));
    FCipherKey := TdxCryptoKey.Create(FProvider, FCipherAlgorithm, TdxByteArray.Resize(AHash.GetHash, FKeySize div 8, 0));
  finally
    AHash.Free;
  end;
end;

procedure TdxXLSStrongEncryptor.Load(AReader: TcxReader);
var
  AHelper :TdxXLSStrongEncryptionHelper;
begin
  Check(AReader.ReadWord = 2);

  AHelper := TdxXLSStrongEncryptionHelper.Create;
  try
    AHelper.LoadInfo(AReader.Stream);
    FProvider := TdxCryptoProvider.Create(PChar(AHelper.CSPName), AHelper.CipherProvider);
    FEncryptedVerifier := TdxByteArray.Clone(AHelper.EncryptedVerifier);
    FEncryptedVerifierHash := TdxByteArray.Clone(AHelper.EncryptedVerifierHash);
    FSalt := TdxByteArray.Clone(AHelper.Salt);
    FCipherAlgorithm := AHelper.CipherAlgorithm;
    FHashAlgorithm := AHelper.HashAlgorithm;
    FKeySize := AHelper.KeySize;
  finally
    AHelper.Free;
  end;
end;

procedure TdxXLSStrongEncryptor.Save(AWriter: TcxWriter);
begin
  Check(False);
end;

{ TdxXLSEncryptors }

class function TdxXLSEncryptors.CreateEncryptor(AVersion: Word): TdxXLSAbstractEncryptor;
begin
  case AVersion of
    1:
      Result := TdxXLSStandardEncryptor.Create;
    2..4:
      Result := TdxXLSStrongEncryptor.Create;
  else
    raise EdxSpreadSheetReaderError.Create(sdxUnsupportedEncryption);
  end;
end;

end.
