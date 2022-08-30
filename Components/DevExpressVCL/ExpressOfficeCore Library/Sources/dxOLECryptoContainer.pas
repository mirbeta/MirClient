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

unit dxOLECryptoContainer;

{$I cxVer.inc}


interface

uses
  Windows, Types, Classes, Generics.Defaults, Generics.Collections, SysUtils, ActiveX, ComObj,
  dxCore, dxOLEDocument, dxCrypto, dxHash, dxXMLDoc, dxProtectionUtils;

type

  { EdxOLECryptoContainerError }

  EdxOLECryptoContainerError = class(EdxException);

  { TdxOLECryptoContainerEncryptor }

  TdxOLECryptoContainerEncryptorClass = class of TdxOLECryptoContainerEncryptor;
  TdxOLECryptoContainerEncryptor = class abstract
  protected
    procedure Decrypt(const ASourceStream, ATargetStream: TStream); virtual; abstract;
    procedure Encrypt(const ASourceStream, ATargetStream: TStream); virtual; abstract;
    // Loading
    function CheckPassword(const APassword: string): Boolean; virtual; abstract;
    procedure DecryptPackage(const ASourceStream, ATargetStream: TStream); virtual;
    procedure LoadInfo(AStream: TStream); virtual; abstract;
    // Saving
    procedure EncryptPackage(const ASourceStream, ATargetStream: TStream); virtual;
    procedure Initialize(const APassword: string); virtual; abstract;
    procedure SaveInfo(AStream: TStream); virtual; abstract;
  public
    constructor Create; overload; virtual;
    constructor Create(const APassword: string); overload;
    class function ID: Integer; virtual;
    class procedure Register;
    class procedure Unregister;
  end;

  { TdxOLECryptoContainerEncryptorCryptoAPI }

  TdxOLECryptoContainerEncryptorCryptoAPI = class abstract(TdxOLECryptoContainerEncryptor)
  protected
    FCipherAlgorithm: Integer;
    FCipherChainingMode: Integer;
    FCipherProvider: Integer;
    FHashAlgorithm: Integer;
    FKeySize: Integer;
    FSalt: TBytes;

    function CalculateHash(const AProvider: IdxCryptoProvider; const ABytes: TBytes): TBytes; overload;
    function CalculateHash(const AProvider: IdxCryptoProvider; const ABytes: TBytes; AHashAlgorithm: Integer): TBytes; overload;
    //
    property CipherChainingMode: Integer read FCipherChainingMode;
    property CipherAlgorithm: Integer read FCipherAlgorithm;
    property CipherProvider: Integer read FCipherProvider;
    property HashAlgorithm: Integer read FHashAlgorithm;
    property KeySize: Integer read FKeySize;
  public
    constructor Create; override;
  end;

  { TdxOLECryptoContainerEncryptorAgile }

  TdxOLECryptoContainerEncryptorAgile = class(TdxOLECryptoContainerEncryptorCryptoAPI)
  strict private type
    PAgileBlock = ^TAgileBlock;
    TAgileBlock = array[0..7] of Byte;
  strict private const
  {$REGION 'Agile Blocks'}
    EncryptedDataIntegrityHMACValueBlockKey: TAgileBlock = ($a0, $67, $7f, $02, $b2, $2c, $84, $33);
    EncryptedDataIntegritySaltBlockKey: TAgileBlock = ($5f, $b2, $ad, $01, $0c, $b9, $e1, $f6);
    EncryptedKeyValueBlockKey: TAgileBlock = ($14, $6e, $0b, $e7, $ab, $ac, $d0, $d6);
    EncryptedVerifierHashInputBlockKey: TAgileBlock = ($fe, $a7, $d2, $76, $3b, $4b, $9e, $79);
    EncryptedVerifierHashValueBlockKey: TAgileBlock = ($d7, $aa, $0f, $6d, $30, $61, $34, $4e);
  {$ENDREGION}
  {$REGION 'EncryptionInfo.xml'}
    XMLBlockSize = 'blockSize';
    XMLCipherAlgorithm = 'cipherAlgorithm';
    XMLCipherChaining = 'cipherChaining';
    XMLEncryptedKey = 'p:encryptedKey';
    XMLEncryptedKeyValue = 'encryptedKeyValue';
    XMLEncryptedVerifierHashInput = 'encryptedVerifierHashInput';
    XMLEncryptedVerifierHashValue = 'encryptedVerifierHashValue';
    XMLEncryption = 'encryption';
    XMLHashAlgorithm = 'hashAlgorithm';
    XMLHashSize = 'hashSize';
    XMLKeyBits = 'keyBits';
    XMLKeyData = 'keyData';
    XMLKeyEncryptor = 'keyEncryptor';
    XMLKeyEncryptors = 'keyEncryptors';
    XMLSaltSize = 'saltSize';
    XMLSaltValue = 'saltValue';
    XMLSpinCount = 'spinCount';
  {$ENDREGION}
  strict private
    FBlockSize: Integer;
    FCryptoKey: IdxCryptoKey;
    FEncryptedKey: AnsiString;
    FEncryptedVerifierHashInput: AnsiString;
    FEncryptedVerifierHashValue: AnsiString;
    FHashSize: Integer;
    FKeyDataBlockSize: Integer;
    FKeyDataCipherAlgorithm: Integer;
    FKeyDataCipherChainingMode: Integer;
    FKeyDataCipherProvider: Integer;
    FKeyDataHashAlgorithm: Integer;
    FKeyDataSalt: TBytes;
    FSpinCount: Integer;

    procedure Check(AValue: Boolean);
    function DecryptSegment(const ABlock, AInitialVector: TBytes; ABlockSize: Integer; AKey: IdxCryptoKey): TBytes;
    function GenerateKey(AProvider: IdxCryptoProvider; const APasswordHash: TBytes; const ABlockKey: PAgileBlock): TBytes;
    function HashInput(AProvider: IdxCryptoProvider; const APasswordHash, AInput: TBytes; ABlock: TAgileBlock): TBytes; overload;
    function HashInput(AProvider: IdxCryptoProvider; const APasswordHash: TBytes; const AInput: AnsiString; ABlock: TAgileBlock): TBytes; overload;
    procedure LoadKeyData(ANode: TdxXMLNode; var ASalt: TBytes;
      var ABlockSize, AKeySize, ACipherAlgorithm, ACipherProvider, AChainingMode, AHashAlgorithm: Integer);
  protected
    procedure Decrypt(const ASourceStream, ATargetStream: TStream); override;
    procedure Encrypt(const ASourceStream, ATargetStream: TStream); override;
    // Loading
    function CheckPassword(const APassword: string): Boolean; override;
    procedure LoadInfo(AStream: TStream); override;
    // Saving
    procedure Initialize(const APassword: string); override;
    procedure SaveInfo(AStream: TStream); override;
  public
    class function ID: Integer; override;
  end;

  { TdxOLECryptoContainerEncryptorStandard }

  TdxOLECryptoContainerEncryptorStandard = class(TdxOLECryptoContainerEncryptorCryptoAPI)
  strict private const
    EncryptedVerifierSize = 16;

		EncryptionFlagsNone		   = 0;
    EncryptionFlagsCryptoAPI = 4;
    EncryptionFlagsDocProps	 = 8;
    EncryptionFlagsExternal	 = 16;
    EncryptionFlagsAES		   = 32;

    ProviderTypeRC4 = $00000001;
  strict private
    FCSPName: UnicodeString;
    FEncryptedVerifier: TBytes;
    FEncryptedVerifierHash: TBytes;
    FFlags: Cardinal;
    FVerifierHashSize: Integer;

    function GetEncryptedVerifierHashSize: Integer;
  protected
    FCipherKey: IdxCryptoKey;

    procedure CreateCipherKey(const APassword: string); virtual;
    procedure Decrypt(const ASourceStream, ATargetStream: TStream); override;
    procedure Encrypt(const ASourceStream, ATargetStream: TStream); override;
    // Loading
    function CheckPassword(const APassword: string): Boolean; override;
    procedure LoadInfo(AStream: TStream); override;
    // Saving
    procedure Initialize(const APassword: string); override;
    procedure SaveInfo(AStream: TStream); override;
    //
    property EncryptedVerifier: TBytes read FEncryptedVerifier write FEncryptedVerifier;
    property EncryptedVerifierHash: TBytes read FEncryptedVerifierHash write FEncryptedVerifierHash;
    property EncryptedVerifierHashSize: Integer read GetEncryptedVerifierHashSize;
    property Flags: Cardinal read FFlags write FFlags;
    property Salt: TBytes read FSalt write FSalt;
    property VerifierHashSize: Integer read FVerifierHashSize write FVerifierHashSize;
    //
    property CipherKey: IdxCryptoKey read FCipherKey;
    property CSPName: UnicodeString read FCSPName;
  public
    constructor Create; override;
    class function ID: Integer; override;
  end;

  { TdxOLECryptoContainerEncryptors }

  TdxOLECryptoContainerEncryptors = class
  strict private
    class var FEncryptors: TList<TdxOLECryptoContainerEncryptorClass>;
  public
    class function FindByID(ID: Integer; out AClass: TdxOLECryptoContainerEncryptorClass): Boolean;
    class function GetByID(ID: Integer): TdxOLECryptoContainerEncryptorClass;
    class procedure Register(AClass: TdxOLECryptoContainerEncryptorClass);
    class procedure Unregister(AClass: TdxOLECryptoContainerEncryptorClass);
  end;

  { TdxOLECryptoContainerDataSpaces }

  TdxOLECryptoContainerDataSpaces = class
  protected const
    DataSpaceDefinition = #6'DataSpaces/DataSpaceInfo/StrongEncryptionDataSpace';
    DataSpaceMap = #6'DataSpaces/DataSpaceMap';
    DataSpaceVersion = #6'DataSpaces/Version';
    DataTransformInfo = #6'DataSpaces/TransformInfo/StrongEncryptionTransform/'#6'Primary';
    EncryptionInfo = 'EncryptionInfo';
    PackageName = 'EncryptedPackage';
  protected
    class procedure ReplaceSizeMark(AStream: TStream; const ASizeMarkPosition: Int64);
    class function WriteSizeMark(AStream: TStream): Int64;
    class procedure WriteUnicodeLPP4(AStream: TStream; const S: UnicodeString);
  public
    class procedure WriteDataSpaceDefinition(AStream: TStream);
    class procedure WriteDataSpaceMap(AStream: TStream);
    class procedure WriteDataTransformInfo(AStream: TStream);
    class procedure WriteVersion(AStream: TStream);
  end;

  { TdxOLECryptoContainer }

  TdxOLECryptoContainerGetPasswordFunc = reference to function (var APassword: string): Boolean;

  TdxOLECryptoContainer = class
  public
    class function IsOurStream(AStream: TStream): Boolean;

    class function Decrypt(ASourceStream: TStream; out ATargetStream: TStream;
      AGetPasswordFunc: TdxOLECryptoContainerGetPasswordFunc; AGetPasswordAttemptCount: Integer = MaxWord): Boolean; overload;
    class function Decrypt(ASourceStream: TStream; out ATargetStream: TStream; var APassword: string;
      AGetPasswordFunc: TdxOLECryptoContainerGetPasswordFunc; AGetPasswordAttemptCount: Integer = MaxWord): Boolean; overload;
    class function Decrypt(ASourceStream: TStream; APassword: string; out ATargetStream: TStream): Boolean; overload;

    class procedure Encrypt(ASourceStream, ATargetStream: TStream; AEncryptor: TdxOLECryptoContainerEncryptor); overload;
    class procedure Encrypt(ASourceStream, ATargetStream: TStream;
      const APassword: string; AEncryptorClass: TdxOLECryptoContainerEncryptorClass); overload;
  end;

implementation

uses
  dxOLECryptoContainerStrs, cxVariants, Math, dxCryptoAPI, StrUtils, dxBase64;

{ TdxOLECryptoContainerEncryptors }

class function TdxOLECryptoContainerEncryptors.FindByID(ID: Integer; out AClass: TdxOLECryptoContainerEncryptorClass): Boolean;
var
  I: Integer;
begin
  Result := False;
  if FEncryptors <> nil then
    for I := 0 to FEncryptors.Count - 1 do
    begin
      Result := FEncryptors[I].ID = ID;
      if Result then
      begin
        AClass := FEncryptors[I];
        Break;
      end;
    end;
end;

class function TdxOLECryptoContainerEncryptors.GetByID(ID: Integer): TdxOLECryptoContainerEncryptorClass;
begin
  if not FindByID(ID, Result) then
    raise EdxOLECryptoContainerError.CreateFmt(cxGetResourceString(@sdxOleCryptoContainerUnsupportedEncryptor), [ID]);
end;

class procedure TdxOLECryptoContainerEncryptors.Register(AClass: TdxOLECryptoContainerEncryptorClass);
begin
  if FEncryptors = nil then
    FEncryptors := TList<TdxOLECryptoContainerEncryptorClass>.Create;
  FEncryptors.Add(AClass);
end;

class procedure TdxOLECryptoContainerEncryptors.Unregister(AClass: TdxOLECryptoContainerEncryptorClass);
begin
  if (FEncryptors <> nil) and (FEncryptors.Remove(AClass) >= 0) and (FEncryptors.Count = 0) then
    FreeAndNil(FEncryptors);
end;

{ TdxOLECryptoContainerEncryptor }

constructor TdxOLECryptoContainerEncryptor.Create;
begin
  inherited Create;
end;

constructor TdxOLECryptoContainerEncryptor.Create(const APassword: string);
begin
  Create;
  Initialize(APassword);
end;

class function TdxOLECryptoContainerEncryptor.ID: Integer;
begin
  Result := 0;
end;

class procedure TdxOLECryptoContainerEncryptor.Register;
begin
  TdxOLECryptoContainerEncryptors.Register(Self);
end;

class procedure TdxOLECryptoContainerEncryptor.Unregister;
begin
  TdxOLECryptoContainerEncryptors.Unregister(Self);
end;

procedure TdxOLECryptoContainerEncryptor.DecryptPackage(const ASourceStream, ATargetStream: TStream);
var
  APosition: Int64;
  ASize: Int64;
begin
  APosition := ATargetStream.Position;
  ReadInt64Proc(ASourceStream, ASize);
  ATargetStream.Size := APosition + ASize;
  Decrypt(ASourceStream, ATargetStream);
  ATargetStream.Size := APosition + ASize;
end;

procedure TdxOLECryptoContainerEncryptor.EncryptPackage(const ASourceStream, ATargetStream: TStream);
begin
  WriteInt64Proc(ATargetStream, ASourceStream.Size - ASourceStream.Position);
  Encrypt(ASourceStream, ATargetStream);
end;

{ TdxOLECryptoContainerEncryptorCryptoAPI }

constructor TdxOLECryptoContainerEncryptorCryptoAPI.Create;
begin
  inherited Create;
  FCipherChainingMode := CRYPT_MODE_CBC;
end;

function TdxOLECryptoContainerEncryptorCryptoAPI.CalculateHash(const AProvider: IdxCryptoProvider; const ABytes: TBytes): TBytes;
begin
  Result := CalculateHash(AProvider, ABytes, HashAlgorithm);
end;

function TdxOLECryptoContainerEncryptorCryptoAPI.CalculateHash(
  const AProvider: IdxCryptoProvider; const ABytes: TBytes; AHashAlgorithm: Integer): TBytes;
var
  AHash: TdxCryptoHashAlgorithm;
begin
  AHash := TdxCryptoHashAlgorithm.Create(AHashAlgorithm, AProvider);
  try
    AHash.Add(ABytes);
    Result := AHash.GetHash;
  finally
    AHash.Free;
  end;
end;

{ TdxOLECryptoContainerEncryptorAgile }

class function TdxOLECryptoContainerEncryptorAgile.ID: Integer;
begin
  Result := MakeLong(4, 4);
end;

procedure TdxOLECryptoContainerEncryptorAgile.Decrypt(const ASourceStream, ATargetStream: TStream);
const
  BlockSize = 4096;
var
  ABlock: TBytes;
  ABlockIndex: Integer;
  ABlockUsage: Integer;
  ADecryptedBlock: TBytes;
begin
  ABlockIndex := 0;
  SetLength(ABlock, BlockSize);
  repeat
    ABlockUsage := ASourceStream.Read(ABlock[0], BlockSize);
    if ABlockUsage > 0 then
    begin
      ADecryptedBlock := DecryptSegment(ABlock,
        CalculateHash(FCryptoKey.GetProvider, TdxByteArray.Concatenate(FKeyDataSalt, ABlockIndex), FKeyDataHashAlgorithm),
        FKeyDataBlockSize, FCryptoKey);
      ATargetStream.Write(ADecryptedBlock[0], ABlockUsage);
    end;
    Inc(ABlockIndex);
  until ABlockUsage = 0;
end;

procedure TdxOLECryptoContainerEncryptorAgile.Encrypt(const ASourceStream, ATargetStream: TStream);
begin
  Check(False);
end;

function TdxOLECryptoContainerEncryptorAgile.CheckPassword(const APassword: string): Boolean;
var
  AHash: TdxCryptoHashAlgorithm;
  APasswordHash: TBytes;
  AProvider: IdxCryptoProvider;
  AVerifierHashCalculated: TBytes;
  AVerifierHashOriginal: TBytes;
begin
  AProvider := TdxCryptoProvider.Create(nil, FCipherProvider);

  AHash := TdxCryptoHashAlgorithm.Create(FHashAlgorithm, AProvider);
  try
    TdxPasswordHashCodeCalculator.CalculatePasswordHash(APassword, FSalt, FSpinCount, AHash, True);
    APasswordHash := AHash.GetHash;
  finally
    AHash.Free;
  end;

  AVerifierHashCalculated := CalculateHash(AProvider,
    HashInput(AProvider, APasswordHash, FEncryptedVerifierHashInput, EncryptedVerifierHashInputBlockKey), FHashAlgorithm);
  AVerifierHashOriginal := TdxByteArray.Resize(
    HashInput(AProvider, APasswordHash, FEncryptedVerifierHashValue, EncryptedVerifierHashValueBlockKey), FHashSize);

  Result := TdxByteArray.Compare(AVerifierHashCalculated, AVerifierHashOriginal);
  if Result then
  begin
    FCryptoKey := TdxCryptoKey.Create(TdxCryptoProvider.Create(nil, FKeyDataCipherProvider),
      FKeyDataCipherAlgorithm, HashInput(AProvider, APasswordHash, FEncryptedKey, EncryptedKeyValueBlockKey));
    FCryptoKey.ChainingMode := FKeyDataCipherChainingMode;
  end;
end;

procedure TdxOLECryptoContainerEncryptorAgile.LoadInfo(AStream: TStream);
var
  ADoc: TdxXMLDocument;
  ANode: TdxXMLNode;
  ATempValue: Integer;
begin
  ADoc := TdxXMLDocument.Create(nil);
  try
    ADoc.LoadFromStream(AStream);

    Check(ADoc.FindChild([XMLEncryption, XMLKeyData], ANode));
    LoadKeyData(ANode, FKeyDataSalt, FKeyDataBlockSize, ATempValue, FKeyDataCipherAlgorithm,
      FKeyDataCipherProvider, FKeyDataCipherChainingMode, FKeyDataHashAlgorithm);

    Check(ADoc.FindChild([XMLEncryption, XMLKeyEncryptors, XMLKeyEncryptor, XMLEncryptedKey], ANode));
    LoadKeyData(ANode, FSalt, FBlockSize, FKeySize, FCipherAlgorithm, FCipherProvider, FCipherChainingMode, FHashAlgorithm);
    FEncryptedVerifierHashInput := ANode.Attributes.GetValue(XMLEncryptedVerifierHashInput);
    FEncryptedVerifierHashValue := ANode.Attributes.GetValue(XMLEncryptedVerifierHashValue);
    FEncryptedKey := ANode.Attributes.GetValue(XMLEncryptedKeyValue);
    FHashSize := ANode.Attributes.GetValueAsInteger(XMLHashSize);
    FSpinCount := ANode.Attributes.GetValueAsInteger(XMLSpinCount);
  finally
    ADoc.Free;
  end;
end;

procedure TdxOLECryptoContainerEncryptorAgile.Initialize(const APassword: string);
begin
  Check(False);
end;

procedure TdxOLECryptoContainerEncryptorAgile.SaveInfo(AStream: TStream);
begin
  Check(False);
end;

procedure TdxOLECryptoContainerEncryptorAgile.Check(AValue: Boolean);
begin
  if not AValue then
    raise EdxOLECryptoContainerError.Create(cxGetResourceString(@sdxOleCryptoContainerInternalError));
end;

function TdxOLECryptoContainerEncryptorAgile.DecryptSegment(
  const ABlock, AInitialVector: TBytes; ABlockSize: Integer; AKey: IdxCryptoKey): TBytes;

  function AdjustSize(AInputLength, ABlockSize: Integer): Integer;
  begin
    Result := ABlockSize;
    while Result < AInputLength do
      Inc(Result, ABlockSize);
  end;

begin
  AKey.SetIV(TdxByteArray.Resize(AInitialVector, ABlockSize, $36));
  Result := TdxCipher.Decrypt(AKey, TdxByteArray.Resize(ABlock, AdjustSize(Length(ABlock), ABlockSize)));
end;

function TdxOLECryptoContainerEncryptorAgile.GenerateKey(
  AProvider: IdxCryptoProvider; const APasswordHash: TBytes; const ABlockKey: PAgileBlock): TBytes;
var
  AHash: TdxCryptoHashAlgorithm;
begin
  AHash := TdxCryptoHashAlgorithm.Create(FHashAlgorithm, AProvider);
  try
    AHash.Add(APasswordHash);
    if ABlockKey <> nil then
      AHash.Add(@ABlockKey^[0], Length(ABlockKey^));
    Result := TdxByteArray.Resize(AHash.GetHash, FKeySize div 8, $36);
  finally
    AHash.Free;
  end;
end;

function TdxOLECryptoContainerEncryptorAgile.HashInput(
  AProvider: IdxCryptoProvider; const APasswordHash, AInput: TBytes; ABlock: TAgileBlock): TBytes;
var
  ASecretKey: IdxCryptoKey;
begin
  ASecretKey := TdxCryptoKey.Create(AProvider, FCipherAlgorithm, GenerateKey(AProvider, APasswordHash, @ABlock));
  ASecretKey.ChainingMode := FCipherChainingMode;
  Result := DecryptSegment(AInput, FSalt, FBlockSize, ASecretKey);
end;

function TdxOLECryptoContainerEncryptorAgile.HashInput(AProvider: IdxCryptoProvider;
  const APasswordHash: TBytes; const AInput: AnsiString; ABlock: TAgileBlock): TBytes;
begin
  Result := HashInput(AProvider, APasswordHash, TdxBase64.FromBase64String(dxAnsiStringToString(AInput)), ABlock);
end;

procedure TdxOLECryptoContainerEncryptorAgile.LoadKeyData(ANode: TdxXMLNode;
  var ASalt: TBytes; var ABlockSize, AKeySize, ACipherAlgorithm, ACipherProvider, AChainingMode, AHashAlgorithm: Integer);

  function StringToChainingMode(const S: AnsiString): Integer;
  const
    ModeIDs: array[0..1] of Integer = (CRYPT_MODE_CBC, CRYPT_MODE_CFB);
    Names: array[0..1] of AnsiString = ('ChainingModeCBC', 'ChainingModeCFB');
  var
    I: Integer;
  begin
    if S = '' then
      Exit(CRYPT_MODE_CBC);
    for I := Low(Names) to High(Names) do
    begin
      if Names[I] = S then
        Exit(ModeIDs[I]);
    end;
    raise EdxOLECryptoContainerError.Create(cxGetResourceString(@sdxOleCryptoContainerInternalError));
  end;

var
  AHashProvider: Integer;
begin
  Check(ANode <> nil);

  ASalt := TdxBase64.FromBase64String(ANode.Attributes.GetValueAsString(XMLSaltValue));
  AChainingMode := StringToChainingMode(ANode.Attributes.GetValue(XMLCipherChaining, ''));
  ABlockSize := ANode.Attributes.GetValueAsInteger(XMLBlockSize);
  AKeySize := ANode.Attributes.GetValueAsInteger(XMLKeyBits);

  Check(TdxCryptoAlgorithms.GetInfo(
    ANode.Attributes.GetValueAsString(XMLCipherAlgorithm, 'AES'),
    AKeySize, ACipherAlgorithm, ACipherProvider));

  Check(TdxCryptoAlgorithms.GetInfo(
    ANode.Attributes.GetValueAsString(XMLHashAlgorithm, 'SHA1'),
    ANode.Attributes.GetValueAsInteger(XMLHashSize) * 8, AHashAlgorithm, AHashProvider));
end;

{ TdxOLECryptoContainerEncryptorStandard }

constructor TdxOLECryptoContainerEncryptorStandard.Create;
begin
  inherited Create;
  FHashAlgorithm := CALG_SHA1;
  FCSPName := CryptoProviderEnhancedRSA_AES_XP;
  FFlags := EncryptionFlagsAES or EncryptionFlagsCryptoAPI;
  FCipherChainingMode := CRYPT_MODE_ECB;
  FCipherAlgorithm := CALG_AES_128;
  FCipherProvider := PROV_RSA_AES;
  FKeySize := 128;
end;

class function TdxOLECryptoContainerEncryptorStandard.ID: Integer;
begin
  Result := MakeLong(4, 2);
end;

procedure TdxOLECryptoContainerEncryptorStandard.CreateCipherKey(const APassword: string);

  function GetActualCSPName: string;
  begin
    if dxSameText(CSPName, CryptoProviderEnhancedRSA_AES_XP) and IsWinVistaOrLater then
      Result := CryptoProviderEnhancedRSA_AES
    else
      Result := CSPName;
  end;

var
  AHash: TdxCryptoHashAlgorithm;
  AKey: TBytes;
begin
  AHash := TdxCryptoHashAlgorithm.Create(HashAlgorithm, TdxCryptoProvider.Create(PChar(GetActualCSPName), CipherProvider));
  try
    TdxPasswordHashCodeCalculator.CalculatePasswordHash(APassword, FSalt, 50000, AHash, True);
    AKey := AHash.GetHash;
    AHash.Reset;
    AHash.Add(TdxByteArray.Concatenate(AKey, 0));

    FCipherKey := TdxCryptoKey.Create(AHash.Provider, CipherAlgorithm, AHash.Handle);
    FCipherKey.ChainingMode := CipherChainingMode;
  finally
    AHash.Free;
  end;
end;

procedure TdxOLECryptoContainerEncryptorStandard.Decrypt(const ASourceStream, ATargetStream: TStream);
begin
  TdxCipher.Decrypt(CipherKey, ASourceStream, ATargetStream);
end;

procedure TdxOLECryptoContainerEncryptorStandard.Encrypt(const ASourceStream, ATargetStream: TStream);
begin
  TdxCipher.Encrypt(CipherKey, ASourceStream, ATargetStream);
end;

function TdxOLECryptoContainerEncryptorStandard.CheckPassword(const APassword: string): Boolean;
var
  AVerifierHashCalculated: TBytes;
  AVerifierHashOriginal: TBytes;
begin
  CreateCipherKey(APassword);

  AVerifierHashCalculated := CalculateHash(CipherKey.GetProvider,
    TdxByteArray.Resize(TdxCipher.Decrypt(CipherKey, EncryptedVerifier), 16));
  AVerifierHashOriginal := TdxCipher.Decrypt(CipherKey, EncryptedVerifierHash);

  Result := (Length(AVerifierHashCalculated) <= Length(AVerifierHashOriginal)) and
    CompareMem(@AVerifierHashOriginal[0], @AVerifierHashCalculated[0], Length(AVerifierHashCalculated));
end;

procedure TdxOLECryptoContainerEncryptorStandard.LoadInfo(AStream: TStream);
var
  AHeaderSize: Integer;
begin
  Flags := ReadCardinalFunc(AStream);
  AHeaderSize := ReadIntegerFunc(AStream);
  Flags := ReadIntegerFunc(AStream);
  ReadIntegerFunc(AStream);
  FCipherAlgorithm := ReadIntegerFunc(AStream);
  FHashAlgorithm := ReadIntegerFunc(AStream);
  FKeySize := ReadIntegerFunc(AStream);
  if FKeySize = 0 then
    FKeySize := $28;
  FCipherProvider := ReadIntegerFunc(AStream);
  ReadIntegerFunc(AStream);
  ReadIntegerFunc(AStream);

  Dec(AHeaderSize, 8 * SizeOf(Integer));
  SetLength(FCSPName, AHeaderSize div SizeOf(WideChar));
  AStream.ReadBuffer(FCSPName[1], AHeaderSize);
  FCSPName := Trim(FCSPName);

  FSalt := ReadBytesFunc(AStream, ReadIntegerFunc(AStream));
  FEncryptedVerifier := ReadBytesFunc(AStream, EncryptedVerifierSize);
  VerifierHashSize := ReadIntegerFunc(AStream);
  FEncryptedVerifierHash := ReadBytesFunc(AStream, EncryptedVerifierHashSize);
end;

procedure TdxOLECryptoContainerEncryptorStandard.Initialize(const APassword: string);
var
  AVerifier: TBytes;
  AVerifierHash: TBytes;
begin
  FSalt := dxGenerateSalt(16);
  CreateCipherKey(APassword);

  AVerifier := dxGenerateSalt(Length(CipherKey.GetIV));
  FEncryptedVerifier := TdxCipher.Encrypt(CipherKey, AVerifier);
  SetLength(FEncryptedVerifier, EncryptedVerifierSize);

  AVerifierHash := CalculateHash(CipherKey.GetProvider, AVerifier);
  FVerifierHashSize := Length(AVerifierHash);
  FEncryptedVerifierHash := TdxCipher.Encrypt(CipherKey, TdxByteArray.Resize(AVerifierHash, 32));
  SetLength(FEncryptedVerifierHash, EncryptedVerifierHashSize);
end;

procedure TdxOLECryptoContainerEncryptorStandard.SaveInfo(AStream: TStream);
begin
  WriteCardinalProc(AStream, Flags);
  WriteCardinalProc(AStream, 8 * SizeOf(Integer) + SizeOf(WideChar) * (Length(CSPName) + 1));

  WriteIntegerProc(AStream, Flags);
  WriteIntegerProc(AStream, 0);
  WriteIntegerProc(AStream, CipherAlgorithm);
  WriteIntegerProc(AStream, HashAlgorithm);
  WriteIntegerProc(AStream, KeySize);
  WriteIntegerProc(AStream, CipherProvider);
  WriteIntegerProc(AStream, 0);
  WriteIntegerProc(AStream, 0);

  WriteBytesProc(AStream, TEncoding.Unicode.GetBytes(CSPName));
  WriteWordProc(AStream, 0);

  WriteIntegerProc(AStream, Length(Salt));
  WriteBytesProc(AStream, Salt);
  WriteBytesProc(AStream, FEncryptedVerifier);
  WriteIntegerProc(AStream, VerifierHashSize);
  WriteBytesProc(AStream, FEncryptedVerifierHash);
end;

function TdxOLECryptoContainerEncryptorStandard.GetEncryptedVerifierHashSize: Integer;
begin
  Result := IfThen(CipherProvider = ProviderTypeRC4, $14, $20);
end;

{ TdxOLECryptoContainerDataSpaces }

class procedure TdxOLECryptoContainerDataSpaces.WriteDataSpaceDefinition(AStream: TStream);
begin
  WriteIntegerProc(AStream, 8);
  WriteIntegerProc(AStream, 1);
  WriteUnicodeLPP4(AStream, 'StrongEncryptionTransform');
end;

class procedure TdxOLECryptoContainerDataSpaces.WriteDataSpaceMap(AStream: TStream);
var
  AMark: Int64;
begin
  WriteIntegerProc(AStream, 8);
  WriteIntegerProc(AStream, 1);

  AMark := WriteSizeMark(AStream);
  WriteIntegerProc(AStream, 1);
  WriteIntegerProc(AStream, 0);
  WriteUnicodeLPP4(AStream, PackageName);
  WriteUnicodeLPP4(AStream, 'StrongEncryptionDataSpace');
  ReplaceSizeMark(AStream, AMark);
end;

class procedure TdxOLECryptoContainerDataSpaces.WriteDataTransformInfo(AStream: TStream);
var
  AMark: Int64;
begin
  AMark := WriteSizeMark(AStream);
  WriteIntegerProc(AStream, 1);
  WriteUnicodeLPP4(AStream, '{FF9A3F03-56EF-4613-BDD5-5A41C1D07246}');
  ReplaceSizeMark(AStream, AMark);
  WriteUnicodeLPP4(AStream, 'Microsoft.Container.EncryptionTransform');
  WriteWordProc(AStream, 1);
  WriteWordProc(AStream, 0);
  WriteWordProc(AStream, 1);
  WriteWordProc(AStream, 0);
  WriteWordProc(AStream, 1);
  WriteWordProc(AStream, 0);

  WriteIntegerProc(AStream, 0);

  WriteIntegerProc(AStream, 0);
  WriteIntegerProc(AStream, 0);

  WriteIntegerProc(AStream, 4);
end;

class procedure TdxOLECryptoContainerDataSpaces.WriteVersion(AStream: TStream);
begin
  WriteUnicodeLPP4(AStream, 'Microsoft.Container.DataSpaces');
  WriteWordProc(AStream, 1);
  WriteWordProc(AStream, 0);
  WriteWordProc(AStream, 1);
  WriteWordProc(AStream, 0);
  WriteWordProc(AStream, 1);
  WriteWordProc(AStream, 0);
end;

class procedure TdxOLECryptoContainerDataSpaces.ReplaceSizeMark(AStream: TStream; const ASizeMarkPosition: Int64);
var
  ASize: Integer;
begin
  ASize := AStream.Position - ASizeMarkPosition;
  AStream.Position := ASizeMarkPosition;
  WriteIntegerProc(AStream, ASize);
  AStream.Position := ASizeMarkPosition + ASize;
end;

class function TdxOLECryptoContainerDataSpaces.WriteSizeMark(AStream: TStream): Int64;
begin
  Result := AStream.Position;
  WriteIntegerProc(AStream, 0);
end;

class procedure TdxOLECryptoContainerDataSpaces.WriteUnicodeLPP4(AStream: TStream; const S: UnicodeString);
var
  ABytes: TBytes;
begin
  ABytes := TEncoding.Unicode.GetBytes(S);
  WriteIntegerProc(AStream, Length(ABytes));
  WriteBytesProc(AStream, ABytes);
  if Length(ABytes) mod 4 = 2 then
    WriteWordProc(AStream, 0);
end;

{ TdxOLECryptoContainer }

class function TdxOLECryptoContainer.IsOurStream(AStream: TStream): Boolean;
var
  ADocument: TdxOLEDocument;
  ASavedPosition: Int64;
begin
  Result := False;
  if dxIsOLEStream(AStream) then
  begin
    ASavedPosition := AStream.Position;
    try
      ADocument := TdxOLEDocument.Create(AStream, dmReading);
      try
        Result :=
          (ADocument.StreamByName(TdxOLECryptoContainerDataSpaces.EncryptionInfo) <> nil) and
          (ADocument.StreamByName(TdxOLECryptoContainerDataSpaces.PackageName) <> nil);
      finally
        ADocument.Free;
      end;
    except
      Result := False;
    end;
    AStream.Position := ASavedPosition;
  end;
end;

class function TdxOLECryptoContainer.Decrypt(ASourceStream: TStream; out ATargetStream: TStream;
  AGetPasswordFunc: TdxOLECryptoContainerGetPasswordFunc; AGetPasswordAttemptCount: Integer = MaxWord): Boolean;
var
  APassword: string;
begin
  Result := Decrypt(ASourceStream, ATargetStream, APassword, AGetPasswordFunc, AGetPasswordAttemptCount);
end;

class function TdxOLECryptoContainer.Decrypt(ASourceStream: TStream; out ATargetStream: TStream; var APassword: string;
  AGetPasswordFunc: TdxOLECryptoContainerGetPasswordFunc; AGetPasswordAttemptCount: Integer = MaxWord): Boolean;
var
  ADocument: TdxOLEDocument;
  AEncryptor: TdxOLECryptoContainerEncryptor;
  AStreamInfo: TStream;
  AStreamPackage: TStream;
begin
  Result := False;
  if dxIsOLEStream(ASourceStream) then
  begin
    ADocument := TdxOLEDocument.Create(ASourceStream, dmReading);
    try
      AStreamInfo := ADocument.StreamByName(TdxOLECryptoContainerDataSpaces.EncryptionInfo);
      AStreamPackage := ADocument.StreamByName(TdxOLECryptoContainerDataSpaces.PackageName);
      if (AStreamInfo <> nil) and (AStreamPackage <> nil) then
      begin
        AEncryptor := TdxOLECryptoContainerEncryptors.GetByID(ReadCardinalFunc(AStreamInfo)).Create;
        try
          ATargetStream := TMemoryStream.Create;
          try
            AEncryptor.LoadInfo(AStreamInfo);

            while (APassword = '') or not AEncryptor.CheckPassword(APassword) do
            begin
              if AGetPasswordAttemptCount <= 0 then
                raise EdxOLECryptoContainerError.Create(cxGetResourceString(@sdxOleCryptoContainerInvalidPassword));
              if not (Assigned(AGetPasswordFunc) and AGetPasswordFunc(APassword)) then
                Abort;
              Dec(AGetPasswordAttemptCount);
            end;

            AEncryptor.DecryptPackage(AStreamPackage, ATargetStream);
            ATargetStream.Position := 0;
            Result := True;
          except
            FreeAndNil(ATargetStream);
            raise;
          end;
        finally
          FreeAndNil(AEncryptor);
        end;
      end;
    finally
      ADocument.Free;
    end;
  end;
end;

class function TdxOLECryptoContainer.Decrypt(ASourceStream: TStream; APassword: string; out ATargetStream: TStream): Boolean;
begin
  Result := Decrypt(ASourceStream, ATargetStream, APassword, nil, 0);
end;

class procedure TdxOLECryptoContainer.Encrypt(ASourceStream, ATargetStream: TStream;
  const APassword: string; AEncryptorClass: TdxOLECryptoContainerEncryptorClass);
var
  AEncryptor: TdxOLECryptoContainerEncryptor;
begin
  AEncryptor := AEncryptorClass.Create(APassword);
  try
    Encrypt(ASourceStream, ATargetStream, AEncryptor);
  finally
    AEncryptor.Free;
  end;
end;

class procedure TdxOLECryptoContainer.Encrypt(
  ASourceStream, ATargetStream: TStream; AEncryptor: TdxOLECryptoContainerEncryptor);

  function CreateStreamInDir(ADocument: TdxOLEDocument;
    AParentDir: TdxOLEDocumentDirectoryEntry; const AFileName: string): TStream;
  var
    AParts: TStringList;
    I: Integer;
  begin
    AParts := TStringList.Create;
    try
      AParts.Text := StringReplace(AFileName, '/', dxCRLF, [rfReplaceAll]);
      for I := 0 to AParts.Count - 2 do
        AParentDir := ADocument.CreateDirEntry(AParts[I], ET_STORAGE, AParentDir, True);
      Result := ADocument.CreateStream(AParentDir, AParts[AParts.Count - 1]);
    finally
      AParts.Free;
    end;
  end;

var
  ADocument: TdxOLEDocument;
  ARootDir: TdxOLEDocumentDirectoryEntry;
  AStream: TStream;
begin
  ADocument := TdxOLEDocument.Create(ATargetStream, dmWriting);
  try
    ARootDir := ADocument.CreateDirEntry(TdxOLEDocument.RootDirName, ET_ROOT);

    AStream := ADocument.CreateStream(ARootDir, TdxOLECryptoContainerDataSpaces.EncryptionInfo);
    WriteCardinalProc(AStream, AEncryptor.ID);
    AEncryptor.SaveInfo(AStream);

    ASourceStream.Position := 0;
    AEncryptor.EncryptPackage(ASourceStream, ADocument.CreateStream(ARootDir, TdxOLECryptoContainerDataSpaces.PackageName));

    TdxOLECryptoContainerDataSpaces.WriteVersion(
      CreateStreamInDir(ADocument, ARootDir, TdxOLECryptoContainerDataSpaces.DataSpaceVersion));
    TdxOLECryptoContainerDataSpaces.WriteDataSpaceMap(
      CreateStreamInDir(ADocument, ARootDir, TdxOLECryptoContainerDataSpaces.DataSpaceMap));
    TdxOLECryptoContainerDataSpaces.WriteDataSpaceDefinition(
      CreateStreamInDir(ADocument, ARootDir, TdxOLECryptoContainerDataSpaces.DataSpaceDefinition));
    TdxOLECryptoContainerDataSpaces.WriteDataTransformInfo(
      CreateStreamInDir(ADocument, ARootDir, TdxOLECryptoContainerDataSpaces.DataTransformInfo));
    ADocument.Commit;
  finally
    ADocument.Free;
  end;
end;

initialization
  TdxOLECryptoContainerEncryptorAgile.Register;
  TdxOLECryptoContainerEncryptorStandard.Register;

finalization
  TdxOLECryptoContainerEncryptorStandard.Unregister;
  TdxOLECryptoContainerEncryptorAgile.Unregister;
end.
