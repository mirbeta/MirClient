{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
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

unit dxPDFEncryption;

{$I cxVer.inc}

interface

uses
  SysUtils, Windows, dxCore, dxCrypto, dxProtectionUtils, dxPDFBase, dxPDFTypes;

type
  TdxPDFCryptMethodVersion = (cmvV2, cmvAESV2, cmvAESV3);
  TdxPDFPasswordCheckingResult = (pcrContinue, pcrSuccess, pcrExit);

  { TdxPDFCustomEncryptionAlgorithm }

  TdxPDFCustomEncryptionAlgorithmClass = class of TdxPDFCustomEncryptionAlgorithm;
  TdxPDFCustomEncryptionAlgorithm = class
  strict private
    function CalculateMD5Hash(const AKey: TBytes): TBytes;
    function CalculateUserPasswordHash(const APassword: TBytes): TBytes;
    function CalculateOwnerEncryptionKey(const APassword: TBytes): TBytes;
    function DoCheckPassword(AOnGetPasswordEvent: TdxGetPasswordEvent): TdxPDFPasswordCheckingResult;
    function GetActualKeyLength: Integer;
    function GetAllowContentExtraction: Boolean;
    function GetAllowPrinting: Boolean;
    function GetBitState(AIndex: Integer): Boolean;
    function GetExtendedKeyLength: Integer;
    function IsExistFilter(ADictionary: TdxPDFDictionary; const AName: string): Boolean;
    function NeedCheckPermission: Boolean;
    function PadOrTruncatePassword(const APasswordString: TBytes): TBytes;
    function ValidateHash(const AHash: TBytes): TBytes;
    function XorKey(const AKey: TBytes; AValue: Integer): TBytes;
    procedure ReadCryptMethodVersion(ADictionary: TdxPDFDictionary);
    procedure ReadKeyLength(ADictionary: TdxPDFDictionary);
    procedure UpdateEncryptionKey(const AKey: TBytes);
  private
    FCryptMethodVersion: TdxPDFCryptMethodVersion;
    FDocumentID: TdxPDFDocumentID;
    FEncryptionFlags: Int64;
    FEncryptMetadata: Boolean;
    FIsOwnerKey: Boolean;
    FPasswordPadding: TBytes;
    FPermissionFlags: Integer;
    FSecurityHandlerRevision: Integer;
    FStreamFilterName: string;
  strict protected const
    HashSize = 32;
  strict protected
    FEncryptedPermissions: TBytes;
    FEncodedOwnerPassword: TBytes;
    FEncodedUserPassword: TBytes;
    FEncryptionKey: TBytes;
    FKeyLength: Integer;
    FOwnerPasswordHash: TBytes;
    FUserPasswordHash: TBytes;
    function CheckUserPassword(const AUserPassword: TBytes): Boolean; virtual;
    function CheckOwnerPassword(const APassword: TBytes): Boolean; virtual;

    function ConvertToBytes(const APassword: string): TBytes; virtual;
    function DoUpdateEncryptionKey: TBytes; virtual;
    function DoValidateHash(const AHash: TBytes): TBytes; virtual;
    function ValidatePassword(const APasswordString: string): TdxPDFPasswordCheckingResult; virtual;
    procedure InitializePassword(var APassword: TBytes); virtual;
    procedure Read(ADictionary: TdxPDFDictionary); virtual;

    function CalculateEncryptionKey(ANumber: Integer): TBytes;
    function CheckPassword(const AExpectedHash, AActualHash: TBytes): Boolean; overload;
    function RC4Crypt(const AKey, AData: TBytes; ADataSize: Integer): TBytes;
    procedure InitializeCryptFilter(ADictionary: TdxPDFDictionary);
  protected
    property ActualKeyLength: Integer read GetActualKeyLength;
    property BitState[AIndex: Integer]: Boolean read GetBitState;
    property CryptMethodVersion: TdxPDFCryptMethodVersion read FCryptMethodVersion;
    property EncryptionFlags: Int64 read FEncryptionFlags;
    property EncryptionKey: TBytes read FEncryptionKey write FEncryptionKey;
    property ExtendedKeyLength: Integer read GetExtendedKeyLength;
    property SecurityHandlerRevision: Integer read FSecurityHandlerRevision;
    property StreamFilterName: string read FStreamFilterName;
  public
    constructor Create(ADictionary: TdxPDFDictionary; const ADocumentID: TdxPDFDocumentID);

    function CheckPassword(AAttemptsLimit: Integer; AOnGetPasswordEvent: TdxGetPasswordEvent): Boolean; overload;
    function Decrypt(const AData: TBytes; ANumber: Integer): TBytes; virtual;

    property AllowContentExtraction: Boolean read GetAllowContentExtraction;
    property AllowPrinting: Boolean read GetAllowPrinting;
  end;

  { TdxPDFAESBasedEncryptionAlgorithm }

  TdxPDFAESBasedEncryptionAlgorithm = class(TdxPDFCustomEncryptionAlgorithm)
  private const
    InitVectorSize = 16;
    SaltSize = 8;
  private
    FOwnerKeySalt: TBytes;
    FOwnerValidationSalt: TBytes;
    FUserKeySalt: TBytes;
    FUserValidationSalt: TBytes;

    function CalculateHash(const APasswordString, AData, AUserKey: TBytes): TBytes;
    function DoAESDecrypt(const AKey, AInitVector, AData: TBytes; APosition, AChainingMode, APaddingMode: Integer): TBytes;
    function DoAESEncrypt(const AKey, AInitVector, AData: TBytes): TBytes;
  protected
    function DoUpdateEncryptionKey: TBytes; override;
    function DoValidateHash(const AHash: TBytes): TBytes; override;

    function AESDecrypt(const AKey, AData: TBytes): TBytes; overload;
    function AESDecrypt(const AKey, AData: TBytes; AChainingMode: Integer): TBytes; overload;
    function CreateAESCryptoKey(const AKey, AInitVector: TBytes): IdxCryptoKey;
  end;

   { TdxPDFMixedEncryptionAlgorithm }

  TdxPDFMixedEncryptionAlgorithm = class(TdxPDFAESBasedEncryptionAlgorithm)
  protected
    procedure Read(ADictionary: TdxPDFDictionary); override;
  public
    function Decrypt(const AData: TBytes; ANumber: Integer): TBytes; override;
  end;

  { TdxPDFAESEncryptionAlgorithm }

  TdxPDFAESEncryptionAlgorithm = class(TdxPDFAESBasedEncryptionAlgorithm)
  strict private
    function CheckPermissions: Boolean;
  protected
    function CheckUserPassword(const AUserPassword: TBytes): Boolean; override;
    function CheckOwnerPassword(const AUserPassword: TBytes): Boolean; override;
    function ConvertToBytes(const APassword: string): TBytes; override;
    function ValidatePassword(const APasswordString: string): TdxPDFPasswordCheckingResult; override;
    procedure InitializePassword(var APassword: TBytes); override;
    procedure Read(ADictionary: TdxPDFDictionary); override;
  public
    function Decrypt(const AData: TBytes; ANumber: Integer): TBytes; override;
  end;

  { TdxPDF40BitEncryptionAlgorithm }

  TdxPDF40BitEncryptionAlgorithm = class(TdxPDFCustomEncryptionAlgorithm)
  protected
    procedure Read(ADictionary: TdxPDFDictionary); override;
  end;

  { TdxPDFGreater40BitEncryptionAlgorithm }

  TdxPDFGreater40BitEncryptionAlgorithm = class(TdxPDFCustomEncryptionAlgorithm)
  protected
    procedure Read(ADictionary: TdxPDFDictionary); override;
  end;

  { TdxPDFEncryptionInfo }

  TdxPDFEncryptionInfo = class(TdxPDFBase, IUnknown, IdxPDFEncryptionInfo)
  strict private
    FAlgorithm: TdxPDFCustomEncryptionAlgorithm;
    FLock: TRtlCriticalSection;
    function GetAllowContentExtraction: Boolean;
    function GetAllowPrinting: Boolean;
    function GetEncryptionAlgorithmClass(AAlgorithm: Integer): TdxPDFCustomEncryptionAlgorithmClass;
  protected
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(ADictionary: TdxPDFDictionary; const ADocumentID: TdxPDFDocumentID);
    destructor Destroy; override;

    function CheckPassword(AAttemptsLimit: Integer; AOnGetPasswordEvent: TdxGetPasswordEvent): Boolean;
    function Decrypt(const AData: TBytes; ANumber: Integer): TBytes;

    property AllowContentExtraction: Boolean read GetAllowContentExtraction;
    property AllowPrinting: Boolean read GetAllowPrinting;
  end;

implementation

uses
  Math, dxHash, dxCryptoAPI, dxPDFUtils, dxPDFDocumentStrs;

const
  PasswordPadding: array[0.. 31] of Byte = ($28, $bf, $4e, $5e, $4e, $75, $8a, $41, $64, $00, $4e, $56, $ff, $fa, $01,
    $08, $2e, $2e, $00, $b6, $d0, $68, $3e, $80, $2f, $0c, $a9, $fe, $64, $53, $69, $7a);

{ TdxPDFCustomEncryptionAlgorithm }

constructor TdxPDFCustomEncryptionAlgorithm.Create(ADictionary: TdxPDFDictionary; const ADocumentID: TdxPDFDocumentID);
begin
  inherited Create;
  FDocumentID[0] := ADocumentID[0];
  FDocumentID[1] := ADocumentID[1];
  Read(ADictionary);
  FKeyLength := FKeyLength div 8;
end;

function TdxPDFCustomEncryptionAlgorithm.CheckPassword(AAttemptsLimit: Integer; AOnGetPasswordEvent: TdxGetPasswordEvent): Boolean;
var
  AAttemptCount: Integer;
  APassword: TBytes;
  AResult: TdxPDFPasswordCheckingResult;
begin
  InitializePassword(APassword);
  FIsOwnerKey := CheckOwnerPassword(APassword);
  Result := CheckUserPassword(APassword);
  if not Result then
  begin
    AAttemptCount := 0;
    AResult := pcrContinue;
    while (AResult = pcrContinue) and (AAttemptsLimit = 0) or (AAttemptsLimit <> 0) and (AAttemptCount < AAttemptsLimit) do
    begin
      AResult := DoCheckPassword(AOnGetPasswordEvent);
      case AResult of
        pcrSuccess:
          Exit(True);
        pcrExit:
          Exit(False);
      end;
      Inc(AAttemptCount);
    end;
    if AResult <> pcrSuccess then
      TdxPDFUtils.RaiseException(cxGetResourceString(@sdxPDFDocumentIncorrectPassword), EdxPDFEncryptionException);
  end;
end;

function TdxPDFCustomEncryptionAlgorithm.Decrypt(const AData: TBytes; ANumber: Integer): TBytes;
begin
  Result := RC4Crypt(CalculateEncryptionKey(ANumber), AData, Length(AData));
end;

function TdxPDFCustomEncryptionAlgorithm.CheckUserPassword(const AUserPassword: TBytes): Boolean;
begin
  Result := CheckPassword(FUserPasswordHash, CalculateUserPasswordHash(AUserPassword))
end;

function TdxPDFCustomEncryptionAlgorithm.CheckOwnerPassword(const APassword: TBytes): Boolean;
var
  I: Integer;
  AUserPasswordString: TBytes;
  AKey: TBytes;
begin
  AKey := CalculateOwnerEncryptionKey(APassword);
  if FSecurityHandlerRevision >= 3 then
  begin
    TdxPDFUtils.AddData(FOwnerPasswordHash, AUserPasswordString);
    for I := 19 downto 0 do
      AUserPasswordString := RC4Crypt(XorKey(AKey, I), AUserPasswordString, Length(AUserPasswordString));
  end
  else
    AUserPasswordString := RC4Crypt(AKey, FOwnerPasswordHash, Length(FOwnerPasswordHash));
  Result := CheckUserPassword(AUserPasswordString);
end;

function TdxPDFCustomEncryptionAlgorithm.ConvertToBytes(const APassword: string): TBytes;
begin
  Result := PadOrTruncatePassword(TEncoding.ASCII.GetBytes(APassword))
end;

function TdxPDFCustomEncryptionAlgorithm.DoUpdateEncryptionKey: TBytes;
begin
  Result := TdxByteArray.Resize(FEncryptionKey, ExtendedKeyLength);
end;

function TdxPDFCustomEncryptionAlgorithm.DoValidateHash(const AHash: TBytes): TBytes;
begin
  if Length(AHash) <> HashSize then
    TdxPDFUtils.Abort
  else
    Result := AHash;
end;

function TdxPDFCustomEncryptionAlgorithm.ValidatePassword(const APasswordString: string): TdxPDFPasswordCheckingResult;
var
  APassword: TBytes;
begin
  APassword := ConvertToBytes(APasswordString);
  if CheckOwnerPassword(APassword) then
  begin
    FIsOwnerKey := True;
    Result := pcrSuccess;
  end
  else
    if CheckUserPassword(APassword) then
      Result := pcrSuccess
    else
      Result := pcrContinue;
end;

procedure TdxPDFCustomEncryptionAlgorithm.InitializePassword(var APassword: TBytes);
begin
  APassword := FPasswordPadding;
end;

procedure TdxPDFCustomEncryptionAlgorithm.Read(ADictionary: TdxPDFDictionary);
begin
  SetLength(FPasswordPadding, Length(PasswordPadding));
  cxCopyData(@PasswordPadding[0], @FPasswordPadding[0], Length(PasswordPadding));

  FSecurityHandlerRevision := ADictionary.GetInteger(TdxPDFKeywords.Revision);
  FUserPasswordHash := ValidateHash(ADictionary.GetBytes(TdxPDFKeywords.UserPasswordHash));
  FEncryptionFlags := ADictionary.GetInteger(TdxPDFKeywords.Permissions);
  FPermissionFlags := FEncryptionFlags;
  FEncryptMetadata := ADictionary.GetBoolean(TdxPDFKeywords.EncryptMetadata, True);
  FKeyLength := ADictionary.GetInteger(TdxPDFKeywords.Length, 40);
  FOwnerPasswordHash := ValidateHash(ADictionary.GetBytes(TdxPDFKeywords.OwnerPasswordHash));
end;

function TdxPDFCustomEncryptionAlgorithm.CalculateEncryptionKey(ANumber: Integer): TBytes;
begin
  FEncryptionKey[FKeyLength] := ANumber and $FF;
  FEncryptionKey[FKeyLength + 1] := (ANumber and $FF00) shr 8;
  FEncryptionKey[FKeyLength + 2] := (ANumber and $FF0000) shr 16;
  FEncryptionKey[FKeyLength + 3] := 0;
  FEncryptionKey[FKeyLength + 4] := 0 shr 8;
  Result := TdxByteArray.Resize(CalculateMD5Hash(FEncryptionKey), ActualKeyLength);
end;

function TdxPDFCustomEncryptionAlgorithm.CheckPassword(const AExpectedHash, AActualHash: TBytes): Boolean;
var
  ACount, I: Integer;
begin
  Result := True;
  ACount := IfThen(FSecurityHandlerRevision = 2, HashSize, 16);
  for I := 0 to ACount - 1 do
    if AActualHash[I] <> AExpectedHash[I] then
    begin
      Result := False;
      Break;
    end;
end;

function TdxPDFCustomEncryptionAlgorithm.RC4Crypt(const AKey, AData: TBytes; ADataSize: Integer): TBytes;
var
  ARC4Key: TdxRC4Key;
begin
  SetLength(Result, ADataSize);
  dxRC4Initialize(ARC4Key, AKey);
  dxRC4Crypt(ARC4Key, @AData[0], @Result[0], ADataSize);
end;

procedure TdxPDFCustomEncryptionAlgorithm.InitializeCryptFilter(ADictionary: TdxPDFDictionary);
var
  AStringFilterName, AEmbeddedFileFilterName: string;
  AFilters, ATemp: TdxPDFDictionary;
begin
  AFilters := ADictionary.GetDictionary(TdxPDFKeywords.CryptFilters);
  FStreamFilterName := ADictionary.GetString(TdxPDFKeywords.StreamCryptFilter);
  AStringFilterName := ADictionary.GetString(TdxPDFKeywords.StringCryptFilter);
  if FSecurityHandlerRevision >= 5 then
  begin
    FEncodedOwnerPassword := ADictionary.GetBytes(TdxPDFKeywords.EncodedOwnerPassword);
    FEncodedUserPassword := ADictionary.GetBytes(TdxPDFKeywords.EncodedUserPassword);
    FEncryptedPermissions := ADictionary.GetBytes(TdxPDFKeywords.EncryptedPermissions);
  end;
  AEmbeddedFileFilterName := ADictionary.GetString(TdxPDFKeywords.EmbeddedFilesCryptFilter);
  if not IsExistFilter(AFilters, FStreamFilterName) or not IsExistFilter(AFilters, AStringFilterName) or
    (AEmbeddedFileFilterName <> '') and not IsExistFilter(AFilters, AEmbeddedFileFilterName) then
      TdxPDFUtils.RaiseTestException;
  if (FStreamFilterName = TdxPDFKeywords.StdCF) or (AStringFilterName = TdxPDFKeywords.StdCF) then
  begin
    ATemp := AFilters.GetDictionary(TdxPDFKeywords.StdCF);
    ReadCryptMethodVersion(ATemp);
    ReadKeyLength(ATemp);
  end;
end;

function TdxPDFCustomEncryptionAlgorithm.CalculateMD5Hash(const AKey: TBytes): TBytes;
var
  AProvider : TdxMD5HashAlgorithm;
begin
  AProvider := TdxMD5HashAlgorithm.Create;
  try
    Result := AProvider.Calculate(AKey);
  finally
    AProvider.Free;
  end;
end;

function TdxPDFCustomEncryptionAlgorithm.CalculateUserPasswordHash(const APassword: TBytes): TBytes;
var
  I: Integer;
  AKey, AData, AUserHash: TBytes;
begin
  AKey := TdxByteArray.Concatenate(APassword, FOwnerPasswordHash);
  TdxPDFUtils.AddByte(FEncryptionFlags and $FF, AKey);
  TdxPDFUtils.AddByte((FEncryptionFlags and $FF00) shr 8, AKey);
  TdxPDFUtils.AddByte((FEncryptionFlags and $FF0000) shr 16, AKey);
  TdxPDFUtils.AddByte((FEncryptionFlags and $FF000000) shr 24, AKey);
  TdxPDFUtils.AddData(FDocumentID[0], AKey);
  if (FSecurityHandlerRevision >= 4) and not FEncryptMetadata then
  begin
    TdxPDFUtils.AddByte($FF, AKey);
    TdxPDFUtils.AddByte($FF, AKey);
    TdxPDFUtils.AddByte($FF, AKey);
    TdxPDFUtils.AddByte($FF, AKey);
  end;

  UpdateEncryptionKey(AKey);

  if FSecurityHandlerRevision < 3 then
  begin
    SetLength(AData, FKeyLength);
    TdxPDFUtils.CopyData(FEncryptionKey, 0, AData, 0, FKeyLength);
    Result := RC4Crypt(AData, FPasswordPadding, Length(FPasswordPadding));
  end
  else
  begin
    TdxPDFUtils.AddData(FPasswordPadding, AData);
    TdxPDFUtils.AddData(FDocumentID[0], AData);
    AUserHash := CalculateMD5Hash(AData);
    for I := 0 to 19 do
    begin
      AData := TdxByteArray.Clone(AUserHash);
      AUserHash := RC4Crypt(XorKey(FEncryptionKey, I), AData, Length(AData));
    end;
    Result := PadOrTruncatePassword(AUserHash);
  end;
end;

function TdxPDFCustomEncryptionAlgorithm.CalculateOwnerEncryptionKey(const APassword: TBytes): TBytes;
var
  I: Integer;
begin
  Result := TdxByteArray.Resize(CalculateMD5Hash(APassword), FKeyLength);
  if FSecurityHandlerRevision >= 3 then
    for I := 0 to 50 - 1 do
      Result := CalculateMD5Hash(Result);
end;

function TdxPDFCustomEncryptionAlgorithm.DoCheckPassword(AOnGetPasswordEvent: TdxGetPasswordEvent): TdxPDFPasswordCheckingResult;
var
  APassword: string;
begin
  if dxCallGetPasswordEvent(AOnGetPasswordEvent, Self, APassword) then
    Result := ValidatePassword(APassword)
  else
    Result := pcrExit;
end;

function TdxPDFCustomEncryptionAlgorithm.GetActualKeyLength: Integer;
begin
  Result := Min(ExtendedKeyLength, 16);
end;

function TdxPDFCustomEncryptionAlgorithm.GetAllowContentExtraction: Boolean;
begin
  Result := not NeedCheckPermission;
  if not Result  then
  begin
    if FSecurityHandlerRevision = 2 then
      Result := BitState[4]
    else
      if FSecurityHandlerRevision >= 3 then
        Result := BitState[4] and BitState[9];
  end
end;

function TdxPDFCustomEncryptionAlgorithm.GetAllowPrinting: Boolean;
begin
  Result := not NeedCheckPermission;
  if not Result then
  begin
    if FSecurityHandlerRevision = 2 then
      Result := BitState[2]
    else
      if FSecurityHandlerRevision >= 3 then
        Result := BitState[11];
  end;
end;

function TdxPDFCustomEncryptionAlgorithm.GetBitState(AIndex: Integer): Boolean;
begin
  Result := (FPermissionFlags and (1 shl AIndex)) <> 0;
end;

function TdxPDFCustomEncryptionAlgorithm.GetExtendedKeyLength: Integer;
begin
  Result := FKeyLength + 5;
end;

function TdxPDFCustomEncryptionAlgorithm.IsExistFilter(ADictionary: TdxPDFDictionary; const AName: string): Boolean;
begin
  Result := (AName = TdxPDFKeywords.Identity) or (AName = TdxPDFKeywords.StdCF) and (ADictionary <> nil) and
    ADictionary.Contains(TdxPDFKeywords.StdCF);
end;

function TdxPDFCustomEncryptionAlgorithm.NeedCheckPermission: Boolean;
begin
  Result := not FIsOwnerKey;
end;

function TdxPDFCustomEncryptionAlgorithm.PadOrTruncatePassword(const APasswordString: TBytes): TBytes;
var
  AData, APassword: TBytes;
begin
  APassword := TdxByteArray.Concatenate(TdxByteArray.Clone(APasswordString), FPasswordPadding);
  SetLength(Result, 0);
  SetLength(AData, HashSize);
  TdxPDFUtils.CopyData(APassword, 0, AData, 0, HashSize);
  TdxPDFUtils.AddData(AData, Result);
end;

function TdxPDFCustomEncryptionAlgorithm.ValidateHash(const AHash: TBytes): TBytes;
begin
  Result := DoValidateHash(AHash);
end;

function TdxPDFCustomEncryptionAlgorithm.XorKey(const AKey: TBytes; AValue: Integer): TBytes;
var
  I: Integer;
begin
  Result := TdxByteArray.Resize(Result, FKeyLength);
  for I := Low(Result) to High(Result) do
    Result[I] := AKey[I] xor AValue;
end;

procedure TdxPDFCustomEncryptionAlgorithm.ReadCryptMethodVersion(ADictionary: TdxPDFDictionary);
var
  AName: string;
begin
  if ADictionary <> nil then
  begin
    AName := ADictionary.GetString(TdxPDFKeywords.CryptFilterMode);
    if AName = 'V2' then
      FCryptMethodVersion := cmvV2
    else
      if AName = 'AESV2' then
        FCryptMethodVersion := cmvAESV2
      else
        FCryptMethodVersion := cmvAESV3;
  end;
end;

procedure TdxPDFCustomEncryptionAlgorithm.ReadKeyLength(ADictionary: TdxPDFDictionary);
var
  AMaxKeyLength, AMaxKeyBits: Integer;
begin
  case FCryptMethodVersion of
    cmvAESV2:
      FKeyLength := 128;
    cmvAESV3:
      FKeyLength := 256;
  else
    if ADictionary <> nil then
    begin
      AMaxKeyLength := IfThen(FSecurityHandlerRevision >= 5, 32, 16);
      AMaxKeyBits := IfThen(FSecurityHandlerRevision >= 5, 256, 128);
      FKeyLength := ADictionary.GetInteger(TdxPDFKeywords.Length);
      if InRange(FKeyLength, 5, AMaxKeyLength) then
        FKeyLength := FKeyLength * 8
      else
        if not InRange(FKeyLength, 40, AMaxKeyBits) or (FKeyLength mod 8 <> 0) then
          TdxPDFUtils.Abort;
    end;
  end;
end;

procedure TdxPDFCustomEncryptionAlgorithm.UpdateEncryptionKey(const AKey: TBytes);
var
  I: Integer;
begin
  FEncryptionKey := CalculateMD5Hash(AKey);
  if FSecurityHandlerRevision >= 3 then
    for I := 0 to 50 - 1 do
      FEncryptionKey := CalculateMD5Hash(TdxByteArray.Resize(FEncryptionKey, FKeyLength));
  FEncryptionKey := TdxByteArray.Resize(FEncryptionKey, FKeyLength);
  FEncryptionKey := DoUpdateEncryptionKey;
end;

{ TdxPDFAESBasedEncryptionAlgorithm }

function TdxPDFAESBasedEncryptionAlgorithm.DoUpdateEncryptionKey: TBytes;
begin
  if CryptMethodVersion = cmvAESV2 then
  begin
    Result := TdxByteArray.Resize(EncryptionKey, ExtendedKeyLength + 4);
    Result[ExtendedKeyLength] := $73;
    Result[ExtendedKeyLength + 1] := $41;
    Result[ExtendedKeyLength + 2] := $6c;
    Result[ExtendedKeyLength + 3] := $54;
  end
  else
    Result := inherited DoUpdateEncryptionKey;
end;

function TdxPDFAESBasedEncryptionAlgorithm.DoValidateHash(const AHash: TBytes): TBytes;
const
  AESV3HashLength = HashSize + 2 * SaltSize;
  PasswordLengthLimit = 127;

  procedure CheckLength;
  begin
    if Length(AHash) <> AESV3HashLength then
      TdxPDFUtils.RaiseTestException;
  end;

begin
  Result := AHash;
  case SecurityHandlerRevision of
    6:
      begin
        if Length(AHash) <> PasswordLengthLimit then
          CheckLength;
        Result := TdxByteArray.Resize(AHash, AESV3HashLength);
      end;
    5:
      CheckLength;
  else
    Result := inherited DoValidateHash(AHash);
  end;
end;

function TdxPDFAESBasedEncryptionAlgorithm.AESDecrypt(const AKey, AData: TBytes): TBytes;
var
  AInitializationVector: TBytes;
begin
  if Length(AData) - InitVectorSize < 0 then
    SetLength(Result, 0)
  else
  begin
    SetLength(AInitializationVector, InitVectorSize);
    TdxPDFUtils.CopyData(AData, 0, AInitializationVector, 0, InitVectorSize);
    Result := DoAESDecrypt(AKey, AInitializationVector, AData, InitVectorSize, CRYPT_MODE_CBC, RANDOM_PADDING);
  end;
end;

function TdxPDFAESBasedEncryptionAlgorithm.AESDecrypt(const AKey, AData: TBytes; AChainingMode: Integer): TBytes;
var
  AInitializationVector: TBytes;
begin
  SetLength(AInitializationVector, InitVectorSize);
  Result := DoAESDecrypt(AKey, AInitializationVector, AData, 0, AChainingMode, PKCS5_PADDING);
end;

function TdxPDFAESBasedEncryptionAlgorithm.CreateAESCryptoKey(const AKey, AInitVector: TBytes): IdxCryptoKey;
var
  ACryptoProvider: IdxCryptoProvider;
  AAlgorithmID, AProviderID: Integer;
begin
  TdxCryptoAlgorithms.GetInfo('AES', Length(AKey) * 8, AAlgorithmID, AProviderID);
  ACryptoProvider := TdxCryptoProvider.Create(nil, AProviderID);
  Result := TdxCryptoKey.Create(ACryptoProvider, AAlgorithmID, AKey);
  Result.SetChainingMode(CRYPT_MODE_CBC);
  Result.SetIV(AInitVector);
end;

function TdxPDFAESBasedEncryptionAlgorithm.CalculateHash(const APasswordString, AData, AUserKey: TBytes): TBytes;

  function DoCalculateHash(AHashAlgorithm: Integer; const AData: TBytes): TBytes;
  var
    AAlgorithm: TdxHashAlgorithm;
  begin
    case AHashAlgorithm of
      1:
        AAlgorithm := TdxSHA384HashAlgorithm.Create;
      2:
        AAlgorithm := TdxSHA512HashAlgorithm.Create;
    else
      AAlgorithm := TdxSHA256HashAlgorithm.Create;
    end;
    try
      Result := AAlgorithm.Calculate(AData);
    finally
      AAlgorithm.Free;
    end;
  end;

var
  AProvider: TdxSHA256HashAlgorithm;
  AHash, AKey, AInitVector, AEncryptedKey: TBytes;
  ARound, I: Integer;
  AK1: TBytes;
  AValue: Int64;
begin
  AProvider := TdxSHA256HashAlgorithm.Create;
  try
    AHash := AProvider.Calculate(AData);
    if SecurityHandlerRevision = 5 then
      Exit(AHash);
    ARound := 0;
    while True do
    begin
      SetLength(AK1, 0);
      for I := 0 to 64 - 1 do
      begin
        TdxPDFUtils.AddData(APasswordString, AK1);
        TdxPDFUtils.AddData(AHash, AK1);
        TdxPDFUtils.AddData(AUserKey, AK1);
      end;
      SetLength(AKey, InitVectorSize);
      SetLength(AInitVector, InitVectorSize);
      TdxPDFUtils.CopyData(AHash, 0, AKey, 0, InitVectorSize);
      TdxPDFUtils.CopyData(AHash, InitVectorSize, AInitVector, 0, InitVectorSize);
      AEncryptedKey := DoAESEncrypt(AKey, AInitVector, AK1);
      AValue := 0;
      for I := 0 to 16 - 1 do
        Inc(AValue, AEncryptedKey[I]);
      AHash := DoCalculateHash(AValue mod 3, AEncryptedKey);
      if (ARound >= 63) and (AEncryptedKey[Length(AEncryptedKey) - 1] <= ARound - 32) then
        Exit(TdxByteArray.Resize(AHash, 32));
      Inc(ARound);
    end;
  finally
    AProvider.Free;
  end;
end;

function TdxPDFAESBasedEncryptionAlgorithm.DoAESDecrypt(const AKey, AInitVector, AData: TBytes;
  APosition, AChainingMode, APaddingMode: Integer): TBytes;
var
  ATemp: TBytes;
begin
  SetLength(ATemp, Length(AData) - APosition);
  TdxPDFUtils.CopyData(AData, APosition, ATemp, 0, Length(AData) - APosition);
  Result := TdxCipher.Decrypt(CreateAESCryptoKey(AKey, AInitVector), ATemp);
end;

function TdxPDFAESBasedEncryptionAlgorithm.DoAESEncrypt(const AKey, AInitVector, AData: TBytes): TBytes;
begin
  Result := TdxCipher.Encrypt(CreateAESCryptoKey(AKey, AInitVector), AData, False);
end;

{ TdxPDFMixedEncryptionAlgorithm }

function TdxPDFMixedEncryptionAlgorithm.Decrypt(const AData: TBytes; ANumber: Integer): TBytes;
var
  AKey: TBytes;
begin
  if StreamFilterName = TdxPDFKeywords.Identity then
    Result := AData
  else
  begin
    AKey := CalculateEncryptionKey(ANumber);
    if CryptMethodVersion = cmvAESV2 then
      Result := AESDecrypt(AKey, AData)
    else
      Result := RC4Crypt(AKey, AData, Length(AData));
  end;
end;

procedure TdxPDFMixedEncryptionAlgorithm.Read(ADictionary: TdxPDFDictionary);
begin
  inherited Read(ADictionary);
  if SecurityHandlerRevision <> 4 then
    TdxPDFUtils.RaiseTestException
  else
    InitializeCryptFilter(ADictionary);
end;

{ TdxPDFAESEncryptionAlgorithm }

function TdxPDFAESEncryptionAlgorithm.Decrypt(const AData: TBytes; ANumber: Integer): TBytes;
begin
  Result := AESDecrypt(EncryptionKey, AData);
end;

function TdxPDFAESEncryptionAlgorithm.CheckUserPassword(const AUserPassword: TBytes): Boolean;
var
  AData, APassword: TBytes;
begin
  TdxPDFUtils.AddData(AUserPassword, APassword);
  TdxPDFUtils.AddData(FUserValidationSalt, APassword);
  SetLength(AData, 0);
  if CheckPassword(FUserPasswordHash, CalculateHash(AUserPassword, APassword, AData)) then
  begin
    SetLength(APassword, 0);
    TdxPDFUtils.AddData(AUserPassword, APassword);
    TdxPDFUtils.AddData(FUserKeySalt, APassword);
    FEncryptionKey := AESDecrypt(CalculateHash(AUserPassword, APassword, AData), FEncodedUserPassword, CRYPT_MODE_CBC);
    Result := CheckPermissions;
  end
  else
    Result := False;
end;

function TdxPDFAESEncryptionAlgorithm.CheckOwnerPassword(const AUserPassword: TBytes): Boolean;
begin
  Result := False;
end;

function TdxPDFAESEncryptionAlgorithm.ConvertToBytes(const APassword: string): TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(APassword);
end;

function TdxPDFAESEncryptionAlgorithm.ValidatePassword(const APasswordString: string): TdxPDFPasswordCheckingResult;
var
  APassword, AOwnerPassword: TBytes;
begin
  Result := inherited ValidatePassword(APasswordString);
  if Result <> pcrSuccess then
  begin
    APassword := ConvertToBytes(APasswordString);
    TdxPDFUtils.AddData(APassword, AOwnerPassword);
    TdxPDFUtils.AddData(FOwnerValidationSalt, AOwnerPassword);
    TdxPDFUtils.AddData(FUserPasswordHash, AOwnerPassword);
    if CheckPassword(FOwnerPasswordHash, CalculateHash(APassword, AOwnerPassword, FUserPasswordHash)) then
    begin
      SetLength(AOwnerPassword, 0);
      TdxPDFUtils.AddData(APassword, AOwnerPassword);
      TdxPDFUtils.AddData(FOwnerKeySalt, AOwnerPassword);
      TdxPDFUtils.AddData(FUserPasswordHash, AOwnerPassword);
      FEncryptionKey := AESDecrypt(CalculateHash(APassword, AOwnerPassword, FUserPasswordHash), FEncodedOwnerPassword, CRYPT_MODE_CBC);
      if CheckPermissions then
        Result := pcrSuccess;
    end;
  end;
end;

procedure TdxPDFAESEncryptionAlgorithm.InitializePassword(var APassword: TBytes);
begin
  SetLength(APassword, 0);
end;

procedure TdxPDFAESEncryptionAlgorithm.Read(ADictionary: TdxPDFDictionary);
const
  KeySaltPosition = HashSize + SaltSize;
begin
  inherited Read(ADictionary);
  if (SecurityHandlerRevision <> 5) and (SecurityHandlerRevision <> 6) then
    TdxPDFUtils.RaiseTestException
  else
  begin
    InitializeCryptFilter(ADictionary);
    FOwnerValidationSalt := TdxByteArray.Resize(FOwnerValidationSalt, SaltSize);
    FOwnerKeySalt := TdxByteArray.Resize(FOwnerKeySalt, SaltSize);
    TdxPDFUtils.CopyData(FOwnerPasswordHash, HashSize, FOwnerValidationSalt, 0, SaltSize);
    TdxPDFUtils.CopyData(FOwnerPasswordHash, KeySaltPosition, FOwnerKeySalt, 0, SaltSize);
    FUserValidationSalt := TdxByteArray.Resize(FUserValidationSalt, SaltSize);
    FUserKeySalt := TdxByteArray.Resize(FUserKeySalt, SaltSize);
    TdxPDFUtils.CopyData(FUserPasswordHash, HashSize, FUserValidationSalt, 0, SaltSize);
    TdxPDFUtils.CopyData(FUserPasswordHash, KeySaltPosition, FUserKeySalt, 0, SaltSize);
  end;
end;

function TdxPDFAESEncryptionAlgorithm.CheckPermissions: Boolean;
var
  APermissions: TBytes;
begin
  APermissions := AESDecrypt(EncryptionKey, FEncryptedPermissions, CRYPT_MODE_ECB);
  Result := (APermissions[9] = Byte('a')) and (APermissions[10] = Byte('d')) and (APermissions[11] = Byte('b')) and
    (((APermissions[2] shl 16) + (APermissions[1] shl 8) + APermissions[0]) = (EncryptionFlags and $FFFFFF));
end;

{ TdxPDF40BitEncryptionAlgorithm }

procedure TdxPDF40BitEncryptionAlgorithm.Read(ADictionary: TdxPDFDictionary);
begin
  inherited Read(ADictionary);
  FKeyLength := 40;
end;

{ TdxPDFGreater40BitEncryptionAlgorithm }

procedure TdxPDFGreater40BitEncryptionAlgorithm.Read(ADictionary: TdxPDFDictionary);
begin
  inherited Read(ADictionary);
  if (FKeyLength < 40) or (FKeyLength > 128) or (FKeyLength mod 8 <> 0) or (SecurityHandlerRevision < 2) or
    (SecurityHandlerRevision > 3) then
    TdxPDFUtils.RaiseTestException;
end;

{ TdxPDFEncryptionInfo }

constructor TdxPDFEncryptionInfo.Create(ADictionary: TdxPDFDictionary; const ADocumentID: TdxPDFDocumentID);
var
  AClass: TdxPDFCustomEncryptionAlgorithmClass;
begin
  inherited Create;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
  if ADictionary <> nil then
  begin
    AClass := GetEncryptionAlgorithmClass(ADictionary.GetInteger(TdxPDFKeywords.Version));
    if AClass <> nil then
      FAlgorithm := AClass.Create(ADictionary, ADocumentID);
  end;
end;

destructor TdxPDFEncryptionInfo.Destroy;
begin
  if FAlgorithm <> nil then
    FreeAndNil(FAlgorithm);
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

function TdxPDFEncryptionInfo.CheckPassword(AAttemptsLimit: Integer; AOnGetPasswordEvent: TdxGetPasswordEvent): Boolean;
begin
  Result := FAlgorithm.CheckPassword(AAttemptsLimit, AOnGetPasswordEvent);
end;

function TdxPDFEncryptionInfo.Decrypt(const AData: TBytes; ANumber: Integer): TBytes;
begin
  EnterCriticalSection(FLock);
  try
    Result := FAlgorithm.Decrypt(AData, ANumber);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TdxPDFEncryptionInfo.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxPDFEncryptionInfo._AddRef: Integer;
begin
  Result := -1;
end;

function TdxPDFEncryptionInfo._Release: Integer;
begin
  Result := -1;
end;

function TdxPDFEncryptionInfo.GetAllowContentExtraction: Boolean;
begin
  Result := FAlgorithm.AllowContentExtraction;
end;

function TdxPDFEncryptionInfo.GetAllowPrinting: Boolean;
begin
  Result := FAlgorithm.AllowPrinting;
end;

function TdxPDFEncryptionInfo.GetEncryptionAlgorithmClass(AAlgorithm: Integer): TdxPDFCustomEncryptionAlgorithmClass;
begin
  case AAlgorithm of
    1:
      Result := TdxPDF40BitEncryptionAlgorithm;
    2:
      Result := TdxPDFGreater40BitEncryptionAlgorithm;
    4:
      Result := TdxPDFMixedEncryptionAlgorithm;
    5:
      Result := TdxPDFAESEncryptionAlgorithm;
  else
    Result := nil;
  end;
end;

end.
