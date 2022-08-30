{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL           }
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

unit dxCrypto;

{$I cxVer.inc}

interface

uses
  Windows, SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCryptoAPI;

type
  TdxRC4Key = record
    State: array[0..255] of Byte;
    X, Y: Integer;
  end;

  { IdxCryptoProvider }

  IdxCryptoProvider = interface
  ['{BC0E1F98-91B7-4A1B-8941-3B8828153CFA}']
    function GetHandle: HCRYPTPROV;
    //
    property Handle: HCRYPTPROV read GetHandle;
  end;

  { IdxCryptoKey }

  IdxCryptoKey = interface
  ['{0A731ECE-8CA2-429B-92AF-E61D49CB8AFA}']
    function GetBlockSize: Integer;
    function GetChainingMode: Integer;
    function GetHandle: HCRYPTKEY;
    procedure SetChainingMode(AValue: Integer);

    function GetIV: TBytes;
    function GetProvider: IdxCryptoProvider;
    procedure SetIV(AValue: TBytes);

    property BlockSize: Integer read GetBlockSize;
    property ChainingMode: Integer read GetChainingMode write SetChainingMode;
    property Handle: HCRYPTKEY read GetHandle;
  end;

  { TdxCryptoProvider }

  TdxCryptoProvider = class(TInterfacedObject, IdxCryptoProvider)
  strict private
    FHandle: HCRYPTPROV;
  public
    constructor Create(const ACSPName: PWideChar; AProviderType: Integer);
    destructor Destroy; override;
    // IdxCryptoProvider
    function GetHandle: HCRYPTPROV;
  end;

  { TdxCryptoKey }

  TdxCryptoKey = class(TInterfacedObject, IdxCryptoKey)
  strict private
    FHandle: HCRYPTKEY;
    FProvider: IdxCryptoProvider;
  public
    constructor Create(AProvider: IdxCryptoProvider; AAlgorithm: Integer; const AKey: TBytes); overload;
    constructor Create(AProvider: IdxCryptoProvider; AAlgorithm: Integer; const APasswordHash: HCRYPTHASH); overload;
    destructor Destroy; override;
    // IdxCryptoKey
    function GetBlockSize: Integer;
    function GetChainingMode: Integer;
    function GetHandle: HCRYPTKEY;
    function GetIV: TBytes;
    function GetProvider: IdxCryptoProvider;
    procedure SetChainingMode(AValue: Integer);
    procedure SetIV(AValue: TBytes);
  end;

  { TdxCipher }

  TdxCipher = class
  protected
    class procedure Transform(AKey: IdxCryptoKey; ACrypt: Boolean;
      const ASource: TBytes; out ATarget: TBytes; AIsFinalBlock: Boolean); overload;
    class procedure Transform(AKey: IdxCryptoKey; ACrypt: Boolean;
      const ASourceStream, ATargetStream: TStream; AIsFinalBlock: Boolean); overload;
  public
    class function Decrypt(AKey: IdxCryptoKey; const ASource: TBytes; AIsFinalBlock: Boolean = True): TBytes; overload;
    class procedure Decrypt(AKey: IdxCryptoKey; const ASourceStream, ATargetStream: TStream; AIsFinalBlock: Boolean = True); overload;
    class function Encrypt(AKey: IdxCryptoKey; const ASource: TBytes; AIsFinalBlock: Boolean = True): TBytes; overload;
    class procedure Encrypt(AKey: IdxCryptoKey; const ASourceStream, ATargetStream: TStream; AIsFinalBlock: Boolean = True); overload;
  end;

  { TdxCryptoAlgorithms }

  TdxCryptoAlgorithms = class
  strict private type
  {$REGION 'Private types'}
    TMapItem = record
      Name: string;
      Provider: Integer;
      Algorithm: Integer;
      KeySize: Integer;
    end;
  {$ENDREGION}
  strict private const
  {$REGION 'Private consts'}
    FMap: array [0..7] of TMapItem = (
      (Name: 'RC4'; Provider: PROV_RSA_FULL; Algorithm: CALG_RC4;     KeySize: 64),
      (Name: 'AES'; Provider: PROV_RSA_AES;  Algorithm: CALG_AES_128; KeySize: 128),
      (Name: 'AES'; Provider: PROV_RSA_AES;  Algorithm: CALG_AES_192; KeySize: 192),
      (Name: 'AES'; Provider: PROV_RSA_AES;  Algorithm: CALG_AES_256; KeySize: 256),

      (Name: 'SHA1';   Provider: PROV_RSA_AES; Algorithm: CALG_SHA1;    KeySize: 160),
      (Name: 'SHA256'; Provider: PROV_RSA_AES; Algorithm: CALG_SHA_256; KeySize: 256),
      (Name: 'SHA384'; Provider: PROV_RSA_AES; Algorithm: CALG_SHA_384; KeySize: 384),
      (Name: 'SHA512'; Provider: PROV_RSA_AES; Algorithm: CALG_SHA_512; KeySize: 512)
    );
  {$ENDREGION}
  public
    class function GetInfo(const AName: string; AKeySize: Integer; out AAlgorithmID, AProviderID: Integer): Boolean;
  end;

  { TdxPBKDF2 }

  TdxPBKDF2 = class
  public
    class function DeriveKey(const APassword, ASalt: TBytes; AIterationCount: Integer; AKeyLength: Integer): TBytes;
  end;

function dxGenerateSalt(ASize: Integer): TBytes;

procedure dxRC4Crypt(var AKey: TdxRC4Key; AInData, AOutData: PByteArray; ADataSize: Integer);
procedure dxRC4Initialize(out AKey: TdxRC4Key; APassword: PByteArray; APasswordLength: Integer); overload;
procedure dxRC4Initialize(out AKey: TdxRC4Key; APassword: TBytes); overload;
implementation

uses
  dxCore, Math, dxHash;

function dxGenerateSalt(ASize: Integer): TBytes;
var
  I: Integer;
begin
  SetLength(Result, ASize);
  for I := 0 to ASize - 1 do
    Result[I] := Random(MaxByte) + 1;
end;

procedure dxRC4Initialize(out AKey: TdxRC4Key; APassword: TBytes);
begin
  dxRC4Initialize(AKey, @APassword[0], Length(APassword));
end;

procedure dxRC4Initialize(out AKey: TdxRC4Key; APassword: PByteArray; APasswordLength: Integer);
var
  ATempKey: TdxRC4Key;
  I, J, K: Integer;
begin
  AKey.X := 0;
  AKey.Y := 0;
  for I := 0 to 255 do
  begin
    ATempKey.State[I] := APassword^[I mod APasswordLength];
    AKey.State[I] := I;
  end;

  J := 0;
  for I := 0 to 255 do
  begin
    J := (J + AKey.State[I] + ATempKey.State[I]) and $FF;
    K := AKey.State[I];
    AKey.State[I] := AKey.State[J];
    AKey.State[j] := K;
  end;
end;

procedure dxRC4Crypt(var AKey: TdxRC4Key; AInData, AOutData: PByteArray; ADataSize: Integer);
var
  T, K, I, J: Integer;
begin
  I := AKey.X;
  J := AKey.Y;
  for K := 0 to ADataSize - 1 do
  begin
    I := Byte(I + 1);
    J := Byte(J + AKey.State[I]);

    T := AKey.State[I];
    AKey.State[I] := AKey.State[J];
    AKey.State[J] := T;

    T := Byte(AKey.State[I] + AKey.State[J]);
    AOutData^[K] := AInData^[K] xor AKey.State[T];
  end;
  AKey.X := I;
  AKey.Y := J;
end;

{ TdxCryptoProvider }

constructor TdxCryptoProvider.Create(const ACSPName: PWideChar; AProviderType: Integer);
begin
  CryptCheck(CryptAcquireContext(FHandle, nil, ACSPName, AProviderType, CRYPT_VERIFYCONTEXT));
end;

destructor TdxCryptoProvider.Destroy;
begin
  CryptCheck(CryptReleaseContext(FHandle, 0));
  inherited Destroy;
end;

function TdxCryptoProvider.GetHandle: HCRYPTPROV;
begin
  Result := FHandle;
end;

{ TdxCryptoKey }

constructor TdxCryptoKey.Create(AProvider: IdxCryptoProvider; AAlgorithm: Integer; const APasswordHash: HCRYPTHASH);
begin
  FProvider := AProvider;
  CryptCheck(CryptDeriveKey(FProvider.Handle, AAlgorithm, APasswordHash, 0, FHandle));
end;

constructor TdxCryptoKey.Create(AProvider: IdxCryptoProvider; AAlgorithm: Integer; const AKey: TBytes);
var
  AKeyBlob: TBytes;
  AKeyBlobHeader: TdxPublicKeyStructure;
begin
  FProvider := AProvider;

  SetLength(AKeyBlob, SizeOf(AKeyBlobHeader) + Length(AKey) + SizeOf(Integer));
  AKeyBlobHeader.bType := PLAINTEXTKEYBLOB;
  AKeyBlobHeader.bVersion := CUR_BLOB_VERSION;
  AKeyBlobHeader.reserved := 0;
  AKeyBlobHeader.aiKeyAlg := AAlgorithm;
  Move(AKeyBlobHeader, AKeyBlob[0], SizeOf(AKeyBlobHeader));
  PInteger(@AKeyBlob[SizeOf(AKeyBlobHeader)])^ := Length(AKey);
  Move(AKey[0], AKeyBlob[SizeOf(AKeyBlobHeader) + SizeOf(Integer)], Length(AKey));

  CryptCheck(CryptImportKey(FProvider.Handle, @AKeyBlob[0], Length(AKeyBlob), 0, 0, FHandle));
end;

destructor TdxCryptoKey.Destroy;
begin
  CryptCheck(CryptDestroyKey(FHandle));
  inherited Destroy;
end;

function TdxCryptoKey.GetBlockSize: Integer;
var
  ABufferCapacity, ABufferSize: Cardinal;
begin
  ABufferCapacity := SizeOf(ABufferSize);
  if CryptGetKeyParam(FHandle, KP_BLOCKLEN, @ABufferSize, ABufferCapacity, 0) then
    Result := ABufferSize div 8
  else
    Result := 0;
end;

function TdxCryptoKey.GetChainingMode: Integer;
var
  ALength: Cardinal;
begin
  ALength := SizeOf(Result);
  CryptCheck(CryptGetKeyParam(FHandle, KP_MODE, @Result, ALength, 0));
end;

function TdxCryptoKey.GetHandle: HCRYPTKEY;
begin
  Result := FHandle;
end;

function TdxCryptoKey.GetIV: TBytes;
var
  ASize: Cardinal;
begin
  ASize := 0;
  CryptCheck(CryptGetKeyParam(FHandle, KP_IV, nil, ASize, 0));
  SetLength(Result, ASize);
  CryptCheck(CryptGetKeyParam(FHandle, KP_IV, @Result[0], ASize, 0));
end;

function TdxCryptoKey.GetProvider: IdxCryptoProvider;
begin
  Result := FProvider;
end;

procedure TdxCryptoKey.SetChainingMode(AValue: Integer);
begin
  CryptCheck(CryptSetKeyParam(FHandle, KP_MODE, @AValue, 0));
end;

procedure TdxCryptoKey.SetIV(AValue: TBytes);
begin
  CryptCheck(CryptSetKeyParam(FHandle, KP_IV, @AValue[0], 0));
end;

{ TdxCipher }

class function TdxCipher.Decrypt(AKey: IdxCryptoKey; const ASource: TBytes; AIsFinalBlock: Boolean = True): TBytes;
begin
  Transform(AKey, False, ASource, Result, AIsFinalBlock);
end;

class procedure TdxCipher.Decrypt(AKey: IdxCryptoKey; const ASourceStream, ATargetStream: TStream; AIsFinalBlock: Boolean = True);
begin
  Transform(AKey, False, ASourceStream, ATargetStream, AIsFinalBlock);
end;

class function TdxCipher.Encrypt(AKey: IdxCryptoKey; const ASource: TBytes; AIsFinalBlock: Boolean = True): TBytes;
begin
  Transform(AKey, True, ASource, Result, AIsFinalBlock);
end;

class procedure TdxCipher.Encrypt(AKey: IdxCryptoKey; const ASourceStream, ATargetStream: TStream; AIsFinalBlock: Boolean = True);
begin
  Transform(AKey, True, ASourceStream, ATargetStream, AIsFinalBlock);
end;

class procedure TdxCipher.Transform(AKey: IdxCryptoKey;
  ACrypt: Boolean; const ASource: TBytes; out ATarget: TBytes; AIsFinalBlock: Boolean);
var
  ASourceStream: TStream;
  ATargetStream: TStream;
begin
  ATargetStream := TMemoryStream.Create;
  try
    ASourceStream := TBytesStream.Create(ASource);
    try
      Transform(AKey, ACrypt, ASourceStream, ATargetStream, AIsFinalBlock);
      SetLength(ATarget, ATargetStream.Size);
      ATargetStream.Position := 0;
      ATargetStream.ReadBuffer(ATarget[0], Length(ATarget));
    finally
      ASourceStream.Free;
    end;
  finally
    ATargetStream.Free;
  end;
end;

class procedure TdxCipher.Transform(AKey: IdxCryptoKey;
  ACrypt: Boolean; const ASourceStream, ATargetStream: TStream; AIsFinalBlock: Boolean);
const
  DefaultBufferSize = 512;
var
  ABuffer: PByte;
  ABufferSize: Cardinal;
  ABufferUsage: Cardinal;
  AChunkSize: Cardinal;
  AIsFinalChunk: Boolean;
begin
  AChunkSize := AKey.BlockSize;
  if AChunkSize = 0 then
    AChunkSize := DefaultBufferSize;
  ABufferSize := IfThen(ACrypt, DefaultBufferSize, AChunkSize);

  ABuffer := AllocMem(ABufferSize);
  try
    repeat
      ABufferUsage := ASourceStream.Read(ABuffer^, AChunkSize);
      AIsFinalChunk := ASourceStream.Position = ASourceStream.Size;

      if ACrypt then
        CryptCheck(CryptEncrypt(AKey.Handle, 0, AIsFinalChunk and AIsFinalBlock, 0, ABuffer, ABufferUsage, ABufferSize))
      else
        CryptCheck(CryptDecrypt(AKey.Handle, 0, AIsFinalChunk and AIsFinalBlock, 0, ABuffer, ABufferUsage) or AIsFinalChunk);

      ATargetStream.Write(ABuffer^, ABufferUsage);
    until AIsFinalChunk;
  finally
    FreeMem(ABuffer, ABufferSize);
  end;
end;

{ TdxCryptoAlgorithms }

class function TdxCryptoAlgorithms.GetInfo(const AName: string; AKeySize: Integer; out AAlgorithmID, AProviderID: Integer): Boolean;
var
  I: Integer;
begin
  for I := Low(FMap) to High(FMap) do
  begin
    Result := (FMap[I].Name = AName) and (FMap[I].KeySize = AKeySize);
    if Result then
    begin
      AAlgorithmID := FMap[I].Algorithm;
      AProviderID := FMap[I].Provider;
      Break;
    end;
  end;
end;

{ TdxPBKDF2 }

class function TdxPBKDF2.DeriveKey(const APassword, ASalt: TBytes; AIterationCount, AKeyLength: Integer): TBytes;

  procedure XORBytes(var ADest, ASource: TBytes);
  var
    I: Integer;
  begin
    for I := 0 to Length(ADest) - 1 do
      ADest[I] := ADest[I] xor ASource[I];
  end;

var
  AHash: TdxHashAlgorithm;
  AHashSize: Integer;
  AHashValue: TBytes;
  AOffset: Integer;
  ATransform: TBytes;
  I, L, R, C: Integer;
begin
  AHash := TdxHMACHashAlgorithm.Create(APassword, CALG_SHA1);
  try
    AHashSize := AHash.HashSize;
    L := Ceil(AKeyLength / AHashSize);
    R := AKeyLength - (L - 1) * AHashSize;

    AOffset := 0;
    SetLength(Result, L * AHashSize);
    SetLength(ATransform, AHashSize);
    for I := 1 to L do
    begin
      ZeroMemory(@ATransform[0], AHashSize);
      AHashValue := TdxByteArray.Concatenate(ASalt, I);
      for C := 0 to AIterationCount - 1 do
      begin
        AHash.Reset;
        AHash.Add(AHashValue);
        AHashValue := AHash.GetHash;
        XORBytes(ATransform, AHashValue);
      end;
      Move(ATransform[0], Result[AOffset], AHashSize);
      Inc(AOffset, AHashSize);
    end;
    if R < AHashSize then
      Result := TdxByteArray.Resize(Result, AKeyLength);
  finally
    AHash.Free;
  end;
end;
initialization
  Randomize;
end.
