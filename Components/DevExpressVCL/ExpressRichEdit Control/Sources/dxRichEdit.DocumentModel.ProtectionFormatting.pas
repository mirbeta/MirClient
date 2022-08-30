{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.DocumentModel.ProtectionFormatting;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCore,
  dxGenerics,
  dxCoreClasses,
  dxHash,
  dxProtectionUtils,
  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core;

type
  TdxDocumentProtectionType = (
      None,
      ReadOnly
  );

  { TdxDocumentProtectionInfo }

  TdxDocumentProtectionInfo = class (TdxCloneable)
  strict private
    FEnforceProtection: Boolean;
    FProtectionType: TdxDocumentProtectionType;
    FHashAlgorithmType: TdxHashAlgorithmType;
    FHashIterationCount: Integer;
    FPasswordHash: TArray<Byte>;
    FPasswordPrefix: TArray<Byte>;
    FWord2003PasswordHash: TArray<Byte>;
    FOpenOfficePasswordHash: TArray<Byte>;
  public
    function Clone: TdxDocumentProtectionInfo; reintroduce; inline;
    function Equals(AObj: TObject): Boolean; override;
    procedure CopyFrom(Source: TdxCloneable); override;

    property EnforceProtection: Boolean read FEnforceProtection write FEnforceProtection;
    property ProtectionType: TdxDocumentProtectionType read FProtectionType write FProtectionType;
    property HashAlgorithmType: TdxHashAlgorithmType read FHashAlgorithmType write FHashAlgorithmType;
    property HashIterationCount: Integer read FHashIterationCount write FHashIterationCount;
    property PasswordHash: TArray<Byte> read FPasswordHash write FPasswordHash;
    property PasswordPrefix: TArray<Byte> read FPasswordPrefix write FPasswordPrefix;
    property Word2003PasswordHash: TArray<Byte> read FWord2003PasswordHash write FWord2003PasswordHash;
    property OpenOfficePasswordHash: TArray<Byte> read FOpenOfficePasswordHash write FOpenOfficePasswordHash;
  end;

  { TdxDocumentProtectionInfoCache }

  TdxDocumentProtectionInfoCache = class(TdxUniqueItemsCache<TdxDocumentProtectionInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxDocumentProtectionInfo; override;
  end;

  { TdxRangePermissionInfo }

  TdxRangePermissionInfo = class(TdxCloneable)
  strict private
    FUserName: string;
    FGroup: string;
  public
    function Clone: TdxRangePermissionInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Equals(AObj: TObject): Boolean; override;

    property UserName: string read FUserName write FUserName;
    property Group: string read FGroup write FGroup;
  end;
  TdxRangePermissionInfoList = class(TdxList<TdxRangePermissionInfo>);

  { TdxRangePermissionProperties }

  TdxRangePermissionProperties = class(TdxRichEditIndexBasedObject<TdxRangePermissionInfo>)
  strict private
    function GetUserName: string;
    procedure SetUserName(const AValue: string);
    function GetGroup: string;
    procedure SetGroup(const AValue: string);
  strict protected
    function SetUserNameCore(const AInfo: TdxRangePermissionInfo; const AValue: string): TdxDocumentModelChangeActions; virtual;
    function SetGroupCore(const AInfo: TdxRangePermissionInfo; const AValue: string): TdxDocumentModelChangeActions; virtual;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxRangePermissionInfo>; override;
  public
    property UserName: string read GetUserName write SetUserName;
    property Group: string read GetGroup write SetGroup;
  end;

  { TdxRangePermissionInfoCache }

  TdxRangePermissionInfoCache = class(TdxUniqueItemsCache<TdxRangePermissionInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxRangePermissionInfo; override;
  end;

  { TdxDocumentProtectionProperties }

  TdxDocumentProtectionProperties = class(TdxRichEditIndexBasedObject<TdxDocumentProtectionInfo>)
  strict private
    function GetEnforceProtection: Boolean;
    procedure SetEnforceProtection(const AValue: Boolean);
    function GetProtectionType: TdxDocumentProtectionType;
    procedure SetProtectionType(const AValue: TdxDocumentProtectionType);
    function GetHashAlgorithmType: TdxHashAlgorithmType;
    procedure SetHashAlgorithmType(const AValue: TdxHashAlgorithmType);
    function GetHashIterationCount: Integer;
    procedure SetHashIterationCount(const AValue: Integer);
    function GetPasswordHash: TArray<Byte>;
    procedure SetPasswordHash(const AValue: TArray<Byte>);
    function GetPasswordPrefix: TArray<Byte>;
    procedure SetPasswordPrefix(const AValue: TArray<Byte>);
    function GetWord2003PasswordHash: TArray<Byte>;
    procedure SetWord2003PasswordHash(const AValue: TArray<Byte>);
    function GetOpenOfficePasswordHash: TArray<Byte>;
    procedure SetOpenOfficePasswordHash(const AValue: TArray<Byte>);
  strict protected
    function SetEnforceProtectionCore(const AInfo: TdxDocumentProtectionInfo; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetProtectionTypeCore(const AInfo: TdxDocumentProtectionInfo; const AValue: TdxDocumentProtectionType): TdxDocumentModelChangeActions; virtual;
    function SetHashAlgorithmTypeCore(const AInfo: TdxDocumentProtectionInfo; const AValue: TdxHashAlgorithmType): TdxDocumentModelChangeActions; virtual;
    function SetHashIterationCountCore(const AInfo: TdxDocumentProtectionInfo; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetPasswordHashCore(const AInfo: TdxDocumentProtectionInfo; const AValue: TArray<Byte>): TdxDocumentModelChangeActions; virtual;
    function SetPasswordPrefixCore(const AInfo: TdxDocumentProtectionInfo; const AValue: TArray<Byte>): TdxDocumentModelChangeActions; virtual;
    function SetWord2003PasswordHashCore(const AInfo: TdxDocumentProtectionInfo; const AValue: TArray<Byte>): TdxDocumentModelChangeActions; virtual;
    function SetOpenOfficePasswordHashCore(const AInfo: TdxDocumentProtectionInfo; const AValue: TArray<Byte>): TdxDocumentModelChangeActions; virtual;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxDocumentProtectionInfo>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce;

    property EnforceProtection: Boolean read GetEnforceProtection write SetEnforceProtection;
    property ProtectionType: TdxDocumentProtectionType read GetProtectionType write SetProtectionType;
    property HashAlgorithmType: TdxHashAlgorithmType read GetHashAlgorithmType write SetHashAlgorithmType;
    property HashIterationCount: Integer read GetHashIterationCount write SetHashIterationCount;
    property PasswordHash: TArray<Byte> read GetPasswordHash write SetPasswordHash;
    property PasswordPrefix: TArray<Byte> read GetPasswordPrefix write SetPasswordPrefix;
    property Word2003PasswordHash: TArray<Byte> read GetWord2003PasswordHash write SetWord2003PasswordHash;
    property OpenOfficePasswordHash: TArray<Byte> read GetOpenOfficePasswordHash write SetOpenOfficePasswordHash;
  end;

function dxGetHashAlgorithm(AHashAlgorithmType: TdxHashAlgorithmType): TdxHashAlgorithmClass;

implementation

uses
  Math,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.DocumentModel.Cache;

function dxGetHashAlgorithm(AHashAlgorithmType: TdxHashAlgorithmType): TdxHashAlgorithmClass;
begin
  case AHashAlgorithmType of
    TdxHashAlgorithmType.Sha1:
      Result := TdxSHA1HashAlgorithm;
    TdxHashAlgorithmType.Sha256:
      Result := TdxSHA256HashAlgorithm;
    TdxHashAlgorithmType.Sha384:
      Result := TdxSHA384HashAlgorithm;
    TdxHashAlgorithmType.Sha512:
      Result := TdxSHA512HashAlgorithm;
    TdxHashAlgorithmType.Md2:
      Result := TdxMD2HashAlgorithm;
    TdxHashAlgorithmType.Md4:
      Result := TdxMD4HashAlgorithm;
    TdxHashAlgorithmType.Md5:
      Result := TdxMD5HashAlgorithm;
    else
      Result := nil;
  end;
end;

{ TdxDocumentProtectionInfo }

function TdxDocumentProtectionInfo.Equals(AObj: TObject): Boolean;
var
  AInfo: TdxDocumentProtectionInfo;
begin
  AInfo := TdxDocumentProtectionInfo(AObj);
  Result :=
    (EnforceProtection = AInfo.EnforceProtection) and
    (ProtectionType = AInfo.ProtectionType) and
    (HashAlgorithmType = AInfo.HashAlgorithmType) and
    (HashIterationCount = AInfo.HashIterationCount) and
    TdxByteArray.Compare(PasswordHash, AInfo.PasswordHash) and
    TdxByteArray.Compare(PasswordPrefix, AInfo.PasswordPrefix) and
    TdxByteArray.Compare(Word2003PasswordHash, AInfo.Word2003PasswordHash) and
    TdxByteArray.Compare(OpenOfficePasswordHash, AInfo.OpenOfficePasswordHash);
end;

function TdxDocumentProtectionInfo.Clone: TdxDocumentProtectionInfo;
begin
  Result := TdxDocumentProtectionInfo(inherited Clone);
end;

procedure TdxDocumentProtectionInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxDocumentProtectionInfo absolute Source;
begin
  EnforceProtection := AInfo.EnforceProtection;
  ProtectionType := AInfo.ProtectionType;
  HashAlgorithmType := AInfo.HashAlgorithmType;
  HashIterationCount := AInfo.HashIterationCount;
  PasswordHash := AInfo.PasswordHash;
  PasswordPrefix := AInfo.PasswordPrefix;
  Word2003PasswordHash := AInfo.Word2003PasswordHash;
  OpenOfficePasswordHash := AInfo.OpenOfficePasswordHash;
end;

{ TdxDocumentProtectionInfoCache }

function TdxDocumentProtectionInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxDocumentProtectionInfo;
begin
  Result := TdxDocumentProtectionInfo.Create;
end;

{ TdxRangePermissionInfo }

function TdxRangePermissionInfo.Clone: TdxRangePermissionInfo;
begin
  Result := TdxRangePermissionInfo(inherited Clone);
end;

procedure TdxRangePermissionInfo.CopyFrom(Source: TdxCloneable);
var
  AValue: TdxRangePermissionInfo absolute Source;
begin
  UserName := AValue.UserName;
  Group := AValue.Group;
end;

function TdxRangePermissionInfo.Equals(AObj: TObject): Boolean;
var
  AInfo: TdxRangePermissionInfo absolute AObj;
begin
  Result := (AInfo.UserName = UserName) and (AInfo.Group = Group);
end;

{ TdxRangePermissionProperties }

function TdxRangePermissionProperties.GetUserName: string;
begin
  Result := Info.UserName;
end;

procedure TdxRangePermissionProperties.SetUserName(const AValue: string);
begin
  if UserName = AValue then
    Exit;
  SetPropertyValue<string>(SetUserNameCore, AValue);
end;

function TdxRangePermissionProperties.SetUserNameCore(const AInfo: TdxRangePermissionInfo; const AValue: string): TdxDocumentModelChangeActions;
begin
  AInfo.UserName := AValue;
  Result := GetBatchUpdateChangeActions;
end;

function TdxRangePermissionProperties.GetGroup: string;
begin
  Result := Info.Group;
end;

procedure TdxRangePermissionProperties.SetGroup(const AValue: string);
begin
  if Group = AValue then
    Exit;
  SetPropertyValue<string>(SetGroupCore, AValue);
end;

function TdxRangePermissionProperties.SetGroupCore(const AInfo: TdxRangePermissionInfo; const AValue: string): TdxDocumentModelChangeActions;
begin
  AInfo.Group := AValue;
  Result := GetBatchUpdateChangeActions;
end;

function TdxRangePermissionProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.RaiseModifiedChanged,
    TdxDocumentModelChangeAction.ResetSecondaryLayout];
end;

function TdxRangePermissionProperties.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxRangePermissionInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).RangePermissionInfoCache;
end;

{ TdxRangePermissionInfoCache }

function TdxRangePermissionInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxRangePermissionInfo;
begin
  Result := TdxRangePermissionInfo.Create;
end;

{ TdxDocumentProtectionProperties }

constructor TdxDocumentProtectionProperties.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  if ADocumentModel = nil then
    raise EArgumentNilException.Create('ADocumentModel');
  inherited Create(ADocumentModel.MainPart);
end;

function TdxDocumentProtectionProperties.GetEnforceProtection: Boolean;
begin
  Result := Info.EnforceProtection;
end;

procedure TdxDocumentProtectionProperties.SetEnforceProtection(const AValue: Boolean);
begin
  if EnforceProtection = AValue then
    Exit;
  SetPropertyValue<Boolean>(SetEnforceProtectionCore, AValue);
end;

function TdxDocumentProtectionProperties.SetEnforceProtectionCore(const AInfo: TdxDocumentProtectionInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.EnforceProtection := AValue;
  Result := GetBatchUpdateChangeActions;
end;

function TdxDocumentProtectionProperties.GetProtectionType: TdxDocumentProtectionType;
begin
  Result := Info.ProtectionType;
end;

procedure TdxDocumentProtectionProperties.SetProtectionType(const AValue: TdxDocumentProtectionType);
begin
  if ProtectionType = AValue then
    Exit;
  SetPropertyValue<TdxDocumentProtectionType>(SetProtectionTypeCore, AValue);
end;

function TdxDocumentProtectionProperties.SetProtectionTypeCore(const AInfo: TdxDocumentProtectionInfo;
  const AValue: TdxDocumentProtectionType): TdxDocumentModelChangeActions;
begin
  AInfo.ProtectionType := AValue;
  Result := GetBatchUpdateChangeActions;
end;

function TdxDocumentProtectionProperties.GetHashAlgorithmType: TdxHashAlgorithmType;
begin
  Result := Info.HashAlgorithmType;
end;

procedure TdxDocumentProtectionProperties.SetHashAlgorithmType(const AValue: TdxHashAlgorithmType);
begin
  if HashAlgorithmType = AValue then
    Exit;
  SetPropertyValue<TdxHashAlgorithmType>(SetHashAlgorithmTypeCore, AValue);
end;

function TdxDocumentProtectionProperties.SetHashAlgorithmTypeCore(const AInfo: TdxDocumentProtectionInfo;
  const AValue: TdxHashAlgorithmType): TdxDocumentModelChangeActions;
begin
  AInfo.HashAlgorithmType := AValue;
  Result := GetBatchUpdateChangeActions;
end;

function TdxDocumentProtectionProperties.GetHashIterationCount: Integer;
begin
  Result := Info.HashIterationCount;
end;

procedure TdxDocumentProtectionProperties.SetHashIterationCount(const AValue: Integer);
begin
  if HashIterationCount = AValue then
    Exit;
  SetPropertyValue<Integer>(SetHashIterationCountCore, AValue);
end;

function TdxDocumentProtectionProperties.SetHashIterationCountCore(const AInfo: TdxDocumentProtectionInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.HashIterationCount := AValue;
  Result := GetBatchUpdateChangeActions;
end;

function TdxDocumentProtectionProperties.GetPasswordHash: TArray<Byte>;
begin
  Result := Info.PasswordHash;
end;

procedure TdxDocumentProtectionProperties.SetPasswordHash(const AValue: TArray<Byte>);
begin
  if TdxByteArray.Compare(PasswordHash, AValue) then
    Exit;
  SetPropertyValue<TArray<Byte>>(SetPasswordHashCore, AValue);
end;

function TdxDocumentProtectionProperties.SetPasswordHashCore(const AInfo: TdxDocumentProtectionInfo;
  const AValue: TArray<Byte>): TdxDocumentModelChangeActions;
begin
  AInfo.PasswordHash := AValue;
  Result := GetBatchUpdateChangeActions;
end;

function TdxDocumentProtectionProperties.GetPasswordPrefix: TArray<Byte>;
begin
  Result := Info.PasswordPrefix;
end;

procedure TdxDocumentProtectionProperties.SetPasswordPrefix(const AValue: TArray<Byte>);
begin
  if TdxByteArray.Compare(PasswordPrefix, AValue) then
    Exit;
  SetPropertyValue<TArray<Byte>>(SetPasswordPrefixCore, AValue);
end;

function TdxDocumentProtectionProperties.SetPasswordPrefixCore(const AInfo: TdxDocumentProtectionInfo;
  const AValue: TArray<Byte>): TdxDocumentModelChangeActions;
begin
  AInfo.PasswordPrefix := AValue;
  Result := GetBatchUpdateChangeActions;
end;

function TdxDocumentProtectionProperties.GetWord2003PasswordHash: TArray<Byte>;
begin
  Result := Info.Word2003PasswordHash;
end;

procedure TdxDocumentProtectionProperties.SetWord2003PasswordHash(const AValue: TArray<Byte>);
begin
  if TdxByteArray.Compare(Word2003PasswordHash, AValue) then
    Exit;
  SetPropertyValue<TArray<Byte>>(SetWord2003PasswordHashCore, AValue);
end;

function TdxDocumentProtectionProperties.SetWord2003PasswordHashCore(const AInfo: TdxDocumentProtectionInfo;
  const AValue: TArray<Byte>): TdxDocumentModelChangeActions;
begin
  AInfo.Word2003PasswordHash := AValue;
  Result := GetBatchUpdateChangeActions;
end;

function TdxDocumentProtectionProperties.GetOpenOfficePasswordHash: TArray<Byte>;
begin
  Result := Info.OpenOfficePasswordHash;
end;

procedure TdxDocumentProtectionProperties.SetOpenOfficePasswordHash(const AValue: TArray<Byte>);
begin
  if TdxByteArray.Compare(OpenOfficePasswordHash, AValue) then
    Exit;
  SetPropertyValue<TArray<Byte>>(SetOpenOfficePasswordHashCore, AValue);
end;

function TdxDocumentProtectionProperties.SetOpenOfficePasswordHashCore(const AInfo: TdxDocumentProtectionInfo;
  const AValue: TArray<Byte>): TdxDocumentModelChangeActions;
begin
  AInfo.OpenOfficePasswordHash := AValue;
  Result := GetBatchUpdateChangeActions;
end;

function TdxDocumentProtectionProperties.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxDocumentProtectionInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).DocumentProtectionInfoCache;
end;

function TdxDocumentProtectionProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.RaiseModifiedChanged,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.RaiseDocumentProtectionChanged];
end;

end.
