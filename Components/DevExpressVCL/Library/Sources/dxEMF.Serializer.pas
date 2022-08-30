{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEntityMapping Framework                           }
{                                                                    }
{           Copyright (c) 2016-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
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

unit dxEMF.Serializer;

{$I cxVer.inc}
{$I dxEMF.inc}

interface

uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections, TypInfo, Rtti, DB;

type

  { TdxCustomBlobSerialize }

  TdxCustomBlobSerializerClass = class of TdxCustomBlobSerializer;
  TdxCustomBlobSerializer = class
  public
    class function GetStorageDBType: TFieldType; virtual;
    function GetAsBytes(AObject: TObject): TBytes;
    function GetAsValue(AObject: TObject): TValue;
    procedure SaveToStream(AObject: TObject; AStream: TStream); virtual; abstract;
    procedure LoadFromStream(AObject: TObject; AStream: TStream); virtual; abstract;
  end;

  { TdxStreamPersistBlobSerializer }

  TdxPersistentBlobSerializer = class(TdxCustomBlobSerializer)
  protected
    class function IsSupported(AClass: TClass): Boolean; static;
  public
    class function GetStorageDBType: TFieldType; override;
    procedure SaveToStream(AObject: TObject; AStream: TStream); override;
    procedure LoadFromStream(AObject: TObject; AStream: TStream); override;
  end;

  { TdxStringsBlobSerializer }

  TdxStringsBlobSerializer = class(TdxCustomBlobSerializer)
  protected
    class function IsSupported(AClass: TClass): Boolean; static;
  public
    class function GetStorageDBType: TFieldType; override;
    procedure SaveToStream(AObject: TObject; AStream: TStream); override;
    procedure LoadFromStream(AObject: TObject; AStream: TStream); override;
  end;

  { TdxBlobSerializerFactory }

  TdxBlobSerializerFactory = class sealed
  strict private class var
    FBlobSerializers: TDictionary<TClass, TdxCustomBlobSerializerClass>;
    class destructor Destroy;
  public
    class procedure Register(AClass: TClass; ABlobSerializeClass: TdxCustomBlobSerializerClass);
    class procedure UnRegister(AClass: TClass);
    class function GetSerializer(AClass: TClass): TdxCustomBlobSerializerClass;
  end;

  { TdxValueConverter }

  TdxValueConverterClass = class of TdxValueConverter;
  TdxValueConverter = class abstract
  public
    class function StorageType: PTypeInfo; virtual; abstract;
    class function ConvertToStorageType(const AValue: TValue): TValue; virtual; abstract;
    class function ConvertFromStorageType(const AValue: TValue): TValue; virtual; abstract;
  end;

  TdxValueConverter<T> = class(TdxValueConverter)
  public
    class function StorageType: PTypeInfo; override;
  end;

implementation

uses
  dxCore,
  dxEMF.Strs,
  dxCoreClasses;

{ TdxCustomBlobSerialize }

function TdxCustomBlobSerializer.GetAsBytes(AObject: TObject): TBytes;
var
  AStream: TdxMemoryStream;
begin
  AStream := TdxMemoryStream.Create;
  try
    SaveToStream(AObject, AStream);
    Result := AStream.ToArray;
  finally
    AStream.Free;
  end;
end;

function TdxCustomBlobSerializer.GetAsValue(AObject: TObject): TValue;
begin
  Result := TValue.From<TBytes>(GetAsBytes(AObject));
end;

class function TdxCustomBlobSerializer.GetStorageDBType: TFieldType;
begin
  Result := TFieldType.ftBlob;
end;

{ TdxStreamPersistBlobSerializer }

class function TdxPersistentBlobSerializer.IsSupported(AClass: TClass): Boolean;
begin
  Result := Supports(AClass, IStreamPersist);
end;

procedure TdxPersistentBlobSerializer.LoadFromStream(AObject: TObject; AStream: TStream);
var
  AStreamPersist: IStreamPersist;
begin
  if not Supports(AObject, IStreamPersist, AStreamPersist) then
    raise EInvalidOperation.Create(sdxSaveToStream);
  AStreamPersist.LoadFromStream(AStream);
end;

procedure TdxPersistentBlobSerializer.SaveToStream(AObject: TObject; AStream: TStream);
var
  AStreamPersist: IStreamPersist;
begin
  if not Supports(AObject, IStreamPersist, AStreamPersist) then
    raise EInvalidOperation.Create(sdxSaveToStream);
  AStreamPersist.SaveToStream(AStream);
end;

class function TdxPersistentBlobSerializer.GetStorageDBType: TFieldType;
begin
  Result := TFieldType.ftBlob;
end;

{ TdxBlobSerializerFactory }

class destructor TdxBlobSerializerFactory.Destroy;
begin
  FreeAndNil(FBlobSerializers);
end;

class function TdxBlobSerializerFactory.GetSerializer(AClass: TClass): TdxCustomBlobSerializerClass;
var
  AItem: TPair<TClass, TdxCustomBlobSerializerClass>;
begin
  if FBlobSerializers <> nil then
  begin
    if FBlobSerializers.TryGetValue(AClass, Result) then
      Exit;
    for AItem in FBlobSerializers do
      if AClass.InheritsFrom(AItem.Key) then
        Exit(AItem.Value);
  end;
  if TdxPersistentBlobSerializer.IsSupported(AClass) then
    Result := TdxPersistentBlobSerializer
  else
    if TdxStringsBlobSerializer.IsSupported(AClass) then
      Result := TdxStringsBlobSerializer
    else
      Result := nil;
end;

class procedure TdxBlobSerializerFactory.Register(AClass: TClass; ABlobSerializeClass: TdxCustomBlobSerializerClass);
begin
  if FBlobSerializers = nil then
    FBlobSerializers := TDictionary<TClass, TdxCustomBlobSerializerClass>.Create;
  FBlobSerializers.AddOrSetValue(AClass, ABlobSerializeClass);
end;

class procedure TdxBlobSerializerFactory.UnRegister(AClass: TClass);
begin
  if FBlobSerializers = nil then
    Exit;
  FBlobSerializers.Remove(AClass);
end;

{ TdxValueConverter<T> }

class function TdxValueConverter<T>.StorageType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

{ TdxStringsBlobSerializer }

class function TdxStringsBlobSerializer.GetStorageDBType: TFieldType;
begin
  Result := TFieldType.ftWideMemo;
end;

class function TdxStringsBlobSerializer.IsSupported(AClass: TClass): Boolean;
begin
  Result := AClass.InheritsFrom(TStrings);
end;

procedure TdxStringsBlobSerializer.LoadFromStream(AObject: TObject; AStream: TStream);
begin
  (AObject as TStrings).LoadFromStream(AStream);
end;

procedure TdxStringsBlobSerializer.SaveToStream(AObject: TObject; AStream: TStream);
begin
  (AObject as TStrings).SaveToStream(AStream);
end;

end.
