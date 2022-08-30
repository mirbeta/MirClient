{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressFlowChart                                         }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSFLOWCHART AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE end USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxSugiyamaLayout.Utils;

interface

{$I cxVer.inc}

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxGenerics, Classes;

type
  { TdxSortedDictionaryOfCollections<TKey, TCollection, T> }

  TdxSortedDictionaryOfCollections<TKey; T; TCollection: TList<T>, constructor> = class
  strict private
    FGetKey: TFunc<T, TKey>;
    FDictionary: TObjectDictionary<TKey, TCollection>;
    function GetAllKeys: TArray<TKey>;
  protected
    procedure ClearIfEmptyCollection(AKey: TKey);
    function GetCollection(AKey: TKey): TCollection;
    function GetFirstKey: TKey;

    property AllKeys: TArray<TKey> read GetAllKeys;
    property Dictionary: TObjectDictionary<TKey, TCollection> read FDictionary;
  public
    constructor Create(AGetKey: TFunc<T, TKey>);
    destructor Destroy; override;
    procedure Add(AItem: T);
    procedure RemoveFirstMin;
    procedure Remove(AItem: T);
    procedure RemoveCore(AItem: T; AKey: TKey; ACollection: TCollection);
    function GetFirstMin: T;
    function Any: Boolean;
  end;

  { TdxSortedDictionaryOfList<TKey, T> }

  TdxSortedDictionaryOfList<TKey, T> = class(TdxSortedDictionaryOfCollections<TKey, T, TList<T>>)
  public
    constructor Create(const AGetKey: TFunc<T, TKey>);
    procedure RemoveWhere(const APredicate: TFunc<T, Boolean>);
  end;

  { TEnumerableHelper }

  TEnumerableHelper = class sealed
  public
    class function ToDictionary<T, TKey, TValue>(
      AEnumerable: TEnumerable<T>;
      const AGetKey: TFunc<T, TKey>;
      const AGetValue: TFunc<T, TValue>; AOwnerships: TDictionaryOwnerships = []): TDictionary<TKey, TValue>; overload; static;
    class function ToDictionary<T, TKey, TValue>(
      const AEnumerable: TArray<T>;
      const AGetKey: TFunc<T, TKey>;
      const AGetValue: TFunc<T, TValue>;
      AOwnerships: TDictionaryOwnerships = []): TDictionary<TKey, TValue>; overload; static;
    class procedure ForEach<T>(AEnumerable: TEnumerable<T>; const AAction: TProc<T>); overload; static;
    class procedure ForEach<T>(const AEnumerable: TArray<T>; const AAction: TProc<T>); overload; static;
    class function Select<TFrom, TTo>(AEnumerable: TEnumerable<TFrom>; const AConvert: TFunc<TFrom, TTo>): TList<TTo>; overload; static;
    class function Select<TFrom, TTo>(const AEnumerable: TArray<TFrom>; const AConvert: TFunc<TFrom, TTo>): TArray<TTo>; overload; static;
    class function Where<T>(AEnumerable: TEnumerable<T>; const ACondition: TFunc<T, Boolean>): TList<T>; overload; static;
    class function Where<T>(const AEnumerable: TArray<T>; const ACondition: TFunc<T, Boolean>): TArray<T>; overload; static;
    class function ToArray<T>(AEnumerable: TEnumerable<T>): TArray<T>; overload; static;
    class function ToArray<T>(const AEnumerable: array of T): TArray<T>; overload; static;
    class function ToGroupsDictionary<T, TKey, TValue>(AItems: TList<T>;
      const AKeySelector: TFunc<T, TKey>;
      const AGetValue: TFunc<TList<T>, TList<TValue>>): TDictionary<TKey, TList<TValue>>; overload; static;
  end;

implementation

{ TdxSortedDictionaryOfCollections }

constructor TdxSortedDictionaryOfCollections<TKey, T, TCollection>.Create(AGetKey: TFunc<T, TKey>);
begin
  FDictionary := TObjectDictionary<TKey, TCollection>.Create([doOwnsValues]);
  FGetKey := AGetKey;
end;

destructor TdxSortedDictionaryOfCollections<TKey, T, TCollection>.Destroy;
begin
  FDictionary.Free;
  inherited Destroy;
end;

function TdxSortedDictionaryOfCollections<TKey, T, TCollection>.GetAllKeys: TArray<TKey>;
{$IFNDEF DELPHIXE}
var
  AKey: TKey;
  AIndex: Integer;
{$ENDIF}
begin
{$IFDEF DELPHIXE}
  Result := FDictionary.Keys.ToArray;
{$ELSE}
  SetLength(Result, FDictionary.Count);
  AIndex := 0;
  for AKey in FDictionary.Keys do
  begin
    Result[AIndex] := AKey;
    Inc(AIndex);
  end;
{$ENDIF}
end;

procedure TdxSortedDictionaryOfCollections<TKey, T, TCollection>.Add(AItem: T);
var
  AKey: TKey;
  ACollection: TCollection;
begin
  AKey := FGetKey(AItem);
  if not FDictionary.TryGetValue(AKey, ACollection) then
  begin
    ACollection := TCollection.Create;
    FDictionary.Add(AKey, ACollection);
  end;
  ACollection.Add(AItem);
end;

procedure TdxSortedDictionaryOfCollections<TKey, T, TCollection>.RemoveFirstMin;
var
  AMinKey: TKey;
  AFirstKeySet: TCollection;
begin
  AMinKey := GetFirstKey;
  AFirstKeySet := FDictionary[AMinKey];
  RemoveCore(AFirstKeySet.First, AMinKey, AFirstKeySet);
end;

procedure TdxSortedDictionaryOfCollections<TKey, T, TCollection>.Remove(AItem: T);
var
  AKey: TKey;
  ACollection: TCollection;
begin
  AKey := FGetKey(AItem);
  ACollection := FDictionary[AKey];
  RemoveCore(AItem, AKey, ACollection);
end;

procedure TdxSortedDictionaryOfCollections<TKey, T, TCollection>.RemoveCore(AItem: T; AKey: TKey; ACollection: TCollection);
begin
  ACollection.Remove(AItem);
  ClearIfEmptyCollection(AKey);
end;

procedure TdxSortedDictionaryOfCollections<TKey, T, TCollection>.ClearIfEmptyCollection(AKey: TKey);
begin
  if FDictionary[AKey].Count = 0 then
    FDictionary.Remove(AKey);
end;

function TdxSortedDictionaryOfCollections<TKey, T, TCollection>.GetFirstMin: T;
var
  AMinKeySet: TCollection;
begin
  AMinKeySet := FDictionary[GetFirstKey];
  Result := AMinKeySet.First;
end;

function TdxSortedDictionaryOfCollections<TKey, T, TCollection>.GetCollection(AKey: TKey): TCollection;
begin
  Result := FDictionary[AKey];
end;

function TdxSortedDictionaryOfCollections<TKey, T, TCollection>.GetFirstKey: TKey;
var
  AKeys: TArray<TKey>;
  AMinKey: TKey;
  I: Integer;
{$IFNDEF DELPHIXE}
  AKey: TKey;
  AIndex: Integer;
{$ENDIF}
begin
  if FDictionary.Count = 0 then
    Exit(Default(TKey));
{$IFDEF DELPHIXE}
  AKeys := FDictionary.Keys.ToArray;
{$ELSE}
  SetLength(AKeys, FDictionary.Count);
  AIndex := 0;
  for AKey in FDictionary.Keys do
  begin
    AKeys[AIndex] := AKey;
    Inc(AIndex);
  end;
{$ENDIF}
  Result := AKeys[0];
  for I := 1 to High(AKeys) do
    if TComparer<TKey>.Default.Compare(Result, AKeys[I]) > 0 then
      Result := AKeys[I];
end;

function TdxSortedDictionaryOfCollections<TKey, T, TCollection>.Any: Boolean;
begin
  Result := FDictionary.Count > 0;
end;

{ TdxSortedDictionaryOfList<TKey, T> }

constructor TdxSortedDictionaryOfList<TKey, T>.Create(const AGetKey: TFunc<T, TKey>);
begin
  inherited Create(AGetKey);
end;

procedure TdxSortedDictionaryOfList<TKey, T>.RemoveWhere(const APredicate: TFunc<T, Boolean>);
var
  AKey: TKey;
  ACollection: TList<T>;
  I: Integer;
begin
  for AKey in Dictionary.Keys do
  begin
    ACollection := Dictionary[AKey];
    for I := ACollection.Count - 1 downto 0 do
      if APredicate(ACollection[I]) then
        ACollection.Delete(I);
    ClearIfEmptyCollection(AKey);
  end;
end;

{ TEnumerableHelper }

class procedure TEnumerableHelper.ForEach<T>(AEnumerable: TEnumerable<T>; const AAction: TProc<T>);
var
  AItem: T;
begin
  for AItem in AEnumerable do
    AAction(AItem);
end;

class procedure TEnumerableHelper.ForEach<T>(const AEnumerable: TArray<T>; const AAction: TProc<T>);
var
  AItem: T;
begin
  for AItem in AEnumerable do
    AAction(AItem);
end;

class function TEnumerableHelper.Select<TFrom, TTo>(AEnumerable: TEnumerable<TFrom>; const AConvert: TFunc<TFrom, TTo>): TList<TTo>;
var
  AItem: TFrom;
begin
  Result := TList<TTo>.Create;
  for AItem in AEnumerable do
    Result.Add(AConvert(AItem));
end;

class function TEnumerableHelper.Select<TFrom, TTo>(const AEnumerable: TArray<TFrom>; const AConvert: TFunc<TFrom, TTo>): TArray<TTo>;
var
  I: Integer;
begin
  SetLength(Result, Length(AEnumerable));
  for I := Low(AEnumerable) to High(AEnumerable) do
    Result[I] := AConvert(AEnumerable[I]);
end;

class function TEnumerableHelper.ToDictionary<T; TKey; TValue>(
  AEnumerable: TEnumerable<T>; const AGetKey: TFunc<T, TKey>; const AGetValue: TFunc<T, TValue>;
  AOwnerships: TDictionaryOwnerships = []): TDictionary<TKey, TValue>;
var
  AItem: T;
begin
  Result := TObjectDictionary<TKey, TValue>.Create(AOwnerships);
  for AItem in AEnumerable do
    Result.AddOrSetValue(AGetKey(AItem), AGetValue(AItem));
end;

class function TEnumerableHelper.ToArray<T>(const AEnumerable: array of T): TArray<T>;
var
  I, ALength: Integer;
begin
  ALength := Length(AEnumerable);
  SetLength(Result, ALength);
  for I := Low(AEnumerable) to High(AEnumerable) do
    Result[I] := AEnumerable[I];
end;

class function TEnumerableHelper.ToDictionary<T, TKey, TValue>(const AEnumerable: TArray<T>; const AGetKey: TFunc<T, TKey>;
  const AGetValue: TFunc<T, TValue>; AOwnerships: TDictionaryOwnerships = []): TDictionary<TKey, TValue>;
var
  AItem: T;
begin
  Result := TObjectDictionary<TKey, TValue>.Create(AOwnerships);
  for AItem in AEnumerable do
    Result.AddOrSetValue(AGetKey(AItem), AGetValue(AItem));
end;

class function TEnumerableHelper.ToArray<T>(AEnumerable: TEnumerable<T>): TArray<T>;
var
  AIndex, ACapacity: Integer;
  AItem: T;
begin
  AIndex := 0;
  ACapacity := 2048;
  SetLength(Result, ACapacity);
  for AItem in AEnumerable do
  begin
    if ACapacity = AIndex then
    begin
      ACapacity := ACapacity * 2;
      SetLength(Result, ACapacity);
    end;
    Result[AIndex] := AItem;
    Inc(AIndex);
  end;
  SetLength(Result, AIndex);
end;

class function TEnumerableHelper.ToGroupsDictionary<T, TKey, TValue>(AItems: TList<T>;
  const AKeySelector: TFunc<T, TKey>;
  const AGetValue: TFunc<TList<T>, TList<TValue>>): TDictionary<TKey, TList<TValue>>;
var
  AItem: T;
  AKey: TKey;
  AGroups: TDictionary<TKey, TList<T>>;
  AGroupValue: TList<T>;
  APair: TPair<TKey, TList<T>>;
begin
  Result := TObjectDictionary<TKey, TList<TValue>>.Create([doOwnsValues]);
  if AItems.Count > 0 then
  begin
    AGroups := TObjectDictionary<TKey, TList<T>>.Create([doOwnsValues]);
    try
      for AItem in AItems do
      begin
        AKey := AKeySelector(AItem);
        if not AGroups.TryGetValue(AKey, AGroupValue) then
        begin
          AGroupValue := TList<T>.Create;
          AGroups.Add(AKey, AGroupValue);
        end;
        AGroupValue.Add(AItem);
      end;
      for APair in AGroups do
        Result.Add(APair.Key, AGetValue(APair.Value));
    finally
      AGroups.Free;
    end;
  end;
end;

class function TEnumerableHelper.Where<T>(AEnumerable: TEnumerable<T>; const ACondition: TFunc<T, Boolean>): TList<T>;
var
  AItem: T;
begin
  Result := TList<T>.Create;
  for AItem in AEnumerable do
    if ACondition(AItem) then
      Result.Add(AItem);
end;

class function TEnumerableHelper.Where<T>(const AEnumerable: TArray<T>; const ACondition: TFunc<T, Boolean>): TArray<T>;
var
  AItem: T;
  I, AIndex: Integer;
begin
  SetLength(Result, Length(AEnumerable));
  AIndex := 0;
  for I := Low(AEnumerable) to High(AEnumerable) do
  begin
    AItem := AEnumerable[I];
    if ACondition(AItem) then
    begin
      Result[AIndex] := AItem;
      Inc(AIndex);
    end;
  end;
  SetLength(Result, AIndex);
end;

end.
