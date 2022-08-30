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

unit dxHashUtils;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI22}
  System.Hash,
{$ENDIF}
  Windows, SysUtils, Classes, Math, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxHash;

const
  dxHashTableSize = MaxWord;

type
  TdxDynamicItemList = class;
  TdxHashTableItem = class;

  { TdxDynamicListItem }

  TdxDynamicListItemClass = class of TdxDynamicListItem;
  TdxDynamicListItem = class(TcxIUnknownObject)
  protected
    FIndex: Integer;
    FNext: TdxDynamicListItem;
    FOwner: TdxDynamicItemList;
    FPrev: TdxDynamicListItem;

    procedure ShiftIndex(ADelta: Integer); virtual;
  public
    constructor Create(AOwner: TdxDynamicItemList; AIndex: Integer); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TdxDynamicListItem); virtual;

    property Index: Integer read FIndex;
  end;

  { TdxDynamicItemList }

  TdxDynamicItemListForEachProcRef = reference to procedure (AItem: TdxDynamicListItem);

  TdxDynamicItemList = class
  strict private
    function GetCount: Integer;
    function GetFirstIndex: Integer;
    function GetItem(AIndex: Integer): TdxDynamicListItem; inline;
    function GetLastIndex: Integer;
    procedure SetItem(AIndex: Integer; const AValue: TdxDynamicListItem); inline;
  protected
    FCurrentItem: TdxDynamicListItem;
    FFirst: TdxDynamicListItem;
    FIsDeletion: Boolean;
    FLast: TdxDynamicListItem;

    function CreateItem(const AIndex: Integer): TdxDynamicListItem; inline;
    procedure DeleteItem(const AItem: TdxDynamicListItem);
    procedure DoItemCreated(AItem: TdxDynamicListItem); virtual;
    function FindItem(const AIndex: Integer): TdxDynamicListItem;
    procedure ForEach(AProc: TdxDynamicItemListForEachProcRef; AGoForward: Boolean = True); overload;
    procedure ForEach(AProc: TdxDynamicItemListForEachProcRef; AStartIndex: Integer;
      AFinishIndex: Integer; AGoForward: Boolean = True); overload;
    function GetItemClass: TdxDynamicListItemClass; virtual;
    procedure InsertItem(const ANewItem, ANeighborItem: TdxDynamicListItem);
    function ItemNeeded(const AIndex: Integer): TdxDynamicListItem;
    procedure ShiftIndexes(AStartFromIndex, ADelta: Integer);
  public
    destructor Destroy; override;
    procedure Clear;

    property Count: Integer read GetCount;
    property FirstIndex: Integer read GetFirstIndex;
    property Items[Index: Integer]: TdxDynamicListItem read GetItem write SetItem; default;
    property LastIndex: Integer read GetLastIndex;
  end;

  { TdxHashTable }

  TdxHashTable = class
  protected
    FCount: Integer;
    FTable: array of TdxDynamicItemList;

    procedure CheckAndAddItem(var AItem);
    procedure DeleteItem(AItem: TdxHashTableItem); virtual;
    procedure ForEach(AProc: TdxDynamicItemListForEachProcRef);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;

    property Count: Integer read FCount;
  end;

  { TdxHashTableItem }

  TdxHashTableItem = class(TdxDynamicListItem)
  strict private
    FRefCount: Integer;

    function GetKey: Cardinal; inline;
    procedure SetKey(const AValue: Cardinal); inline;
  protected
    FHashTable: TdxHashTable;

    procedure AddToHash(var AHash: Integer; const AData: AnsiString); overload; inline;
    procedure AddToHash(var AHash: Integer; const AData: string); overload; inline;
    procedure AddToHash(var AHash: Integer; const AData; ADataSize: Integer); overload; inline;
    procedure CalculateHash; virtual;
    function DoIsEqual(const AItem: TdxHashTableItem): Boolean; virtual;

    property HashTable: TdxHashTable read FHashTable;
  public
    destructor Destroy; override;
    procedure AddRef;
    function IsEqual(const AItem: TdxHashTableItem): Boolean;
    procedure Release;

    property Key: Cardinal read GetKey write SetKey;
    property RefCount: Integer read FRefCount;
  end;

implementation

{ TdxDynamicListItem }

constructor TdxDynamicListItem.Create(AOwner: TdxDynamicItemList; AIndex: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FIndex := AIndex;
end;

destructor TdxDynamicListItem.Destroy;
begin
  if FOwner <> nil then
    FOwner.DeleteItem(Self);
  inherited Destroy;
end;

procedure TdxDynamicListItem.Assign(ASource: TdxDynamicListItem);
begin
end;

procedure TdxDynamicListItem.ShiftIndex(ADelta: Integer);
begin
  Inc(FIndex, ADelta);
end;

{ TdxDynamicItemList }

destructor TdxDynamicItemList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TdxDynamicItemList.Clear;
var
  AItem: TdxDynamicListItem;
begin
  AItem := FFirst;
  FFirst := nil;
  FLast := nil;
  if AItem = nil then Exit;
  FIsDeletion := True;
  try
    while AItem.FNext <> nil do
    begin
      AItem := AItem.FNext;
      AItem.FPrev.Free;
    end;
    AItem.Free;
  finally
    FIsDeletion := False;
  end;
end;

function TdxDynamicItemList.CreateItem(const AIndex: Integer): TdxDynamicListItem;
begin
  Result := FindItem(AIndex);
  if (Result = nil) or (Result.Index <> AIndex) then
  begin
    FCurrentItem := GetItemClass.Create(Self, AIndex);
    InsertItem(FCurrentItem, Result);
    Result := FCurrentItem;
    DoItemCreated(Result);
  end;
end;

procedure TdxDynamicItemList.DeleteItem(const AItem: TdxDynamicListItem);
begin
  if FCurrentItem = AItem then
    FCurrentItem := nil;
  if FIsDeletion then Exit;

  if AItem.FPrev <> nil then
    AItem.FPrev.FNext := AItem.FNext
  else
  begin
    FFirst := AItem.FNext;
    if FFirst <> nil then
      FFirst.FPrev := nil;
  end;

  if AItem.FNext <> nil then
    AItem.FNext.FPrev := AItem.FPrev
  else
  begin
    FLast := AItem.FPrev;
    if FLast <> nil then
      FLast.FNext := nil;
  end;

  AItem.FNext := nil;
  AItem.FPrev := nil;
end;

procedure TdxDynamicItemList.DoItemCreated(AItem: TdxDynamicListItem);
begin
  // do nothing
end;

function TdxDynamicItemList.FindItem(const AIndex: Integer): TdxDynamicListItem;
var
  AFirst, ALast: TdxDynamicListItem;
begin
  Result := nil;
  if (FFirst = nil) or ((FFirst.Index > AIndex) or (FLast.Index < AIndex)) then
    Exit;

  ALast := FLast;
  AFirst := FFirst;
  if FCurrentItem <> nil then
  begin
    if FCurrentItem.Index > AIndex then
      ALast := FCurrentItem
    else
      AFirst := FCurrentItem;
  end;

  if AIndex > (AFirst.Index + ALast.Index) div 2 then
  begin
    Result := ALast;
    while (Result <> nil) and (Result.Index > AIndex) do
      Result := Result.FPrev;
  end
  else
    Result := AFirst;

  while (Result <> nil) and (Result.Index < AIndex) do
    Result := Result.FNext;
  while (Result <> nil) and (Result.FPrev <> nil) and (Result.FPrev.Index = AIndex) do
    Result := Result.FPrev;

  if Result <> nil then
    FCurrentItem := Result;
end;

procedure TdxDynamicItemList.ForEach(AProc: TdxDynamicItemListForEachProcRef; AGoForward: Boolean = True);
begin
  ForEach(AProc, FirstIndex, LastIndex, AGoForward);
end;

procedure TdxDynamicItemList.ForEach(AProc: TdxDynamicItemListForEachProcRef;
  AStartIndex, AFinishIndex: Integer; AGoForward: Boolean = True);
var
  AItem: TdxDynamicListItem;
  AItemNext: TdxDynamicListItem;
begin
  AItemNext := nil;
  if AGoForward then
  begin
    if (FCurrentItem <> nil) and ((FCurrentItem.Index <= AStartIndex) or
      (Abs(FCurrentItem.Index - AStartIndex) < Abs(FFirst.Index - AStartIndex))) then
    begin
      AItem := FCurrentItem;
      while AItem.Index > AStartIndex do
        AItem := AItem.FPrev;
    end
    else
      AItem := FFirst;

    while (AItem <> nil) and (AItem.Index < AStartIndex) do
      AItem := AItem.FNext;
    while (AItem <> nil) and (AItem.Index <= AFinishIndex) do
    begin
      AItemNext := AItem.FNext;
      AProc(AItem);
      AItem := AItemNext;
    end;
  end
  else
  begin
    if (FCurrentItem <> nil) and ((FCurrentItem.Index >= AFinishIndex) or
      (Abs(FCurrentItem.Index - AFinishIndex) < Abs(FLast.Index - AFinishIndex))) then
    begin
      AItem := FCurrentItem;
      while AItem.Index < AFinishIndex do
        AItem := AItem.FNext;
    end
    else
      AItem := FLast;

    while (AItem <> nil) and (AItem.Index > AFinishIndex) do
      AItem := AItem.FPrev;
    while (AItem <> nil) and (AItem.Index >= AStartIndex) do
    begin
      AItemNext := AItem.FPrev;
      AProc(AItem);
      AItem := AItemNext;
    end;
  end;

  if AItemNext <> nil then
    FCurrentItem := AItemNext;
end;

function TdxDynamicItemList.GetItemClass: TdxDynamicListItemClass;
begin
  Result := TdxDynamicListItem;
end;

procedure TdxDynamicItemList.InsertItem(const ANewItem, ANeighborItem: TdxDynamicListItem);

  procedure DoInsertItem(const AFirst, ASecond: TdxDynamicListItem);
  begin
    if AFirst.FNext <> nil then
    begin
      AFirst.FNext.FPrev := ASecond;
      ASecond.FNext := AFirst.FNext;
    end;
    AFirst.FNext := ASecond;
    if ASecond.FPrev <> nil then
    begin
      AFirst.FPrev := ASecond.FPrev;
      ASecond.FPrev.FNext := AFirst;
    end;
    ASecond.FPrev := AFirst;
    if AFirst.FPrev = nil then
      FFirst := AFirst;
    if ASecond.FNext = nil then
      FLast := ASecond;
  end;

begin
  if FFirst = nil then
  begin
    FFirst := ANewItem;
    FLast := ANewItem;
  end
  else

  if FFirst.Index > ANewItem.Index then
    DoInsertItem(ANewItem, FFirst)
  else

  if FLast.Index < ANewItem.Index then
    DoInsertItem(FLast, ANewItem)
  else

  if ANeighborItem = nil then
    raise EdxException.Create('TdxDynamicItemList.InsertItem')
  else

  if ANeighborItem.Index > ANewItem.Index then
    DoInsertItem(ANewItem, ANeighborItem)
  else
    DoInsertItem(ANeighborItem, ANewItem);
end;

function TdxDynamicItemList.ItemNeeded(const AIndex: Integer): TdxDynamicListItem;
begin
  Result := Items[AIndex];
  if Result = nil then
    Result := CreateItem(AIndex);
end;

procedure TdxDynamicItemList.ShiftIndexes(AStartFromIndex, ADelta: Integer);
var
  AItem: TdxDynamicListItem;
begin
  if ADelta > 0 then
  begin
    AItem := FLast;
    while (AItem <> nil) and (AItem.Index >= AStartFromIndex) do
    begin
      AItem.ShiftIndex(ADelta);
      AItem := AItem.FPrev;
    end;
  end
  else
    if ADelta < 0 then
    begin
      AItem := FFirst;
      while (AItem <> nil) and (AItem.Index < AStartFromIndex) do
        AItem := AItem.FNext;
      while (AItem <> nil) and (AItem.Index >= AStartFromIndex) do
      begin
        AItem.ShiftIndex(ADelta);
        AItem := AItem.FNext;
      end;
    end;
end;

function TdxDynamicItemList.GetCount: Integer;
begin
  Result := LastIndex + 1;
end;

function TdxDynamicItemList.GetFirstIndex: Integer;
begin
  if FFirst <> nil then
    Result := FFirst.Index
  else
    Result := 0;
end;

function TdxDynamicItemList.GetItem(AIndex: Integer): TdxDynamicListItem;
begin
  Result := FindItem(AIndex);
  if (Result <> nil) and (Result.Index <> AIndex) then
    Result := nil;
end;

function TdxDynamicItemList.GetLastIndex: Integer;
begin
  if FLast <> nil then
    Result := FLast.Index
  else
    Result := -1;
end;

procedure TdxDynamicItemList.SetItem(AIndex: Integer; const AValue: TdxDynamicListItem);
begin
  ItemNeeded(AIndex).Assign(AValue);
end;

{ TdxHashTable }

constructor TdxHashTable.Create;
begin
  SetLength(FTable, dxHashTableSize);
  FillChar(FTable[0], dxHashTableSize * SizeOf(TdxDynamicItemList), 0);
end;

destructor TdxHashTable.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TdxHashTable.Clear;
var
  I: Integer;
begin
  try
    for I := 0 to dxHashTableSize - 1 do
      FTable[I].Free;
  finally
    FillChar(FTable[0], dxHashTableSize * SizeOf(TdxDynamicItemList), 0);
    FCount := 0;
  end;
end;

procedure TdxHashTable.CheckAndAddItem(var AItem);
var
  ACandidate: TdxHashTableItem;
  AIndex: Integer;
  AItemObject: TdxHashTableItem;
  AItems: TdxDynamicItemList;
begin
  AItemObject := TdxHashTableItem(AItem);
  AIndex := AItemObject.Key mod dxHashTableSize;
  AItems := FTable[AIndex];
  if AItems = nil then
  begin
    AItems := TdxDynamicItemList.Create;
    FTable[AIndex] := AItems;
  end;
  AItemObject.FOwner := AItems;

  ACandidate := TdxHashTableItem(AItems.FindItem(AItemObject.Index));
  if ACandidate = nil then
    AItems.InsertItem(AItemObject, nil)
  else
  begin
    while ACandidate.Index = AItemObject.Index do
    begin
      if ACandidate.IsEqual(AItemObject) then
      begin
        TdxHashTableItem(AItem) := ACandidate;
        if ACandidate <> AItemObject then
        begin
          AItemObject.FOwner := nil;
          FreeAndNil(AItemObject);
        end;
        Break;
      end;
      if ACandidate.FNext <> nil then
        ACandidate := TdxHashTableItem(ACandidate.FNext)
      else
        Break;
    end;
    if TdxHashTableItem(AItem) <> ACandidate then
      AItems.InsertItem(AItemObject, ACandidate);
  end;
  TdxHashTableItem(AItem).FHashTable := Self;
end;

procedure TdxHashTable.DeleteItem(AItem: TdxHashTableItem);
begin
end;

procedure TdxHashTable.ForEach(AProc: TdxDynamicItemListForEachProcRef);
var
  AList: TdxDynamicItemList;
  I: Integer;
begin
  for I := 0 to dxHashTableSize - 1 do
  begin
    AList := FTable[I];
    if AList <> nil then
      AList.ForEach(AProc, True);
  end;
end;

{ TdxHashTableItem }

destructor TdxHashTableItem.Destroy;
begin
  if HashTable <> nil then
    HashTable.DeleteItem(Self);
  inherited Destroy;
end;

procedure TdxHashTableItem.AddRef;
begin
  Inc(FRefCount);
  if (HashTable <> nil) then
    Inc(HashTable.FCount);
end;

procedure TdxHashTableItem.AddToHash(var AHash: Integer; const AData: AnsiString);
begin
  AddToHash(AHash, AData[1], Length(AData));
end;

procedure TdxHashTableItem.AddToHash(var AHash: Integer; const AData: string);
begin
  AddToHash(AHash, AData[1], Length(AData) * SizeOf(WideChar));
end;

procedure TdxHashTableItem.AddToHash(var AHash: Integer; const AData; ADataSize: Integer);
begin
  AHash := dxBobJenkinsHash(AData, ADataSize, AHash);
end;

procedure TdxHashTableItem.CalculateHash;
begin
end;

function TdxHashTableItem.DoIsEqual(const AItem: TdxHashTableItem): Boolean;
begin
  Result := Byte(Self) = Byte(AItem);
end;

function TdxHashTableItem.IsEqual(const AItem: TdxHashTableItem): Boolean;
begin
  Result := (AItem = Self) or DoIsEqual(AItem);
end;

procedure TdxHashTableItem.Release;
begin
  if (FRefCount > 0) and (HashTable <> nil) then
    Dec(HashTable.FCount);
  Dec(FRefCount);
  if FRefCount <= 0 then
    Free;
end;

function TdxHashTableItem.GetKey: Cardinal;
begin
  Result := Cardinal(Index);
end;

procedure TdxHashTableItem.SetKey(const AValue: Cardinal);
begin
  FIndex := Integer(AValue);
end;

end.
