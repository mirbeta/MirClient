{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit dxServerModeClasses;

{$I cxVer.inc}

interface

uses
  SysUtils, Variants, Classes, dxCore, dxCoreClasses, cxVariants, cxFilter,
  cxCustomData;

const
  dxDefaultDictionaryHashTableSize = 16843;
  dxKeysChunkDictionaryHashTableSize = 4201;

type
  TdxNullableBoolean = (bNull, bTrue, bFalse);
  TdxKeyType = (ktInteger);
  TdxRowType = (rtRow);
  TdxBlockRelation = (brNone, brLess, brPrev, brIntersectStart, brEquals,
    brIntersectFinish, brNext, brGreater);

  TdxServerModeException = class(EdxException);
  TdxServerModeInvalidOperationException = class(TdxServerModeException);
  TdxServerModeInconsistencyDetectedException = class(TdxServerModeException);

  TdxServerModeRow = Variant;

  { TdxServerModeRowList }

  TdxServerModeRowList = class
  private
    FList: TVariantArray;
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxServerModeRow;
    function GetLast: TdxServerModeRow;
    procedure SetCount(const Value: Integer);
    procedure SetLast(const Value: TdxServerModeRow);
  public
    destructor Destroy; override;

    function Add(const Value: TdxServerModeRow): Integer;
    procedure Insert(Index: Integer; const Value: TdxServerModeRow);

    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TdxServerModeRow read GetItem; default;
    property Last: TdxServerModeRow read GetLast write SetLast;
    property List: TVariantArray read FList;
  end;

  { TdxServerModeKeyList }

  TdxServerModeKeyList = class(TdxServerModeRowList);

  { TdxServerModeSummaryList }

  TdxServerModeSummaryList = class(TdxServerModeRowList);

  { TdxServerModeIntDictionaryItem }

  TdxServerModeIntDictionaryItem = class
  private
    FKey: Integer;
    FNext: TdxServerModeIntDictionaryItem;
    FValue: TdxServerModeRow;
  public
    constructor Create(AKey: Integer; const AValue: TdxServerModeRow);

    property Key: Integer read FKey;
    property Next: TdxServerModeIntDictionaryItem read FNext;
    property Value: TdxServerModeRow read FValue write FValue;
  end;

  { TdxCustomServerModeIntDictionary }

  TdxCustomServerModeIntDictionary = class
  private
    FCount: Integer;
    FTable: array of TdxServerModeIntDictionaryItem;
    FTableSize: Integer;
  protected
    function Find(AKey: Integer): TdxServerModeIntDictionaryItem;
  public
    constructor Create(ATableSize: Integer = dxDefaultDictionaryHashTableSize);
    destructor Destroy; override;
    function Add(AKey: Integer; const AValue: Variant): TdxServerModeIntDictionaryItem;
    procedure Clear;
    function ContainsKey(AKey: Integer): Boolean;
    function TryGetValue(AKey: Integer; out AValue: Variant): Boolean;

    property Count: Integer read FCount;
  end;

  { TdxServerModeLinkedDictionaryItem }

  TdxServerModeLinkedDictionaryItem = class
  private
    FKey: Cardinal;
    FNext: TdxServerModeLinkedDictionaryItem;
    FValue: TdxServerModeIntDictionaryItem;
  public
    constructor Create(AKey: Cardinal; AValue: TdxServerModeIntDictionaryItem);

    property Key: Cardinal read FKey;
    property Next: TdxServerModeLinkedDictionaryItem read FNext;
    property Value: TdxServerModeIntDictionaryItem read FValue write FValue;
  end;

  { TdxCustomServerModeLinkedDictionary }

  TdxServerModeLinkedDictionary = class
  private
    FCount: Integer;
    FTable: array of TdxServerModeLinkedDictionaryItem;
    FTableSize: Cardinal;
  protected
    function Find(const AKey: Variant): TdxServerModeLinkedDictionaryItem;
  public
    constructor Create(ATableSize: Integer = dxDefaultDictionaryHashTableSize);
    destructor Destroy; override;
    procedure Add(const AKey: Variant; const AValue: TdxServerModeIntDictionaryItem);
    procedure Clear;
    function ContainsKey(const AKey: Variant): Boolean;
    function TryGetValue(const AKey: Variant; out AValue: Integer): Boolean;

    property Count: Integer read FCount;
  end;

  { TdxServerModeCustomDictionary }

  TdxServerModeCustomDictionary = class
  private
    FIntDictionary: TdxCustomServerModeIntDictionary;
    FLinkedDictionary: TdxServerModeLinkedDictionary;
    function GetCount: Integer; inline;
  protected
    function GetKeyByValue(const AValue: Variant; out AKey: Integer): Boolean;
  public
    constructor Create(ATableSize: Integer = dxDefaultDictionaryHashTableSize);
    destructor Destroy; override;
    procedure Add(AKey: Integer; const AValue: Variant);
    procedure Clear; virtual;
    function ContainsKey(AKey: Integer): Boolean;
    function TryGetValue(AKey: Integer; out AValue: Variant): Boolean;

    property Count: Integer read GetCount;
  end;

  { TdxServerModeRowsDictionary }

  TdxServerModeRowsDictionary = class(TdxCustomServerModeIntDictionary);

  { TdxLoadedRecordsBlock }

  TdxLoadedRecordsBlock = class
  private
    FFinish: Integer;
    FStart: Integer;
  public
    constructor Create(AStart, AFinish: Integer);
    function GetStatus(AStart, AFinish: Integer): TdxBlockRelation;
    function InRange(AIndex: Integer): Boolean;

    property Finish: Integer read FFinish write FFinish;
    property Start: Integer read FStart write FStart;
  end;

  { TdxLoadedRecordsBlockList }

  TdxLoadedRecordsHelper = class
  private
    FList: TdxFastObjectList;
    function GetCount: Integer; inline;
    function GetFirst: TdxLoadedRecordsBlock; inline;
    function GetItem(Index: Integer): TdxLoadedRecordsBlock; inline;
    function GetLast: TdxLoadedRecordsBlock; inline;
  protected
    function FindPosition(AStart: Integer; out AItemIndex: Integer): Boolean;
    procedure UpdateNext(AItemIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AFirstRecord, ACount: Integer; AIsFromBottom: Boolean); overload;
    procedure Add(AStart, AFinish: Integer); overload;
    procedure Clear;

    procedure GetClosestPopulatedIndexes(AIndex: Integer; out AStartIndex, AFinishIndex: Integer);

    property Count: Integer read GetCount;
    property First: TdxLoadedRecordsBlock read GetFirst;
    property Items[Index: Integer]: TdxLoadedRecordsBlock read GetItem; default;
    property Last: TdxLoadedRecordsBlock read GetLast;
  end;

  { TdxServerModeRowDictionary }

  TdxServerModeKeysDictionary = class(TdxServerModeCustomDictionary)
  protected
    FLoadedRecordsHelper: TdxLoadedRecordsHelper;
  public
    constructor Create(ATableSize: Integer = dxDefaultDictionaryHashTableSize);
    destructor Destroy; override;
    procedure Clear; override;
    function TryGetKeyByValue(const AValue: Variant; out AKey: Integer): Boolean;

    property LoadedRecordsHelper: TdxLoadedRecordsHelper read FLoadedRecordsHelper;
  end;

  { TdxServerModeRecordIndexDictionary }

  TdxServerModeLoadedKeysChunkDictionary = class(TdxServerModeCustomDictionary)
  public
    constructor Create;
    procedure Add(const AIndex: TdxServerModeRow; AValue: Integer);
    function ContainsKey(const AKey: TdxServerModeRow): Boolean;
    function TryGetValue(const AIndex: TdxServerModeRow; out AValue: Integer): Boolean;
  end;

  { TdxListSourceGroupInfo }

  TdxListSourceGroupInfo = class
  private
    FDataRowCount: Integer;
    FLevel: Integer;
  public
    constructor Create(ALevel: Integer);

    property DataRowCount: Integer read FDataRowCount write FDataRowCount;
    property Level: Integer read FLevel;
  end;

  TdxServerModeGroupInfoList = class;

  { TdxServerModeGroupInfo }

  TdxServerModeGroupInfo = class(TdxListSourceGroupInfo)
  private
    FChildrenGroups: TdxServerModeGroupInfoList;
    FGroupValue: Variant;
    FGroupValueHash: Cardinal;
    FParent: TdxServerModeGroupInfo;
    FSummary: TdxServerModeSummaryList;
    FTopDataRowIndex: Integer;
    FOnDestroy: TNotifyEvent;
  protected
    procedure DoDestroy; virtual;
  public
    constructor Create(AParent: TdxServerModeGroupInfo; const AGroupValue: Variant;
      ALevel, ATopRecordIndex: Integer);
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure CreateChildren;
    function Contains(ADataRowIndex: Integer): Boolean;
    function IsEqual(const AValue: Variant; AHash: Cardinal): Boolean;
    procedure ResetChildren;

    property ChildrenGroups: TdxServerModeGroupInfoList read FChildrenGroups;
    property GroupValue: Variant read FGroupValue;
    property Parent: TdxServerModeGroupInfo read FParent;
    property Summary: TdxServerModeSummaryList read FSummary;
    property TopDataRowIndex: Integer read FTopDataRowIndex;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

  { TdxServerModeGroupInfoList }

  TdxServerModeGroupInfoList = class(TdxFastObjectList)
  private
    FParent: TdxServerModeGroupInfo;
    function GetItem(Index: Integer): TdxServerModeGroupInfo;
  public
    constructor Create(AParent: TdxServerModeGroupInfo);
    function Find(const AGroupValue: Variant): Integer; overload;
    procedure UpdateDataRowIndexes;

    property Items[Index: Integer]: TdxServerModeGroupInfo read GetItem; default;
    property Parent: TdxServerModeGroupInfo read FParent;
  end;

  { TdxServerModeGroupInfoData }

  TdxServerModeGroupInfoData = class
  private
    FDataRowCount: Integer;
    FGroupValue: Variant;
    FSummary: TdxServerModeRow;
  public
    constructor Create(const AGroupValue: Variant; AChildDataRowCount: Integer; const ASummary: TdxServerModeRow); overload;
    constructor Create(const AGroupValue: Variant; const ASummary: TdxServerModeRow); overload;

    property DataRowCount: Integer read FDataRowCount;
    property GroupValue: Variant read FGroupValue;
    property Summary: TdxServerModeRow read FSummary;
  end;

  TdxServerModeGroupInfoDataList = class(TdxFastObjectList)
  private
    function GetItem(Index: Integer): TdxServerModeGroupInfoData;
  public
    property Items[Index: Integer]: TdxServerModeGroupInfoData read GetItem; default;
  end;

implementation

uses
  RTLConsts, Math, VarUtils;


{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

{ TdxServerModeRowList }

destructor TdxServerModeRowList.Destroy;
begin
  SetLength(FList, 0);
  inherited Destroy;
end;

function TdxServerModeRowList.Add(const Value: TdxServerModeRow): Integer;
begin
  Result := Count;
  Count := Result + 1;
  Last := Value;
end;

procedure TdxServerModeRowList.Insert(Index: Integer; const Value: TdxServerModeRow);
begin
  Count := Count + 1;
  if Index < Count - 1 then
    System.Move(FList[Index], FList[Index + 1],
      (Count - 1 - Index) * SizeOf(Variant));
  FList[Index] := Value;
end;

function TdxServerModeRowList.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TdxServerModeRowList.GetItem(Index: Integer): TdxServerModeRow;
begin
  Result := FList[Index];
end;

function TdxServerModeRowList.GetLast: TdxServerModeRow;
begin
  Result := FList[Count - 1];
end;

procedure TdxServerModeRowList.SetCount(const Value: Integer);
begin
  SetLength(FList, Value);
end;

procedure TdxServerModeRowList.SetLast(const Value: TdxServerModeRow);
begin
  FList[Count - 1] := Value;
end;

{ TdxServerModeIntDictionaryItem }

constructor TdxServerModeIntDictionaryItem.Create(AKey: Integer; const AValue: TdxServerModeRow);
begin
  inherited Create;
  FKey := AKey;
  FValue := AValue;
end;

{ TdxServerModeDictionary }

constructor TdxCustomServerModeIntDictionary.Create(ATableSize: Integer = dxDefaultDictionaryHashTableSize);
begin
  inherited Create;
  FCount := 0;
  FTableSize := ATableSize;
  SetLength(FTable, FTableSize);
  FillChar(FTable[0], FTableSize * SizeOf(TdxServerModeIntDictionaryItem), 0);
end;

destructor TdxCustomServerModeIntDictionary.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TdxCustomServerModeIntDictionary.Add(AKey: Integer; const AValue: Variant): TdxServerModeIntDictionaryItem;
var
  AIndex: Integer;
  AItem: TdxServerModeIntDictionaryItem;
begin
{$IFNDEF DELPHIBERLIN}
  {$IFNDEF CPUX64}
    Result := nil;
  {$ENDIF}
{$ENDIF}
  AIndex := AKey mod FTableSize;
  AItem := FTable[AIndex];
  if AItem = nil then
  begin
    Result := TdxServerModeIntDictionaryItem.Create(AKey, AValue);
    FTable[AIndex] := Result;
  end
  else
    repeat
      if AItem.Key = AKey then
        raise Exception.Create('Duplicates not allowed');
      if AItem.Next = nil then
      begin
        Result := TdxServerModeIntDictionaryItem.Create(AKey, AValue);
        AItem.FNext := Result;
        Break;
      end
      else
        AItem := AItem.Next;
    until False;
  Inc(FCount);
end;

procedure TdxCustomServerModeIntDictionary.Clear;
var
  I: TdxNativeInt;
  AItem, ATemp: TdxServerModeIntDictionaryItem;
begin
  if Count = 0 then
    Exit;
  for I := 0 to FTableSize - 1 do
  begin
    AItem := FTable[I];
    if AItem <> nil then
    begin
      while AItem <> nil do
      begin
        ATemp := AItem;
        AItem := AItem.Next;
        ATemp.Free;
      end;
      FTable[I] := nil;
    end;
  end;
  FCount := 0;
end;

function TdxCustomServerModeIntDictionary.ContainsKey(AKey: Integer): Boolean;
begin
  Result := Find(AKey) <> nil;
end;

function TdxCustomServerModeIntDictionary.TryGetValue(AKey: Integer; out AValue: Variant): Boolean;
var
  AItem: TdxServerModeIntDictionaryItem;
begin
  AItem := Find(AKey);
  Result := AItem <> nil;
  if Result then
    AValue := AItem.Value;
end;

function TdxCustomServerModeIntDictionary.Find(AKey: Integer): TdxServerModeIntDictionaryItem;
var
  AIndex: Integer;
  AItem: TdxServerModeIntDictionaryItem;
begin
  Result := nil;
  if AKey < 0 then
    Exit;
  AIndex := AKey mod FTableSize;
  AItem := FTable[AIndex];
  while AItem <> nil do
  begin
    if AItem.Key = AKey then
    begin
      Result := AItem;
      Break;
    end;
    AItem := AItem.Next;
  end;
end;

{ TdxServerModeLinkedDictionaryItem }

constructor TdxServerModeLinkedDictionaryItem.Create(AKey: Cardinal; AValue: TdxServerModeIntDictionaryItem);
begin
  inherited Create;
  FKey := AKey;
  FValue := AValue;
end;

{ TdxCustomServerModeLinkedDictionary }

constructor TdxServerModeLinkedDictionary.Create(ATableSize: Integer = dxDefaultDictionaryHashTableSize);
begin
  inherited Create;
  FCount := 0;
  FTableSize := ATableSize;
  SetLength(FTable, FTableSize);
  FillChar(FTable[0], FTableSize * SizeOf(TdxServerModeLinkedDictionaryItem), 0);
end;

destructor TdxServerModeLinkedDictionary.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TdxServerModeLinkedDictionary.Add(const AKey: Variant; const AValue: TdxServerModeIntDictionaryItem);
var
  AIndex: Integer;
  AItem: TdxServerModeLinkedDictionaryItem;
  AHashKey: Cardinal;
begin
  AHashKey := GetVariantHash(AKey);
  AIndex := AHashKey mod FTableSize;
  AItem := FTable[AIndex];
  if AItem = nil then
    FTable[AIndex] := TdxServerModeLinkedDictionaryItem.Create(AHashKey, AValue)
  else
    repeat
      if AItem.Next = nil then
      begin
        AItem.FNext := TdxServerModeLinkedDictionaryItem.Create(AHashKey, AValue);
        Break;
      end
      else
        AItem := AItem.Next;
    until False;
  Inc(FCount);
end;

procedure TdxServerModeLinkedDictionary.Clear;
var
  I: TdxNativeInt;
  AItem, ATemp: TdxServerModeLinkedDictionaryItem;
begin
  if Count = 0 then
    Exit;
  for I := 0 to FTableSize - 1 do
  begin
    AItem := FTable[I];
    if AItem <> nil then
    begin
      while AItem <> nil do
      begin
        ATemp := AItem;
        AItem := AItem.Next;
        ATemp.Free;
      end;
      FTable[I] := nil;
    end;
  end;
  FCount := 0;
end;

function TdxServerModeLinkedDictionary.ContainsKey(const AKey: Variant): Boolean;
begin
  Result := Find(AKey) <> nil;
end;

function TdxServerModeLinkedDictionary.TryGetValue(const AKey: Variant; out AValue: Integer): Boolean;
var
  AItem: TdxServerModeLinkedDictionaryItem;
begin
  AItem := Find(AKey);
  Result := AItem <> nil;
  if Result then
    AValue := AItem.Value.Key;
end;

function TdxServerModeLinkedDictionary.Find(const AKey: Variant): TdxServerModeLinkedDictionaryItem;
var
  AIndex: Integer;
  AHashKey: Cardinal;
  AItem: TdxServerModeLinkedDictionaryItem;
begin
  AHashKey := GetVariantHash(AKey);
  Result := nil;
  AIndex := AHashKey mod FTableSize;
  AItem := FTable[AIndex];
  while AItem <> nil do
  begin
    if (AItem.Key = AHashKey) and VarEquals(AKey, AItem.Value.Value) then
    begin
      Result := AItem;
      Break;
    end;
    AItem := AItem.Next;
  end;
end;

{ TdxServerModeCustomDictionary }

constructor TdxServerModeCustomDictionary.Create(ATableSize: Integer = dxDefaultDictionaryHashTableSize);
begin
  inherited Create;
  FIntDictionary := TdxCustomServerModeIntDictionary.Create(ATableSize);
  FLinkedDictionary := TdxServerModeLinkedDictionary.Create(ATableSize);
end;

destructor TdxServerModeCustomDictionary.Destroy;
begin
  FreeAndNil(FLinkedDictionary);
  FreeAndNil(FIntDictionary);
  inherited Destroy;
end;

procedure TdxServerModeCustomDictionary.Add(AKey: Integer; const AValue: Variant);
var
  AItem: TdxServerModeIntDictionaryItem;
begin
  AItem := FIntDictionary.Add(AKey, AValue);
  FLinkedDictionary.Add(AValue, AItem);
end;

procedure TdxServerModeCustomDictionary.Clear;
begin
  FIntDictionary.Clear;
  FLinkedDictionary.Clear;
end;

function TdxServerModeCustomDictionary.ContainsKey(AKey: Integer): Boolean;
begin
  Result := FIntDictionary.ContainsKey(AKey);
end;

function TdxServerModeCustomDictionary.GetKeyByValue(const AValue: Variant; out AKey: Integer): Boolean;
begin
  Result := FLinkedDictionary.TryGetValue(AValue, AKey);
end;

function TdxServerModeCustomDictionary.TryGetValue(AKey: Integer; out AValue: Variant): Boolean;
begin
  Result := FIntDictionary.TryGetValue(AKey, AValue);
end;

function TdxServerModeCustomDictionary.GetCount: Integer;
begin
  Result := FIntDictionary.Count;
end;

{ TdxLoadedRecordsBlock }

constructor TdxLoadedRecordsBlock.Create(AStart, AFinish: Integer);
begin
  inherited Create;
  FStart := AStart;
  FFinish := AFinish;
end;

function TdxLoadedRecordsBlock.GetStatus(AStart, AFinish: Integer): TdxBlockRelation;
begin
  if (Start > AFinish + 1) or (Finish < AStart - 1) then
    Result := brNone
  else
    if (AFinish = Finish) and (AStart = Start) then
      Result := brEquals
    else
      if (Finish >= AFinish) and (Start <= AStart) then
        Result := brGreater
      else
        if (Finish <= AFinish) and (Start >= AStart) then
          Result := brLess
        else
          if Start = AFinish + 1 then
            Result := brPrev
          else
            if Finish = AStart - 1 then
              Result := brNext
            else
              if (AFinish + 1 > Start) and (AStart - 1 < Start) then
                Result := brIntersectStart
              else
                  Result := brIntersectFinish
end;

function TdxLoadedRecordsBlock.InRange(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= Start) and (AIndex <= Finish);
end;

{ TdxLoadedRecordsHelper }

constructor TdxLoadedRecordsHelper.Create;
begin
  inherited Create;
  FList := TdxFastObjectList.Create;
end;

destructor TdxLoadedRecordsHelper.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TdxLoadedRecordsHelper.Add(AFirstRecord, ACount: Integer; AIsFromBottom: Boolean);
begin
  if ACount > 0 then
    if AIsFromBottom then
      Add(AFirstRecord - ACount + 1, AFirstRecord)
    else
      Add(AFirstRecord, AFirstRecord + ACount - 1);
end;

procedure TdxLoadedRecordsHelper.Add(AStart, AFinish: Integer);
var
  I: Integer;
  AStatus: TdxBlockRelation;
  AItem: TdxLoadedRecordsBlock;
begin
  if Count = 0 then
    FList.Add(TdxLoadedRecordsBlock.Create(AStart, AFinish))
  else
  begin
    if not FindPosition(AStart, I) then
    begin
      I := Count - 1;
      AItem := Last;
      AStatus := AItem.GetStatus(AStart, AFinish);
      if AStatus = brNone then
        if First.Start > AStart then
        begin
          FList.Insert(0, TdxLoadedRecordsBlock.Create(AStart, AFinish));
          UpdateNext(0);
        end
        else
          FList.Add(TdxLoadedRecordsBlock.Create(AStart, AFinish));
    end
    else
    begin
      AItem := Items[I];
      AStatus := AItem.GetStatus(AStart, AFinish);
      if AStatus = brNone then
      begin
        FList.Insert(I + 1, TdxLoadedRecordsBlock.Create(AStart, AFinish));
        UpdateNext(I + 1);
      end;
    end;
    case AStatus of
      brPrev, brIntersectStart:
      begin
        AItem.Start := AStart;
        if I > 0 then
          UpdateNext(I - 1);
      end;
      brIntersectFinish, brNext:
      begin
        AItem.Finish := AFinish;
        UpdateNext(I);
      end;
      brLess:
      begin
        AItem.Start := AStart;
        AItem.Finish := AFinish;
        if I > 0 then
          UpdateNext(I - 1)
        else
          UpdateNext(I);
      end;
    end;
  end;
end;

procedure TdxLoadedRecordsHelper.Clear;
begin
  FList.Clear;
end;

procedure TdxLoadedRecordsHelper.GetClosestPopulatedIndexes(AIndex: Integer; out AStartIndex, AFinishIndex: Integer);
var
  I: Integer;
  AItem: TdxLoadedRecordsBlock;
begin
  AStartIndex := -1;
  AFinishIndex := High(Integer);
  for I := 0 to FList.Count - 1 do
  begin
    AItem := Items[I];
    if AItem.InRange(AIndex) then
    begin
      AStartIndex := AIndex;
      Break;
    end;
    if (AItem.Finish < AIndex) and (AItem.Finish > AStartIndex) then
      AStartIndex := AItem.Finish;
    if (AItem.Start > AIndex) and (AItem.Start < AFinishIndex) then
      AFinishIndex := AItem.Start;
  end;
end;

function TdxLoadedRecordsHelper.FindPosition(AStart: Integer; out AItemIndex: Integer): Boolean;
var
  I, L, H, C: Integer;
begin
  AItemIndex := -1;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Items[I].Start;
    if C < AStart then
    begin
      L := I + 1;
      AItemIndex := I;
    end
    else
    begin
      H := I - 1;
      if C = AStart then
      begin
        AItemIndex := I;
        Break;
      end;
    end;
  end;
  Result := (AItemIndex >= 0) and (Items[AItemIndex].Start <= AStart);
end;

procedure TdxLoadedRecordsHelper.UpdateNext(AItemIndex: Integer);
var
  I: Integer;
  AStatus: TdxBlockRelation;
begin
  I := AItemIndex + 1;
  while Count > I do
  begin
    AStatus := Items[I].GetStatus(Items[AItemIndex].Start, Items[AItemIndex].Finish);
    if AStatus = brNone then
      Break;
    if (AStatus in [brPrev, brNext, brIntersectStart, brIntersectFinish, brGreater]) and
      (Items[AItemIndex].Finish < Items[I].Finish)
    then
      Items[AItemIndex].Finish := Items[I].Finish;
    FList.Delete(I);
  end;
end;

function TdxLoadedRecordsHelper.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TdxLoadedRecordsHelper.GetFirst: TdxLoadedRecordsBlock;
begin
  Result := TdxLoadedRecordsBlock(FList.First);
end;

function TdxLoadedRecordsHelper.GetItem(Index: Integer): TdxLoadedRecordsBlock;
begin
  Result := TdxLoadedRecordsBlock(FList[Index]);
end;

function TdxLoadedRecordsHelper.GetLast: TdxLoadedRecordsBlock;
begin
  Result := TdxLoadedRecordsBlock(FList.Last);
end;

{ TdxServerModeKeysDictionary }

constructor TdxServerModeKeysDictionary.Create(ATableSize: Integer = dxDefaultDictionaryHashTableSize);
begin
  inherited Create(ATableSize);
  FLoadedRecordsHelper := TdxLoadedRecordsHelper.Create;
end;

destructor TdxServerModeKeysDictionary.Destroy;
begin
  FreeAndNil(FLoadedRecordsHelper);
  inherited Destroy;
end;

procedure TdxServerModeKeysDictionary.Clear;
begin
  inherited Clear;
  LoadedRecordsHelper.Clear;
end;

function TdxServerModeKeysDictionary.TryGetKeyByValue(const AValue: Variant; out AKey: Integer): Boolean;
begin
  Result := GetKeyByValue(AValue, AKey);
end;

{ TdxServerModeRecordIndexDictionary }

constructor TdxServerModeLoadedKeysChunkDictionary.Create;
begin
  inherited Create(dxKeysChunkDictionaryHashTableSize);
end;

procedure TdxServerModeLoadedKeysChunkDictionary.Add(const AIndex: TdxServerModeRow; AValue: Integer);
begin
  inherited Add(AValue, AIndex);
end;

function TdxServerModeLoadedKeysChunkDictionary.ContainsKey(const AKey: TdxServerModeRow): Boolean;
var
  AIndex: Integer;
begin
  Result := GetKeyByValue(AKey, AIndex);
end;

function TdxServerModeLoadedKeysChunkDictionary.TryGetValue(const AIndex: TdxServerModeRow; out AValue: Integer): Boolean;
begin
  Result := GetKeyByValue(AIndex, AValue);
end;

{ TdxListSourceGroupInfo }

constructor TdxListSourceGroupInfo.Create(ALevel: Integer);
begin
  inherited Create;
  FLevel := ALevel;
end;

{ TdxServerModeGroupInfo }

constructor TdxServerModeGroupInfo.Create(AParent: TdxServerModeGroupInfo; const AGroupValue: Variant;
  ALevel, ATopRecordIndex: Integer);
begin
  inherited Create(ALevel);
  FParent := AParent;
  FGroupValue := AGroupValue;
  FGroupValueHash := GetVariantHash(AGroupValue);
  FTopDataRowIndex := ATopRecordIndex;
  FSummary := TdxServerModeSummaryList.Create;
end;

destructor TdxServerModeGroupInfo.Destroy;
begin
  FreeAndNil(FChildrenGroups);
  FreeAndNil(FSummary);
  inherited Destroy;
end;

procedure TdxServerModeGroupInfo.BeforeDestruction;
begin
  DoDestroy;
  inherited BeforeDestruction;
end;

procedure TdxServerModeGroupInfo.CreateChildren;
begin
  FChildrenGroups := TdxServerModeGroupInfoList.Create(Self);
end;

function TdxServerModeGroupInfo.Contains(ADataRowIndex: Integer): Boolean;
begin
  Result := (ADataRowIndex >= FTopDataRowIndex) and
    (ADataRowIndex < FTopDataRowIndex + DataRowCount);
end;

procedure TdxServerModeGroupInfo.ResetChildren;
begin
  FreeAndNil(FChildrenGroups);
end;

function TdxServerModeGroupInfo.IsEqual(const AValue: Variant; AHash: Cardinal): Boolean;

  function CanUseHash(VType1, VType2: TVarType): Boolean; inline;
  begin
    Result := (VType1 = VType2)
       or (((VType1 = varOleStr) or (VType1 = varUString)) and
           ((VType2 = varOleStr) or (VType2 = varUString)))
  end;

begin
  if CanUseHash(TVarData(FGroupValue).VType, TVarData(AValue).VType) then
    Result := (FGroupValueHash = AHash) and VarEquals(FGroupValue, AValue)
  else
    Result := VarEquals(FGroupValue, AValue);
end;

procedure TdxServerModeGroupInfo.DoDestroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
end;

{ TdxServerModeGroupInfoList }

constructor TdxServerModeGroupInfoList.Create(AParent: TdxServerModeGroupInfo);
begin
  inherited Create;
  FParent := AParent;
end;

function TdxServerModeGroupInfoList.Find(const AGroupValue: Variant): Integer;
var
  I: Integer;
  AGroupValueHash: Cardinal;
begin
  Result := -1;
  AGroupValueHash := GetVariantHash(AGroupValue);
  for I := 0 to Count - 1 do
    if Items[I].IsEqual(AGroupValue, AGroupValueHash) then
    begin
      Result := I;
      Break;
    end;
end;

procedure TdxServerModeGroupInfoList.UpdateDataRowIndexes;
var
  I: Integer;
  ATopDataRowIndex: Integer;
begin
  ATopDataRowIndex := Parent.TopDataRowIndex;
  if (Count = 0) or (Items[0].TopDataRowIndex = ATopDataRowIndex) then
    Exit;
  for I := 0 to Count - 1 do
  begin
    Items[I].FTopDataRowIndex := ATopDataRowIndex;
    Inc(ATopDataRowIndex, Items[I].DataRowCount);
  end;
end;

function TdxServerModeGroupInfoList.GetItem(Index: Integer): TdxServerModeGroupInfo;
begin
  Result := TdxServerModeGroupInfo(inherited Items[Index]);
end;

{ TdxServerModeGroupInfoData }

constructor TdxServerModeGroupInfoData.Create(const AGroupValue: Variant; AChildDataRowCount: Integer; const ASummary: TdxServerModeRow);
begin
  inherited Create;
  FGroupValue := AGroupValue;
  FDataRowCount := AChildDataRowCount;
  FSummary := ASummary;
end;

constructor TdxServerModeGroupInfoData.Create(const AGroupValue: Variant; const ASummary: TdxServerModeRow);
begin
  Create(AGroupValue, ASummary[0], ASummary);
end;

{ TdxServerModeGroupInfoDataList }

function TdxServerModeGroupInfoDataList.GetItem(Index: Integer): TdxServerModeGroupInfoData;
begin
  Result := TdxServerModeGroupInfoData(inherited Items[Index]);
end;

end.
