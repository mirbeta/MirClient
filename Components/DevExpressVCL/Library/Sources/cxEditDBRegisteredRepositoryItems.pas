{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit cxEditDBRegisteredRepositoryItems;

{$I cxVer.inc}

interface

uses
  Variants,
  SysUtils, Classes, DB, Generics.Collections, cxClasses, cxGraphics, cxDropDownEdit, cxEdit,
  cxDBEditRepository, cxEditRegisteredRepositoryItems, cxEditRepositoryItems, dxCore;

type
  { TcxEditDBRegisteredRepositoryItems }

  TcxEditDBRegisteredRepositoryItems = class(TcxEditRegisteredRepositoryItems)
  private
    FCurrencyItems: TcxEditRegisteredRepositoryItemsDataArray;
    FLookupItems: TcxEditRegisteredRepositoryItemsDataArray;
    FUnshareableItems: TDictionary<TObject, TcxEditRepositoryItem>;
    function GetCurrencyCount: Integer;
    function GetLookupCount: Integer;
  protected
    { IcxEditRepositoryItemListener }
    procedure ItemRemoved(Sender: TcxEditRepositoryItem); override;

    function Add(var AItems: TcxEditRegisteredRepositoryItemsDataArray;
      AFieldType: TFieldType; AVersion: Integer;
      ARepositoryItem: TcxEditRepositoryItem): Integer; reintroduce;
    procedure Clear;
    property CurrencyCount: Integer read GetCurrencyCount;
    property LookupCount: Integer read GetLookupCount;
  public
    destructor Destroy; override;
    function GetCurrencyItem: TcxEditRepositoryItem;
    function GetItem(AFieldType: TFieldType): TcxEditRepositoryItem; reintroduce;
    function GetItemByField(AField: TField): TcxEditRepositoryItem;
    function GetItemByDataBinding(AField: TField; ADataBinding: TObject): TcxEditRepositoryItem;
    function GetLookupItem: TcxEditRepositoryItem;
    procedure RegisterItem(AFieldType: TFieldType; AVersion: Integer; ARepositoryItem: TcxEditRepositoryItem); reintroduce;
    procedure RegisterCurrencyItem(AVersion: Integer; ARepositoryItem: TcxEditRepositoryItem);
    procedure RegisterLookupItem(AVersion: Integer; ARepositoryItem: TcxEditRepositoryItem);
    procedure UnregisterItem(AFieldType: TFieldType; AVersion: Integer); reintroduce;
    procedure UnregisterCurrencyItem(AVersion: Integer);
    procedure UnregisterLookupItem(AVersion: Integer);
  end;

function GetDefaultEditDBRepositoryItems: TcxEditDBRegisteredRepositoryItems;

implementation

uses
  TypInfo, dxGDIPlusAPI;

var
  DefaultDBEditRepositoryItems: TcxEditDBRegisteredRepositoryItems;
  DBRepositoryCurrencyItemClass: TcxEditRepositoryItemClass = TcxEditRepositoryCurrencyItem;
  DBRepositoryItemClasses: array[TFieldType] of TcxEditRepositoryItemClass = (
    nil,                                // ftUnknown
    nil,                                // ftString
    nil,                                // ftSmallint
    nil,                                // ftInteger
    nil,                                // ftWord
    TcxEditRepositoryCheckBoxItem,      // ftBoolean
    nil,                                // ftFloat
    nil,                                // ftCurrency
    nil,                                // ftBCD
    TcxEditRepositoryDateItem,          // ftDate
    TcxEditRepositoryTimeItem,          // ftTime
    TcxEditRepositoryDateItem,          // ftDateTime
    TcxEditRepositoryBlobItem,          // ftBytes
    TcxEditRepositoryBlobItem,          // ftVarBytes
    nil,                                // ftAutoInc
    TcxEditRepositoryBlobItem,          // ftBlob
    TcxEditRepositoryMemoItem,          // ftMemo
    TcxEditRepositoryImageItem,         // ftGraphic
    TcxEditRepositoryBlobItem,          // ftFmtMemo
    TcxEditRepositoryBlobItem,          // ftParadoxOle
    TcxEditRepositoryBlobItem,          // ftDBaseOle
    TcxEditRepositoryBlobItem,          // ftTypedBinary
    nil,                                // ftCursor
    nil,                                // ftFixedChar
    nil,                                // ftWideString
    nil,                                // ftLargeint
    nil,                                // ftADT
    nil,                                // ftArray
    nil,                                // ftReference
    nil,                                // ftDataSet
    nil,                                // ftOraBlob
    TcxEditRepositoryMemoItem,          // ftOraClob
    nil,                                // ftVariant
    nil,                                // ftInterface
    nil,                                // ftIDispatch
    nil,                                // ftGuid
    TcxEditRepositoryDateItem,          // ftTimeStamp
    nil                                 // ftFMTBcd
    , nil,                              // ftFixedWideChar
    TcxEditRepositoryMemoItem,          // ftWideMemo
    nil,                                // ftOraTimeStamp
    nil                                 // ftOraInterval
    , nil,                              // ftLongWord
    nil,                                // ftShortint
    nil,                                // ftByte
    nil,                                // ftExtended
    nil,                                // ftConnection
    nil,                                // ftParams
    nil                                 // ftStream
    , nil,                              // ftTimeStampOffset
    nil,                                // ftObject,
    nil                                 // ftSingle
  );

procedure CreateDefaultRepositoryItems;
var
  I: TFieldType;
  ALookupComboBoxItem: TcxEditRepositoryLookupComboBoxItem;
begin
  // Simple
  GetDefaultEditDBRepositoryItems.RegisterDefaultItem(cxEditRegisteredItemsStandardVersion,
    GetDefaultEditRepository.CreateItem(TcxEditRepositoryMaskItem));
  // Currency
  GetDefaultEditDBRepositoryItems.RegisterCurrencyItem(cxEditRegisteredItemsStandardVersion,
    GetDefaultEditRepository.CreateItem(DBRepositoryCurrencyItemClass));
  // Lookup
  ALookupComboBoxItem := GetDefaultEditRepository.CreateItem(TcxEditRepositoryLookupComboBoxItem) as TcxEditRepositoryLookupComboBoxItem;
  ALookupComboBoxItem.Properties.ListOptions.GridLines := glNone;
  ALookupComboBoxItem.Properties.DropDownListStyle := lsEditFixedList;
  ALookupComboBoxItem.Properties.ListOptions.ShowHeader := False;
  GetDefaultEditDBRepositoryItems.RegisterLookupItem(cxEditRegisteredItemsStandardVersion,
    ALookupComboBoxItem);
  // Misc
  for I := Low(TFieldType) to High(TFieldType) do
    if DBRepositoryItemClasses[I] <> nil then
      GetDefaultEditDBRepositoryItems.RegisterItem(I, cxEditRegisteredItemsStandardVersion,
        GetDefaultEditRepository.CreateItem(DBRepositoryItemClasses[I]));
end;

procedure DeleteDefaultRepositoryItems;
var
  I: TFieldType;
begin
  GetDefaultEditDBRepositoryItems.UnregisterDefaultItem(cxEditRegisteredItemsStandardVersion);
//  GetDefaultEditDBRepositoryItems.UnregisterLookupItem(cxEditRegisteredItemsStandardVersion);
  for I := Low(TFieldType) to High(TFieldType) do
    if DBRepositoryItemClasses[I] <> nil then
      GetDefaultEditDBRepositoryItems.UnregisterItem(I, cxEditRegisteredItemsStandardVersion);
end;

function GetDefaultEditDBRepositoryItems: TcxEditDBRegisteredRepositoryItems;
begin
  if DefaultDBEditRepositoryItems = nil then
    DefaultDBEditRepositoryItems := TcxEditDBRegisteredRepositoryItems.Create(nil);
  Result := DefaultDBEditRepositoryItems;
end;

{ TcxEditDBRegisteredRepositoryItems }

destructor TcxEditDBRegisteredRepositoryItems.Destroy;
begin
  Destroying := True;
  Clear;
  FreeAndNil(FUnshareableItems);
  inherited Destroy;
end;

function TcxEditDBRegisteredRepositoryItems.GetCurrencyItem: TcxEditRepositoryItem;
begin
  if CurrencyCount > 0 then
    Result := FCurrencyItems[0].RepositoryItem
  else
    Result := GetDefaultItem;
end;

function TcxEditDBRegisteredRepositoryItems.GetItem(
  AFieldType: TFieldType): TcxEditRepositoryItem;
begin
  Result := inherited GetItem(Integer(AFieldType));
end;

function TcxEditDBRegisteredRepositoryItems.GetItemByField(
  AField: TField): TcxEditRepositoryItem;

  function IsCurrency(AField: TField): Boolean;
  var
    V: Variant;
  begin
    Result := False;
    if IsPublishedProp(AField, 'currency') then
    begin
      V := GetPropValue(AField, 'currency', False);
      if (VarType(V) = varBoolean) and V then
        Result := True;
    end;
  end;

begin
  if Assigned(AField) then
  begin
    if AField.Lookup then
      Result := GetLookupItem
    else
    begin
      if (AField is TNumericField) and IsCurrency(AField) then
        Result := GetCurrencyItem
      else
        Result := GetItem(AField.DataType);
    end;
  end
  else
    Result := GetDefaultItem;
end;

function TcxEditDBRegisteredRepositoryItems.GetItemByDataBinding(
  AField: TField; ADataBinding: TObject): TcxEditRepositoryItem;
var
  APrevResult: TcxEditRepositoryItem;
begin
  Result := GetItemByField(AField);
  if not Result.Properties.AllowRepositorySharing then
  begin
    if FUnshareableItems = nil then
      FUnshareableItems := TObjectDictionary<TObject, TcxEditRepositoryItem>.Create([doOwnsValues]);
    APrevResult := Result;
    if not FUnshareableItems.TryGetValue(ADataBinding, Result) then
    begin
      Result := GetDefaultEditRepository.CreateItem(TcxEditRepositoryItemClass(APrevResult.ClassType));
      Result.Properties.Assign(APrevResult.Properties);
      FUnshareableItems.Add(ADataBinding, Result);
    end;
  end;
end;

function TcxEditDBRegisteredRepositoryItems.GetLookupItem: TcxEditRepositoryItem;
begin
  if LookupCount > 0 then
    Result := FLookupItems[0].RepositoryItem
  else
    Result := GetDefaultItem;
end;

procedure TcxEditDBRegisteredRepositoryItems.RegisterItem(
  AFieldType: TFieldType; AVersion: Integer;
  ARepositoryItem: TcxEditRepositoryItem);
begin
  inherited RegisterItem(Integer(AFieldType), AVersion, ARepositoryItem);
end;

procedure TcxEditDBRegisteredRepositoryItems.RegisterCurrencyItem(
  AVersion: Integer; ARepositoryItem: TcxEditRepositoryItem);
begin
  Add(FCurrencyItems, ftUnknown, AVersion, ARepositoryItem);
end;

procedure TcxEditDBRegisteredRepositoryItems.RegisterLookupItem(
  AVersion: Integer; ARepositoryItem: TcxEditRepositoryItem);
begin
  Add(FLookupItems, ftUnknown, AVersion, ARepositoryItem);
end;

procedure TcxEditDBRegisteredRepositoryItems.UnregisterItem(
  AFieldType: TFieldType; AVersion: Integer);
begin
  inherited UnregisterItem(Integer(AFieldType), AVersion);
end;

procedure TcxEditDBRegisteredRepositoryItems.UnregisterCurrencyItem(
  AVersion: Integer);
var
  AIndex: Integer;
begin
  if FindIndexForInsertion(FCurrencyItems, Integer(ftUnknown), AVersion, AIndex) then
    Delete(FCurrencyItems, AIndex);
end;

procedure TcxEditDBRegisteredRepositoryItems.UnregisterLookupItem(
  AVersion: Integer);
var
  AIndex: Integer;
begin
  if FindIndexForInsertion(FLookupItems, Integer(ftUnknown), AVersion, AIndex) then
    Delete(FLookupItems, AIndex);
end;

procedure TcxEditDBRegisteredRepositoryItems.ItemRemoved(
  Sender: TcxEditRepositoryItem);
var
  I: Integer;
begin
  inherited ItemRemoved(Sender);
  for I := LookupCount - 1 downto 0 do
    if FLookupItems[I].RepositoryItem = Sender then
      Delete(FLookupItems, I);
  for I := CurrencyCount - 1 downto 0 do
    if FCurrencyItems[I].RepositoryItem = Sender then
      Delete(FCurrencyItems, I);
end;

function TcxEditDBRegisteredRepositoryItems.Add(
  var AItems: TcxEditRegisteredRepositoryItemsDataArray; AFieldType: TFieldType;
  AVersion: Integer; ARepositoryItem: TcxEditRepositoryItem): Integer;
begin
  Result := inherited Add(AItems, Integer(AFieldType), AVersion, ARepositoryItem);
end;

procedure TcxEditDBRegisteredRepositoryItems.Clear;
begin
  inherited Clear;
  while LookupCount > 0 do
    Delete(FLookupItems, LookupCount - 1);
  while CurrencyCount > 0 do
    Delete(FCurrencyItems, CurrencyCount - 1);
  if FUnshareableItems <> nil then
    FUnshareableItems.Clear;
end;

function TcxEditDBRegisteredRepositoryItems.GetCurrencyCount: Integer;
begin
  Result := Length(FCurrencyItems);
end;

function TcxEditDBRegisteredRepositoryItems.GetLookupCount: Integer;
begin
  Result := Length(FLookupItems);
end;

initialization
  dxUnitsLoader.AddUnit(@CreateDefaultRepositoryItems, @DeleteDefaultRepositoryItems);

finalization
  dxUnitsLoader.RemoveUnit(@DeleteDefaultRepositoryItems);
  FreeAndNil(DefaultDBEditRepositoryItems);

end.
