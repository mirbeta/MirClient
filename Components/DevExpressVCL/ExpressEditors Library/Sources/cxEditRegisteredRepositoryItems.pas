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

unit cxEditRegisteredRepositoryItems;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, dxCoreClasses, cxClasses, cxEdit;

const
  cxEditRegisteredItemsStandardVersion = -1;
  cxEditRegisteredItemsDefaultVersion = MaxInt;

type
  // TODO: notification!!!
  // TODO: BeginUpdate/EndUpdate!!!

  { TcxEditRegisteredRepositoryItems }

  TcxEditRegisteredRepositoryItemsData = record
    DataType: Integer;
    Version: Integer;
    RepositoryItem: TcxEditRepositoryItem;
  end;

  TcxEditRegisteredRepositoryItemsDataArray = array of TcxEditRegisteredRepositoryItemsData;

  TcxEditRegisteredRepositoryItems = class(TcxInterfacedPersistent,
    IUnknown, IcxEditRepositoryItemListener) // singleton
  private
    FDestroying: Boolean;
    FDefaultItems: TcxEditRegisteredRepositoryItemsDataArray;
    FItems: TcxEditRegisteredRepositoryItemsDataArray;
    function Find(const AItems: TcxEditRegisteredRepositoryItemsDataArray;
      ADataType, AVersion: Integer; AFindItemWithMaxVersion: Boolean;
      out AIndex: Integer): Boolean;
    function GetCount: Integer;
    function GetDefaultCount: Integer;
//    function GetDefaultRepositoryItem(Index: Integer): PcxEditRegisteredRepositoryItemsData;
//    function GetRepositoryItem(Index: Integer): PcxEditRegisteredRepositoryItemsData;
  protected
    { IcxEditRepositoryItemListener }
    procedure ItemRemoved(Sender: TcxEditRepositoryItem); virtual;
    procedure PropertiesChanged(Sender: TcxEditRepositoryItem); virtual;

    function Add(var AItems: TcxEditRegisteredRepositoryItemsDataArray;
      ADataType: Integer; AVersion: Integer;
      ARepositoryItem: TcxEditRepositoryItem): Integer; virtual;
    procedure Changed; virtual;
    procedure Clear;
    procedure Delete(var AItems: TcxEditRegisteredRepositoryItemsDataArray;
      AIndex: Integer);
  	function FindIndexForInsertion(
      const AItems: TcxEditRegisteredRepositoryItemsDataArray;
      ADataType, AVersion: Integer; out AIndex: Integer): Boolean;
  	function FindItemWithMaxVersion(
      const AItems: TcxEditRegisteredRepositoryItemsDataArray;
  		ADataType: Integer): Integer;
    property Count: Integer read GetCount;
    property DefaultCount: Integer read GetDefaultCount;
    property Destroying: Boolean read FDestroying write FDestroying;
//    property DefaultItems[Index: Integer]: PcxEditRegisteredRepositoryItemsData read GetDefaultRepositoryItem;
//    property Items[Index: Integer]: PcxEditRegisteredRepositoryItemsData read GetRepositoryItem;
  public
    destructor Destroy; override;
    function GetDefaultItem: TcxEditRepositoryItem; virtual;
    function GetItem(ADataType: Integer): TcxEditRepositoryItem; virtual;
    procedure RegisterDefaultItem(AVersion: Integer; ARepositoryItem: TcxEditRepositoryItem); virtual;
    procedure RegisterItem(ADataType: Integer; AVersion: Integer; ARepositoryItem: TcxEditRepositoryItem); virtual;
    procedure UnregisterDefaultItem(AVersion: Integer); virtual;
    procedure UnregisterItem(ADataType: Integer; AVersion: Integer); virtual;
  end;

implementation

uses
  Math;

{ TcxEditRegisteredRepositoryItems }

destructor TcxEditRegisteredRepositoryItems.Destroy;
begin
  Destroying := True;
  Clear;
  inherited Destroy;
end;

function TcxEditRegisteredRepositoryItems.GetDefaultItem: TcxEditRepositoryItem;
begin
  if DefaultCount > 0 then
    Result := FDefaultItems[0].RepositoryItem
  else
    Result := nil;
end;

function TcxEditRegisteredRepositoryItems.GetItem(
  ADataType: Integer): TcxEditRepositoryItem;
var
  AIndex: Integer;
begin
  AIndex := FindItemWithMaxVersion(FItems, ADataType);
  if AIndex <> -1 then
    Result := FItems[AIndex].RepositoryItem
  else
    Result := GetDefaultItem;
end;

procedure TcxEditRegisteredRepositoryItems.RegisterDefaultItem(
  AVersion: Integer; ARepositoryItem: TcxEditRepositoryItem);
begin
  Add(FDefaultItems, 0, AVersion, ARepositoryItem);
end;

procedure TcxEditRegisteredRepositoryItems.RegisterItem(ADataType: Integer;
  AVersion: Integer; ARepositoryItem: TcxEditRepositoryItem);
begin
  Add(FItems, ADataType, AVersion, ARepositoryItem);
end;

procedure TcxEditRegisteredRepositoryItems.UnregisterDefaultItem(AVersion: Integer);
var
  AIndex: Integer;
begin
  if FindIndexForInsertion(FDefaultItems, 0, AVersion, AIndex) then
    Delete(FDefaultItems, AIndex);
end;

procedure TcxEditRegisteredRepositoryItems.UnregisterItem(ADataType: Integer;
  AVersion: Integer);
var
  AIndex: Integer;
begin
  if FindIndexForInsertion(FItems, ADataType, AVersion, AIndex) then
    Delete(FItems, AIndex);
end;

procedure TcxEditRegisteredRepositoryItems.ItemRemoved(Sender: TcxEditRepositoryItem);
var
  I: Integer;
begin
  for I := DefaultCount - 1 downto 0 do
    if FDefaultItems[I].RepositoryItem = Sender then
      Delete(FDefaultItems, I);
  for I := Count - 1 downto 0 do
    if FItems[I].RepositoryItem = Sender then
      Delete(FItems, I);
end;

procedure TcxEditRegisteredRepositoryItems.PropertiesChanged(Sender: TcxEditRepositoryItem);
begin
  Changed;
end;

function TcxEditRegisteredRepositoryItems.Add(
  var AItems: TcxEditRegisteredRepositoryItemsDataArray; ADataType: Integer;
  AVersion: Integer; ARepositoryItem: TcxEditRepositoryItem): Integer;
begin
  if FindIndexForInsertion(AItems, ADataType, AVersion, Result) then
    Exit;
  SetLength(AItems, Length(AItems) + 1);
  if Result < Length(AItems) - 1 then
    Move(AItems[Result], AItems[Result + 1],
      (Length(AItems) - Result - 1) * SizeOf(TcxEditRegisteredRepositoryItemsData));
  AItems[Result].DataType := ADataType;
  AItems[Result].Version := AVersion;
  AItems[Result].RepositoryItem := ARepositoryItem;
  ARepositoryItem.AddListener(Self);
  Changed;
end;

procedure TcxEditRegisteredRepositoryItems.Changed;
begin
  if Destroying then Exit;
  // TODO: notification
end;

procedure TcxEditRegisteredRepositoryItems.Clear;
begin
  while DefaultCount > 0 do
    Delete(FDefaultItems, DefaultCount - 1);
  while Count > 0 do
    Delete(FItems, Count - 1);
end;

procedure TcxEditRegisteredRepositoryItems.Delete(
  var AItems: TcxEditRegisteredRepositoryItemsDataArray; AIndex: Integer);
begin
  AItems[AIndex].RepositoryItem.RemoveListener(Self);
  if AIndex < Length(AItems) - 1 then
    Move(AItems[AIndex + 1], AItems[AIndex],
      (Length(AItems) - AIndex - 1) * SizeOf(TcxEditRegisteredRepositoryItemsData));
  SetLength(AItems, Length(AItems) - 1);
  Changed;
end;

function TcxEditRegisteredRepositoryItems.FindIndexForInsertion(
  const AItems: TcxEditRegisteredRepositoryItemsDataArray;
  ADataType, AVersion: Integer; out AIndex: Integer): Boolean;
begin
	Result := Find(AItems, ADataType, AVersion, False, AIndex);
end;

function TcxEditRegisteredRepositoryItems.FindItemWithMaxVersion(
  const AItems: TcxEditRegisteredRepositoryItemsDataArray;
  ADataType: Integer): Integer;
var
  AIndex: Integer;
begin
  if Find(AItems, ADataType, 0, True, AIndex) then
    Result := AIndex
  else
    Result := -1;
end;

function TcxEditRegisteredRepositoryItems.Find(
  const AItems: TcxEditRegisteredRepositoryItemsDataArray;
  ADataType, AVersion: Integer; AFindItemWithMaxVersion: Boolean;
  out AIndex: Integer): Boolean;

  function Compare(const AItem1, AItem2: TcxEditRegisteredRepositoryItemsData): Integer;
  begin
    Result := Sign(AItem1.DataType - AItem2.DataType);
    if (Result = 0) and not AFindItemWithMaxVersion then
      Result := Sign(AItem1.Version - AItem2.Version);
  end;

var
  AItem, ATempItem: TcxEditRegisteredRepositoryItemsData;
  AItemCount, H, I, L: Integer;
begin
  AItem.DataType := ADataType;
  AItem.Version := AVersion;
  AItemCount := Length(AItems);
  L := 0;
  H := AItemCount - 1;
  I := 0;
  while L <= H do
  begin
    I := (L + H) div 2;
    ATempItem := AItems[I];
    case Compare(ATempItem, AItem) of
      -1:
        L := I + 1;
      0:
        Break;
    else
      H := I - 1
    end;
  end;

  if L > H then
    AIndex := L
  else
    AIndex := I;

  if AFindItemWithMaxVersion then
    while (AIndex < AItemCount - 1) and (AItems[AIndex + 1].DataType = AItem.DataType) do
      Inc(AIndex);

  Result := (AIndex < AItemCount) and (AItems[AIndex].DataType = AItem.DataType) and
    (AFindItemWithMaxVersion or (AItems[AIndex].Version = AItem.Version));
end;

function TcxEditRegisteredRepositoryItems.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TcxEditRegisteredRepositoryItems.GetDefaultCount: Integer;
begin
  Result := Length(FDefaultItems);
end;

end.
