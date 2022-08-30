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

unit cxEditDataRegisteredRepositoryItems;

{$I cxVer.inc}

interface

uses
  SysUtils, cxClasses, cxDataStorage, cxEdit, cxEditRegisteredRepositoryItems,
  cxEditRepositoryItems;

type
  { TcxEditDataRegisteredRepositoryItems }

  TcxEditDataRegisteredRepositoryItems = class(TcxEditRegisteredRepositoryItems)
  public
    function GetItem(AValueTypeClass: TcxValueTypeClass): TcxEditRepositoryItem; reintroduce;
    procedure RegisterItem(AValueTypeClass: TcxValueTypeClass; AVersion: Integer; ARepositoryItem: TcxEditRepositoryItem); reintroduce;
    procedure UnregisterItem(AValueTypeClass: TcxValueTypeClass; AVersion: Integer); reintroduce;
  end;

function GetDefaultEditDataRepositoryItems: TcxEditDataRegisteredRepositoryItems;

implementation

var
  DefaultDataEditRepositoryItems: TcxEditDataRegisteredRepositoryItems;

function GetDefaultEditDataRepositoryItems: TcxEditDataRegisteredRepositoryItems;
begin
  if DefaultDataEditRepositoryItems = nil then
    DefaultDataEditRepositoryItems := TcxEditDataRegisteredRepositoryItems.Create(nil);
  Result := DefaultDataEditRepositoryItems;
end;

procedure CreateDefaultRepositoryItems;
begin
  GetDefaultEditDataRepositoryItems.RegisterDefaultItem(cxEditRegisteredItemsStandardVersion,
    GetDefaultEditRepository.CreateItem(TcxEditRepositoryTextItem));
  GetDefaultEditDataRepositoryItems.RegisterItem(TcxBooleanValueType, cxEditRegisteredItemsStandardVersion,
    GetDefaultEditRepository.CreateItem(TcxEditRepositoryCheckBoxItem));
  GetDefaultEditDataRepositoryItems.RegisterItem(TcxCurrencyValueType, cxEditRegisteredItemsStandardVersion,
    GetDefaultEditRepository.CreateItem(TcxEditRepositoryCurrencyItem));
  GetDefaultEditDataRepositoryItems.RegisterItem(TcxDateTimeValueType, cxEditRegisteredItemsStandardVersion,
    GetDefaultEditRepository.CreateItem(TcxEditRepositoryDateItem));
  GetDefaultEditDataRepositoryItems.RegisterItem(TcxFMTBcdValueType, cxEditRegisteredItemsStandardVersion,
    GetDefaultEditRepository.CreateItem(TcxEditRepositoryCurrencyItem));
  GetDefaultEditDataRepositoryItems.RegisterItem(TcxSQLTimeStampValueType, cxEditRegisteredItemsStandardVersion,
    GetDefaultEditRepository.CreateItem(TcxEditRepositoryDateItem));
end;

procedure DeleteDefaultRepositoryItems;
begin
  GetDefaultEditDataRepositoryItems.UnregisterDefaultItem(cxEditRegisteredItemsStandardVersion);
  GetDefaultEditDataRepositoryItems.UnregisterItem(TcxBooleanValueType, cxEditRegisteredItemsStandardVersion);
  GetDefaultEditDataRepositoryItems.UnregisterItem(TcxCurrencyValueType, cxEditRegisteredItemsStandardVersion);
  GetDefaultEditDataRepositoryItems.UnregisterItem(TcxDateTimeValueType, cxEditRegisteredItemsStandardVersion);
  GetDefaultEditDataRepositoryItems.UnregisterItem(TcxFMTBcdValueType, cxEditRegisteredItemsStandardVersion);
  GetDefaultEditDataRepositoryItems.UnregisterItem(TcxSQLTimeStampValueType, cxEditRegisteredItemsStandardVersion);
end;

function GetClassID(AClass: TClass): Integer;
begin
  Result := Integer(AClass);
end;

{ TcxEditDataRegisteredRepositoryItems }

function TcxEditDataRegisteredRepositoryItems.GetItem(
  AValueTypeClass: TcxValueTypeClass): TcxEditRepositoryItem;
begin
  Result := inherited GetItem(GetClassID(AValueTypeClass));
end;

procedure TcxEditDataRegisteredRepositoryItems.RegisterItem(AValueTypeClass: TcxValueTypeClass;
  AVersion: Integer; ARepositoryItem: TcxEditRepositoryItem);
begin
  inherited RegisterItem(GetClassID(AValueTypeClass), AVersion, ARepositoryItem);
end;

procedure TcxEditDataRegisteredRepositoryItems.UnregisterItem(AValueTypeClass: TcxValueTypeClass;
  AVersion: Integer);
begin
  inherited UnregisterItem(GetClassID(AValueTypeClass), AVersion);
end;

initialization
  CreateDefaultRepositoryItems;

finalization
  DeleteDefaultRepositoryItems;
  FreeAndNil(DefaultDataEditRepositoryItems);

end.
