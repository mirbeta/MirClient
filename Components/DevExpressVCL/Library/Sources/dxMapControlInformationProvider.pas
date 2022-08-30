{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
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

unit dxMapControlInformationProvider;

interface

{$I cxVer.inc}

uses
  Types, Classes, SysUtils, Graphics, Math,
  dxCoreClasses, cxGraphics, cxClasses, cxGeometry;

type
  TdxMapControlInformationProvider = class(TcxComponentCollectionItem)
  protected
    procedure DoAssign(Source: TPersistent); virtual;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
  public
    procedure Assign(Source: TPersistent); override;
  end;
  TdxMapControlInformationProviderClass = class of TdxMapControlInformationProvider;

  TdxMapControlInformationProviders = class(TcxComponentCollection)
  private
    function GetItem(Index: Integer): TdxMapControlInformationProvider;
    procedure SetItem(Index: Integer; Value: TdxMapControlInformationProvider);
  protected
    function GetItemPrefixName: string; override;
    procedure SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1); override;
  public
    constructor Create(AParentComponent: TComponent; AItemClass: TcxComponentCollectionItemClass); override;
    destructor Destroy; override;
    function Add(AItemClass: TdxMapControlInformationProviderClass): TdxMapControlInformationProvider;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TdxMapControlInformationProvider read GetItem write SetItem; default;
  end;

implementation

uses
  dxMapControl;

{ TdxMapControlInformationProvider }

procedure TdxMapControlInformationProvider.Assign(Source: TPersistent);
begin
  if Source is TdxMapControlInformationProvider then
  begin
    Collection.BeginUpdate;
    try
      DoAssign(Source);
    finally
      Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TdxMapControlInformationProvider.DoAssign(Source: TPersistent);
begin
// do nothing
end;

function TdxMapControlInformationProvider.GetCollectionFromParent(
  AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxCustomMapControl).InformationProviders;
end;

{ TdxMapControlInformationProviders }

constructor TdxMapControlInformationProviders.Create(
  AParentComponent: TComponent; AItemClass: TcxComponentCollectionItemClass);
begin
  inherited Create(AParentComponent, AItemClass);

end;

destructor TdxMapControlInformationProviders.Destroy;
begin

  inherited Destroy;
end;

function TdxMapControlInformationProviders.Add(
  AItemClass: TdxMapControlInformationProviderClass): TdxMapControlInformationProvider;
begin
  Result := AItemClass.Create(ParentComponent.Owner);
  Result.SetParentComponent(ParentComponent);
  SetItemName(Result);
end;

procedure TdxMapControlInformationProviders.Assign(Source: TPersistent);
var
  I: Integer;
  AItem: TdxMapControlInformationProvider;
begin
  if Source is TdxMapControlInformationProviders then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TdxMapControlInformationProviders(Source).Count - 1 do
      begin
        AItem := TdxMapControlInformationProviders(Source).Items[I];
        Add(TdxMapControlInformationProviderClass(AItem.ClassType)).Assign(AItem);
      end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TdxMapControlInformationProviders.GetItemPrefixName: string;
begin
  Result := 'TdxMapControl';
end;

procedure TdxMapControlInformationProviders.SetItemName(
  AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1);
begin
  if csDesigning in ParentComponent.ComponentState then
    inherited SetItemName(AItem, ABaseIndex);
end;

function TdxMapControlInformationProviders.GetItem(
  Index: Integer): TdxMapControlInformationProvider;
begin
  Result := inherited GetItem(Index) as TdxMapControlInformationProvider;
end;

procedure TdxMapControlInformationProviders.SetItem(Index: Integer;
  Value: TdxMapControlInformationProvider);
begin
  inherited SetItem(Index, Value);
end;

end.
