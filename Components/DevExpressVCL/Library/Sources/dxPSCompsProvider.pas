{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSCompsProvider;

interface

{$I cxVer.inc}

uses
  Classes, dxPSCore, dxPSSngltn;

type
  TdxComponentItem = class
  private
    FCaption: string;
    FComponent: TComponent;
    FDescription: string;
  public
    constructor Create(AComponent: TComponent; const ACaption, ADescription: string);
    property Caption: string read FCaption write FCaption;
    property Component: TComponent read FComponent write FComponent;
    property Description: string read FDescription write FDescription;
  end;

  TdxPSGetComponentOption = (gcoExcludeExisting, gcoExcludeOutOfActiveForm, gcoHideCustomContainers);
  TdxPSGetComponentOptions = set of TdxPSGetComponentOption;

  TdxPSComponentsProviderClass = class of TAbstractdxPSComponentsProvider;

  TAbstractdxPSComponentsProvider = class
  public
    constructor Create; virtual;
    procedure GetComponents(AComponentPrinter: TdxComponentPrinter; AReportLink: TBasedxReportLink;
      AComponents: TStrings; AnOptions: TdxPSGetComponentOptions); virtual; abstract;

    class procedure Register;
    class procedure Unregister;
  end;

  TdxPSComponentProvidersFactory = class(TBasedxPSSingleton)
  private
    FItems: TList;
    function GetActiveProvider: TAbstractdxPSComponentsProvider;
    function GetCount: Integer;
    function GetProvider(Index: Integer): TAbstractdxPSComponentsProvider;
  protected
    procedure FinalizeInstance; override;
    procedure InitializeInstance; override;
  public
    class function Instance: TdxPSComponentProvidersFactory; reintroduce; overload;

    procedure GetComponents(AComponentPrinter: TdxComponentPrinter; AReportLink: TBasedxReportLink;
      AComponents: TStrings; AnOptions: TdxPSGetComponentOptions);
    function IndexOf(AProviderClass: TdxPSComponentsProviderClass): Integer;
    procedure RegisterProvider(AProviderClass: TdxPSComponentsProviderClass);
    procedure UnregisterProvider(AProviderClass: TdxPSComponentsProviderClass);

    property ActiveProvider: TAbstractdxPSComponentsProvider read GetActiveProvider;
    property Count: Integer read GetCount;
    property Providers[Index: Integer]: TAbstractdxPSComponentsProvider read GetProvider; default;
  end;

function dxPSCreateComponentItem(AComponent: TComponent; const ACaption, ADescription: string): TdxComponentItem;
function dxPSComponentProvidersFactory: TdxPSComponentProvidersFactory;

implementation

uses
  SysUtils, dxPSUtl;

function dxPSCreateComponentItem(AComponent: TComponent; const ACaption, ADescription: string): TdxComponentItem;
begin
  Result := TdxComponentItem.Create(AComponent, ACaption, ADescription);
end;

{ TdxComponentItem }

constructor TdxComponentItem.Create(AComponent: TComponent; const ACaption, ADescription: string);
begin
  inherited Create;
  FComponent := AComponent;
  FCaption := ACaption;
  FDescription := ADescription;
end;

{ TdxPSComponentProvidersFactory }

function dxPSComponentProvidersFactory: TdxPSComponentProvidersFactory;
begin
  Result := TdxPSComponentProvidersFactory.Instance;
end;

class function TdxPSComponentProvidersFactory.Instance: TdxPSComponentProvidersFactory;
begin
  Result := inherited Instance as TdxPSComponentProvidersFactory;
end;

procedure TdxPSComponentProvidersFactory.GetComponents(AComponentPrinter: TdxComponentPrinter;
  AReportLink: TBasedxReportLink; AComponents: TStrings; AnOptions: TdxPSGetComponentOptions);
begin
  if ActiveProvider <> nil then
    ActiveProvider.GetComponents(AComponentPrinter, AReportLink, AComponents, AnOptions);
end;

function TdxPSComponentProvidersFactory.IndexOf(AProviderClass: TdxPSComponentsProviderClass): Integer;
begin
  for Result := 0 to Count - 1 do
    if Providers[Result].ClassType = AProviderClass then
      Exit;
  Result := -1;
end;

procedure TdxPSComponentProvidersFactory.RegisterProvider(AProviderClass: TdxPSComponentsProviderClass);
begin
  if IndexOf(AProviderClass) = -1 then
    FItems.Add(AProviderClass.Create);
end;

procedure TdxPSComponentProvidersFactory.UnregisterProvider(AProviderClass: TdxPSComponentsProviderClass);
var
  Index: Integer;
begin
  Index := IndexOf(AProviderClass);
  if Index <> -1 then
  begin
    Providers[Index].Free;
    FItems.Delete(Index);
  end;
end;

procedure TdxPSComponentProvidersFactory.FinalizeInstance;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do Providers[I].Free;
  FreeAndNil(FItems);
  inherited;
end;

procedure TdxPSComponentProvidersFactory.InitializeInstance;
begin
  inherited;
  FItems := TList.Create;
end;

function TdxPSComponentProvidersFactory.GetActiveProvider: TAbstractdxPSComponentsProvider;
begin
  if Count <> 0 then
    Result := Providers[Count - 1]
  else
    Result := nil;
end;

function TdxPSComponentProvidersFactory.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxPSComponentProvidersFactory.GetProvider(Index: Integer): TAbstractdxPSComponentsProvider;
begin
  Result := TAbstractdxPSComponentsProvider(FItems[Index]);
end;

{ TAbstractdxPSComponentsProvider }

constructor TAbstractdxPSComponentsProvider.Create;
begin
  inherited Create;
end;

class procedure TAbstractdxPSComponentsProvider.Register;
begin
  dxPSComponentProvidersFactory.RegisterProvider(Self);
end;

class procedure TAbstractdxPSComponentsProvider.Unregister;
begin
  dxPSComponentProvidersFactory.UnregisterProvider(Self);
end;

end.

