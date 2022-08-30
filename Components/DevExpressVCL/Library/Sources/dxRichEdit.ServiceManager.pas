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

unit dxRichEdit.ServiceManager;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, dxCore, dxCoreClasses, Generics.Defaults, Generics.Collections;

type
  TdxServiceManager = class;

  { IdxServiceProvider }

  TdxServiceType = TGUID;

  IdxServiceProvider = interface
  ['{1188A15C-3C34-4C64-8C99-A43CED18F4C7}']
    function GetService(const AServiceType: TdxServiceType): IInterface;
  end;

  { IdxServiceCreatorCallback }

  IdxServiceCreatorCallback = interface
  ['{0DC56176-9232-4147-A3C2-C25C1C632888}']
    function GetService(const AManager: TdxServiceManager; const AServiceType: TdxServiceType): IInterface;
    function GetSelf: IInterface;
  end;

  { IdxServiceContainer }

  IdxServiceContainer = interface(IdxServiceProvider)
  ['{4FE3F549-537D-4E39-B76A-CC4784F83BB9}']
    procedure AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface); overload;
    procedure AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface; APromote: Boolean); overload;
    procedure AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback); overload;
    procedure AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback; APromote: Boolean); overload;
    procedure RemoveService(const AServiceType: TdxServiceType); overload;
    procedure RemoveService(const AServiceType: TdxServiceType; APromote: Boolean); overload;
  end;

  { TdxServiceManager }

  TdxServiceManager = class(TcxIUnknownObject, IdxServiceContainer, IdxServiceProvider)
  strict private
    FServices: TObjectDictionary<TdxServiceType, IInterface>;
    FServiceListChanged: TdxNotifyEventHandler;
  protected
    procedure RaiseServiceListChanged;
  public
    constructor Create;
    destructor Destroy; override;

    function ServiceExists(const AServiceType: TdxServiceType): Boolean;

    //IdxServiceProvider
    function GetService(const AServiceType: TdxServiceType): IInterface;

    //IdxServiceContainer
    procedure AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface); overload;
    procedure AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface; APromote: Boolean); overload;
    procedure AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback); overload;
    procedure AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback; APromote: Boolean); overload;
    procedure RemoveService(const AServiceType: TdxServiceType); overload;
    procedure RemoveService(const AServiceType: TdxServiceType; APromote: Boolean); overload;

    property Services: TObjectDictionary<TdxServiceType, IInterface> read FServices;
    property ServiceListChanged: TdxNotifyEventHandler read FServiceListChanged;
  end;

  TdxServiceUtils<T: IUnknown> = class
  protected
    class function GetGuid: TGUID; static;
    class function GetService(const AProvider: IdxServiceProvider): T; static;
    class function ReplaceService(const AContainer: IdxServiceContainer; const ANewService: IInterface): T; static;
  end;

implementation

uses
  TypInfo;

{ TdxServiceManager }

constructor TdxServiceManager.Create;
begin
  inherited Create;
  FServices := TObjectDictionary<TdxServiceType, IInterface>.Create;
end;

destructor TdxServiceManager.Destroy;
begin
  FreeAndNil(FServices);
  inherited Destroy;
end;

function TdxServiceManager.GetService(const AServiceType: TdxServiceType): IInterface;
var
  ACallback: IdxServiceCreatorCallback;
begin
  if Services.TryGetValue(AServiceType, Result) then
  begin
    if Supports(Result, IdxServiceCreatorCallback, ACallback) then
    begin
      Result := ACallback.GetService(Self, AServiceType);
      if Result <> nil then
        Services[AServiceType] := Result;
    end;
  end
  else
    Result := nil;
end;

function TdxServiceManager.ServiceExists(
  const AServiceType: TdxServiceType): Boolean;
begin
  Result := FServices.ContainsKey(AServiceType);
end;

procedure TdxServiceManager.AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface);
begin
  AddService(AServiceType, AServiceInstance, False);
end;

procedure TdxServiceManager.AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface; APromote: Boolean);
begin
  if not ServiceExists(AServiceType) then
  begin
    Services.Add(AServiceType, AServiceInstance);
    RaiseServiceListChanged;
  end;
end;

procedure TdxServiceManager.AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback);
begin
  AddService(AServiceType, ACallback.GetSelf, False);
end;

procedure TdxServiceManager.AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback; APromote: Boolean);
begin
  AddService(AServiceType, ACallback.GetSelf, APromote);
end;

procedure TdxServiceManager.RemoveService(const AServiceType: TdxServiceType);
begin
  RemoveService(AServiceType, False);
end;

procedure TdxServiceManager.RemoveService(const AServiceType: TdxServiceType; APromote: Boolean);
begin
  if ServiceExists(AServiceType) then
  begin
    Services.Remove(AServiceType);
    RaiseServiceListChanged;
  end;
end;

procedure TdxServiceManager.RaiseServiceListChanged;
begin
  if not FServiceListChanged.Empty then
    FServiceListChanged.Invoke(Self);
end;

{ TdxServiceUtils }

class function TdxServiceUtils<T>.GetGuid: TGUID;
begin
  Result := GetTypeData(TypeInfo(T))^.GUID;
end;

class function TdxServiceUtils<T>.GetService(
  const AProvider: IdxServiceProvider): T;
var
  AObj: T;
  AObject: IInterface;
  AGuid: TGUID;
begin
  if (AProvider <> nil) then
  begin
    AGuid := GetGuid;
    AObject := AProvider.GetService(AGuid);
    Supports(AObject, AGuid, Result);
  end
  else
    Result := Default(T);
end;

class function TdxServiceUtils<T>.ReplaceService(
  const AContainer: IdxServiceContainer; const ANewService: IInterface): T;
var
  AGuid: TGUID;
begin
  AGuid := GetGuid;
  Result := GetService(AContainer);
  if Result <> nil then
    AContainer.RemoveService(AGuid);
  if ANewService <> nil then
    AContainer.AddService(AGuid, ANewService);
end;

end.

