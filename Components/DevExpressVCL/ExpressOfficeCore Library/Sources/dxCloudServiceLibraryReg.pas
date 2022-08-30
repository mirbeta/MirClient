{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressOfficeCore Library classes                        }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL     }
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

unit dxCloudServiceLibraryReg;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, Menus, TypInfo, Windows, Graphics, Controls, DesignIntf;

const
  dxExpressOfficeCoreProductName = 'ExpressOfficeCore Library';

procedure Register;

implementation

uses
  Types, DesignMenus, VCLEditors,
  dxCoreReg, cxLibraryReg,
  cxDesignWindows, cxEditPropEditors, DesignEditors,
  dxCloudStorage,
  dxAuthorizationAgents;

type
  { TdxExpressOfficeCoreComponentEditor }

  TdxExpressOfficeCoreComponentEditor = class(TdxComponentEditor)
  protected
    function GetProductName: string; override;
  end;

  { TdxCloudStorageProviderProperty }

  TdxCloudStorageProviderProperty = class(TClassProperty)
  strict private
    function GetStorage: TdxCloudStorage;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TdxExpressOfficeCoreComponentEditor }

function TdxExpressOfficeCoreComponentEditor.GetProductName: string;
begin
  Result := dxExpressOfficeCoreProductName;
end;

{ TdxCloudStorageProviderProperty }

function TdxCloudStorageProviderProperty.GetAttributes: TPropertyAttributes;
var
  AStorage: TdxCloudStorage;
begin
  Result := inherited GetAttributes;
  AStorage := GetStorage;
  if (AStorage <> nil) and (AStorage.ProviderClass <> nil) then
    Include(Result, paSubProperties)
  else
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TdxCloudStorageProviderProperty.GetValue: string;
var
  AStorage: TdxCloudStorage;
begin
  AStorage := GetStorage;
  if AStorage <> nil then
  begin
    if AStorage.ProviderClass <> nil then
      Exit(AStorage.ProviderClass.GetDisplayName);
  end;
  Result := '';
end;

procedure TdxCloudStorageProviderProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  inherited GetValues(Proc);
  for I := 0 to TdxCloudStorage.RegisteredProviders.Count - 1 do
    Proc(TdxCloudStorage.RegisteredProviders.Descriptions[I]);
end;

procedure TdxCloudStorageProviderProperty.SetValue(const Value: string);
var
  AStorage: TdxCloudStorage;
  AClass: TClass;
begin
  AStorage := GetStorage;
  if AStorage <> nil then
  begin
    if Value <> '' then
      AClass := TdxCloudStorage.RegisteredProviders.FindByDescription(Value)
    else
      AClass := nil;
    if AClass = nil then
        AStorage.ProviderClass := nil
      else
        AStorage.ProviderClass := TdxCloudStorageProviderClass(AClass);
    Modified;
  end;
end;

function TdxCloudStorageProviderProperty.GetStorage: TdxCloudStorage;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
    if GetComponent(I) is TdxCloudStorage then
      Exit(TdxCloudStorage(GetComponent(I)));
  Result := nil;
end;

procedure Register;
begin
  RegisterComponents(dxLibraryProductPage, [TdxGoogleAPIOAuth2AuthorizationAgent, TdxMicrosoftGraphAPIOAuth2AuthorizationAgent]);
  RegisterComponents(dxLibraryProductPage, [TdxCloudStorage]);
  RegisterComponentEditor(TdxCustomAuthorizationAgent, TdxExpressOfficeCoreComponentEditor);
  RegisterComponentEditor(TdxCloudStorage, TdxExpressOfficeCoreComponentEditor);
  RegisterPropertyEditor(TypeInfo(TdxCloudStorageProvider), TdxCloudStorage, 'Provider', TdxCloudStorageProviderProperty);
  HideClassProperties(TdxCloudStorage, ['ProviderClassName']);
end;

end.
