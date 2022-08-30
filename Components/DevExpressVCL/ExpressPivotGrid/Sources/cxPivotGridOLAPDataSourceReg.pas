{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPivotGrid                                         }
{                                                                    }
{           Copyright (c) 2005-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPIVOTGRID AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxPivotGridOLAPDataSourceReg;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, CommCtrl, ImgList,
  Types, DesignIntf, DesignEditors, VCLEditors, ADOReg,
  cxGraphics, cxControls, cxClasses, cxGeometry, cxCustomPivotGrid, cxPivotGridOLAPDataSource, cxButtons,
  cxPivotGrid, dxCoreReg, cxPivotGridReg, cxLibraryReg;

procedure Register;

implementation

uses
  cxPivotGridOLAPOLEDBProvider, cxPivotGridOLAPADOMDProvider, ADODB, AdoConEd;

type
  { TcxPivotGridOLAPDataSourceProviderProperty }

  TcxPivotGridOLAPDataSourceProviderProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxConnectionStringProperty }

  TcxConnectionStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

{ TcxPivotGridOLAPDataSourceProviderProperty }

function TcxPivotGridOLAPDataSourceProviderProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties)
  else
    Include(Result, paSubProperties);
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TcxPivotGridOLAPDataSourceProviderProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := cxGetPivotGridOLAPDataSourceProviders.GetDescriptionByClass(
      TcxCustomPivotGridOLAPProvider(GetOrdValue).ClassType)
  else
    Result := '';
end;

procedure TcxPivotGridOLAPDataSourceProviderProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  ADesc: string;
begin
  for I := 0 to cxGetPivotGridOLAPDataSourceProviders.Count - 1 do
  begin
    ADesc := cxGetPivotGridOLAPDataSourceProviders.Descriptions[I];
    if ADesc <> '' then
      Proc(ADesc);
  end;
end;

function TcxPivotGridOLAPDataSourceProviderProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := (GetComponent(I) is TcxPivotGridOLAPDataSource) and
      (TcxPivotGridOLAPDataSource(GetComponent(I)).Provider <> nil);
    if not Result then
      Exit;
  end;
  Result := True;
end;

procedure TcxPivotGridOLAPDataSourceProviderProperty.SetValue(const Value: string);
var
  I: Integer;
  AProviderClass: TcxCustomPivotGridOLAPProviderClass;
begin
  AProviderClass := TcxCustomPivotGridOLAPProviderClass(cxGetPivotGridOLAPDataSourceProviders.FindByClassName(Value));
  if AProviderClass = nil then
    AProviderClass := TcxCustomPivotGridOLAPProviderClass(cxGetPivotGridOLAPDataSourceProviders.FindByDescription(Value));
  for I := 0 to PropCount - 1 do
    TcxPivotGridOLAPDataSource(GetComponent(I)).ProviderClass := AProviderClass;
  Modified;
end;

{ TcxConnectionStringProperty }

function TcxConnectionStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TcxConnectionStringProperty.Edit;
var
  AConnEditForm: TConnEditForm;
  AOldConnectionString, AConnectionString: string;
begin
  AOldConnectionString := GetStrValue;
  AConnEditForm := TConnEditForm.Create(Application);
  try
    AConnectionString := AConnEditForm.Edit(AOldConnectionString);
  finally
    AConnEditForm.Free;
  end;
  if AConnectionString <> AOldConnectionString then
  begin
    SetStrValue(AConnectionString);
    Modified;
  end;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents(dxCoreLibraryProductPage, [TcxPivotGridOLAPDataSource]);
  RegisterClasses([TcxPivotGridOLAPDataSource]);
  RegisterComponentEditor(TcxPivotGridOLAPDataSource, cxPivotGridCustomComponentEditor);
  RegisterPropertyEditor(TypeInfo(TcxCustomPivotGridOLAPProvider), TcxPivotGridOLAPDataSource,
    'Provider', TcxPivotGridOLAPDataSourceProviderProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxPivotGridOLAPOLEDBProvider, 'ConnectionString', TcxConnectionStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxPivotGridOLAPADOMDProvider, 'ConnectionString', TcxConnectionStringProperty);
  HideClassProperties(TcxPivotGridOLAPDataSource, ['ConnectionString', 'ProviderClassName']);
end;

end.



