{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressFilterControl                                     }
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
unit cxFilterControlReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  DesignEditors, DesignIntf, VCLEditors,
  DB, TypInfo, SysUtils, Classes, Controls, cxClasses, cxFilterControl,
  cxDBFilterControl, cxEdit, cxEditRepositoryItems, cxEditPropEditors,
  dxCore, dxCoreReg;

type
  { TcxFilterItemPropertiesProperty }

  TcxFilterItemPropertiesProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxFilterControlLinkSourceProperty }

  TcxFilterControlLinkComponentProperty = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TcxDBFilterControlSelectionEditor = class(TSelectionEditor)
  protected
    ComponentsList: TStringList;
  public
    procedure AddComponent(const Name: string);
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure TcxDBFilterControlSelectionEditor.AddComponent(const Name: string);
begin
  ComponentsList.Add(Name);
end;

procedure TcxDBFilterControlSelectionEditor.RequiresUnits(Proc: TGetStrProc);

  procedure AddPropertiesUnitName(AProperties: TcxCustomEditProperties);
  begin
    if AProperties <> nil then
      Proc(cxGetUnitName(AProperties.ClassType));
  end;

var
  AComponent: TComponent;
  I, J: Integer;
begin
  inherited RequiresUnits(Proc);
  ComponentsList := TStringList.Create;
  try
    Designer.GetComponentNames(GetTypeData(PTypeInfo(TcxDBFilterControl.ClassInfo)), AddComponent);
    for I := 0 to ComponentsList.Count - 1 do
    begin
      AComponent := Designer.GetComponent(ComponentsList[I]);
      if AComponent is TcxDBFilterControl then
        with TcxDBFilterControl(AComponent) do
          for J := 0 to Items.Count - 1 do
            AddPropertiesUnitName(Items[J].Properties);
    end;
  finally
    ComponentsList.Free;
  end;
end;

{ TcxFilterItemPropertiesProperty }

function TcxFilterItemPropertiesProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := TcxFilterItem(GetComponent(I)).Properties <> nil;
    if not Result then Exit;
  end;
  Result := True;
end;

function TcxFilterItemPropertiesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] +
    [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TcxFilterItemPropertiesProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := GetRegisteredEditProperties.GetDescriptionByClass(TcxCustomEditProperties(GetOrdValue).ClassType)
  else
    Result := '';
end;

procedure TcxFilterItemPropertiesProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to GetRegisteredEditProperties.Count - 1 do
    if IsSupportFiltering(TcxCustomEditPropertiesClass(GetRegisteredEditProperties[I])) then
      Proc(GetRegisteredEditProperties.Descriptions[I]);
end;

procedure TcxFilterItemPropertiesProperty.SetValue(const Value: string);
var
  APropertiesClass: TcxCustomEditPropertiesClass;
  I: Integer;
begin
  APropertiesClass := TcxCustomEditPropertiesClass(GetRegisteredEditProperties.FindByClassName(Value));
  if APropertiesClass = nil then
    APropertiesClass := TcxCustomEditPropertiesClass(GetRegisteredEditProperties.FindByDescription(Value));
  if (APropertiesClass = nil) or IsSupportFiltering(APropertiesClass) then
  begin
    for I := 0 to PropCount - 1 do
      TcxFilterItem(GetComponent(I)).PropertiesClass := APropertiesClass;
    Modified;
  end;
end;

{ TcxFilterControlLinkComponentProperty }

procedure TcxFilterControlLinkComponentProperty.CheckComponent(
  const Value: string);
var
  AComponent: TComponent;
begin
  AComponent := TComponent(Designer.GetComponent(Value));
  if (AComponent <> nil) and not Supports(AComponent, IcxFilterControl) then
    Exit;
  FCheckProc(Value);
end;

procedure TcxFilterControlLinkComponentProperty.GetValues(Proc: TGetStrProc);
begin
  FCheckProc := Proc;
  inherited GetValues(CheckComponent);
end;

type
  TcxFilterControlItemFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

{ TcxFilterControlItemFieldNameProperty }

function TcxFilterControlItemFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TcxDBFilterControl(TcxFilterItem(GetComponent(0)).FilterControl).DataSource;
end;

procedure Register;
begin
  RegisterComponents(dxCoreLibraryProductPage, [TcxFilterControl, TcxDBFilterControl]);
  RegisterComponentEditor(TcxCustomFilterControl, TcxFilterControlComponentEditor);
  RegisterClasses([TcxFilterItem]);
  RegisterPropertyEditor(TypeInfo(TComponent), TcxFilterControl,
    'LinkComponent', TcxFilterControlLinkComponentProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxFilterItem, 'FieldName', TcxFilterControlItemFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxFilterItem, 'PropertiesClassName', nil);
  RegisterPropertyEditor(TypeInfo(TcxCustomEditProperties), TcxFilterItem,
    'Properties', TcxFilterItemPropertiesProperty);
  RegisterSelectionEditor(TcxDBFilterControl, TcxDBFilterControlSelectionEditor);
end;

initialization
  StartClassGroup(TControl);
  GroupDescendentsWith(TcxFilterControl, TControl);
  GroupDescendentsWith(TcxDBFilterControl, TControl);
  GroupDescendentsWith(TcxFilterItem, TControl);

end.

