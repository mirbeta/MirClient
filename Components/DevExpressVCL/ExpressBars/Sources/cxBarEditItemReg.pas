{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit cxBarEditItemReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  DesignConst, DesignEditors, DesignIntf, Variants,
  Classes, TypInfo, cxBarEditItem, cxBarEditItemValueEditor, cxEdit,
  cxPropEditors, dxBarStrs, dxCore, cxClasses;

type
  TcxCustomBarEditItemAccess = class(TcxCustomBarEditItem);

  { TcxBarEditItemPropertiesProperty }

  TcxBarEditItemPropertiesProperty = class(TClassProperty)
  private
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxBarEditItemPropertiesEventsProperty }

  TcxBarEditItemPropertiesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxBarEditItemSelectionEditor }

  TcxBarEditItemSelectionEditor = class(TSelectionEditor)
  private
    FProc: TGetStrProc;
    procedure GetBarEditItem(const S: string);
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TcxBarEditItemEditValueProperty }

  TcxBarEditItemEditValueProperty = class(TVariantProperty)
  private
    function CanShowDialog: Boolean;
    function IsBlobEditValue: Boolean;
  public
    function AllEqual: Boolean; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TcxBarEditItemPropertiesProperty }

function TcxBarEditItemPropertiesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TcxBarEditItemPropertiesProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := GetRegisteredEditProperties.GetDescriptionByClass(
      TcxCustomBarEditItem(GetComponent(0)).PropertiesClass)
  else
    Result := '';
end;

procedure TcxBarEditItemPropertiesProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to GetRegisteredEditProperties.Count - 1 do
    Proc(GetRegisteredEditProperties.Descriptions[I]);
end;

procedure TcxBarEditItemPropertiesProperty.SetValue(const Value: string);
var
  APropertiesClass: TcxCustomEditPropertiesClass;
  I: Integer;
begin
  APropertiesClass := TcxCustomEditPropertiesClass(GetRegisteredEditProperties.FindByClassName(Value));
  if APropertiesClass = nil then
    APropertiesClass := TcxCustomEditPropertiesClass(GetRegisteredEditProperties.FindByDescription(Value));
  for I := 0 to PropCount - 1 do
    TcxCustomBarEditItem(GetComponent(I)).PropertiesClass := APropertiesClass;
  Modified;
end;

function TcxBarEditItemPropertiesProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to PropCount - 1 do
    if TcxCustomBarEditItem(GetComponent(I)).Properties = nil then
    begin
      Result := False;
      Break;
    end;
end;

{ TcxBarEditItemPropertiesEventsProperty }

function TcxBarEditItemPropertiesEventsProperty.GetInstance: TPersistent;
begin
  Result := TcxCustomBarEditItem(GetComponent(0)).Properties;
end;

{ TcxBarEditItemSelectionEditor }

procedure TcxBarEditItemSelectionEditor.RequiresUnits(
  Proc: TGetStrProc);
begin
  FProc := Proc;
  Designer.GetComponentNames(GetTypeData(TcxCustomBarEditItem.ClassInfo), GetBarEditItem);
end;

procedure TcxBarEditItemSelectionEditor.GetBarEditItem(const S: string);
var
  AItem: TcxCustomBarEditItem;
begin
  AItem := TcxCustomBarEditItem(Designer.GetComponent(S));
  if AItem.Properties <> nil then
    FProc(cxGetUnitName(AItem.Properties.ClassType));
end;

{ TcxBarEditItemEditValueProperty }

function TcxBarEditItemEditValueProperty.AllEqual: Boolean;
var
  I: Integer;
begin
  if IsBlobEditValue then
  begin
    Result := PropCount = 1;
    if not Result then
      for I := 0 to PropCount - 1 do
      begin
        Result := VarIsNull(TcxCustomBarEditItem(GetComponent(I)).EditValue);
        if not Result then
          Break;
      end;
  end
  else
    Result := inherited AllEqual;
end;

procedure TcxBarEditItemEditValueProperty.Edit;
var
  AItem: TcxCustomBarEditItem;
  I: Integer;
begin
  if CanShowDialog then
  begin
    AItem := TcxCustomBarEditItem(GetComponent(0));
    if ShowValueEditor(AItem) then
    begin
      for I := 1 to PropCount - 1 do
        TcxCustomBarEditItem(GetComponent(I)).EditValue := AItem.EditValue;
      Modified;
    end;
  end;
end;

function TcxBarEditItemEditValueProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if CanShowDialog then
    Include(Result, paDialog);
end;

function TcxBarEditItemEditValueProperty.GetValue: string;
begin
  if IsBlobEditValue then
    if VarIsNull(TcxCustomBarEditItem(GetComponent(0)).EditValue) then
      Result := SNull
    else
      Result := SBlob
  else
    Result := inherited GetValue;
end;

procedure TcxBarEditItemEditValueProperty.SetValue(const Value: string);
begin
  if not IsBlobEditValue then
    inherited SetValue(Value);
end;

function TcxBarEditItemEditValueProperty.CanShowDialog: Boolean;
var
  ABlobEditValue: Boolean;
  AProperties, ATempProperties: TcxCustomEditProperties;
  I: Integer;
begin
  Result := not TcxCustomBarEditItemAccess(GetComponent(0)).CaptionIsEditValue;
  if not Result then
    Exit;
  AProperties := TcxCustomBarEditItem(GetComponent(0)).GetProperties;
  ABlobEditValue := esfBlobEditValue in AProperties.GetSpecialFeatures;
  for I := 1 to PropCount - 1 do
  begin
    ATempProperties := TcxCustomBarEditItem(GetComponent(I)).GetProperties;
    if (ATempProperties.ClassType <> AProperties.ClassType) or
      (esfBlobEditValue in ATempProperties.GetSpecialFeatures <> ABlobEditValue) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TcxBarEditItemEditValueProperty.IsBlobEditValue: Boolean;
begin
  Result := CanShowDialog and
    (esfBlobEditValue in TcxCustomBarEditItem(GetComponent(0)).GetProperties.GetSpecialFeatures);
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterNoIcon([TcxBarEditItem]);
  RegisterPropertyEditor(TypeInfo(TcxEditValue), TcxCustomBarEditItem,
    'EditValue', TcxBarEditItemEditValueProperty);
  RegisterPropertyEditor(TypeInfo(TcxCustomEditProperties), TcxCustomBarEditItem,
    'Properties', TcxBarEditItemPropertiesProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxCustomBarEditItem, 'PropertiesClassName', nil);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomBarEditItem, 'PropertiesEvents',
    TcxBarEditItemPropertiesEventsProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxCustomBarEditItem, 'Text', nil);
  RegisterSelectionEditor(TcxCustomBarEditItem, TcxBarEditItemSelectionEditor);
end;

end.
