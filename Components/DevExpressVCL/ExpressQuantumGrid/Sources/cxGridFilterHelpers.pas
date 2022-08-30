{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridFilterHelpers;

{$I cxVer.inc}

interface

uses
  Controls, cxGridCustomTableView, cxDataStorage, cxEdit, cxDropDownEdit,
  cxFilterControlUtils;

type
  { TcxFilterExtLookupComboBoxHelper }

  TcxFilterExtLookupComboBoxHelper = class(TcxFilterComboBoxHelper)
  public
    class function GetFilterEditClass: TcxCustomEditClass; override;
    class procedure GetFilterValue(AEdit: TcxCustomEdit;
      AEditProperties: TcxCustomEditProperties; var V: Variant; var S: TCaption); override;
    class function GetSupportedFilterOperators(
      AProperties: TcxCustomEditProperties;
      AValueTypeClass: TcxValueTypeClass;
      AExtendedSet: Boolean = False): TcxFilterControlOperators; override;
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
    class function IsValueValid(AValueTypeClass: TcxValueTypeClass;
      var AValue: Variant): Boolean; override;
  end;

implementation

uses
  cxDBLookupComboBox, cxDBExtLookupComboBox, cxGridDBDataDefinitions,
  cxDBLookupEdit, cxDBData, cxTextEdit;

type
  TcxCustomTextEditPropertiesAccess = class(TcxCustomTextEditProperties);

{ TcxFilterExtLookupComboBoxHelper }

class function TcxFilterExtLookupComboBoxHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := TcxLookupComboBox;
end;

class procedure TcxFilterExtLookupComboBoxHelper.GetFilterValue(
  AEdit: TcxCustomEdit; AEditProperties: TcxCustomEditProperties;
  var V: Variant; var S: TCaption);
begin
  V := AEdit.EditValue;
  S := TcxCustomTextEditPropertiesAccess(AEditProperties).GetDefaultDisplayValue(V, True);
end;

class function TcxFilterExtLookupComboBoxHelper.GetSupportedFilterOperators(
  AProperties: TcxCustomEditProperties; AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  Result := [fcoEqual, fcoNotEqual, fcoBlanks, fcoNonBlanks];
end;

class procedure TcxFilterExtLookupComboBoxHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
var
  AView: TcxCustomGridTableView;
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  with TcxLookupComboBoxProperties(AProperties) do
  begin
    DropDownAutoSize := False;
    DropDownListStyle := lsFixedList;
    AView := TcxExtLookupComboBoxProperties(AEditProperties).View;
    if (AView <> nil) and (TcxExtLookupComboBoxProperties(AEditProperties).ListFieldItem <> nil) then
    begin
      ListSource := TcxDBDataController(AView.DataController).DataSource;
      KeyFieldNames := TcxExtLookupComboBoxProperties(AEditProperties).KeyFieldNames;
      ListFieldNames := TcxGridItemDBDataBinding(TcxExtLookupComboBoxProperties(AEditProperties).ListFieldItem.DataBinding).FieldName;
      DropDownSizeable := True;
      IncrementalFiltering := True;
    end;
  end;
end;

class function TcxFilterExtLookupComboBoxHelper.IsValueValid(
  AValueTypeClass: TcxValueTypeClass; var AValue: Variant): Boolean;
begin
  Result := True;
end;

initialization
  FilterEditsController.Register(TcxExtLookupComboBoxProperties,
    TcxFilterExtLookupComboBoxHelper);

finalization
  FilterEditsController.Unregister(TcxExtLookupComboBoxProperties,
    TcxFilterExtLookupComboBoxHelper);

end.
