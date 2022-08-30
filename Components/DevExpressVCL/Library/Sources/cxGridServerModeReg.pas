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

unit cxGridServerModeReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

{$IFNDEF NONDB}
uses
  Windows, Types, Classes, DesignIntf, DesignEditors,
  cxGridCustomTableView, cxGridServerModeDataDefinitions, cxGridServerModeTableView,
  cxGridServerModeBandedTableView, cxGridReg;

type
  TcxServerModeDataBindingFieldNamePropertyEditor = class(TStringProperty)
  private
    function GetDataBinding: TcxGridItemServerModeDataBinding;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    property DataBinding: TcxGridItemServerModeDataBinding read GetDataBinding;
  end;

function TcxServerModeDataBindingFieldNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

function TcxServerModeDataBindingFieldNamePropertyEditor.GetDataBinding: TcxGridItemServerModeDataBinding;
begin
  Result := nil;
  if GetComponent(0) is TcxGridItemServerModeDataBinding then
      Result := TcxGridItemServerModeDataBinding(GetComponent(0));
end;

procedure TcxServerModeDataBindingFieldNamePropertyEditor.GetValues(
  Proc: TGetStrProc);
var
  ADataController: TcxGridServerModeDataController;
  I: Integer;
begin
  if DataBinding = nil then
    Exit;
  ADataController := DataBinding.DataController;
  if ADataController.DataSource <> nil then
    with ADataController.DataSource do
    for I := 0 to Fields.Count - 1 do
      if Assigned(Fields[I]) then
        Proc(Fields[I].FieldName);
end;

{$ENDIF}

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
{$IFNDEF NONDB}
  RegisterNoIcon([TcxGridServerModeTableView, TcxGridServerModeColumn]);
  RegisterPropertyEditor(TypeInfo(string), TcxGridItemServerModeDataBinding, 'FieldName',
    TcxServerModeDataBindingFieldNamePropertyEditor);
  UnlistPublishedProperty(TcxGridItemServerModeDataBinding, 'ValueType');
  UnlistPublishedProperty(TcxGridServerModeTableView, 'NewItemRow');
  UnlistPublishedProperty(TcxGridServerModeBandedTableView, 'NewItemRow');
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(TcxGridServerModeColumn), TcxGridServerModeSummaryItem,
    'Column', TcxGridTableSummaryItemColumnProperty);
end;

end.
