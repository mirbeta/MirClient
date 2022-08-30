{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express data-aware OrgChart                              }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSORGCHART AND ALL ACCOMPANYING  }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE end USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxdbcreg;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DesignIntf, DesignEditors, dxCoreReg, dxorgchr, dxdborgc, dxorgced, db, dxotcreg;

procedure Register;

implementation

type
  TFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect,paValueList,paSortList];
end;

procedure TFieldProperty.GetValues(Proc: TGetStrProc);
var
  Table: TDataSet;
  List: TStringList;
  I: Integer;
begin
  Table := (GetComponent(0) as TdxDbOrgChart).DataSet;
  if Table=nil then Exit;
  List := TStringList.Create;
{$WARN SYMBOL_DEPRECATED OFF}
  Table.GetFieldNames(List);
{$WARN SYMBOL_DEPRECATED ON}
  for I:=0 to List.Count-1 do Proc(List[I]);
  List.Free;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents(dxOCProductPage, [TdxDbOrgChart]);
  RegisterPropertyEditor(TypeInfo(String),TdxDbOrgChart,'KeyFieldName',TFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TdxDbOrgChart,'ParentFieldName',TFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TdxDbOrgChart,'TextFieldName',TFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TdxDbOrgChart,'OrderFieldName',TFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TdxDbOrgChart,'ImageFieldName',TFieldProperty);
  RegisterComponentEditor(TdxDBOrgChart, TdxOCCustomEditor);
  RegisterSelectionEditor(TdxDBOrgChart, TdxOCSelectionEditor);
end;

end.
