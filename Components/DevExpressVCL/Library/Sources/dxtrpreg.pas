{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express tree view printed dataset                        }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSGRID AND ALL ACCOMPANYING VCL  }
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

unit dxtrpreg;

{$I cxVer.inc}

interface

uses Classes, SysUtils, dxmdaset, Windows, DB;

procedure Register;

implementation

uses
  DesignIntf, DesignEditors, dxtrprds;

type
  TCustomDBTreeViewFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TCustomDBTreeViewFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TCustomDBTreeViewFieldProperty.GetValueList(List: TStrings);
begin
end;

procedure TCustomDBTreeViewFieldProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

type
  TDBTreePrnDataFieldProperty = class(TCustomDBTreeViewFieldProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TDBTreePrnDataFieldProperty.GetValueList(List: TStrings);
var
  DataSet : TDataSet;
begin
  DataSet := nil;
  if ((GetComponent(0) as TdxDBTreePrintData).DataSource <> nil) then
    DataSet := (GetComponent(0) as TdxDBTreePrintData).DataSource.DataSet;
  if(DataSet <> nil) then
  {$WARN SYMBOL_DEPRECATED OFF}
    DataSet.GetFieldNames(List);
  {$WARN SYMBOL_DEPRECATED ON}
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents('ExpressDBTree', [TdxDBTreePrintData]);
  RegisterPropertyEditor(TypeInfo(string), TdxDBTreePrintData, 'KeyField', TDBTreePrnDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxDBTreePrintData, 'ParentField', TDBTreePrnDataFieldProperty);
end;

end.
