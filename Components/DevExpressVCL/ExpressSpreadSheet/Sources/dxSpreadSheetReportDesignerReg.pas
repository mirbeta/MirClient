{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetReportDesignerReg;

{$I cxVer.Inc}

interface

uses
  Classes, dxSpreadSheet, dxSpreadSheetReportDesigner, cxPC,
  DesignIntf, DesignEditors, DB, cxEditPropEditors;

const
  dxSpreadSheetReportProductName = 'ExpressSpreadSheet Report Designer';

procedure Register;

implementation

uses
  cxLibraryReg, dxCoreReg, dxBuiltInPopupMenu, cxClasses, dxUIGeneratorDesignHelpers, dxSpreadSheetReportDesignerUIGeneratorScheme;

type
  TReportDataControllerAccess = class(TdxSpreadSheetReportDataController);

  { TdxSpreadSheetReportComponentEditor }

  TdxSpreadSheetReportComponentEditor = class(TdxUIGeneratorComponentEditor)
  protected
    function GetProductName: string; override;
  end;

  { TdxSpreadSheetReportMasterKeyFieldNameProperty }

  TdxSpreadSheetReportMasterKeyFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

  { TdxSpreadSheetReportDetailKeyFieldNameProperty }

  TdxSpreadSheetReportDetailKeyFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

  { TdxSpreadSheetReportSortedFieldNameProperty }

  TdxSpreadSheetReportSortedFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

{ TdxSpreadSheetReportComponentEditor }

function TdxSpreadSheetReportComponentEditor.GetProductName: string;
begin
  Result := dxSpreadSheetReportProductName;
end;

{ TdxSpreadSheetReportMasterKeyFieldNameProperty }

function TdxSpreadSheetReportMasterKeyFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TReportDataControllerAccess(TdxSpreadSheetReportDetail(
    GetComponent(0)).DataController).MasterDataController.DataSource;
end;

{ TdxSpreadSheetReportDetailKeyFieldNameProperty }

function TdxSpreadSheetReportDetailKeyFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TdxSpreadSheetReportDetail(GetComponent(0)).DataController.DataSource;
end;

{ TdxSpreadSheetReportSortedFieldNameProperty }

function TdxSpreadSheetReportSortedFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TdxSpreadSheetReportSortedFields(TdxSpreadSheetReportSortedField(
    GetComponent(0)).Collection).DataController.DataSource;
end;

//--
procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents(dxCoreLibraryProductPage, [TdxSpreadSheetReportDesigner]);
  RegisterComponentEditor(TdxSpreadSheetReportDesigner, TdxSpreadSheetReportComponentEditor);
  RegisterClasses([TdxSpreadSheetReportDetails, TdxSpreadSheetReportDetail]);
  RegisterNoIcon([TdxSpreadSheetReportDetail]);
  RegisterPropertyEditor(TypeInfo(string), TdxSpreadSheetReportDetail,
    'MasterKeyFieldName', TdxSpreadSheetReportMasterKeyFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxSpreadSheetReportDetail,
    'DetailKeyFieldName', TdxSpreadSheetReportDetailKeyFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxSpreadSheetReportSortedField,
    'FieldName', TdxSpreadSheetReportSortedFieldNameProperty);
  RegisterReportDesignerUIGeneratorScheme;
  RegisterActionsForComponent(TdxSpreadSheetReportDesigner);
end;

end.
