{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressExport                                            }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEXPORT AND ALL                 }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
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

unit cxExportStrs;

{$I cxVer.inc}

interface

resourcestring
  scxUnsupportedExport = 'Unsupported export type: %d';
  scxStyleManagerKill = 'The Style Manager is currently being used elsewhere and can not be released at this stage';
  scxStyleManagerCreate = 'Can''t create style manager';

  scxExportToCSV  = 'Export to comma delimited text format (*.csv)';
  scxExportToHtml  = 'Export to Web page (*.html)';
  scxExportToXml   = 'Export to XML document (*.xml)';
  scxExportToText  = 'Export to text format (*.txt)';

  scxEmptyExportCache = 'Export cache is empty';
  scxIncorrectUnion = 'Incorrect union of cells';
  scxIllegalWidth = 'Illegal width of the column';
  scxInvalidColumnRowCount = 'Invalid column or row count';
  scxIllegalHeight = 'Illegal height of the row';
  scxInvalidColumnIndex = 'The column index %d out of bounds';
  scxInvalidRowIndex = 'The row index %d out of bounds';
  scxInvalidStyleIndex = 'Invalid style index %d';

  scxExportToExcel = 'Export to MS Excel (*.xls)';
  scxExportToXlsx  = 'Export to MS Excel 2007 (*.xlsx)';
  scxWorkbookWrite = 'Error write XLS file';
  scxInvalidCellDimension = 'Invalid cell dimension';
  scxBoolTrue  = 'True';
  scxBoolFalse = 'False';

  scxDefaultSheetCaption = 'Sheet';


implementation

uses
  dxCore;

  procedure AddExpressExportResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAddress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAddress);
  end;

begin
  InternalAdd('scxUnsupportedExport', @scxUnsupportedExport);
  InternalAdd('scxStyleManagerKill', @scxStyleManagerKill);
  InternalAdd('scxStyleManagerCreate', @scxStyleManagerCreate);
  InternalAdd('scxExportToCSV', @scxExportToCSV);
  InternalAdd('scxExportToHtml', @scxExportToHtml);
  InternalAdd('scxExportToXml', @scxExportToXml);
  InternalAdd('scxExportToText', @scxExportToText);
  InternalAdd('scxEmptyExportCache', @scxEmptyExportCache);
  InternalAdd('scxIncorrectUnion', @scxIncorrectUnion);
  InternalAdd('scxIllegalWidth', @scxIllegalWidth);
  InternalAdd('scxInvalidColumnRowCount', @scxInvalidColumnRowCount);
  InternalAdd('scxIllegalHeight', @scxIllegalHeight);
  InternalAdd('scxInvalidColumnIndex', @scxInvalidColumnIndex);
  InternalAdd('scxInvalidRowIndex', @scxInvalidRowIndex);
  InternalAdd('scxInvalidStyleIndex', @scxInvalidStyleIndex);
  InternalAdd('scxExportToExcel', @scxExportToExcel);
  InternalAdd('scxExportToXlsx', @scxExportToXlsx);
  InternalAdd('scxWorkbookWrite', @scxWorkbookWrite);
  InternalAdd('scxInvalidCellDimension', @scxInvalidCellDimension);
  InternalAdd('scxBoolTrue', @scxBoolTrue);
  InternalAdd('scxBoolFalse', @scxBoolFalse);
  InternalAdd('scxDefaultSheetCaption', @scxDefaultSheetCaption);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressExport', @AddExpressExportResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressExport');

end.
