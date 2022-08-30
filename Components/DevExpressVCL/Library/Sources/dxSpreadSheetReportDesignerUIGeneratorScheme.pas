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

unit dxSpreadSheetReportDesignerUIGeneratorScheme;

{$I cxVer.Inc}

interface

const
  sdxReportDesignerCategoryName = 'DevExpress ExpressSpreadSheet';
  sdxSpreadSheetTabReport = 'Report';

  sdxSpreadSheetReportMode = 'Mode';
  sdxSpreadSheetReportTemplateSections = 'Template Sections';
  sdxSpreadSheetReportSortAndGroup = 'Sort & Group';
  sdxSpreadSheetReportFilter = 'Filter';
  sdxSpreadSheetReportDesign = 'Design';

  sdxSpreadSheetDocumentOrientation = 'Document Orientation';
  sdxSpreadSheetMasterDetail = 'Master-Detail';


procedure RegisterReportDesignerUIGeneratorScheme;

implementation

uses
  dxUIGenerator, dxSpreadSheet, dxSpreadSheetActions, dxSpreadSheetUIGeneratorScheme, dxSpreadSheetReportDesigner,
  dxSpreadSheetReportDesignerActions;

procedure RegisterCategoryMode(ACategory: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACategory.Add(TdxSpreadSheetReportDesignerSingleSheetReportMode, [ugivlSmallIcon, ugivlText], ugigpNone, ugipBeginsNewColumn);
  ACategory.Add(TdxSpreadSheetReportDesignerMultipleSheetsReportMode, [ugivlSmallIcon, ugivlText]);
  ACategory.Add(TdxSpreadSheetReportDesignerMultipleDocumentsReportMode, [ugivlSmallIcon, ugivlText]);

  ACommand := ACategory.Add('Report Designer\Mode\ReportOrientation.png', sdxSpreadSheetDocumentOrientation);
  ACommand.Add(TdxSpreadSheetReportDesignerHorizontalOrientation);
  ACommand.Add(TdxSpreadSheetReportDesignerVerticalOrientation);
end;

procedure RegisterCategoryRanges(ACategory: TdxUIGeneratorCategoryInfo);
var
  ACommand: TdxUIGeneratorCommandInfo;
begin
  ACategory.Add(TdxSpreadSheetReportDesignerHeaderSection, [ugivlSmallIcon, ugivlText], ugigpNone, ugipBeginsNewColumn);
  ACategory.Add(TdxSpreadSheetReportDesignerFooterSection, [ugivlSmallIcon, ugivlText]);
  ACategory.Add(TdxSpreadSheetReportDesignerDetailSection);

  ACommand := ACategory.Add('Report Designer\TemplateRanges\SetDetailLevel.png', sdxSpreadSheetMasterDetail);
  ACommand.Add(TdxSpreadSheetReportDesignerDetailLevel);
  ACommand.Add(TdxSpreadSheetReportDesignerDataMember);

  ACategory.Add(TdxSpreadSheetReportDesignerResetSection);
end;

procedure RegisterCategorySortAndGroup(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSpreadSheetReportDesignerSortFields);
  ACategory.Add(TdxSpreadSheetReportDesignerGroupHeaderSection, [ugivlSmallIcon, ugivlText], ugigpNone, ugipBeginsNewColumn);
  ACategory.Add(TdxSpreadSheetReportDesignerGroupFooterSection, [ugivlSmallIcon, ugivlText]);
end;

procedure RegisterCategoryFilter(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSpreadSheetReportDesignerEditFilter);
  ACategory.Add(TdxSpreadSheetReportDesignerResetFilter);
end;

procedure RegisterCategoryDesign(ACategory: TdxUIGeneratorCategoryInfo);
begin
  ACategory.Add(TdxSpreadSheetReportDesignerDesignView);
  ACategory.Add(TdxSpreadSheetReportDesignerReportPreview);
end;

procedure RegisterReportDesignerUIGeneratorScheme;
var
  AComponent: TdxUIGeneratorComponentInfo;
begin
  AComponent := TdxUIGenerator.RegisterComponent(TdxSpreadSheetReportDesigner, sdxReportDesignerCategoryName);
  RegisterCategoryMode(AComponent.Add(sdxSpreadSheetTabReport, sdxSpreadSheetReportMode, 'Report Designer\Mode\SingleSheet_16x16.png', 0));

  RegisterCategoryRanges(AComponent.Add(sdxSpreadSheetTabReport, sdxSpreadSheetReportTemplateSections, 'Report Designer\TemplateRanges\SetDetailRange_16x16.png', 0));
  RegisterCategorySortAndGroup(AComponent.Add(sdxSpreadSheetTabReport, sdxSpreadSheetReportSortAndGroup, 'Report Designer\SortAndGroup\GroupHeader_16x16.png', 0));
  RegisterCategoryFilter(AComponent.Add(sdxSpreadSheetTabReport, sdxSpreadSheetReportFilter, 'Report Designer\Filter\EditFilter_16x16.png', 0));
  RegisterCategoryDesign(AComponent.Add(sdxSpreadSheetTabReport, sdxSpreadSheetReportDesign, 'Report Designer\Design\ShowRanges_16x16.png', 0));
end;

end.
