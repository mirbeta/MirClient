{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSRichEditControlLnkReg;

interface

{$I cxVer.inc}

procedure Register;

implementation

uses
  DesignIntf, DesignEditors, Classes, ActnList,
{$IFDEF DELPHIXE3}
  Actions,
{$ENDIF}
  dxPSRichEditControlLnk, dxPSReg, dxPSDsgUtils, dxPSCore, dxPSGlbl, dxPrnPg,
  dxRichEdit.Control, dxRichEdit.Actions, dxRichEdit.Actions.Reg, dxUIGenerator;

procedure RegisterRichEditControlPrintingActions;
var
  ACategory: TdxUIGeneratorCategoryInfo;
  AComponent: TdxUIGeneratorComponentInfo;
begin
  if TdxUIGenerator.GetComponentInfo(TdxRichEditControl, AComponent) then
  begin
    ACategory := AComponent.Add('File', 'Print', 'Print\Print_16x16.png', 0);
    ACategory.Add(TdxRichEditControlShowPrintForm);
    ACategory.Add(TdxRichEditControlShowPrintPreviewForm);
    ACategory.Add(TdxRichEditControlShowPageSetupForm);

    RegisterActions(TdxUIGeneratorHelper.GenerateCategoryName(ACategory), [
      TdxRichEditControlShowPrintForm,
      TdxRichEditControlShowPrintPreviewForm
      ], nil);
  end;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  dxPSRegisterReportLinkUnit('dxPSRichEditControlLnk', TdxRichEditControlReportLink);

  HideProperty(TypeInfo(Boolean), TdxRichEditControlReportLink, 'ShowDesigner');
  HideProperty(TypeInfo(Boolean), TdxRichEditControlReportLink, 'ShowPageFooter');
  HideProperty(TypeInfo(Boolean), TdxRichEditControlReportLink, 'ShowPageHeader');
  HideProperty(TypeInfo(Boolean), TdxRichEditControlReportLink, 'ShowPageRowHeader');
  HideProperty(TypeInfo(Integer), TdxRichEditControlReportLink, 'DateFormat');
  HideProperty(TypeInfo(Integer), TdxRichEditControlReportLink, 'StartPageIndex');
  HideProperty(TypeInfo(Integer), TdxRichEditControlReportLink, 'TimeFormat');
  HideProperty(TypeInfo(string), TdxRichEditControlReportLink, 'DesignerCaption');
  HideProperty(TypeInfo(TdxAssignedFormatValues), TdxRichEditControlReportLink, 'AssignedFormatValues');
  HideProperty(TypeInfo(TdxPageNumberFormat), TdxRichEditControlReportLink, 'PageNumberFormat');
  HideProperty(TypeInfo(TdxPrinterPage), TdxRichEditControlReportLink, 'PrinterPage');
  HideProperty(TypeInfo(TdxReportFootnotes), TdxRichEditControlReportLink, 'ReportFootnotes');
  HideProperty(TypeInfo(TdxReportTitle), TdxRichEditControlReportLink, 'ReportTitle');
  HideProperty(TypeInfo(THelpContext), TdxRichEditControlReportLink, 'DesignerHelpContext');

  RegisterNoIcon([TdxRichEditControlReportLink]);
  RegisterRichEditControlPrintingActions;
end;

end.
