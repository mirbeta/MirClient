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

unit dxPSdxPDFViewerLnkReg;

{$I cxVer.inc}

interface


procedure Register;

implementation

uses
  DesignIntf, DesignEditors, Classes, ActnList,
{$IFDEF DELPHIXE3}
  Actions,
{$ENDIF}
  dxPSdxPDFViewerLnk, dxPSReg, dxPSDsgUtils, dxPSCore, dxPSGlbl, dxPrnPg,
  dxUIGenerator, dxPDFViewerActions, dxPDFViewerUIGeneratorScheme, dxPDFViewer;

procedure RegisterPDFViewerPrintingActions;

  function FindCategory(AComponent: TdxUIGeneratorComponentInfo; const ATabName, ABArName: string;
    out ACategory: TdxUIGeneratorCategoryInfo): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to AComponent.Categories.Count - 1 do
    begin
      Result := (AComponent.Categories[I].TabName = ATabName) and (AComponent.Categories[I].BarName = ABArName);
      if Result then
      begin
        ACategory := AComponent.Categories[I];
        Break;
      end;
    end;
  end;

var
  ACategory: TdxUIGeneratorCategoryInfo;
  AComponent: TdxUIGeneratorComponentInfo;
begin
  if TdxUIGenerator.GetComponentInfo(TdxPDFViewer, AComponent) then
  begin
    if not FindCategory(AComponent, sdxPDFViewerTabPDFViewer, sdxPDFViewerBarPDFViewerFile, ACategory) then
      ACategory := AComponent.Add(sdxPDFViewerTabPDFViewer, sdxPDFViewerBarPDFViewerFile, 'Print\Print_16x16.png', 0);
    ACategory.Add(TdxPDFViewerShowPrintForm);
    RegisterActions(TdxUIGeneratorHelper.GenerateCategoryName(ACategory), [TdxPDFViewerShowPrintForm], nil);
  end;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  dxPSRegisterReportLinkUnit('dxPSdxPDFViewerLnk', TdxPDFViewerReportLink);
  RegisterClass(TdxPDFViewerPrinterPage);

  HideProperty(TypeInfo(Boolean), TdxPDFViewerReportLink, 'ShowDesigner');
  HideProperty(TypeInfo(THelpContext), TdxPDFViewerReportLink, 'DesignerHelpContext');
  HideProperty(TypeInfo(TdxPSPDFReportExportOptions), TdxPDFViewerReportLink, 'PDFExportOptions');

  RegisterNoIcon([TdxPDFViewerReportLink]);
  RegisterPDFViewerPrintingActions;
end;

end.
