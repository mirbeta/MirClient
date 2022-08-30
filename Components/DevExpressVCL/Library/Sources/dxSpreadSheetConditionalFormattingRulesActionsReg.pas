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

unit dxSpreadSheetConditionalFormattingRulesActionsReg;

{$I cxVer.Inc}

interface

uses
  ActnList,
{$IFDEF DELPHIXE3}
  Actions,
{$ENDIF}
  dxSpreadSheet, dxSpreadSheetConditionalFormattingRulesActions, dxSpreadSheetUIGeneratorScheme, dxUIGenerator;

procedure Register;

implementation

procedure RegisterSpreadSheetUIGeneratorSchemeCategoryConditionalFormatting;
var
  ACategory: TdxUIGeneratorCategoryInfo;
  ACommand, ASubItemCommand: TdxUIGeneratorCommandInfo;
  AComponent: TdxUIGeneratorComponentInfo;
  AGallery: TdxUIGeneratorGalleryInfo;
begin
  if TdxUIGenerator.GetComponentInfo(TdxSpreadSheet, AComponent) then
  begin
    ACategory := AComponent.Add(sdxSpreadSheetTabHome, sdxSpreadSheetBarHomeConditionalFormatting,
      'Conditional Formatting\ConditionalFormatting_16x16.png', 2);

    ACommand := ACategory.Add('Conditional Formatting\ConditionalFormatting.png',
      sdxSpreadSheetSubItemHomeConditionalFormattingConditionalFormatting);

    AGallery := ACommand.Add(TdxSpreadSheetConditionalFormattingTopBottomRulesGallery, True, False, True, False, 1, 2,
      False);
    AGallery.Add(TdxSpreadSheetConditionalFormattingMoreRules);

    AGallery := ACommand.Add(TdxSpreadSheetConditionalFormattingDataBarsGallery, False, False, True, False, 3);
    AGallery.Add(TdxSpreadSheetConditionalFormattingMoreRules);

    AGallery := ACommand.Add(TdxSpreadSheetConditionalFormattingColorScalesGallery, False, False, True, False, 4);
    AGallery.Add(TdxSpreadSheetConditionalFormattingMoreRules);

    AGallery := ACommand.Add(TdxSpreadSheetConditionalFormattingIconSetsGallery, False, False, True, False, 2);
    AGallery.Add(TdxSpreadSheetConditionalFormattingMoreRules);

    ACommand.Add(TdxSpreadSheetConditionalFormattingNewRule, dxUIGeneratorItemDefaultViewLevels, ugigpNone,
      ugipBeginsNewRow, True);
    ASubItemCommand := ACommand.Add('Conditional Formatting\ClearRules.png',
      sdxSpreadSheetSubItemHomeConditionalFormattingConditionalFormattingClearRules);
    ASubItemCommand.Add(TdxSpreadSheetConditionalFormattingClearRulesFromSelectedCells);
    ASubItemCommand.Add(TdxSpreadSheetConditionalFormattingClearRulesFromEntireSheet);
    ACommand.Add(TdxSpreadSheetShowConditionalFormattingRulesManager);

    RegisterActions(TdxUIGeneratorHelper.GenerateCategoryName(ACategory),
      [TdxSpreadSheetShowConditionalFormattingRulesManager, TdxSpreadSheetConditionalFormattingTopBottomRulesGallery,
      TdxSpreadSheetConditionalFormattingColorScalesGallery, TdxSpreadSheetConditionalFormattingDataBarsGallery,
      TdxSpreadSheetConditionalFormattingIconSetsGallery, TdxSpreadSheetConditionalFormattingMoreRules,
      TdxSpreadSheetConditionalFormattingNewRule, TdxSpreadSheetConditionalFormattingClearRulesFromSelectedCells,
      TdxSpreadSheetConditionalFormattingClearRulesFromEntireSheet], nil);
  end;
end;

procedure Register;
begin
  RegisterSpreadSheetUIGeneratorSchemeCategoryConditionalFormatting;
end;

end.
