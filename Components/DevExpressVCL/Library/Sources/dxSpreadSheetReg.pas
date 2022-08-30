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

unit dxSpreadSheetReg;

{$I cxVer.Inc}

interface

uses
  Classes, dxSpreadSheet, dxSpreadSheetCore, cxPC, DesignIntf, DesignEditors;

const
  dxSpreadSheetProductName = 'ExpressSpreadSheet';

procedure Register;

implementation

uses
  cxLibraryReg, dxCoreReg, dxBuiltInPopupMenu, cxClasses, dxUIGeneratorDesignHelpers, dxSpreadSheetFormulaBar;

type

  { TdxSpreadSheetComponentEditor }

  TdxSpreadSheetComponentEditor = class(TdxUIGeneratorComponentEditor)
  protected
    function GetProductName: string; override;
  end;

  { TdxSpreadSheetSelectionEditor }

  TdxSpreadSheetSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

{ TdxSpreadSheetComponentEditor }

function TdxSpreadSheetComponentEditor.GetProductName: string;
begin
  Result := dxSpreadSheetProductName;
end;

{ TdxSpreadSheetSelectionEditor }

procedure TdxSpreadSheetSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
  Proc('dxCore');
  Proc('dxCoreClasses');
  Proc('dxHashUtils');
  Proc('cxGraphics');
  Proc('dxSpreadSheetCore');
  Proc('dxSpreadSheetCoreFormulas');
  Proc('dxSpreadSheetCoreHistory');
  Proc('dxSpreadSheetCoreStyles');
  Proc('dxSpreadSheetCoreStrs');
  Proc('dxSpreadSheetConditionalFormatting');
  Proc('dxSpreadSheetConditionalFormattingRules');
  Proc('dxSpreadSheetClasses');
  Proc('dxSpreadSheetContainers');
  Proc('dxSpreadSheetFormulas');
  Proc('dxSpreadSheetHyperlinks');
  Proc('dxSpreadSheetFunctions');
  Proc('dxSpreadSheetStyles');
  Proc('dxSpreadSheetGraphics');
  Proc('dxSpreadSheetPrinting');
  Proc('dxSpreadSheetTypes');
  Proc('dxSpreadSheetUtils');
  Proc(dxSpreadSheetTextService.UnitName);
  dxSkinsRequiresAdditionalUnits(TcxPageControl, Proc);
  if not TdxBuiltInPopupMenuAdapterManager.IsActualAdapterStandard then
    Proc(cxGetUnitName(TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass));
end;

//--

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents(dxCoreLibraryProductPage, [TdxSpreadSheet, TdxSpreadSheetFormulaBar]);

  RegisterComponentEditor(TdxSpreadSheet, TdxSpreadSheetComponentEditor);
  RegisterSelectionEditor(TdxSpreadSheet, TdxSpreadSheetSelectionEditor);

  RegisterComponentEditor(TdxSpreadSheetFormulaBar, TdxSpreadSheetComponentEditor);
  RegisterSelectionEditor(TdxSpreadSheetFormulaBar, TdxSpreadSheetSelectionEditor);
end;

end.
