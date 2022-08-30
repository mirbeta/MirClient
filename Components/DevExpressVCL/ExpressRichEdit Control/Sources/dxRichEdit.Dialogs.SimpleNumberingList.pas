{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Dialogs.SimpleNumberingList;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, dxCore, dxRichEdit.Dialogs.CustomNumberingList, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, Menus, cxContainer, cxEdit, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, dxLayoutContainer, cxSpinEdit, dxMeasurementUnitEdit, cxTextEdit, cxMaskEdit,
  cxDropDownEdit, cxLabel, StdCtrls, cxButtons, dxLayoutControl, dxLayoutLookAndFeels, cxClasses,
  dxRichEditDialogsSimpleControl,
  dxRichEdit.Control,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.NumberingFormController;

type
  TdxRichEditSimpleNumberingListDialogForm = class(TdxRichEditCustomSimpleNumberingListForm)
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
  public
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs;

{$R *.dfm}

{ TdxRichEditCustomNumberingList1 }

procedure TdxRichEditSimpleNumberingListDialogForm.ApplyLocalization;
begin
  inherited ApplyLocalization;
  Caption := cxGetResourceString(@sdxRichEditSimpleNumberingListDialogForm);
  lblNumberFormat.Caption := cxGetResourceString(@sdxRichEditCustomNumberingListNumberFormat);
end;

function TdxRichEditSimpleNumberingListDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxSimpleNumberingListController.Create(AControllerParameters as TdxSimpleNumberingListFormControllerParameters);
end;

end.

