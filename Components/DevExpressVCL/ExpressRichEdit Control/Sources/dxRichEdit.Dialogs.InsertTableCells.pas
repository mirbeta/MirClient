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

unit dxRichEdit.Dialogs.InsertTableCells;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, dxCore, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, Menus, dxLayoutControlAdapters, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer,
  StdCtrls, cxRadioGroup, cxButtons, dxLayoutControl,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Dialogs.InsertDeleteTableCellsFormController,
  dxRichEdit.Dialogs.CustomInsertDeleteTableCells;

type
  TdxRichEditInsertTableCellsDialogForm = class(TdxRichEditCustomInsertDeleteTableCellsDialogForm)
  private
    function GetController: TdxInsertTableCellsFormController; inline;
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
  public
    property Controller: TdxInsertTableCellsFormController read GetController;
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs;

{$R *.dfm}

{ TdxRichEditInsertTableCellsDialogForm }

procedure TdxRichEditInsertTableCellsDialogForm.ApplyLocalization;
begin
  inherited ApplyLocalization;
  Caption := cxGetResourceString(@sdxRichEditInsertTableCellsDialogForm);
  rbCellOperationShiftLeft.Caption := cxGetResourceString(@sdxRichEditInsertTableCellsDialogCellOperationShiftLeft);
  rbCellOperationShiftUp.Caption := cxGetResourceString(@sdxRichEditInsertTableCellsDialogCellOperationShiftUp);
  rbCellOperationDeleteRow.Caption := cxGetResourceString(@sdxRichEditInsertTableCellsDialogCellOperationDeleteRow);
  rbCellOperationDeleteColumn.Caption := cxGetResourceString(@sdxRichEditInsertTableCellsDialogCellOperationDeleteColumn);
end;

function TdxRichEditInsertTableCellsDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxInsertTableCellsFormController.Create(AControllerParameters as TdxInsertTableCellsFormControllerParameters);
end;

function TdxRichEditInsertTableCellsDialogForm.GetController: TdxInsertTableCellsFormController;
begin
  Result := TdxInsertTableCellsFormController(inherited Controller);
end;

end.
