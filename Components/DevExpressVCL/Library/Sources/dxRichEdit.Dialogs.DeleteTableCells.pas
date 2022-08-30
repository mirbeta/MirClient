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

unit dxRichEdit.Dialogs.DeleteTableCells;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, dxCore, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, Menus, dxLayoutControlAdapters, StdCtrls, cxRadioGroup, dxLayoutContainer, cxButtons,
  dxLayoutControl, dxLayoutLookAndFeels, cxClasses,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Dialogs.InsertDeleteTableCellsFormController,
  dxRichEdit.Dialogs.CustomInsertDeleteTableCells;

type
  TdxRichEditDeleteTableCellsDialogForm = class(TdxRichEditCustomInsertDeleteTableCellsDialogForm)
  private
    function GetController: TdxDeleteTableCellsFormController;
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
  public
    property Controller: TdxDeleteTableCellsFormController read GetController;
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs;

{$R *.dfm}

{ TdxRichEditDeleteTableCellsDialogForm }

procedure TdxRichEditDeleteTableCellsDialogForm.ApplyLocalization;
begin
  inherited ApplyLocalization;
  Caption := cxGetResourceString(@sdxRichEditDeleteTableCellsDialogForm);
  rbCellOperationShiftLeft.Caption := cxGetResourceString(@sdxRichEditDeleteTableCellsDialogCellOperationShiftLeft);
  rbCellOperationShiftUp.Caption := cxGetResourceString(@sdxRichEditDeleteTableCellsDialogCellOperationShiftUp);
  rbCellOperationDeleteRow.Caption := cxGetResourceString(@sdxRichEditDeleteTableCellsDialogCellOperationDeleteRow);
  rbCellOperationDeleteColumn.Caption := cxGetResourceString(@sdxRichEditDeleteTableCellsDialogCellOperationDeleteColumn);
end;

function TdxRichEditDeleteTableCellsDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxDeleteTableCellsFormController.Create(AControllerParameters as TdxDeleteTableCellsFormControllerParameters);
end;

function TdxRichEditDeleteTableCellsDialogForm.GetController: TdxDeleteTableCellsFormController;
begin
  Result := TdxDeleteTableCellsFormController(inherited Controller);
end;

end.
