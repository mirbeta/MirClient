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

unit dxRichEdit.Dialogs.CustomInsertDeleteTableCells;

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
  dxRichEdit.Dialogs.InsertDeleteTableCellsFormController,
  dxRichEdit.Utils.Properties;

type
  TdxRichEditCustomInsertDeleteTableCellsDialogForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item3: TdxLayoutItem;
    dxLayoutControl1Item4: TdxLayoutItem;
    dxLayoutControl1Item5: TdxLayoutItem;
    dxLayoutControl1Item6: TdxLayoutItem;
    lcMainGroup_Root: TdxLayoutGroup;
    rbCellOperationDeleteColumn: TcxRadioButton;
    rbCellOperationDeleteRow: TcxRadioButton;
    rbCellOperationShiftLeft: TcxRadioButton;
    rbCellOperationShiftUp: TcxRadioButton;
    procedure btnOkClick(Sender: TObject);
    procedure CellOperationDblClick(Sender: TObject);
  private
    procedure CellOperationChanged(Sender: TObject);
    function GetCellOperation: TdxTableCellOperation;
    function GetController: TdxInsertDeleteTableCellsFormController;
    procedure SetCellOperation(const Value: TdxTableCellOperation);
  protected
    procedure ApplyLocalization; override;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    procedure UpdateFormCore; override;
    property Controller: TdxInsertDeleteTableCellsFormController read GetController;
  public
    property CellOperation: TdxTableCellOperation read GetCellOperation write SetCellOperation;
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs;

{$R *.dfm}

{ TdxRichEditCustomInsertDeleteTableCellsDialogForm }

procedure TdxRichEditCustomInsertDeleteTableCellsDialogForm.btnOkClick(Sender: TObject);
begin
  Controller.ApplyChanges;
end;

procedure TdxRichEditCustomInsertDeleteTableCellsDialogForm.ApplyLocalization;
begin
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
end;

procedure TdxRichEditCustomInsertDeleteTableCellsDialogForm.SubscribeControlsEvents;
begin
  rbCellOperationDeleteColumn.OnClick := CellOperationChanged;
  rbCellOperationDeleteRow.OnClick := CellOperationChanged;
  rbCellOperationShiftLeft.OnClick := CellOperationChanged;
  rbCellOperationShiftUp.OnClick := CellOperationChanged;
end;

procedure TdxRichEditCustomInsertDeleteTableCellsDialogForm.UnsubscribeControlsEvents;
begin
  rbCellOperationDeleteColumn.OnClick := nil;
  rbCellOperationDeleteRow.OnClick := nil;
  rbCellOperationShiftLeft.OnClick := nil;
  rbCellOperationShiftUp.OnClick := nil;
end;

procedure TdxRichEditCustomInsertDeleteTableCellsDialogForm.UpdateFormCore;
begin
  CellOperation := Controller.CellOperation;
end;

procedure TdxRichEditCustomInsertDeleteTableCellsDialogForm.CellOperationChanged(Sender: TObject);
begin
  Controller.CellOperation := CellOperation;
end;

function TdxRichEditCustomInsertDeleteTableCellsDialogForm.GetCellOperation: TdxTableCellOperation;
begin
  if rbCellOperationShiftLeft.Checked then
    Result := TdxTableCellOperation.ShiftToTheHorizontally
  else
    if rbCellOperationShiftUp.Checked then
      Result := TdxTableCellOperation.ShiftToTheVertically
    else
      if rbCellOperationDeleteRow.Checked then
        Result := TdxTableCellOperation.RowOperation
      else
        if rbCellOperationDeleteColumn.Checked then
          Result := TdxTableCellOperation.ColumnOperation
        else
          Result := TdxTableCellOperation.ShiftToTheHorizontally;
end;

function TdxRichEditCustomInsertDeleteTableCellsDialogForm.GetController: TdxInsertDeleteTableCellsFormController;
begin
  Result := TdxInsertDeleteTableCellsFormController(inherited Controller);
end;

procedure TdxRichEditCustomInsertDeleteTableCellsDialogForm.CellOperationDblClick(Sender: TObject);
begin
  Controller.ApplyChanges;
  ModalResult := mrOk;
end;

procedure TdxRichEditCustomInsertDeleteTableCellsDialogForm.SetCellOperation(const Value: TdxTableCellOperation);
begin
  case Value of
    TdxTableCellOperation.ShiftToTheHorizontally:
      rbCellOperationShiftLeft.Checked := True;
    TdxTableCellOperation.ShiftToTheVertically:
      rbCellOperationShiftUp.Checked := True;
    TdxTableCellOperation.RowOperation:
      rbCellOperationDeleteRow.Checked := True;
    TdxTableCellOperation.ColumnOperation:
      rbCellOperationDeleteColumn.Checked := True;
  end;
end;

end.
