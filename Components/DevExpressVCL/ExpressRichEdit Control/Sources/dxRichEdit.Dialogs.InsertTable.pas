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

unit dxRichEdit.Dialogs.InsertTable;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxCore, dxRichEdit.Dialogs.CustomDialog, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  dxLayoutLookAndFeels, cxClasses, dxLayoutContainer, dxLayoutControl, cxContainer, cxEdit, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, Menus, StdCtrls, cxButtons, cxTextEdit, cxMaskEdit, cxSpinEdit, cxLabel,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Dialogs.InsertTableFormController,
  dxRichEdit.Dialogs.Core;

type
  TdxRichEditInsertTableForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Item4: TdxLayoutItem;
    dxLayoutControl1Item5: TdxLayoutItem;
    edtColumns: TcxSpinEdit;
    edtRows: TcxSpinEdit;
    lblTableSize: TdxLayoutSeparatorItem;
    liColumns: TdxLayoutItem;
    liRows: TdxLayoutItem;
    procedure btnOkClick(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure edtPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
  private
    function GetController: TdxInsertTableFormController; inline;
    procedure edtColumnsPropertiesChange(Sender: TObject);
    procedure edtRowsPropertiesChange(Sender: TObject);
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    procedure UpdateFormCore; override;
  public
    property Controller: TdxInsertTableFormController read GetController;
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs;

{$R *.dfm}

{ TdxRichEditInsertTableForm }

procedure TdxRichEditInsertTableForm.btnOkClick(Sender: TObject);
begin
  Controller.ApplyChanges;
end;

procedure TdxRichEditInsertTableForm.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, ['e', 'E', '-']) then
    Key := #0;
end;

procedure TdxRichEditInsertTableForm.edtPropertiesValidate(Sender: TObject; var DisplayValue: Variant;
  var ErrorText: TCaption; var Error: Boolean);
var
  AEditor: TcxSpinEdit;
  AMinValue, AMaxValue: Integer;
begin
  if Error then
  begin
    AEditor := Sender as TcxSpinEdit;
    AMinValue := Trunc(AEditor.Properties.MinValue);
    AMaxValue := Trunc(AEditor.Properties.MaxValue);
    ErrorText := Format(cxGetResourceString(@sdxRichEditInvalidSize), [AMinValue, AMaxValue]);
  end;
end;

function TdxRichEditInsertTableForm.GetController: TdxInsertTableFormController;
begin
  Result := TdxInsertTableFormController(inherited Controller);
end;

procedure TdxRichEditInsertTableForm.edtColumnsPropertiesChange(Sender: TObject);
begin
  if DoValidate(edtColumns) then
    Controller.ColumnCount := edtColumns.Value
end;

procedure TdxRichEditInsertTableForm.edtRowsPropertiesChange(Sender: TObject);
begin
  if DoValidate(edtRows) then
    Controller.RowCount := edtRows.Value;
end;

procedure TdxRichEditInsertTableForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditInsertTableForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  lblTableSize.Caption := cxGetResourceString(@sdxRichEditInsertTableTableSize);
  liColumns.CaptionOptions.Text := cxGetResourceString(@sdxRichEditInsertTableColumns);
  liRows.CaptionOptions.Text := cxGetResourceString(@sdxRichEditInsertTableRows);
end;

function TdxRichEditInsertTableForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxInsertTableFormController.Create(AControllerParameters as TdxInsertTableFormControllerParameters);
end;

procedure TdxRichEditInsertTableForm.SubscribeControlsEvents;
begin
  edtRows.Properties.OnChange := edtRowsPropertiesChange;
  edtColumns.Properties.OnChange := edtColumnsPropertiesChange;
end;

procedure TdxRichEditInsertTableForm.UnsubscribeControlsEvents;
begin
  edtRows.Properties.OnChange := nil;
  edtColumns.Properties.OnChange := nil;
end;

procedure TdxRichEditInsertTableForm.UpdateFormCore;
begin
  edtColumns.Value := Controller.ColumnCount;
  edtRows.Value := Controller.RowCount;
end;

end.
