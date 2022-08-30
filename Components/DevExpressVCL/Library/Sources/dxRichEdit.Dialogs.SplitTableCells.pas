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

unit dxRichEdit.Dialogs.SplitTableCells;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Generics.Defaults, Generics.Collections,
  Controls, Forms, Dialogs, dxCore, dxRichEdit.Dialogs.CustomDialog, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, Menus, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit, cxCheckBox,
  cxTextEdit, cxMaskEdit, cxSpinEdit, dxMeasurementUnitEdit, dxLayoutContainer, StdCtrls, cxButtons, dxLayoutControl,
  dxLayoutLookAndFeels, cxClasses,
  dxRichEdit.Utils.Types,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.SplitTableCellsFormController,
  dxGenerics;

type
  TdxValidationEquals = record
  private
    FValue: Integer;
    FErrorMessage: string;
    FUseMessage: Boolean;
  public
    constructor Create(AValue: Integer; const AErrorMessage: string);
    function Validate(AValue: Integer): Boolean; overload;
    function Validate(AValue: Integer; AControl: TWinControl): Boolean; overload;
    property ErrorMessage: string read FErrorMessage;
    property UseMessage: Boolean read FUseMessage write FUseMessage;
  end;

  { TdxRichEditSplitTableCellsDialogForm }

  TdxRichEditSplitTableCellsDialogForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cbMergeBeforeSplit: TcxCheckBox;
    dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item5: TdxLayoutItem;
    edtNumberOfColumns: TcxSpinEdit;
    edtNumberOfRows: TcxSpinEdit;
    lciNumberOfColumns: TdxLayoutItem;
    lciNumberOfRows: TdxLayoutItem;
    lcMainGroup_Root: TdxLayoutGroup;
    procedure btnOkClick(Sender: TObject);
    procedure cbMergeBeforeSplitClick(Sender: TObject);
    procedure edtNumberOfColumnsPropertiesChange(Sender: TObject);
    procedure edtNumberOfRowsPropertiesChange(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    FValidationEquals: TdxValidationEquals;
    FIsRowsCountValid: Boolean;
    function GetController: TdxSplitTableCellsFormController; inline;
    function GetSourceRowsCount: Integer;
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure InitializeForm; override;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    procedure UpdateFormCore; override;
    procedure ValidateRowsCount;
    function Validate(AEditor: TcxSpinEdit): Boolean;

    property IsRowsCountValid: Boolean read FIsRowsCountValid;
  public
    property Controller: TdxSplitTableCellsFormController read GetController;
    property SourceRowsCount: Integer read GetSourceRowsCount;
  end;

implementation

uses
  Math,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Utils.Exceptions.Strs;

{$R *.dfm}

{ TdxRichEditSplitTableCellsDialogForm }

function TdxRichEditSplitTableCellsDialogForm.GetController: TdxSplitTableCellsFormController;
begin
  Result := TdxSplitTableCellsFormController(inherited Controller);
end;

procedure TdxRichEditSplitTableCellsDialogForm.btnOkClick(Sender: TObject);
begin
  Controller.ApplyChanges;
end;

procedure TdxRichEditSplitTableCellsDialogForm.cbMergeBeforeSplitClick(Sender: TObject);
begin
  if cbMergeBeforeSplit.Checked then
  begin
    edtNumberOfRows.Enabled := True;
    lciNumberOfRows.Enabled := True;
    edtNumberOfColumns.Value := Controller.SourceParameters.ColumnsCount;
    edtNumberOfRows.Value := SourceRowsCount;
  end
  else
  begin
    if (SourceRowsCount > 1) then
    begin
      edtNumberOfRows.Enabled := False;
      lciNumberOfRows.Enabled := False;
    end;
    edtNumberOfColumns.Value := 2;
    edtNumberOfRows.Value := 1;
  end;
  Controller.MergeCellsBeforeSplit := cbMergeBeforeSplit.Checked;
end;

procedure TdxRichEditSplitTableCellsDialogForm.EditPropertiesValidate(Sender: TObject;
  var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
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

procedure TdxRichEditSplitTableCellsDialogForm.edtNumberOfRowsPropertiesChange(Sender: TObject);
begin
  FIsRowsCountValid := Validate(edtNumberOfRows);
  Controller.RowsCount := edtNumberOfRows.Value;
  if Controller.AllowedRowsCount = nil then
    Exit;

  UnsubscribeControlsEvents;
  try
    ValidateRowsCount;
  finally
    SubscribeControlsEvents;
  end;
end;

procedure TdxRichEditSplitTableCellsDialogForm.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, ['e', 'E', '-']) then
    Key := #0;
end;

procedure TdxRichEditSplitTableCellsDialogForm.edtNumberOfColumnsPropertiesChange(Sender: TObject);
begin
  Controller.ColumnsCount := edtNumberOfColumns.Value;
end;

procedure TdxRichEditSplitTableCellsDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk then
  begin
    CanClose := IsRowsCountValid;
    if not CanClose then
    begin
      MessageDlg(FValidationEquals.ErrorMessage, mtWarning, [mbOK], 0);
      edtNumberOfRows.SetFocus;
    end;
  end;
end;

procedure TdxRichEditSplitTableCellsDialogForm.FormShow(Sender: TObject);
begin
  FValidationEquals.UseMessage := True;
end;

function TdxRichEditSplitTableCellsDialogForm.GetSourceRowsCount: Integer;
begin
  Result := Controller.SourceParameters.RowsCount;
end;

procedure TdxRichEditSplitTableCellsDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditSplitTableCellsDialogForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  cbMergeBeforeSplit.Caption := cxGetResourceString(@sdxRichEditSplitTableCellsDialogMergeBeforeSplit);
  lciNumberOfColumns.CaptionOptions.Text := cxGetResourceString(@sdxRichEditSplitTableCellsDialogNumberOfColumns);
  lciNumberOfRows.CaptionOptions.Text := cxGetResourceString(@sdxRichEditSplitTableCellsDialogNumberOfRows);
end;

function TdxRichEditSplitTableCellsDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxSplitTableCellsFormController.Create(AControllerParameters as TdxSplitTableCellsFormControllerParameters);
end;

procedure TdxRichEditSplitTableCellsDialogForm.InitializeForm;
var
  ARowsCountAfterMerge: string;
begin
  FIsRowsCountValid := True;
  ARowsCountAfterMerge := IntToStr(Controller.RowsCountAfterMerge);
  FValidationEquals := TdxValidationEquals.Create(0, Format(cxGetResourceString(@sdxRichEditExceptionInvalidDivisor), [ARowsCountAfterMerge]));
  edtNumberOfColumns.Properties.MaxValue := Controller.MaxColumnsCount;
  edtNumberOfRows.Properties.MaxValue := Controller.MaxRowsCount;
end;

procedure TdxRichEditSplitTableCellsDialogForm.SubscribeControlsEvents;
begin
  cbMergeBeforeSplit.OnClick := cbMergeBeforeSplitClick;
  edtNumberOfRows.Properties.OnChange := edtNumberOfRowsPropertiesChange;
  edtNumberOfColumns.Properties.OnChange := edtNumberOfColumnsPropertiesChange;
end;

procedure TdxRichEditSplitTableCellsDialogForm.UnsubscribeControlsEvents;
begin
  cbMergeBeforeSplit.OnClick := nil;
  edtNumberOfRows.Properties.OnChange := nil;
  edtNumberOfColumns.Properties.OnChange := nil;
end;

procedure TdxRichEditSplitTableCellsDialogForm.UpdateFormCore;
var
  ARowsCountAfterMerge: Integer;
  AIsMergeCellsBeforeSplit: Boolean;
begin
  ARowsCountAfterMerge := Controller.RowsCountAfterMerge;
  edtNumberOfRows.Properties.MaxValue := IfThen(ARowsCountAfterMerge > 1, ARowsCountAfterMerge, Controller.MaxRowsCount);

  if Controller.SourceParameters.IsSelectedCellsSquare then
  begin
    edtNumberOfRows.Value := SourceRowsCount;
    ValidateRowsCount;
  end
  else
  begin
    Controller.RowsCount := 1;
    edtNumberOfRows.Value := 1;
    edtNumberOfRows.Enabled := False;
  end;

  edtNumberOfColumns.Value := Controller.ColumnsCount;
  edtNumberOfColumns.Properties.MaxValue := Controller.MaxColumnsCount;

  AIsMergeCellsBeforeSplit := Controller.MergeCellsBeforeSplit;
  cbMergeBeforeSplit.Checked := AIsMergeCellsBeforeSplit;
  cbMergeBeforeSplit.Enabled := AIsMergeCellsBeforeSplit;
end;

function TdxRichEditSplitTableCellsDialogForm.Validate(AEditor: TcxSpinEdit): Boolean;
begin
  Result := AEditor.ValidateEdit(True);
end;

procedure TdxRichEditSplitTableCellsDialogForm.ValidateRowsCount;
var
  ACurrentValue: Integer;
  AAllowedValues: TdxIntegerList;
begin
  ACurrentValue := edtNumberOfRows.Value;
  FIsRowsCountValid := FValidationEquals.Validate(Controller.RowsCountAfterMerge mod ACurrentValue, edtNumberOfRows);
  AAllowedValues := Controller.AllowedRowsCount;
  if Assigned(AAllowedValues) and not AAllowedValues.Contains(ACurrentValue) then
    FIsRowsCountValid := false;
end;


{ TdxValidationEquals }

constructor TdxValidationEquals.Create(AValue: Integer; const AErrorMessage: string);
begin
  FValue := AValue;
  FErrorMessage := AErrorMessage;
  FUseMessage := False;
end;

function TdxValidationEquals.Validate(AValue: Integer): Boolean;
begin
  Result := AValue = FValue;
  if not Result and FUseMessage then
    MessageDlg(ErrorMessage, mtWarning, [mbOK], 0);
end;

function TdxValidationEquals.Validate(AValue: Integer; AControl: TWinControl): Boolean;
begin
  Result := Validate(AValue);
  if not Result and FUseMessage then
    AControl.SetFocus;
end;

end.
